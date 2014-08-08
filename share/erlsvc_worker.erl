%%-
%% Copyright 2011 Yakaz. All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 
%%    1. Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%% 
%%    2. Redistributions in binary form must reproduce the above
%%       copyright notice, this list of conditions and the following
%%       disclaimer in the documentation and/or other materials provided
%%       with the distribution.
%% 
%% THIS SOFTWARE IS PROVIDED BY YAKAZ ``AS IS'' AND ANY EXPRESS OR
%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL YAKAZ OR CONTRIBUTORS BE LIABLE
%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
%% OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
%% OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(erlsvc_worker).

-behaviour(gen_server).

-include("erlsvc.hrl").

%% Public API.
-export([
    erlsvc_cm_deps/0,
    start_link/1,
    stop/1,
    silence/1,
    exec_command/2
  ]).

%% Basic commands.
-export([
    system_version/0,
    ulimit/0,
    test_log_error/0,
    test_log_warning/0,
    test_log_info/0,
    test_log_nonl/0,
    test_waiting/0
  ]).

%% gen_server's callbacks.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
  ]).

-record(state, {
    controller,
    use_syslog,
    silent = false
  }).

-define(INFO(Fmt, Args),    catch syslog:info_msg(erlsvc, Fmt, Args)).
-define(WARNING(Fmt, Args), catch syslog:warning_msg(erlsvc, Fmt, Args)).
-define(ERROR(Fmt, Args),   catch syslog:error_msg(erlsvc, Fmt, Args)).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

erlsvc_cm_deps() ->
    [erlsvc_lib].

start_link(self) ->
    %% Spawn the worker locally.
    gen_server:start_link(?MODULE, [self()], []);
start_link(Node) when Node == node() ->
    start_link(self);
start_link(Node) when is_atom(Node) ->
    %% We want to execute the worker on a remote node, therefore we
    %% first load this module on the remote node.
    case erlsvc_lib:send_module(Node, ?MODULE) of
        ok ->
            %% Spawn the worker remotely.
            Ret = rpc:call(Node, gen_server, start, [?MODULE, [self()], []]),
            case Ret of
                {ok, Pid} ->
                    erlang:link(Pid),
                    {ok, Pid};
                _ ->
                    Ret
            end;
        Ret ->
            Ret
    end.

stop(Worker) ->
    gen_server:cast(Worker, stop).

silence(Worker) ->
    gen_server:cast(Worker, silence).

exec_command(Worker, {Fun, Args}) ->
    Command = {?MODULE, Fun, Args},
    do_exec_command(Worker, Command);
exec_command(Worker, {Mod, _, _} = Command) ->
    %% The command may be provided by a commands module. All commands
    %% modules export the "erlsvc_cm_deps/0" function.
    case erlsvc_lib:is_command_module(Mod) of
        true ->
            case erlsvc_lib:send_module(node(Worker), Mod) of
                ok ->
                    ok;
                {error, Mod, Reason} ->
                    erlang:throw({module_load_failed, Mod, Reason})
            end;
        false ->
            %% The module isn't a command module or doesn't exist at
            %% all: nothing to upload.
            ok
    end,
    do_exec_command(Worker, Command).

do_exec_command(Worker, Command) ->
    gen_server:call(Worker, {command, Command}, infinity).

%% -------------------------------------------------------------------
%% Internal commands.
%% -------------------------------------------------------------------

system_version() ->
    lists:flatten(io_lib:format("~s", [erlang:system_info(system_version)])).

ulimit() ->
    io_lib:format("~s", [os:cmd("bash -c 'ulimit -a'")]).

test_log_error() ->
    erlsvc_lib:report_error("Log from test_log_error/0~n", []).

test_log_warning() ->
    erlsvc_lib:report_warning("Log from test_log_warning/0~n", []).

test_log_info() ->
    erlsvc_lib:report_warning("Log from test_log_info/0~n", []).

test_log_nonl() ->
    erlsvc_lib:report_debug("Log from test_log_nonl/0:", []),
    erlsvc_lib:report_debug(" 1", []),
    erlsvc_lib:report_debug(" 2", []),
    erlsvc_lib:report_debug(" 3~n", []).

test_waiting() ->
    erlsvc_lib:report_debug("Log from test_waiting/0:", []),
    erlsvc_lib:report_waiting(true),
    erlsvc_lib:report_debug(" 1", []),
    timer:sleep(1000),
    erlsvc_lib:report_debug(" 2", []),
    timer:sleep(1000),
    erlsvc_lib:report_debug(" 3", []).

%% -------------------------------------------------------------------
%% gen_server's callbacks.
%% -------------------------------------------------------------------

init([Controller]) ->
    process_flag(trap_exit, true),
    Use_Syslog = try
        syslog:add(erlsvc, "erlsvc", daemon, info, [log_pid]),
        true
    catch
        _:_ ->
            false
    end,
    ?INFO("Worker started by controller ~p on node ~s",
      [Controller, erlang:node(Controller)]),
    State = #state{
      controller = Controller,
      use_syslog = Use_Syslog
    },
    {ok, State}.

handle_call({command, {Mod, Fun, Args}}, _, State) ->
    Reply = try
        %% Log what we're about to do.
        Args_S = case Args of
            [] ->
                "";
            _ ->
                lists:flatten("\n" ++ string:join(
                    [io_lib:format("    ~p", [Arg]) || Arg <- Args],
                    ",\n"))
        end,
        case State#state.silent of
            true ->
                ok;
            false ->
                report_debug("Execute on node '~s':~n  ~s:~s(~s)~n",
                  [node(), Mod, Fun, Args_S])
        end,
        %% Execute the command.
        apply(Mod, Fun, Args)
    catch
        _:Exception ->
            %% The command crashed: return the exception and the stacktrace.
            Stacktrace = erlang:get_stacktrace(),
            {exception, Exception, Stacktrace}
    end,
    {reply, Reply, State}.

handle_cast(silence, State) ->
    State2 = State#state{
      silent = true
    },
    {noreply, State2};

handle_cast(stop, #state{controller = Controller} = State) ->
    erlang:unlink(Controller),
    {stop, normal, State}.

handle_info({'EXIT', Controller, Reason},
  #state{controller = Controller} = State) ->
    ?ERROR("Lost link with controller:~n~p", [Reason]),
    {stop, {controller_exited, Reason}, State};

handle_info({'EXIT', _, _}, State) ->
    %% For instance, *_app:set_loglevel/1 uses compile:file/2 which
    %% spawn_link a temporary process. We ignore the EXIT signal from
    %% this process.
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    ?INFO("Received an out-of-sync nodedown event for node '~s', ignored~n",
      [Node]),
    {noreply, State}.

terminate(_, #state{use_syslog = Use_Syslog}) ->
    ?INFO("Worker stopped", []),
    if
        Use_Syslog -> syslog:remove(erlsvc);
        true       -> ok
    end.

code_change(_, State, _) ->
    {ok, State}.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

report_debug(Format, Args) ->
    ?INFO(Format, Args),
    erlsvc_lib:report_debug(Format, Args).
