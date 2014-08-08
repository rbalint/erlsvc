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

-module(erlsvc_controller).

-behaviour(gen_fsm).

-include("erlsvc.hrl").

-export([
    start_link/1
  ]).

%% gen_fsm's callbacks.
-export([
    init/1,
    handle_event/3,
    handle_sync_event/4,
    handle_info/3,
    terminate/3,
    code_change/4,

    next_command/2,
    wait_for_command/2
  ]).

-record(state, {
    parent,
    node,
    worker
  }).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

start_link(Node) ->
    Parent = self(),
    gen_fsm:start_link(?MODULE, [Parent, Node], []).

%% -------------------------------------------------------------------
%% Command execution.
%% -------------------------------------------------------------------

exec_command(#state{worker = Worker} = State, Command) ->
    try
        %% Send the command to the worker.
        Ret = case erlsvc_worker:exec_command(Worker, Command) of
            {exception, Exception1, Stacktrace1} ->
                {command, exception, Exception1, Stacktrace1};
            Result ->
                {command, return, Result}
        end,
        %% Report the result.
        gen_fsm:send_event(self(), Ret),
        State
    catch
        _:Exception ->
            %% Couldn't execute the command: return the exception and
            %% the stacktrace.
            Stacktrace = erlang:get_stacktrace(),
            gen_fsm:send_event(self(),
              {command, exception, Exception, Stacktrace}),
            State
    end.

%% -------------------------------------------------------------------
%% gen_fsm's callbacks.
%% -------------------------------------------------------------------

init([Parent, Node]) ->
    process_flag(trap_exit, true),
    State = #state{
      parent = Parent
    },
    case target_node(State, Node) of
        {ok, State2} ->
            erlsvc_lib:report_ready(),
            {ok, next_command, State2, 0};
        Ret ->
            {stop, Ret}
    end.

next_command(timeout, State) ->
    %% We read the next command from stdin.
    Prompt = case os:getenv("ERLSVC") of
        false -> "wctl> ";
        _     -> ""
    end,
    case io:read(standard_io, Prompt) of
        {ok, {target_node, Node}} ->
            case target_node(State, Node) of
                {ok, State2} ->
                    erlsvc_lib:report_cmd_result(),
                    {next_state, next_command, State2, 0};
                {error, Code} ->
                    {next_state, {exit, Code}, State}
            end;
        {ok, {Fun, Args}} when is_atom(Fun), is_list(Args) ->
            %% The peer sent a command for execution.
            State2 = exec_command(State, {Fun, Args}),
            {next_state, wait_for_command, State2};
        {ok, {Mod, Fun, Args}} when is_atom(Mod), is_atom(Fun), is_list(Args) ->
            %% The peer sent a command for execution.
            State2 = exec_command(State, {Mod, Fun, Args}),
            {next_state, wait_for_command, State2};
        {ok, stop} ->
            %% The peer doesn't need this node anymore, exit.
            {stop, normal, State};
        {ok, Bad_Command} ->
            %% The term sent by the peer is not expected.
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Unexpected command:~n"
              "  ~p~n", [Bad_Command]),
            {stop, {exit, ?EX_USAGE}, State};
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to parse command:~n"
              "  ~p~n", [Reason]),
            {stop, {exit, ?EX_PROTOCOL}, State};
        eof ->
            {stop, normal, State}
    end.

wait_for_command({command, return, Ret}, State) ->
    %% The command finished: we can format and print the result.
    erlsvc_lib:report_cmd_result(Ret),
    {next_state, next_command, State, 0};
wait_for_command({command, exception, Exception, Stacktrace}, State) ->
    %% The command crashed: we format and print the exception.
    erlsvc_lib:report_cmd_exception("Failed to execute the command:~n~p~n",
      [Exception], Stacktrace),
    {next_state, next_command, State, 0}.

handle_event(_, State_Name, State) ->
    {next_state, State_Name, State}.

handle_sync_event(_, _, State_Name, State) ->
    {reply, ok, State_Name, State}.

handle_info({nodedown, Node}, State_Name, State) ->
    % A node stopped: it doesn't have the uploaded modules anymore.
    erlsvc_lib:mods_not_uploaded(Node),
    {next_state, State_Name, State};

handle_info({'EXIT', Parent, Reason}, _, #state{parent = Parent} = State) ->
    erlsvc_lib:report_error(
      "Problem:~n"
      "  Lost link with parent:~n"
      "  ~p~n", [Reason]),
    {stop, normal, State};
handle_info({'EXIT', Worker, Reason}, _, #state{worker = Worker} = State) ->
    erlsvc_lib:report_error(
      "Problem:~n"
      "  Lost link with worker:~n"
      "  ~p~n", [Reason]),
    {stop, {exit, ?EX_UNAVAILABLE}, State}.

terminate(Reason, _, #state{parent = Parent, worker = Worker}) ->
    %% Stop the worker process.
    erlsvc_worker:stop(Worker),
    %% Remove the link with the parent only when we terminate normally
    %% or with a specified exit code.
    erlsvc_lib:report_debug("Erlang controller node terminating~n", []),
    case Reason of
        normal ->
            erlang:unlink(Parent),
            Parent ! {controller, done, ?EX_OK};
        {exit, Code} ->
            erlang:unlink(Parent),
            Parent ! {controller, done, Code};
        _ ->
            ok
    end.

code_change(_, State_Name, State, _) ->
    {ok, State_Name, State}.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

target_node(#state{node = Prev_Node, worker = Prev_Worker} = State, Node0) ->
    case norm_node_name(Node0) of
        undefined ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Invalid node name:~n"
              "  ~s~n", [Node0]),
            {error, badarg};
        Prev_Node ->
            {ok, State};
        Node ->
            case Node of
                self ->
                    erlsvc_lib:report_debug(
                      "Install worker on controller node~n", []),
                    ok;
                _ ->
                    erlsvc_lib:report_debug(
                      "Install worker on node '~s'~n", [Node]),
                    erlsvc_lib:wait_nodeup(Node),
                    erlang:monitor_node(Node, true)
            end,
            case erlsvc_worker:start_link(Node) of
                {ok, Worker} ->
                    case Prev_Node of
                        undefined -> ok;
                        _         -> erlsvc_worker:stop(Prev_Worker)
                    end,
                    State2 = State#state{
                      node   = Node,
                      worker = Worker
                    },
                    {ok, State2};
                Ret ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Failed to start worker on node ~s:~n"
                      "  ~p~n", [Node0, Ret]),
                    {error, nodedown}
            end
    end.

norm_node_name(Node_Name) ->
    %% Check and eventually convert the target node name.
    case Node_Name of
        self ->
            self;
        undefined ->
            self;
        _ when is_atom(Node_Name) ->
            Node_Name;
        _ ->
            case io_lib:deep_char_list(Node_Name) of
                true  -> list_to_atom(lists:flatten(Node_Name));
                false -> undefined
            end
    end.
