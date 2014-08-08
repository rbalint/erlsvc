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

-module(erlsvc_lib).

%% Command modules handling.
-export([
    erlsvc_cm_deps/0,
    is_command_module/1,
    send_module/2,
    mods_not_uploaded/1
  ]).

%% Node monitoring.
-export([
    wait_nodeup/1,
    wait_nodeup/2,
    wait_nodedown/1,
    wait_nodedown/2
  ]).

%% Syslog loglevels.
-export([
    any_to_loglevel/1
  ]).

%% Report handling.
-export([
    report_ready/0,
    report_cmd_result/0,
    report_cmd_result/1,
    report_cmd_exception/2,
    report_cmd_exception/3,
    report_error/1,
    report_error/2,
    report_warning/1,
    report_warning/2,
    report_info/1,
    report_info/2,
    report_debug/1,
    report_debug/2,
    report_debug/3,
    report_waiting/1
  ]).

%% -------------------------------------------------------------------
%% Command modules handling.
%% -------------------------------------------------------------------

erlsvc_cm_deps() ->
    [].

is_command_module(Mod) ->
    catch Mod:module_info(),
    erlang:function_exported(Mod, erlsvc_cm_deps, 0).

send_module(Node, _) when Node == node() ->
    ok;
send_module(Node, Mod) ->
    %% A command module may depend on other erlsvc Erlang modules. The
    %% function erlsvc_cm_deps/0 returns a list of those modules.
    All_Mods = get_cm_deps(Mod, []),
    %% We can now send all those modules to the node.
    send_modules2(Node, All_Mods).

get_cm_deps(Mod, All_Deps) ->
    case lists:member(Mod, All_Deps) of
        true  -> All_Deps;
        false -> get_cm_deps2(Mod:erlsvc_cm_deps(), lists:sort([Mod | All_Deps]))
    end.

get_cm_deps2([Mod | Rest], All_Deps) ->
    Mod_Deps = get_cm_deps(Mod, []),
    New_Deps = lists:umerge(All_Deps, lists:sort(Mod_Deps)),
    get_cm_deps2(Rest, New_Deps);
get_cm_deps2([], All_Deps) ->
    All_Deps.

send_modules2(Node, [Mod | Rest]) ->
    case is_mod_uploaded(Node, Mod) of
        true ->
            send_modules2(Node, Rest);
        false ->
            report_debug("Upload module '~s' from '~s' to ~s~n",
              [Mod, node(), Node]),
            Ret = do_send_module(Node, Mod),
            case Ret of
                {module, _} ->
                    %% This module was loaded correctly. Record it and
                    %% continue with the next one.
                    mod_uploaded(Node, Mod),
                    send_modules2(Node, Rest);
                {error, Reason} ->
                    %% Failed to load this module!
                    {error, Mod, Reason}
            end
    end;
send_modules2(_, []) ->
    ok.

is_mod_uploaded(Node, Mod) ->
    Key = {erlsvc_uploaded_mods, Node},
    case erlang:get(Key) of
        undefined -> false;
        Mods      -> lists:member(Mod, Mods)
    end.

mod_uploaded(Node, Mod) ->
    Key = {erlsvc_uploaded_mods, Node},
    Previous_Mods = case erlang:get(Key) of
        undefined -> [];
        Mods      -> Mods
    end,
    erlang:put(Key, [Mod | Previous_Mods]).

mods_not_uploaded(Node) ->
    Key = {erlsvc_uploaded_mods, Node},
    erlang:erase(Key).

do_send_module(Node, Mod) ->
    {_, Bin, File} = code:get_object_code(Mod),
    do_send_module2(Node, Mod, Bin, File).

do_send_module2(Node, Mod, Bin, File) ->
    Ret = rpc:call(Node, code, load_binary, [Mod, File, Bin], infinity),
    case Ret of
        {badrpc, {'EXIT', {badarg, [{code_server, call, 2} | _]}}} ->
            %% Sometimes, remote code_server isn't ready but the exact
            %% reason couldn't be found. To work around this case, we
            %% try the upload again. This could lead to an infinite
            %% loop, though.
            report_debug("Retry module '~s' upload~n", [Mod]),
            timer:sleep(100),
            do_send_module2(Node, Mod, Bin, File);
        {badrpc, _} ->
            {error, Ret};
        _ ->
            Ret
    end.

%% -------------------------------------------------------------------
%% Node monitoring.
%% -------------------------------------------------------------------

wait_nodeup(Node) ->
    wait_nodeup(Node, infinity).

wait_nodeup(Node, Timeout) ->
    report_debug("Wait for node '~s' to start:", [Node]),
    wait_nodeup2(Node, Timeout).

wait_nodeup2(Node, Timeout) ->
    {Time, Ret} = timer:tc(net_adm, ping, [Node]),
    case Ret of
        pong ->
            report_debug(" ok", []),
            ok;
        pang ->
            timer:sleep(100),
            case Timeout of
                infinity ->
                    report_debug(" (retry)", []),
                    wait_nodeup2(Node, Timeout);
                _ ->
                    Timeout2 = Timeout - (Time / 1000) - 100,
                    if
                        Timeout2 =< 0 ->
                            report_debug(" failed~n", []),
                            error;
                        true ->
                            report_debug(" (retry)", []),
                            wait_nodeup2(Node, Timeout2)
                    end
            end
    end.

wait_nodedown(Node) ->
    wait_nodedown(Node, infinity).

wait_nodedown(Node, Timeout) ->
    report_debug("Wait for node '~s' to stop:", [Node]),
    erlang:monitor_node(Node, true),
    receive
        {nodedown, Node} ->
            report_debug(" ok~n"),
            ok
    after Timeout ->
            report_debug(" failed~n"),
            error
    end.

%% -------------------------------------------------------------------
%% Syslog loglevels.
%% -------------------------------------------------------------------

any_to_loglevel(debug)       -> debug;
any_to_loglevel(info)        -> info;
any_to_loglevel(notice)      -> notice;
any_to_loglevel(warning)     -> warning;
any_to_loglevel(error)       -> error;
any_to_loglevel(critical)    -> critical;
any_to_loglevel(alert)       -> alert;
any_to_loglevel(emergency)   -> emergency;
any_to_loglevel("debug")     -> debug;
any_to_loglevel("info")      -> info;
any_to_loglevel("notice")    -> notice;
any_to_loglevel("warning")   -> warning;
any_to_loglevel("error")     -> error;
any_to_loglevel("critical")  -> critical;
any_to_loglevel("alert")     -> alert;
any_to_loglevel("emergency") -> emergency;
any_to_loglevel("7")         -> debug;
any_to_loglevel("6")         -> info;
any_to_loglevel("5")         -> notice;
any_to_loglevel("4")         -> warning;
any_to_loglevel("3")         -> error;
any_to_loglevel("2")         -> critical;
any_to_loglevel("1")         -> alert;
any_to_loglevel("0")         -> emergency;
any_to_loglevel(7)           -> debug;
any_to_loglevel(6)           -> info;
any_to_loglevel(5)           -> notice;
any_to_loglevel(4)           -> warning;
any_to_loglevel(3)           -> error;
any_to_loglevel(2)           -> critical;
any_to_loglevel(1)           -> alert;
any_to_loglevel(0)           -> emergency;
any_to_loglevel(_)           -> undefined.

%% -------------------------------------------------------------------
%% Report handling.
%% -------------------------------------------------------------------

report_ready() ->
    io:format("CTL READY~n", []).

report_cmd_result() ->
    io:format(
      "CTL RESULT BEGIN~n"
      "status: ok~n"
      "CTL RESULT END~n",
      []).

report_cmd_result({Status, Result}) when Status == ok orelse Status == error ->
    Formatted = case Result of
        {yaml, YAML} ->
            io_lib:format(
              "return:~n"
              "~s", [YAML]);
        _ ->
            case io_lib:deep_char_list(Result) of
                true ->
                    Lines = string:tokens(lists:flatten(Result), "\n"),
                    io_lib:format(
                      "return: |~n"
                      "~s", [["  " ++ Line ++ "\n" || Line <- Lines]]);
                false ->
                    io_lib:format(
                      "return: |~n"
                      "  ~p~n", [Result])
            end
    end,
    io:format(
      "CTL RESULT BEGIN~n"
      "status: ~s~n"
      "~s"
      "CTL RESULT END~n",
      [Status, Formatted]);
report_cmd_result(Result) ->
    report_cmd_result({ok, Result}).

report_cmd_exception(Format, Fmt_Args) ->
    report_cmd_exception(Format, Fmt_Args, []).

report_cmd_exception(Format, Fmt_Args, Stacktrace) ->
    Reason1 = io_lib:format(Format, Fmt_Args),
    Reason2 = re:replace(Reason1, "^", "  ", [global, multiline]),
    Stacktrace2 = case Stacktrace of
        [] ->
            "";
        _ ->
            "stacktrace:\n" ++ [
              if
                  is_integer(Args) ->
                      io_lib:format("  - \"~s:~s/~b\"~n", [Mod, Fun, Args]);
                  true ->
                      io_lib:format("  - |~n    ~s:~s(~p)~n", [Mod, Fun, Args])
              end
              || {Mod, Fun, Args} <- Stacktrace
            ]
    end,
    io:format(
      "CTL RESULT BEGIN~n"
      "status: exception~n"
      "reason: |~n"
      "~s~n"
      "~s"
      "CTL RESULT END~n",
      [Reason2, Stacktrace2]).

report_error(Format) ->
    report_error(Format, []).

report_error(Format, Args) ->
    report_log("ERROR", Format, Args).

report_warning(Format) ->
    report_warning(Format, []).

report_warning(Format, Args) ->
    report_log("WARNING", Format, Args).

report_info(Format) ->
    report_info(Format, []).

report_info(Format, Args) ->
    report_log("INFO", Format, Args).

report_debug(Format) ->
    report_debug(Format, []).

report_debug(Format, Args) ->
    report_log("ERLSCRIPT", Format, Args).

report_debug(Facility, Format, Args) ->
    report_log(Facility, Format, Args).

report_log(Level, Format, Args) ->
    Message1 = lists:flatten(io_lib:format(Format, Args)),
    {Newline_Terminated, Message2} = case lists:reverse(Message1) of
        [$\n | Rest] -> {true,  lists:reverse(Rest)};
        _            -> {false, Message1}
    end,
    Message3 = re:replace(Message2, "^", "CTL LOG " ++ Level ++ " ",
      [multiline, global, {return, iodata}]),
    if
        Newline_Terminated -> io:format("~s~n", [Message3]);
        true               -> io:format("~s NONL~n", [Message3])
    end.

report_waiting(true) ->
    io:format("CTL WAITING START~n");
report_waiting(false) ->
    io:format("CTL WAITING STOP~n").
