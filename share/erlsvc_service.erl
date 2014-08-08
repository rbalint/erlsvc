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

-module(erlsvc_service).

%% Public API.
-export([
    erlsvc_cm_deps/0,
    is_running/0,
    watch_start/1,
    reload_config/0,
    stop/2,
    get_start_opts/0
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

erlsvc_cm_deps() ->
    [erlsvc_lib, erlsvc_erlenv, erlsvc_release].

is_running() ->
    %% First check: The environment variable WCTL_START_OPTS must be
    %% set. This indicates that the node was started by erlsvc in the
    %% first place.
    %% TODO: When all deployed nodes are up-to-date, change this to
    %% "WCTL_VERSION".
    case os:getenv("WCTL_START_OPTS") of
        false ->
            erlsvc_lib:report_debug(
              "The node was not started with \"erlsvc start\"~n", []),
            false;
        _ ->
            %% Now, we need to get the list of applications started by
            %% the current release.
            case erlsvc_release:current() of
                {_, Rel_Name, _, _, _} ->
                    case get_expected_apps(Rel_Name) of
                        undefined ->
                            false;
                        Expected ->
                            %% We compare this list to the currently
                            %% running applications. The expected
                            %% applications must be running, though
                            %% additional applications may be started
                            %% too.
                            Running = get_running_apps(),
                            case Expected -- Running of
                                [] ->
                                    true;
                                Diff ->
                                    Diff_S = [
                                      io_lib:format("  ~s ~s~n", [A, V])
                                      || {A, V} <- Diff],
                                    erlsvc_lib:report_debug(
                                      "The following applications are "
                                      "missing:~n"
                                      "~s", [Diff_S]),
                                    false
                            end
                    end;
                undefined ->
                    false
            end
    end.

watch_start(Options) ->
    erlsvc_lib:report_debug("Waiting for SASL's release_handler~n", []),
    wait_for_sasl(),
    {_, Rel_Name} = init:script_id(),
    case get_expected_apps(Rel_Name) of
        undefined ->
            {error, failed};
        Expected ->
            Verbose = proplists:get_bool(verbose, Options),
            do_watch_start(Expected, Verbose),
            done
    end.

reload_config() ->
    %% The following code was taken from a post on erlang-questions@ by
    %% Serge Aleynikov.
    %% http://www.erlang.org/pipermail/erlang-questions/2006-July/021543.html
    case init:get_argument(config) of
        {ok, [Files]} ->
            Conf_Files = [
              begin
                  S = filename:basename(F, ".config"),
                  filename:join(filename:dirname(F), S ++ ".config")
              end || F <- Files],
            %% Move sys.config to the head of the list
            Config = lists:sort(fun
                  ("sys.config", _) -> true;
                  (_, _)            -> false end,
              Conf_Files),
            Old_Env = application_controller:prep_config_change(),
            Apps = [{application, A, make_appl(A)}
              || {A, _, _} <- application:which_applications()],
            application_controller:change_application_data(Apps, Config),
            case application_controller:config_change(Old_Env) of
                ok ->
                    ok;
                {error, Reason} ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Failed to reload configuration.~n"
                      "  Erlang reports:~n"
                      "    ~p~n", [Reason]),
                    ok
            end;
        _ ->
            erlsvc_lib:report_info("No configuration file to reload~n", []),
            ok
    end.

stop(Node, Options) ->
    %% This function doesn't run in the context of the target node but
    %% from the controller node!
    case erlsvc_lib:wait_nodedown(Node, 0) of
        ok ->
            done;
        error ->
            Force   = proplists:get_bool(force, Options),
            Verbose = proplists:get_bool(verbose, Options),
            if
                Force ->
                    forced_stop(Node, Verbose);
                true ->
                    Timeout = proplists:get_value(timeout, Options, 30000),
                    case graceful_stop(Node, Timeout, Verbose) of
                        ok    -> done;
                        error -> forced_stop(Node, Verbose)
                    end
            end
    end.

get_start_opts() ->
    os:getenv("WCTL_START_OPTS").

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

get_expected_apps(Rel_Name) ->
    erlsvc_lib:report_debug("Get expected applications list~n", []),
    case get_rel_script(Rel_Name) of
        undefined ->
            undefined;
        Rel_Script ->
            get_release_apps(Rel_Script)
    end.

get_running_apps() ->
    Apps = application:which_applications(infinity),
    [{A, V} || {A, _, V} <- Apps].

get_rel_script(Rel_Name) ->
    Rel_Dir = get_rel_dir(Rel_Name),
    Fun = fun(F) ->
        filename:join([Rel_Dir, F])
    end,
    Rel_Files = lists:map(Fun, ["start.rel", "start_clean.rel"]),
    Ret = get_rel_file(Rel_Files),
    case Ret of
        undefined ->
            erlsvc_lib:report_info(
              "Problem:~n"
              "  No rel script found for release '~s'~n", [Rel_Name]);
        _ ->
            ok
    end,
    Ret.

get_rel_dir(Rel_Name) ->
    Rels_Dir = erlsvc_erlenv:releases_dir(),
    filename:join([Rels_Dir, Rel_Name]).

get_rel_file([Rel_File | Rest]) ->
    case file:consult(Rel_File) of
        {ok, [Content]} ->
            Content;
        {error, Reason} ->
            erlsvc_lib:report_warning(
              "Problem:~n"
              "  Failed to read rel script '~s':~n"
              "    ~p~n", [Rel_File, Reason]),
            get_rel_file(Rest)
    end;
get_rel_file([]) ->
    undefined.

get_release_apps({release, _, _, Apps}) ->
    get_release_apps2(Apps, []).

get_release_apps2([{App, Ver} | Rest], Apps) ->
    get_release_apps2(Rest, [{App, Ver} | Apps]);
get_release_apps2([{App, Ver, Type} | Rest], Apps)
  when Type == permanent; Type == transient; Type == temporary ->
    get_release_apps2(Rest, [{App, Ver} | Apps]);
get_release_apps2([{App, Ver, Type, _} | Rest], Apps)
  when Type == permanent; Type == transient; Type == temporary ->
    get_release_apps2(Rest, [{App, Ver} | Apps]);
get_release_apps2([_ | Rest], Apps) ->
    get_release_apps2(Rest, Apps);
get_release_apps2([], Apps) ->
    lists:reverse(Apps).

do_watch_start(Expected, Verbose) ->
    wait_for_apps(Expected, Expected, [], Verbose).

wait_for_sasl() ->
    timer:sleep(200),
    case erlang:whereis(release_handler) of
        undefined ->
            application:start(sasl),
            wait_for_sasl();
        _ ->
            ok
    end.

wait_for_apps(Expected, Pending, Started, Verbose) ->
    Apps1 = lists:reverse(application:which_applications(infinity)),
    Apps2 = [{A, V} || {A, _, V} <- Apps1],
    Apps  = Apps2 -- Started,
    Ret = check_apps(Apps, Pending, Started, Verbose),
    case Ret of
        {Pending2, Started2} ->
            timer:sleep(500),
            wait_for_apps(Expected, Pending2, Started2, Verbose);
        _ ->
            ok
    end.

check_apps(_, [], _, _) ->
    erlsvc_lib:report_info(".~n", []);
check_apps([App | Apps_Rest], [App | Pending_Rest], Started, Verbose) ->
    {App_Name, _} = App,
    if
        Verbose ->
            erlsvc_lib:report_info(" ~s", [App_Name]);
        true ->
            case Pending_Rest of
                [] -> erlsvc_lib:report_info(" done", []);
                _  -> ok
            end
    end,
    check_apps(Apps_Rest, Pending_Rest, [App | Started], Verbose);
check_apps([Unexpected | Apps_Rest], Pending, Started, Verbose) ->
    {App_Name, _} = Unexpected,
    if
        Verbose -> erlsvc_lib:report_info(" (~s)", [App_Name]);
        true    -> ok
    end,
    check_apps(Apps_Rest, Pending, Started, Verbose);
check_apps([], Pending, Started, _) ->
    {Pending, lists:reverse(Started)}.

make_appl(App) when is_atom(App) ->
     App_List = element(2,application:get_all_key(App)),
     App_File = code:where_is_file(atom_to_list(App) ++ ".app"),
     case file:consult(App_File) of
         {ok, [{application, _, Opts}]} ->
             Env = proplists:get_value(env, Opts, []),
             lists:keyreplace(env, 1, App_List, {env, Env});
         {error, Reason} ->
             erlsvc_lib:report_error(
               "Problem:~n"
               "  Failed to read ~s's app file '~s'~n"
               "  Erlang reports:~n"
               "    ~p~n", [App, App_File, Reason]),
             lists:keyreplace(env, 1, App_List, {env, []})
     end.

graceful_stop(Node, Timeout, Verbose) ->
    if
        Verbose -> erlsvc_lib:report_info(" init:stop/0", []);
        true    -> ok
    end,
    rpc:cast(Node, init, stop, []),
    erlsvc_lib:wait_nodedown(Node, Timeout).

forced_stop(Node, Verbose) ->
    [Short_Node | _] = string:tokens(atom_to_list(Node), "@"),
    %% Note that, on FreeBSD, the following command will only work if
    %% /proc is mounted. Otherwise, beam arguments are not available to
    %% ps(1) and pgrep(1).
    Pgrep = string:tokens(
      os:cmd("pgrep -f 'beam(\.smp | ).* -sname " ++ Short_Node ++ " '"),
      "\n"),
    case Pgrep of
        [] ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to get Erlang node's PID.~n"
              "  Note that it's not possible to issue a forced stop to a~n"
              "  remote host.~n"),
            {error, failed};
        [Pid | _] ->
            if
                Verbose -> erlsvc_lib:report_info(" kill-heart", []);
                true    -> ok
            end,
            os:cmd("pkill -f 'heart -pid " ++ Pid ++ "'"),
            if
                Verbose -> erlsvc_lib:report_info(" erlang:halt/0", []);
                true    -> ok
            end,
            rpc:cast(Node, erlang, halt, []),
            case erlsvc_lib:wait_nodedown(Node, 3000) of
                ok ->
                    done;
                error ->
                    if
                        Verbose ->
                            erlsvc_lib:report_info(" kill-node(~s)", [Pid]);
                        true ->
                            ok
                    end,
                    os:cmd("kill " ++ Pid),
                    killed
            end
    end.
