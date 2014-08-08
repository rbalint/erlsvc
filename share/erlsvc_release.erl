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

-module(erlsvc_release).

%% Public API.
-export([
    erlsvc_cm_deps/0,
    list/0,
    list/1,
    current/0,
    upgradable/2,
    set_unpacked/1,
    upgrade/1,
    reset/0,
    set_removed/1,
    sync_vanilla/4
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

erlsvc_cm_deps() ->
    [erlsvc_lib, erlsvc_erlenv].

list() ->
    list(default).

list(RELEASES) ->
    {yaml, [
        begin
            Apps_S = [
              io_lib:format(
                "      - name: ~s~n"
                "        version: ~s~n"
                "        path: \"~s\"~n",
                [N, V, P])
              || {N, V, P} <- Apps],
            io_lib:format(
              "  ~s:~n"
              "    id: \"~s\"~n"
              "    erts: ~s~n"
              "    state: ~s~n"
              "    applications:~n"
              "~s",
              [Name, ID, ERTS, State, Apps_S])
        end
        || {ID, Name, ERTS, Apps, State} <- get_releases(RELEASES)
      ]}.

current() ->
    Rels = get_releases(default),
    case lists:keyfind(current, 5, Rels) of
        false ->
            case lists:keyfind(permanent, 5, Rels) of
                false -> undefined;
                Rel   -> Rel
            end;
        Rel ->
            Rel
    end.

upgradable(Relup, From) ->
    case file:consult(Relup) of
        {ok, [{To, Upgradable_Rels, _}]} ->
            case lists:keyfind(From, 1, Upgradable_Rels) of
                false ->
                    erlsvc_lib:report_debug(
                      "No instruction to upgrade from '~s' to '~s'~n",
                      [From, To]),
                    false;
                {_, _, Instructions} ->
                    Restart = lists:member(restart_new_emulator, Instructions),
                    case Restart of
                        false ->
                            erlsvc_lib:report_debug(
                              "Release '~s' can be upgraded to '~s'~n",
                              [From, To]),
                            true;
                        true ->
                            erlsvc_lib:report_debug(
                              "Upgrade from '~s' to '~s' requires an "
                              "emulator restart; unacceptable~n",
                              [From, To]),
                            false
                    end
            end;
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to read relup script '~s'~n"
              "  file:consult/1 reports:~n"
              "    ~p~n",
              [Relup, Reason]),
            false
    end.

set_unpacked(Rel_File) ->
    case prepare_app_dirs(Rel_File) of
        undefined ->
            {error, failed};
        App_Dirs ->
            application:start(sasl),
            set_unpacked2(Rel_File, App_Dirs)
    end.

set_unpacked2(Rel_File, App_Dirs) ->
    case release_handler:set_unpacked(Rel_File, App_Dirs) of
        {ok, Rel} ->
            Rel;
        {error, {existing_release, Rel}} ->
            erlsvc_lib:report_warning(
              "Problem:~n"
              "  The relase '~s' already exists. It'll be removed first~n"
              "  then the unpack will be retried.~n", [Rel]),
            set_unpacked3(Rel_File, App_Dirs, Rel);
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to set release as unpacked.~n"
              "  Rel script is:~n"
              "    ~s~n"
              "  Erlang reports:~n"
              "    ~p~n", [Rel_File, Reason]),
            {error, Reason}
    end.

set_unpacked3(Rel_File, App_Dirs, Rel) ->
    case release_handler:set_removed(Rel) of
        ok ->
            set_unpacked2(Rel_File, App_Dirs);
        {error, {permanent, _}} ->
            erlsvc_lib:report_warning(
              "Problem:~n"
              "  The relase '~s' is permanent but not used. The current~n"
              "  release will become permanent to permit the removal of "
              "'~s'.~n",
              [Rel, Rel]),
            case current() of
                undefined ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Failed to get the current release.~n", []),
                    {error, failed};
                {_, Rel, _, _} ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  The current release is used.~n"
                      "  This should never happen...~n", []),
                    {error, failed};
                {_, Current_Rel, _, _} ->
                    case release_handler:make_permanent(Current_Rel) of
                        ok ->
                            set_unpacked3(Rel_File, App_Dirs, Rel);
                        {error, Reason} ->
                            erlsvc_lib:report_error(
                              "Problem:~n"
                              "  Failed to make current release '~s' "
                              "permanent.~n"
                              "  Erlang reports:~n"
                              "    ~p~n", [Current_Rel, Reason]),
                            {error, Reason}
                    end
            end;
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to remove existing version of release '~s'.~n"
              "  Erlang reports:~n"
              "    ~p~n", [Rel, Reason]),
            {error, Reason}
    end.

upgrade(Rel) ->
    application:start(sasl),
    case release_handler:install_release(Rel) of
        {ok, _, _} ->
            case release_handler:make_permanent(Rel) of
                ok ->
                    ok;
                {error, Reason} ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Failed to make release '~s' permanent.~n"
                      "  Erlang reports:~n"
                      "    ~p~n", [Rel, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            %% There's little to no chance that we end up here: either
            %% the release was upgraded properly or the node has crashed
            %% during upgrade because an application didn't make it.
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to install release '~s'.~n"
              "  Erlang reports:~n"
              "    ~p~n", [Rel, Reason]),
            {error, Reason}
    end.

reset() ->
    application:start(sasl),
    OTP_Rel = get_vanilla_release(),
    erlsvc_lib:report_debug("Reset to Erlang vanilla release '~s'~n", [OTP_Rel]),
    case OTP_Rel of
        undefined ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  No suitable Erlang vanilla release found.~n", []),
            {error, failed};
        _ ->
            case release_handler:install_release(OTP_Rel) of
                {ok, _, _} ->
                    case release_handler:make_permanent(OTP_Rel) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            erlsvc_lib:report_error(
                              "Problem:~n"
                              "  Failed to make Erlang release '~s' "
                              "permanent.~n"
                              "  Erlang reports:~n"
                              "    ~p~n", [OTP_Rel, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Failed to restore Erlang release '~s'.~n"
                      "  Erlang reports:~n"
                      "    ~p~n", [OTP_Rel, Reason]),
                    {error, Reason}
            end
    end.

set_removed(Rel) ->
    application:start(sasl),
    case release_handler:set_removed(Rel) of
        ok ->
            ok;
        {error, {no_such_release, Rel}} ->
            ok;
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to remove release '~s'.~n"
              "  Erlang reports:~n"
              "    ~p~n", [Rel, Reason]),
            {error, Reason}
    end.

sync_vanilla(Rels_Dir, Old_Rels, New_Rels, Permanent) ->
    application:stop(sasl),
    RELEASES = filename:join([Rels_Dir, "RELEASES"]),
    case file:consult(RELEASES) of
        {ok, [Releases0]} ->
            Releases1 = remove_old_rels(Releases0, Old_Rels),
            {Releases2, ERTS} = add_new_rels(Releases1, New_Rels, Permanent,
              "-"),
            erlsvc_lib:report_debug("REL", " RELEASES", []),
            Ret = release_handler:do_write_release(Rels_Dir, "RELEASES",
              Releases2),
            case Ret of
                ok ->
                    sync_vanilla2(Rels_Dir, Permanent, ERTS);
                {error, Reason} ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Failed to write '~s'.~n"
                      "  System reports:~n"
                      "    ~p~n",
                      [RELEASES, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to read '~s'.~n"
              "  System reports:~n"
              "    ~p~n",
              [RELEASES, Reason]),
            {error, Reason}
    end.

sync_vanilla2(_, none, _) ->
    application:start(sasl);
sync_vanilla2(Rels_Dir, Permanent, ERTS) ->
    Start_Erl_Data = filename:join([Rels_Dir, "start_erl.data"]),
    erlsvc_lib:report_debug("REL", " start_erl.data", []),
    Ret = release_handler:do_write_file(Start_Erl_Data,
      ERTS ++ " " ++ Permanent ++ "\n"),
    case Ret of
        ok ->
            application:start(sasl);
        {error, {Reason, _}} ->
            {error, Reason}
    end.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

get_releases(default) ->
    Releases_Dir = erlsvc_erlenv:releases_dir(),
    RELEASES = filename:join([Releases_Dir, "RELEASES"]),
    get_releases(RELEASES);
get_releases(RELEASES) ->
    erlsvc_lib:report_debug("Get releases list from '~s'~n", [RELEASES]),
    case file:consult(RELEASES) of
        {ok, [Releases]} ->
            [
              {ID, Name, ERTS, Apps, State}
              || {_, ID, Name, ERTS, Apps, State} <- Releases
            ];
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to read '~s'.~n"
              "  System reports:~n"
              "    ~p~n",
              [RELEASES, Reason]),
            []
    end.

get_vanilla_release() ->
    OTP_Rel = erlang:system_info(otp_release),
    Rels = release_handler:which_releases(),
    case lists:keyfind(OTP_Rel, 2, Rels) of
        false ->
            get_vanilla_release2(Rels);
        _ ->
            OTP_Rel
    end.

get_vanilla_release2([{_, Rel, _, _} | Rest]) ->
    case re:run(Rel, "^R[0-9]+[AB]") of
        {match, _} -> Rel;
        _          -> get_vanilla_release2(Rest)
    end;
get_vanilla_release2([]) ->
    undefined.

prepare_app_dirs(Rel_File) ->
    case file:consult(Rel_File) of
        {ok, [{release, _, _, Apps}]} ->
            prepare_app_dirs2(Apps, []);
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to read '~s' to prepare release app. dirs.~n"
              "  System reports:~n"
              "    ~p~n",
              [Rel_File, Reason]),
            undefined
    end.

prepare_app_dirs2([{App, Vsn} | Rest], App_Dirs) ->
    New_App_Dirs = prepare_app_dirs3(App, Vsn, App_Dirs),
    prepare_app_dirs2(Rest, New_App_Dirs);
prepare_app_dirs2([{App, Vsn, _} | Rest], App_Dirs) ->
    New_App_Dirs = prepare_app_dirs3(App, Vsn, App_Dirs),
    prepare_app_dirs2(Rest, New_App_Dirs);
prepare_app_dirs2([{App, Vsn, _, _} | Rest], App_Dirs) ->
    New_App_Dirs = prepare_app_dirs3(App, Vsn, App_Dirs),
    prepare_app_dirs2(Rest, New_App_Dirs);
prepare_app_dirs2([], App_Dirs) ->
    lists:reverse(App_Dirs).

prepare_app_dirs3(App, Vsn, App_Dirs) ->
    Lib_Dir = code:lib_dir() ++ "/",
    case get_app_dir(App, Vsn) of
        undefined ->
            App_Dirs;
        Dir ->
            case string:substr(Dir, 1, length(Lib_Dir)) of
                Lib_Dir ->
                    App_Dirs;
                _ ->
                    App_Dir = {App, Vsn, filename:dirname(Dir)},
                    [App_Dir | App_Dirs]
            end
    end.

get_app_dir(App, Vsn) ->
    App_S = atom_to_list(App),
    case get_app_dir_from_code_path(App_S, Vsn) of
        undefined ->
            case get_app_dir_from_ERL_LIBS(App_S, Vsn) of
                undefined -> undefined;
                Dir       -> Dir
            end;
        Dir ->
            Dir
    end.

get_app_dir_from_code_path(App, Vsn) ->
    Code_Path = code:get_path(),
    Suffix = filename:join(["", App ++ "-" ++ Vsn, "ebin"]),
    get_app_dir_from_code_path2(Code_Path, Suffix).

get_app_dir_from_code_path2(["." | Rest], Suffix) ->
    get_app_dir_from_code_path2(Rest, Suffix);
get_app_dir_from_code_path2([Dir | Rest], Suffix) ->
    if
        length(Dir) > length(Suffix) ->
            Substr = string:substr(Dir,
              length(Dir) - length(Suffix) + 1,
              length(Suffix)),
            case Substr of
                Suffix -> filename:dirname(Dir);
                _      -> get_app_dir_from_code_path2(Rest, Suffix)
            end;
        true ->
            get_app_dir_from_code_path2(Rest, Suffix)
    end;
get_app_dir_from_code_path2([], _) ->
    undefined.

get_app_dir_from_ERL_LIBS(App, Vsn) ->
    case os:getenv("ERL_LIBS") of
        false ->
            undefined;
        ERL_LIBS ->
            Dirs = string:tokens(ERL_LIBS, ":"),
            Suffix = filename:join([App ++ "-" ++ Vsn, "ebin"]),
            get_app_dir_from_ERL_LIBS2(Dirs, Suffix)
    end.

get_app_dir_from_ERL_LIBS2([Parent_Dir | Rest], Suffix) ->
    Dir = filename:join([Parent_Dir, Suffix]),
    case filelib:is_dir(Dir) of
        true  -> filename:dirname(Dir);
        false -> get_app_dir_from_ERL_LIBS2(Rest, Suffix)
    end;
get_app_dir_from_ERL_LIBS2([], _) ->
    undefined.

remove_old_rels(Releases, [Rel | Rest]) ->
    Releases2 = lists:keydelete(Rel, 3, Releases),
    remove_old_rels(Releases2, Rest);
remove_old_rels(Releases, []) ->
    Releases.

add_new_rels(Releases, [Rel | Rest], Permanent, ERTS) ->
    Release = lists:keyfind(Rel, 3, Releases),
    {Release2, ERTS2} = case Permanent of
        Rel ->
            {setelement(6, Release, permanent), element(4, Release)};
        _ ->
            {setelement(6, Release, old), ERTS}
    end,
    Releases2 = lists:keyreplace(Rel, 3, Releases, Release2),
    add_new_rels(Releases2, Rest, Permanent, ERTS2);
add_new_rels(Releases, [], _, ERTS) ->
    {Releases, ERTS}.
