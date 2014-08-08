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

-module(erlsvc_erlenv).

%% Public API.
-export([
    erlsvc_cm_deps/0,
    erts_version/0,
    root_dir/0,
    lib_dir/0,
    lib_dir/1,
    priv_dir/1,
    releases_dir/0
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

erlsvc_cm_deps() ->
    [].

erts_version() ->
    erlang:system_info(version).

root_dir() ->
    code:root_dir().

lib_dir() ->
    code:lib_dir().

lib_dir(App) ->
    code:lib_dir(App).

priv_dir(App) ->
    code:priv_dir(App).

releases_dir() ->
    %% This code was stolen from release_handler.erl (R13B04).
    {ok, [[Root]]} = init:get_argument(root),
    case application:get_env(sasl, releases_dir) of
        undefined ->
            case os:getenv("RELDIR") of
                false  -> filename:join([Root, "releases"]);
                RELDIR -> RELDIR
            end;
        {ok, Dir} ->
            Dir
    end.
