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

-module(erlsvc_mnesia).

%% Public API.
-export([
    erlsvc_cm_deps/0,
    directory/0,
    is_directory_used/0,
    db_nodes/0,
    create_schema/0,
    join_cluster/1,
    leave_cluster/0,
    check_consistency/2
  ]).

%% Internal exports.
-export([
    get_db_nodes/0,
    get_local_replicas/0,
    get_table_snapshot/1
  ]).

%% -------------------------------------------------------------------
%% Public API.
%% -------------------------------------------------------------------

erlsvc_cm_deps() ->
    [erlsvc_lib, erlsvc_service].

directory() ->
    mnesia:system_info(directory).

is_directory_used() ->
    mnesia:system_info(use_dir).

db_nodes() ->
    {All_Nodes, Running_Nodes} = get_db_nodes(),
    {yaml, [
        case lists:member(Node, Running_Nodes) of
            true  -> io_lib:format("  ~s: up~n", [Node]);
            false -> io_lib:format("  ~s: down~n", [Node])
        end
        || Node <- All_Nodes
      ]}.

create_schema() ->
    %% Mnesia must be stopped in order for create_schema/1 to work.
    mnesia:stop(),
    Node = node(),
    Dir  = directory(),
    case mnesia:create_schema([Node]) of
        ok ->
            erlsvc_lib:report_debug("Schema created in '~s'~n", [Dir]);
        {error, {_, {already_exists, _}}} ->
            erlsvc_lib:report_debug("Schema already present in '~s'~n", [Dir]);
        {error, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to create schema in directory '~s'.~n"
              "  Mnesia reports:~n"
              "    ~p~n"
              "~n"
              "Solution(s):~n"
              "  1. Check that the user has permission to write to the~n"
              "     directory above. If this directory doesn't exist,~n"
              "     check that the parent directory exists and the user~n"
              "     has write permission on it.~n",
              [Dir, Reason]),
            {error, Reason}
    end.

join_cluster([]) ->
    erlsvc_lib:report_error(
      "Problem:~n"
      "  Can't join a cluster without at least one one in argument~n"),
    {error, failed};
join_cluster(Extra_DB_Nodes) ->
    %% Mnesia must be started for change_config/2 et
    %% change_table_copy_type/3 to work.
    mnesia:start(),
    Node = node(),
    case mnesia:change_config(extra_db_nodes, Extra_DB_Nodes) of
        {ok, []} ->
            %% Mnesia couldn't contact any node.
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to contact a node while joining cluster.~n"
              "~n"
              "Solution(s):~n"
              "  1. Check that the given nodes are up and runs Mnesia and~n"
              "     that they ping from this node.~n"),
            {error, failed};
        {ok, _} ->
            %% Ok, we could contact some nodes. Now, we must change the
            %% copy type for the schema and a few other tables.
            join_cluster2(Node, Extra_DB_Nodes);
        {error, Reason} ->
            {error, Reason}
    end.

join_cluster2(Node, Extra_DB_Nodes) ->
    case mnesia:change_table_copy_type(schema, Node, disc_copies) of
        {atomic, _} ->
            %% We're part of the cluster now \o/
            Dir     = mnesia:system_info(directory),
            Nodes_S = [
              io_lib:format(" - ~s~n", [N])
              || N <- lists:sort(mnesia:system_info(db_nodes))],
            erlsvc_lib:report_info(
              "Schema created in '~s'~n"
              "Joined cluster comprising the following nodes now:~n"
              "~s",
              [Dir, Nodes_S]),
            Tables = [
              %% Use Ejabberd default storage type.
              {acl, disc_copies},
              {config, disc_copies},
              {local_config, disc_copies},
              %% Ejabberd forgets to add a copy of the
              %% session_counter table.
              {session_counter, ram_copies}
            ],
            Fun = fun({Table, Type}) ->
                add_table_copy(Table, Node, Type)
            end,
            lists:foreach(Fun, Tables),
            ok;
        {aborted, {already_exists, schema, _, _}} ->
            %% The schema already exists. We check if we're
            %% already part of the cluster.
            Dir       = mnesia:system_info(directory),
            Nodes     = mnesia:system_info(db_nodes),
            Clustered = lists:member(Node, Nodes) andalso
              Extra_DB_Nodes -- Nodes /= Extra_DB_Nodes,
            Nodes_S   = [
              io_lib:format(" - ~s~n", [N])
              || N <- lists:sort(mnesia:system_info(db_nodes))],
            if
                Clustered ->
                    erlsvc_lib:report_info(
                      "Schema already exists in '~s'~n"
                      "Node '~s' is already part of the cluster comprising "
                      "the following nodes:~n"
                      "~s",
                      [Dir, Node, Nodes_S]),
                    ok;
                true ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Schema already exists in '~s' and~n"
                      "  Node '~s' is not part of the cluster comprising "
                      "the following nodes:~n"
                      "~s"
                      "~n"
                      "Solution(s):~n"
                      "  1. Leave the current cluster or remove the~n"
                      "     schema if not part of a cluster.~n",
                      [Dir, Node, Nodes_S]),
                    {error, failed}
            end;
        {aborted, Reason} ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed join cluster.~n"
              "  Mnesia reports:~n"
              "    ~p~n",
              [Reason]),
            {error, Reason}
    end.

leave_cluster() ->
    %% Mnesia must be stopped in order for del_table_copy/2 to work.
    mnesia:stop(),
    Node  = node(),
    Nodes = mnesia:system_info(running_db_nodes),
    case Nodes of
        [] ->
            All_Nodes = mnesia:system_info(db_nodes) -- [Node],
            case All_Nodes of
                [] ->
                    erlsvc_lib:report_debug(
                      "Node '~s' is not part of a cluster~n", [Node]),
                    ok;
                _ ->
                    Nodes_S = [
                      "      - " ++ atom_to_list(N) ++ "\n"
                      || N <- All_Nodes],
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Failed to leave cluster because no other nodes~n"
                      "  are currently running.~n"
                      "~n"
                      "Solutions(s):~n"
                      "  1. Start one of the following nodes:~n"
                      "~s"
                      "  2. Check that the cluster is not partitioned.~n",
                      [Nodes_S]),
                    {error, cluster_down}
            end;
        _ ->
            leave_cluster2(Node, Nodes)
    end.

leave_cluster2(Node, [Remote | Rest]) ->
    case rpc:call(Remote, mnesia, del_table_copy, [schema, Node], infinity) of
        {atomic, _} ->
            erlsvc_lib:report_debug(
              "Nodes '~s' removed from cluster using remote node '~s'~n",
              [Node, Remote]),
            ok;
        {aborted, Reason} ->
            erlsvc_lib:report_debug(
              "Failed to remove node '~s' from cluster "
              "using remote node '~s'.~n"
              "Mnesia reports:~n"
              "  ~p~n", [Node, Remote, Reason]),
            leave_cluster2(Node, Rest);
        {badrpc, Reason} ->
            erlsvc_lib:report_debug(
              "Failed contact remote node '~s' "
              "to remove node '~s' from cluster.~n"
              "Erlang reports:~n"
              "  ~p~n", [Remote, Node, Reason]),
            leave_cluster2(Node, Rest)
    end;
leave_cluster2(Node, []) ->
    erlsvc_lib:report_error(
      "Problem:~n"
      "  Failed to remove node '~s' from cluster.~n"
      "  None of the cluster nodes could be contacted or could remove~n"
      "  the node.~n", [Node]),
    {error, failed}.

check_consistency(Node, Options) ->
    Verbose = proplists:get_bool(verbose, Options),
    %% We query the given node to get the list of running and stopped DB
    %% nodes.
    case get_db_nodes(Node) of
        {error, Reason} ->
            {error, Reason};
        {All_Nodes, Running_Nodes} ->
            Stopped_Nodes = All_Nodes -- Running_Nodes,
            %% The first step consists of checking if the stopped DB
            %% nodes are really stopped or at least, don't run the
            %% service.
            check_stopped_nodes(lists:sort(All_Nodes), Stopped_Nodes),
            %% The next step consists of checking the data in each
            %% version of each table (ie. each replica on each node).
            check_tables(Running_Nodes, Verbose)
    end.

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

get_db_nodes() ->
    {
      mnesia:system_info(db_nodes),
      mnesia:system_info(running_db_nodes)
    }.

add_table_copy(Table, Node, Type) ->
    case mnesia:add_table_copy(Table, Node, Type) of
        {atomic, _} ->
            ok;
        {aborted, Reason} ->
            erlsvc_lib:report_warning(
              "Problem:~n"
              "  Failed to add copy of type ~s for table '~s'.~n"
              "  Mnesia reports:~n"
              "    ~p~n",
              [Type, Table, Reason]),
            {error, Reason}
    end.

get_worker(Node) ->
    Key = {erlsvc_worker, Node},
    case erlang:get(Key) of
        Worker when is_pid(Worker) ->
            Worker;
        undefined ->
            case erlsvc_worker:start_link(Node) of
                {ok, Worker} ->
                    erlsvc_worker:silence(Worker),
                    erlang:put(Key, Worker),
                    Worker;
                {error, Reason} ->
                    erlsvc_lib:report_error(
                      "Problem:~n"
                      "  Failed to start a worker on node '~s'~n"
                      "  Erlang reports:~n"
                      "    ~p~n", [Node, Reason]),
                    {error, Reason}
            end
    end.

check_stopped_nodes(Nodes, Stopped) ->
    erlsvc_lib:report_info("Checking cluster members consistency:~n", []),
    Max     = lists:max([length(atom_to_list(N)) || N <- Nodes]),
    Total   = length(Nodes),
    Total_S = integer_to_list(Total),
    Format  = lists:flatten(io_lib:format("  ~~~bb/~s  ~~~bs: ",
        [length(Total_S), Total_S, Max])),
    check_stopped_nodes2(Nodes, Stopped, 1, Total, 0, Format).

check_stopped_nodes2([Node | Rest], Stopped, Count, Total, Bad_Count, Format) ->
    erlsvc_lib:report_info(Format, [Count, Node]),
    Ret = case lists:member(Node, Stopped) of
        false ->
            {ok, running};
        true ->
            case net_adm:ping(Node) of
                pang ->
                    {ok, pang};
                pong ->
                    case erlsvc_worker:start_link(Node) of
                        {ok, Worker} ->
                            Is_Running = erlsvc_worker:exec_command(Worker,
                              {erlsvc_service, is_running, []}),
                            erlsvc_worker:stop(Worker),
                            case Is_Running of
                                false ->
                                    {ok, service_down};
                                true ->
                                    {error, service_up}
                            end;
                        {error, Reason} ->
                            erlsvc_lib:report_error(
                              "Problem:~n"
                              "  Failed to start a worker on node '~s'~n"
                              "  Erlang reports:~n"
                              "    ~p~n", [Node, Reason]),
                            ok
                    end
            end
    end,
    Bad_Count2 = case Ret of
        {ok, running} ->
            erlsvc_lib:report_info("OK - service up~n", []),
            Bad_Count;
        {ok, pang} ->
            erlsvc_lib:report_info("OK - node down~n", []),
            Bad_Count;
        {ok, service_down} ->
            erlsvc_lib:report_info("OK - node up but service down~n", []),
            Bad_Count;
        {error, service_up} ->
            erlsvc_lib:report_info(
              "<b>NOT OK - service up but out of cluster</b>~n", []),
            Bad_Count + 1
    end,
    check_stopped_nodes2(Rest, Stopped, Count + 1, Total, Bad_Count2, Format);
check_stopped_nodes2([], _, _, Total, Bad_Count, _) ->
    if
        Bad_Count > 0 ->
            erlsvc_lib:report_info("  ~b out of ~n nodes(s) suspicious!~n",
              [Bad_Count, Total]);
        true ->
            erlsvc_lib:report_info("  OK~n", [])
    end.

check_tables(Nodes, Verbose) ->
    erlsvc_lib:report_info("Checking tables consistency:~n", []),
    Tables  = get_tables_list(Nodes, dict:new()),
    Max     = lists:max([length(atom_to_list(T)) || {T, _} <- Tables]),
    Total   = length(Tables),
    Total_S = integer_to_list(Total),
    Format  = case Verbose of
        false ->
            lists:flatten(io_lib:format("<r/>  ~~~bb/~s  ~~~bs: ~~s",
                [length(Total_S), Total_S, Max]));
        true ->
            lists:flatten(io_lib:format("<r/>  ~~~bb/~s  ~~~bs: ~~s~n",
                [length(Total_S), Total_S, Max]))
    end,
    check_tables2(Tables, 1, Total, 0, Format, Verbose).

check_tables2([{Table, Nodes} | Rest], Count, Total, Bad_Count,
  Format, Verbose) ->
    Fun1 = fun
        ({N, {error, Reason}}, D) ->
            erlsvc_lib:report_error(
              "Problem:~n"
              "  Failed to get snapshot of table '~s' on node '~s'."
              "  Erlang reports:~n"
              "    ~p~n", [Table, N, Reason]),
            D;
        ({N, S}, D) ->
            try
                dict:append(S, N, D)
            catch
                _:_ ->
                    dict:store(S, [N], D)
            end
    end,
    Samples = [{N, get_table_snapshot(N, Table)} || N <- Nodes],
    Table_Versions = lists:foldl(Fun1, dict:new(), Samples),
    Bad_Count2 = case dict:size(Table_Versions) of
        1 ->
            erlsvc_lib:report_info(Format, [Count, Table, "OK"]),
            Bad_Count;
        Diff ->
            erlsvc_lib:report_info(Format, [Count, Table, "INCONSISTENT"]),
            display_diff(Diff, Table_Versions, Verbose),
            Bad_Count + 1
    end,
    check_tables2(Rest, Count + 1, Total, Bad_Count2, Format, Verbose);
check_tables2([], _, Total, Bad_Count, _, _) ->
    if
        Bad_Count > 0 ->
            erlsvc_lib:report_info(
              "<r/>  ~b out of ~n table(s) inconsistent(s)!~n",
              [Bad_Count, Total]);
        true ->
            erlsvc_lib:report_info("<r/>  OK~n", [])
    end.

get_tables_list([Node | Rest], Dict) ->
    case get_local_replicas(Node) of
        {error, Reason} ->
            {error, Reason};
        Local_Tables ->
            Fun = fun(T, D) ->
                try
                    dict:append(T, Node, D)
                catch
                    _:_ ->
                        dict:store(T, [Node], D)
                end
            end,
            Dict2 = lists:foldl(Fun, Dict, Local_Tables),
            get_tables_list(Rest, Dict2)
    end;
get_tables_list([], Dict) ->
    lists:keysort(1, dict:to_list(Dict)).

get_db_nodes(Node) ->
    case get_worker(Node) of
        Worker when is_pid(Worker) ->
            Ret = erlsvc_worker:exec_command(Worker,
              {erlsvc_mnesia, get_db_nodes, []}),
            case Ret of
                {error, Reason} -> {error, Reason};
                DB_Nodes        -> DB_Nodes
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_local_replicas(Node) ->
    case get_worker(Node) of
        Worker when is_pid(Worker) ->
            erlsvc_worker:exec_command(Worker,
              {erlsvc_mnesia, get_local_replicas, []});
        {error, Reason} ->
            {error, Reason}
    end.

get_local_replicas() ->
    Local_Tables = mnesia:system_info(local_tables),
    get_local_replicas2(Local_Tables, []).

get_local_replicas2([Table | Rest], Local_Replicas) ->
    case mnesia:table_info(Table, local_content) of
        true  -> get_local_replicas2(Rest, Local_Replicas);
        false -> get_local_replicas2(Rest, [Table | Local_Replicas])
    end;
get_local_replicas2([], Local_Replicas) ->
    Local_Replicas -- [schema].

get_table_snapshot(Node, Table) ->
    case get_worker(Node) of
        Worker when is_pid(Worker) ->
            erlsvc_worker:exec_command(Worker,
              {erlsvc_mnesia, get_table_snapshot, [Table]});
        {error, Reason} ->
            {error, Reason}
    end.

get_table_snapshot(Table) ->
    lists:keysort(2, ets:tab2list(Table)).

display_diff(2, Table_Versions, _) ->
    [V1, V2] = dict:fetch_keys(Table_Versions),
    Fun2 = fun(N, Str) ->
        lists:flatten(io_lib:format("~s, ~s", [Str, N]))
    end,
    Fun3 = fun(E) -> not lists:member(E, V2) end,
    Fun4 = fun(E) -> not lists:member(E, V1) end,
    Only_In_V1 = lists:filter(Fun3, V1),
    Only_In_V2 = lists:filter(Fun4, V2),
    erlsvc_lib:report_info("~n    <b>2 different versions:</b>~n", []),
    Ns1 = dict:fetch(V1, Table_Versions),
    Ns2 = dict:fetch(V2, Table_Versions),
    [N1 | R1] = lists:sort(Ns1),
    [N2 | R2] = lists:sort(Ns2),
    Nodes_String1 = lists:foldl(Fun2, "", R1),
    Nodes_String2 = lists:foldl(Fun2, "", R2),
    io:format("     - Only on node(s) ~s~s:~n       ~p~n~n",
      [N1, Nodes_String1, Only_In_V1]),
    io:format("     - Only on node(s) ~s~s:~n       ~p~n~n",
      [N2, Nodes_String2, Only_In_V2]);
display_diff(Size, Table_Versions, _) ->
    erlsvc_lib:report_info("~n    <b>~b different versions:</b>~n", [Size]),
    Fun2 = fun(N, Str) ->
        lists:flatten(io_lib:format("~s, ~s", [Str, N]))
    end,
    Fun3 = fun(V, Ns, Acc) ->
        [N | R] = lists:sort(Ns),
        Nodes_String = lists:foldl(Fun2, "", R),
        io:format("     - Node(s) ~s~s:~n       ~p~n~n",
          [N, Nodes_String, V]),
        Acc
    end,
    dict:fold(Fun3, ok, Table_Versions).
