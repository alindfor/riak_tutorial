-module(key_value_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(NAME(Node), element(1, Node)).

all() ->
  [
      ping_test,
      key_value_test,
      exist_test
  ].

init_per_suite(Config) ->
    Nodes =
        [
            {'node1@127.0.0.1', 8198, 8199},
            {'node2@127.0.0.1', 8298, 8299},
            {'node3@127.0.0.1', 8398, 8399}
        ],

    lists:foreach(
        fun({Node, WPort, HPort}) ->
            start_node(Node, WPort, HPort)
        end, Nodes
    ),

    %% build_cluster will join the nodes in a cluster and return a list of the node names
    [{nodes, build_cluster(Nodes)} | Config].

end_per_suite(Config) ->
    lists:foreach(
        fun(Node) ->
            stop_node(Node)
        end, ?config(nodes, Config)),
    ok.

%% =============================================================================
%% Tests
%% =============================================================================

ping_test(Config) ->
    lists:foreach(
        fun(Node) ->
            {pong, _Partition} = rc_command(Node, ping)
        end, ?config(nodes, Config)),

    ok.

key_value_test(Config) ->
    Nodes = [Node1 | _ ] = ?config(nodes, Config),

    KVProp = [{k1,v1}, {k2, v2}, {k3, v3}],
    lists:foreach(
        fun({K,V}) ->
            ok = rc_command(Node1, put, [K,V])
        end, KVProp),

    %%% GET FROM ALL OF THE NODES
    lists:foreach(
        fun(Node) ->
            lists:foreach(
                fun({K,V}) ->
                    V = rc_command(Node, get, [K])
                end, KVProp)
        end, Nodes),

    lists:foreach(
        fun(Node) ->
            not_found = rc_command(Node, get, [k10])
        end, Nodes),

    %% TEST RESET AND DELETE
    ok = rc_command(Node1, put, [k1, v_new]),
    v_new = rc_command(Node1, get, [k1]),

    v_new = rc_command(Node1, delete, [k1]),
    not_found = rc_command(Node1, get, [k1]),

    ok = rc_command(Node1, put, [k1, v_new]),
    v_new = rc_command(Node1, get, [k1]),
    ok.

exist_test(Config) ->
    Nodes = [Node1 |_ ] = ?config(nodes, Config),
    KVProp = [{k12, v1}, {k150, v2}, {k1024, v3}],

    %% Iterate over each KV pair and insert it on Node1
    lists:foreach(
        fun({K,V}) ->
            ok = rc_command(Node1, put, [K, V])
        end, KVProp),

    %% Iterate over each Node, check that all inserted KV pairs are available
    lists:foreach(
        fun(Node) ->
            lists:foreach(
                fun({K, _}) ->
                    true = rc_command(Node, exist, [K])
                end, KVProp)
        end, Nodes),

    %% Check the false case of the exist functionality
    False = [k177, k1337, k9000],
    lists:foreach(
        fun(K) ->
            false = rc_command(Node1, exist, [K])
        end, False),
    ok.

%% =============================================================================
%% Internal Test HELPERS
%% =============================================================================

start_node(NodeName, WebPort, HandoffPort) ->
    %% Need to set the code path so the same modules are available in the slaves
    CodePath = code:get_path(),
    PathFlag = "-pa " ++ lists:concat(lists:join(" ", CodePath)),
    {ok, _} = ct_slave:start(NodeName, [{erl_flags, PathFlag}]),

    %% set the required environment for riak core
    DataDir = "./data/" ++ atom_to_list(NodeName),
    rpc:call(NodeName, application, load, [riak_core]),
    rpc:call(NodeName, application, set_env, [riak_core, ring_state_dir, DataDir]),
    rpc:call(NodeName, application, set_env, [riak_core, platform_data_dir, DataDir]),
    rpc:call(NodeName, application, set_env, [riak_core, web_port, WebPort]),
    rpc:call(NodeName, application, set_env, [riak_core, handoff_port, HandoffPort]),
    rpc:call(NodeName, application, set_env, [riak_core, schema_dirs, ["../../lib/rc_example/priv"]]),

    {ok, _} = rpc:call(NodeName, application, ensure_all_started, [rc_example]),

    ok.

stop_node(NodeName) ->
    ct_slave:stop(NodeName).

build_cluster(Nodes) when is_list(Nodes) ->
    build_cluster(Nodes, []).

build_cluster([N1, N2], BuiltNodes) ->
    rpc:call(?NAME(N2), riak_core, join, [?NAME(N1)]),
    [?NAME(N1), ?NAME(N2) | BuiltNodes];
build_cluster([N1, N2 | T], BuiltNodes) ->
    rpc:call(?NAME(N2), riak_core, join, [?NAME(N1)]),
    build_cluster([N1 | T], [?NAME(N2) | BuiltNodes]).

rc_command(Node, Command) ->
    rc_command(Node, Command, []).
rc_command(Node, Command, Arguments) ->
    rpc:call(Node, rc_example, Command, Arguments).
