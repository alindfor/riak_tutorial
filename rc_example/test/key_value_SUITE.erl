-module(key_value_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
  [
      ping_test,
      key_value_test,
      exist_test
  ].

init_per_suite(Config) ->
    Node1 = 'node1@127.0.0.1',
    Node2 = 'node2@127.0.0.1',
    Node3 = 'node3@127.0.0.1',


    start_node(Node1, 8198, 8199),
    start_node(Node2, 8298, 8299),
    start_node(Node3, 8398, 8399),
    build_cluster(Node1, Node2, Node3),

    [{node1, Node1},
        {node2, Node2},
        {node3, Node3} | Config].

end_per_suite(Config) ->
    Node1 = ?config(node1, Config),
    Node2 = ?config(node2, Config),
    Node3 = ?config(node3, Config),
    stop_node(Node1),
    stop_node(Node2),
    stop_node(Node3),
    ok.

%% =============================================================================
%% Tests
%% =============================================================================

ping_test(Config) ->
    Node1 = ?config(node1, Config),
    Node2 = ?config(node2, Config),
    Node3 = ?config(node3, Config),

    {pong, _Partition1} = rc_command(Node1, ping),
    {pong, _Partition2} = rc_command(Node2, ping),
    {pong, _Partition3} = rc_command(Node3, ping),

    ok.

key_value_test(Config) ->
    Node1 = ?config(node1, Config),
    Node2 = ?config(node2, Config),
    Node3 = ?config(node3, Config),

    ok = rc_command(Node1, put, [k1, v1]),
    ok = rc_command(Node1, put, [k2, v2]),
    ok = rc_command(Node1, put, [k3, v3]),

    %%% GET FROM ANY OF THE NODES
    v1 = rc_command(Node1, get, [k1]),
    v2 = rc_command(Node1, get, [k2]),
    v3 = rc_command(Node1, get, [k3]),
    not_found = rc_command(Node1, get, [k10]),

    v1 = rc_command(Node2, get, [k1]),
    v2 = rc_command(Node2, get, [k2]),
    v3 = rc_command(Node2, get, [k3]),
    not_found = rc_command(Node2, get, [k10]),

    v1 = rc_command(Node3, get, [k1]),
    v2 = rc_command(Node3, get, [k2]),
    v3 = rc_command(Node3, get, [k3]),
    not_found = rc_command(Node3, get, [k10]),

    %% TEST RESET AND DELETE
    ok = rc_command(Node1, put, [k1, v_new]),
    v_new = rc_command(Node1, get, [k1]),

    v_new = rc_command(Node1, delete, [k1]),
    not_found = rc_command(Node1, get, [k1]),

    ok = rc_command(Node1, put, [k1, v_new]),
    v_new = rc_command(Node1, get, [k1]),
    ok.

exist_test(Config) ->
    Node1 = ?config(node1, Config),
    Node2 = ?config(node2, Config),
    Node3 = ?config(node3, Config),

    %% For nice tests
    Nodes = [Node1, Node2, Node3],
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
            false = rc_command(Node2, exist, [K])
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

build_cluster(Node1, Node2, Node3) ->
    rpc:call(Node2, riak_core, join, [Node1]),
    rpc:call(Node3, riak_core, join, [Node1]),
    ok.

rc_command(Node, Command) ->
    rc_command(Node, Command, []).
rc_command(Node, Command, Arguments) ->
    rpc:call(Node, rc_example, Command, Arguments).
