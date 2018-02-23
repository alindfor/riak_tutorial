-module(key_value_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(NAME(Node), element(1, Node)).

all() ->
  [
      ping_test,
      key_value_test,
      exist_test,
      coverage_test
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

coverage_test(Config) ->
    [Node1, Node2|_] = ?config(nodes, Config),

    ok = rc_command(Node1, clear),
    [] = rc_coverage(Node1, keys),
    [] = rc_coverage(Node1, values),

    ToKey =
        fun(N) ->
            "key" ++ integer_to_list(N)
        end,

    ToValue =
        fun(N) ->
            "value" ++ integer_to_list(N)
        end,

    Range = lists:seq(1, 100),

    lists:foreach(
        fun(N) ->
            ok = rc_command(Node1, put, [ToKey(N), ToValue(N)])
        end, Range
    ),

    ActualKeys = rc_coverage(Node2, keys),
    ActualValues = rc_coverage(Node2, values),

    100 = length(ActualKeys),
    100 = length(ActualValues),

    true = have_same_elements(ActualKeys, lists:map(ToKey, Range)),
    true = have_same_elements(ActualValues, lists:map(ToValue, Range)),

    ok = rc_command(Node1, clear),
    [] = rc_coverage(Node1, keys),
    [] = rc_coverage(Node1, values),

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

rc_coverage(Node, Command) ->
    {ok, List} = rc_command(Node, Command),
    %% convert coverage result to plain list
    lists:foldl(
        fun({_Partition, _Node, Values}, Accum) ->
            lists:append(Accum, Values)
        end,[], List).

have_same_elements(L1, L2) ->
    S1 = sets:from_list(L1),
    S2 = sets:from_list(L2),
    sets:is_subset(S1, S2) andalso sets:is_subset(S2,S1).