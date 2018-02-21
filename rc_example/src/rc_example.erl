-module(rc_example).

-export([
    ping/0,
    ring_status/0]).

ping()->
    Key = os:timestamp(),
    DocIdx = hash_key(Key),
    PrefList = riak_core_apl:get_apl(DocIdx, 1, rc_example),
    [IndexNode] = PrefList,
    Command = ping,
    riak_core_vnode_master:sync_spawn_command(IndexNode, Command, rc_example_vnode_master).

ring_status() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    riak_core_ring:pretty_print(Ring, [legend]).

%% Internal

hash_key(Key) ->
    riak_core_util:chash_key({<<"rc_example">>, term_to_binary(Key)}).
