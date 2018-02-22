.PHONY: dev1 dev2 dev3 test clean

dev1:
	rebar3 as dev1 release && _build/dev1/rel/rc_example/bin/rc_example

dev2:
	rebar3 as dev2 release && _build/dev2/rel/rc_example/bin/rc_example

dev3:
	rebar3 as dev3 release && _build/dev3/rel/rc_example/bin/rc_example

test:
	rebar3 ct --name test@127.0.0.1

clean:
	rm -rf _build/dev1/rel/rc_example/data*;
	rm -rf _build/dev2/rel/rc_example/data*;
	rm -rf _build/dev3/rel/rc_example/data*
