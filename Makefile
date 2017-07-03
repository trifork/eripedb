compile:
	./rebar compile < /dev/null

clean:
	./rebar clean < /dev/null

test:
	./rebar eunit < /dev/null

xref:
	./rebar xref skip_deps=true </dev/null

.PHONY: compile clean test xref


console:
	erl -pa ebin -eval 'c:l(eripedb_parser), c:l(eripedb).'

.PHONY: console
