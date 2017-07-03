compile:
	./rebar compile < /dev/null

clean:
	./rebar clean < /dev/null


console:
	erl -pa ebin -eval 'c:l(eripedb_parser), c:l(eripedb).'

xref:
	./rebar xref skip_deps=true </dev/null
