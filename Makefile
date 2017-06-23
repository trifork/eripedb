compile:
	./rebar compile < /dev/null


console:
	erl -pa ebin -eval 'c:l(eripedb_parser), c:l(eripedb).'
