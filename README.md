`eripedb` &mdash; Erlang implementation of IP to ASN database
=============================================================

This library maps IP addresses to ASN identifiers,
based on the RIPE database.

Both IPv4 and IPv6 addresses are supported.

The library does not include the actual database.
The latest version of the database can be downloaded from these locations:

  <ftp://ftp.ripe.net/ripe/dbase/split/ripe.db.route.gz>

  <ftp://ftp.ripe.net/ripe/dbase/split/ripe.db.route6.gz>

The library can use these files in their compressed states;
you do not need to uncompress them.
(Although you may if you wish; both compressed and uncompressed
database files are supported.)

Example use:

    > eripedb:lookup(ipv4, {172,217,22,174}).
    {ok,<<"AS1849">>}

Example configuration:

    {eripedb,
     [{database_files, ["ripe.db.route.gz",
                        "ripe.db.route6.gz"]}
     ]}
