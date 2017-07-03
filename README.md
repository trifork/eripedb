Erlang implementation of IP to ASN database
===========================================

This library maps IP addresses to ASN identifiers,
based on the RIPE database.

Both IPv4 and IPv6 addresses are supported.
The latest version of the database (which the library needs)
can be downloaded from:

  ftp://ftp.ripe.net/ripe/dbase/split/ripe.db.route.gz
  ftp://ftp.ripe.net/ripe/dbase/split/ripe.db.route6.gz

The library can use these files in their compressed states;
you do not need to uncompress them.

Example use:

    > eripedb:lookup(ipv4, {172,217,22,174}).
    {ok,<<"AS1849">>}

Example configuration:

    TODO