aqlc
====

[![CI](https://github.com/mrshankly/aqlc/workflows/CI/badge.svg)](https://github.com/mrshankly/aqlc/actions?query=workflow%3ACI)
[![Hex.pm](https://img.shields.io/hexpm/v/aqlc)](https://hex.pm/packages/aqlc)

Erlang client for AntidoteDB's SQL interface (AQL). You will need at least
Erlang/OTP 22.0, this library won't compile in previous versions.

# Quick Start

### Connecting to the server

Assuming you have at least one AQL node running, you can connect to it with
`aqlc:connect/2`:

```erlang
Conn = aqlc:connect("127.0.0.1", 8321).
```

Once done, you should close the connection with `aqlc:close/1`:

```erlang
aqlc:close(Conn).
```

For instructions on how to launch AQL and AntidoteDB instances, check the
documentation [here](https://antidotedb.gitbook.io/documentation/) and
[here](https://github.com/mrshankly/secure-aql).

### Queries

To issue queries, use the `aqlc:query/2` function:

```erlang
CreateTable = "CREATE UPDATE-WINS TABLE Artist (ArtistID INT PRIMARY KEY, Name VARCHAR);".
aqlc:query(Conn, CreateTable).

aqlc:query(Conn, "INSERT INTO Artist (ArtistID, Name) VALUES (42, 'Joee');").

aqlc:query(Conn, "SELECT * FROM Artist WHERE ArtistID = 42;").
```

### Transactions

`aqlc:query/2` runs the given query in a single transaction. To run multiple
queries in a single transaction use `aqlc:start_transaction/1` and
`aqlc:query/3`:

```erlang
Tx = aqlc:start_transaction(Conn).
aqlc:query(Conn, "UPDATE Artist SET Name = 'Joe' WHERE ArtistID = 42;", Tx).
aqlc:query(Conn, "SELECT * FROM Artist WHERE ArtistID = 42;", Tx).
```

You may commit a transaction with `aqlc:commit_transaction/2`:

```erlang
aqlc:commit_transaction(Conn, Tx).
```

Or abort it with `aqlc:abort_transaction/2`:

```erlang
aqlc:abort_transaction(Conn, Tx).
```
