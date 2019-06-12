# etsmgr

`etsmgr` is a simple application that can act as the `heir` for ETS
tables owned by other applications/processes. This allows the table
owners to crash and restart without loosing the data stored in the
table.

The implementation is inspired by the two blog posts from Steve
Vinoski:

* [Don't Lose Your ets Tables](http://steve.vinoski.net/blog/2011/03/23/dont-lose-your-ets-tables/)
* [Implementation of “Don’t Lose Your ets Tables”](http://steve.vinoski.net/blog/2013/05/08/implementation-of-dont-lose-your-ets-tables/).

While adapting and embedding DeadZen's
[implementation](https://github.com/DeadZen/etsgive), as referenced in
the second article, would have sufficed, it was felt that a more
generic reusable approach would help future projects.

## Operation

`etsmgr` can manage multiple ETS tables for multiple client processes.

Multiple instances of `etsmgr` can co-exist on the same node by
starting the applications with unique instance names.

An instance of `etsmgr` can run as a standalone application, or be
embedded within the supervision tree of another application. The
former allows a single server to look after tables belonging to
independent applications, while the latter will let applications to
have their own table manager.

A client can request `etsmgr` to take an ETS table under management
with the `new_table/3,4` or `add_table/2,3` calls. The client will
then use `del_table/1,2` when table management is no longer required.

Internally, the `etsmgr` server will maintain a directory of client
pids and ETS table ids. We always use table ids, even for named ETS
tables. The directory entries will be indexed by a unique name
supplied by the client.

If a client process with a table managed by `etsmgr`
crashes/terminates, the table will not be lost. Once the client
restarts, it can ask `etsmgr` for the ownership of the table.

If `etsmgr` crashes, provided that the client has the `trap_exit` flag
set, it will receive an `'EXIT'` message with the process id of
`etsmgr`. This should trigger a mechanism in the client to reregister
its ETS table(s) with the new instance of `etsmgr` as soon as the
server restarts.

If the client does not set the `trap_exit` flag, then it will
terminate as soon as `etsmgr` terminates. However, this would be very
unusual in an application that cares about resilience!

It is up to the client software on how to wait and/or check for the
recovery of `etsmgr`. A single helper function, `wait4etsmgr`, has
been provided for a very simple scenario.

If a client terminates without deleting its ETS table(s), `etsmgr`
will inherit the managed client table(s) and keep the data alive until
the client restarts, or `etsmgr` itself terminates. Hence, it is
important for the client to delete the ETS table(s) prior to
**normal** termination.

### Simple operation

* An application (the client) starts, we assume `etsmgr` is already
  running.
* The client calls `new_table`, which
  * creates the table
  * links to the client
  * makes the client the owner
  * makes `etsmgr` the heir
  * returns the pid of the manager and the ETS table id
* The client needs the manager's pid in case the manager crashes
* Note that the client will also receive an `'ETS-TRANSFER'` message.
* Once the client has finished, it calls `del_table`, and deletes the
  table.

### Client restart

* As with the simple case above, but at some point the client crashes,
  and in due course restarts.
* Following the client termination, `etsmgr` will receive the `'EXIT'`
  message and update its internal state for the table(s) owned by the
  client.
* The client restarts, and calls `new_table` for its table(s).
* `etsmgr` will see that the table entry already exists, and subject
  to some further checks, it will give the table to the client.
* `etsmgr` does not distinguish between a client that has recovered
  from a crash and a client that happens to have chosen the same table
  name as an existing application, hence the above checks to ensure
  there are no obvious conflicts.

#### `etsmgr` crashes

* As with the second case, but instead of the client it's the `etsmgr`
  server that crashes, and restarts.
* The client will receive an `'EXIT'` message, and should wait for the
  `etsmgr` server to come back up.
* Once `etsmgr` is back up, the client calls `add_table`, so that
  `etsmgr` can continue managing the ETS table.

## Structure

The application consists of the following components

| Module       | Purpose                                          |
| :---------   | :---------                                       |
| `etsmgr`     | client API module                                |
| `etsmgr_app` | The main application for the standalone version  |
| `etsmgr_srv` | The main `gen_server` managing the ETS tables    |
| `etsmgr_sup` | The supervisor managing the `etsmgr_srv` server. |

For standalone mode, all four components are used. It is just a matter
of ensuring that the `etsmgr` application is started with or before
the client application.

For embedded mode, the main server, `etsmgr_srv`, can be put under the
supervision of the client application's supervisor. The embedding can
be done with or without the `etsmgr_sup` supervisor.

The client module, `etsmgr`, is used in both modes to communicate with
the table manager.

## API

The main component of `etsmgr` is the gen_server, `etsmgr_srv`. Below
are the functions for accessing the services of the manager.  There
are two variants of each function, one for the unnamed instance, and
one for a named instance.

### `etsmgr:new_table/3,4`

This will create a new ETS table, and the manager will link to the
client process. We expect this call when the client has started, or
restarted following a crash. In the latter case we would expect the
manager to be aware of the entry. On successful completion, the client
will be the owner of the ETS table, and `etsmgr_srv` will be the heir.

If a client has multiple ETS tables that need to be managed by
`etsmgr`, then `new_table` should be called multiple times, once per
table.

### `etsmgr:add_table/2,3`

This is similar to `new_table`, except it will not create a new ETS
table, but start managing an existing ETS table. We expect this call
when a client prefers to create the ETS table itself, or when the
manager has crashed and restarted and the client is reestablishing the
arrangement. On successful completion, the client will be the owner of
the ETS table, and `etsmgr_srv` will be the heir.

If a client has multiple ETS tables that need to be managed by
`etsmgr`, then `add_table` should be called multiple times, once per
table.

### `etsmgr:del_table/1,2`

This call is used when the client no longer needs a table to be
managed, such as prior to termination. It will be up to the client to
delete the ETS table.

If a client has multiple ETS tables that are being managed by
`etsmgr`, then `del_table` should be called multiple times, once per
table.

### `etsmgr:wait4etsmgr/0,1`

This is a helper function that can be used by the client to ensure
`etsmgr` is up and running, either at start up or following the
termination of `etsmgr`. The function will block until `etsmgr` is
available.

### `etsmgr:info/0,1`

This will return the tables currently under management as a map of
maps, or an empty map.

## Build

`rebar3` is used for all compilation and testing:

    $ rebar3 compile
	$ rebar3 eunit
	$ rebar3 dialyzer
	$ rebar3 edoc

## Feedback and contribution

All feedback and contribution is welcome. Please use the Github issue
tracker and pull requests for this.

The software is releasd under the Apache License, see the LICENSE
file. All code contributions should be provided under this license.


Enjoy!

Fred
