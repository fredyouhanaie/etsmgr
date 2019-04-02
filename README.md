# etsmgr

`etsmgr` is a simple application that can act as the `heir` for ETS
tables owned by other applications/processes. This allows the table
owners to crash and restart without loosing the data in the table.

The implementation was inspired by the two blog posts from Steve Vinoski:

* [Don't Lose Your ets Tables](http://steve.vinoski.net/blog/2011/03/23/dont-lose-your-ets-tables/)
* [Implementation of “Don’t Lose Your ets Tables”](http://steve.vinoski.net/blog/2013/05/08/implementation-of-dont-lose-your-ets-tables/).

While adapting and embedding DeadZen's implementation, as referenced
in the second article, would have sufficed, it was felt that a more
generic reusable approach would help future projects.

Some of the planned features of `etsmgr` are:

* Manage multiple tables and client processes.
* Run as an standalone application, or embedded within the supervision
  tree of another application.

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

The client module, `etsmgr`, is used in both mode to communicate with
the table manager.

## Build

    $ rebar3 compile
