# etscounter

`etscounter` is a simple example to demonstrate how `etsmgr` can be
used to make an application's ETS tables resilient to the crashes.

The example can also be used to experiment with various recovery
schemes when `etsmgr` crashes and restarts.

The `etscounter` application itself is based on DeadZen's
[etsgive](https://github.com/DeadZen/etsgive) example. However, here
we have distinct implementations of the ETS based application,
`etscounter`, and the ETS table manager, `etsmgr`.

The server, `etscounter_srv`, maintains a counter using an entry in an
ETS table. The counter starts at zero and can only count up.

On start up, `etscounter_srv` will wait for `etsmgr_srv` to start, it
will then ask `etsmgr` to create and manage an ETS table.

Once up and running, `etscounter` will accept and process any of the
following requests:

* `etscounter_srv:count/0` - The counter in the ETS table is
  incremented, and the current count reported via `logger`.
* `etscounter_srv:check/0` - the server dumps the contents of the ETS
  table via `logger`.
* `etscounter_srv:die/0` - the server kills itself, this should result
  in a restart via the supervisor.

---

## Build

`rebar3` is used for all build and test activities. All rebar3
commands should be run within this directory:

    $ rebar3 compile
	$ rebar3 dialyzer
	$ rebar3 shell

---

## The `etscounter` client

This is a helper module for working with the `etscounter`
application. It allows the start and stop of the application with
`start/0` and `stop/0`.

The `etsmgr` application will be started by `etscounter:start/0`, if
it is not running. However, `etscounter:stop/0` will not stop it.

The two functions `count/0` and `check/0` increment and print the
counter stored the ETS table.

The function `die/0` will kill the `etscounter_srv` server. This is
there to simulate a crash.

---

## Example run

### Application start / crash / stop

Below is a sample run of the application and increment the counter a few times:

```console
$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling etscounter
Erlang/OTP 27 [erts-15.1.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit:ns]

Eshell V15.1.2 (press Ctrl+G to abort, type help(). for help)
1> etscounter:start().
=NOTICE REPORT==== 27-Nov-2024::18:33:41.991055 ===
etscounter_srv: got a table #Ref<0.1206219756.3245473793.80306> from <0.215.0>, with data etsmgr.
ok
2> etscounter:check().
ok
=NOTICE REPORT==== 27-Nov-2024::18:33:52.617686 ===
etscounter_srv: Table: #Ref<0.1206219756.3245473793.80306>, Data [{count,0}].
3> etscounter:count().
=NOTICE REPORT==== 27-Nov-2024::18:34:00.786521 ===
etscounter_srv: Counter 1.
ok
4> etscounter:count().
=NOTICE REPORT==== 27-Nov-2024::18:34:01.828353 ===
etscounter_srv: Counter 2.
ok
5> etscounter:count().
=NOTICE REPORT==== 27-Nov-2024::18:34:03.270734 ===
etscounter_srv: Counter 3.
ok
6> etscounter:check().
=NOTICE REPORT==== 27-Nov-2024::18:34:09.467376 ===
etscounter_srv: Table: #Ref<0.1206219756.3245473793.80306>, Data [{count,3}].
ok
```

Now that we have some data in the table, lets kill the server. Once
`etsmgr` detects the server exit, it takes over the table.

In the mean time, the supervisor restarts the server.

```console
7> etscounter:die().
=NOTICE REPORT==== 27-Nov-2024::18:34:15.418274 ===
etscounter_srv: about to die.
=NOTICE REPORT==== 27-Nov-2024::18:34:15.418749 ===
etsmgr_srv:handle_exit: from pid=<0.219.0>, reason=killed.
ok
=SUPERVISOR REPORT==== 27-Nov-2024::18:34:15.419045 ===
    supervisor: {local,etscounter_sup}
    errorContext: child_terminated
    reason: killed
    offender: [{pid,<0.219.0>},
               {id,etscounter_srv},
               {mfargs,{etscounter_srv,start_link,[]}},
               {restart_type,permanent},
               {significant,false},
               {shutdown,5000},
               {child_type,worker}]

=NOTICE REPORT==== 27-Nov-2024::18:34:15.420143 ===
etscounter_srv: got a table #Ref<0.1206219756.3245473793.80306> from <0.215.0>, with data etsmgr.
```

After the server is restart, it gets the old table. At this point the
data in the table is intact, and we can increment the counter:

```console
8> etscounter:check().
ok
=NOTICE REPORT==== 27-Nov-2024::18:34:22.875673 ===
etscounter_srv: Table: #Ref<0.1206219756.3245473793.80306>, Data [{count,3}].
9> etscounter:count().
ok
=NOTICE REPORT==== 27-Nov-2024::18:34:27.527989 ===
etscounter_srv: Counter 4.
10> etscounter:count().
=NOTICE REPORT==== 27-Nov-2024::18:34:28.473545 ===
etscounter_srv: Counter 5.
ok
11> etscounter:check().
ok
=NOTICE REPORT==== 27-Nov-2024::18:34:40.571460 ===
etscounter_srv: Table: #Ref<0.1206219756.3245473793.80306>, Data [{count,5}].
```

When we stop the application properly, `etsmgr` forgets about it too:

```console
12> etsmgr:info().
#{etscounter =>
      #{tabid => #Ref<0.1206219756.3245473793.80306>,
        clipid => <0.226.0>}}
13> etscounter:stop().
=NOTICE REPORT==== 27-Nov-2024::18:34:55.574259 ===
etscounter_srv: terminating
=INFO REPORT==== 27-Nov-2024::18:34:55.580286 ===
    application: etscounter
    exited: stopped
    type: temporary

ok
14> etsmgr:info().
#{}
```
---

### Table manager crash

We start the etscounter application and increment the counter a few times.

```console
$ rebar3 shell 
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling etscounter
Erlang/OTP 27 [erts-15.1.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [jit:ns]

Eshell V15.1.2 (press Ctrl+G to abort, type help(). for help)
1> etscounter:start().
=NOTICE REPORT==== 27-Nov-2024::21:45:19.596323 ===
etscounter_srv: got a table #Ref<0.2401600935.1396047875.201237> from <0.196.0>, with data etsmgr.
ok
2> etscounter:count().
=NOTICE REPORT==== 27-Nov-2024::21:45:26.132965 ===
etscounter_srv: Counter 1.
ok
3> etscounter:count().
=NOTICE REPORT==== 27-Nov-2024::21:45:27.240615 ===
etscounter_srv: Counter 2.
ok
4> etscounter:count().
=NOTICE REPORT==== 27-Nov-2024::21:45:28.149160 ===
etscounter_srv: Counter 3.
ok
5> etscounter:check().
=NOTICE REPORT==== 27-Nov-2024::21:45:32.700151 ===
etscounter_srv: Table: #Ref<0.2401600935.1396047875.201237>, Data [{count,3}].
ok
6> etscounter:check().
=NOTICE REPORT==== 27-Nov-2024::21:46:19.886322 ===
etscounter_srv: Table: #Ref<0.2401600935.1396047875.201237>, Data [{count,3}].
ok
7> etsmgr:info().
#{etscounter =>
      #{tabid => #Ref<0.2401600935.1396047875.201237>,
        clipid => <0.200.0>}}
```

We kill the `etsmgr_srv` server (pid `<0.196.0>`) to simulate a
crash. The `etscounter` application detects that and waits for
`etsmgr_srv` to restart.

```console
8> exit(whereis(etsmgr_srv), kill).
=WARNING REPORT==== 27-Nov-2024::21:46:49.951089 ===
etscounter_srv: etsmgr (<0.196.0>) has died, with Reason killed.
true
=NOTICE REPORT==== 27-Nov-2024::21:46:49.951550 ===
etscounter_srv: waiting for etsmgr to restart.
=SUPERVISOR REPORT==== 27-Nov-2024::21:46:49.951640 ===
    supervisor: {local,etsmgr_sup}
    errorContext: child_terminated
    reason: killed
    offender: [{pid,<0.196.0>},
               {id,etsmgr_srv},
               {mfargs,{etsmgr_srv,start_link,[etsmgr]}},
               {restart_type,permanent},
               {significant,false},
               {shutdown,5000},
               {child_type,worker}]
```

The `etsmgr` server is back up with new pid - `<0.210.0>`.

```console
9> etsmgr:info().
#{etscounter =>
      #{tabid => #Ref<0.2401600935.1396047875.201237>,
        clipid => <0.200.0>}}
10> etscounter:check().
=NOTICE REPORT==== 27-Nov-2024::21:48:57.707433 ===
etscounter_srv: Table: #Ref<0.2401600935.1396047875.201237>, Data [{count,3}].
ok
11> whereis(etsmgr_srv).
<0.210.0>
```

---
