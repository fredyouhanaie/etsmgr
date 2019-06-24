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

## Build

`rebar3` is used for all build and test activities. All rebar3
commands should be run within this directory:

    $ rebar3 compile
	$ rebar3 dialyzer
	$ rebar3 shell

## Example run

### Start up

Start up via rebar3 and increment the counter:

	$ rebar3 shell
	===> Verifying dependencies...
	===> Compiling etscounter
	Erlang/OTP 22 [erts-10.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]
	
	Eshell V10.4  (abort with ^G)
	1> ===> The rebar3 shell is a development tool; to deploy applications in production, consider using releases (http://www.rebar3.org/docs/releases)
	===> Booted etsmgr
	===> Booted etscounter
	2019-06-24T19:06:09.126790+01:00 notice: etscounter_srv: got a table #Ref<0.3143534795.120979457.190595> from <0.148.0>, with data etsmgr.
	
	1> etscounter_srv:check().
	2019-06-24T19:06:23.061987+01:00 notice: etscounter_srv: Table: #Ref<0.3143534795.120979457.190595>, Data [{count,0}].
	ok

	2> etscounter_srv:count().
	2019-06-24T19:06:33.189959+01:00 notice: etscounter_srv: Counter 1.
	ok

	3> etscounter_srv:count().
	2019-06-24T19:06:35.075598+01:00 notice: etscounter_srv: Counter 2.
	ok

	4> etscounter_srv:check().
	2019-06-24T19:06:37.518082+01:00 notice: etscounter_srv: Table: #Ref<0.3143534795.120979457.190595>, Data [{count,2}].
	ok
	5> 


### Application crash (self terminate)

Let the `etscounter` server to self terminate:

	5> etscounter_srv:die().
	ok
	6> 2019-06-24T19:06:48.648467+01:00 error: ** Generic server etscounter_srv terminating , ** Last message in was {'$gen_cast',{die}}, ** When Server state == [[{'$initial_call',{etscounter_srv,init,1}},{'$ancestors',[etscounter_sup,<0.151.0>]}],{state,<0.148.0>,#Ref<0.3143534795.120979457.190595>}], ** Reason for termination ==, ** {kill,[{etscounter_srv,handle_cast,2,[{file,"/home/fy/Projects/etsmgr/etsmgr-src/Examples/etscounter/src/etscounter_srv.erl"},{line,127}]},{gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,637}]},{gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,711}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}
	2019-06-24T19:06:48.648935+01:00 error: crasher: initial call: etscounter_srv:init/1, pid: <0.153.0>, registered_name: etscounter_srv, exit: {kill,[{etscounter_srv,handle_cast,2,[{file,"/home/fy/Projects/etsmgr/etsmgr-src/Examples/etscounter/src/etscounter_srv.erl"},{line,127}]},{gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,637}]},{gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,711}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ancestors: [etscounter_sup,<0.151.0>], message_queue_len: 0, messages: [], links: [<0.152.0>,<0.148.0>], dictionary: [], trap_exit: true, status: running, heap_size: 6772, stack_size: 27, reductions: 11788; neighbours:
	2019-06-24T19:06:48.649499+01:00 notice: etsmgr_srv:ETS-TRANSFER: for table=#Ref<0.3143534795.120979457.190595>, from pid=<0.153.0>, heir_data=etscounter.
	2019-06-24T19:06:48.649646+01:00 notice: etsmgr_srv:EXIT: from pid=<0.153.0>, reason=kill.
	2019-06-24T19:06:48.649768+01:00 error: supervisor: {local,etscounter_sup}, errorContext: child_terminated, reason: kill, offender: [{pid,<0.153.0>},{id,etscounter_srv},{mfargs,{etscounter_srv,start_link,[]}},{restart_type,permanent},{shutdown,5000},{child_type,worker}]
	2019-06-24T19:06:48.650030+01:00 notice: etscounter_srv: got a table #Ref<0.3143534795.120979457.190595> from <0.148.0>, with data etsmgr.

	6> etscounter_srv:check().
	2019-06-24T19:06:53.822097+01:00 notice: etscounter_srv: Table: #Ref<0.3143534795.120979457.190595>, Data [{count,2}].
	ok

	7> etscounter_srv:count().
	ok
	8> 2019-06-24T19:06:58.813759+01:00 notice: etscounter_srv: Counter 3.

	8> etscounter_srv:count().
	ok
	9> 2019-06-24T19:07:09.622162+01:00 notice: etscounter_srv: Counter 4.
	



### Application crash

Kill the application server, `etscounter_srv` from outside:

	9> exit(whereis(etscounter_srv), kill).
	true
	10> 2019-06-24T19:07:33.646743+01:00 notice: etsmgr_srv:ETS-TRANSFER: for table=#Ref<0.3143534795.120979457.190595>, from pid=<0.160.0>, heir_data=etscounter.
	2019-06-24T19:07:33.647256+01:00 notice: etsmgr_srv:EXIT: from pid=<0.160.0>, reason=killed.
	2019-06-24T19:07:33.647032+01:00 error: supervisor: {local,etscounter_sup}, errorContext: child_terminated, reason: killed, offender: [{pid,<0.160.0>},{id,etscounter_srv},{mfargs,{etscounter_srv,start_link,[]}},{restart_type,permanent},{shutdown,5000},{child_type,worker}]
	2019-06-24T19:07:33.647764+01:00 notice: etscounter_srv: got a table #Ref<0.3143534795.120979457.190595> from <0.148.0>, with data etsmgr.

	10> etscounter_srv:check().
	ok
	11> 2019-06-24T19:07:39.724325+01:00 notice: etscounter_srv: Table: #Ref<0.3143534795.120979457.190595>, Data [{count,4}].

	11> etscounter_srv:count().
	ok
	12> 2019-06-24T19:07:46.902302+01:00 notice: etscounter_srv: Counter 5.

	12> etscounter_srv:count().
	ok
	13> 2019-06-24T19:07:49.406015+01:00 notice: etscounter_srv: Counter 6.

### Table manager crash

We kill the table manager, `etsmgr`, but it is restarted by its
supervisor, and the table is intact:


	13> exit(whereis(etsmgr_srv), kill).
	2019-06-24T19:08:08.132937+01:00 warning: etscounter_srv: etsmgr (<0.148.0>) has died, with Reason killed.
	true
	2019-06-24T19:08:08.132916+01:00 error: supervisor: {local,etsmgr_sup}, errorContext: child_terminated, reason: killed, offender: [{pid,<0.148.0>},{id,etsmgr_srv},{mfargs,{etsmgr_srv,start_link,[etsmgr]}},{restart_type,permanent},{shutdown,5000},{child_type,worker}]
	14> 2019-06-24T19:08:08.133083+01:00 notice: etscounter_srv: waiting for etsmgr to restart.
	2019-06-24T19:08:09.134405+01:00 error: ** Generic server etscounter_srv terminating , ** Last message in was {'EXIT',<0.148.0>,killed}, ** When Server state == [[{'$initial_call',{etscounter_srv,init,1}},{'$ancestors',[etscounter_sup,<0.151.0>]}],{state,<0.148.0>,#Ref<0.3143534795.120979457.190595>}], ** Reason for termination ==, ** {{badmatch,{ok,<0.169.0>,#Ref<0.3143534795.120979457.190595>}},[{etscounter_srv,handle_info,2,[{file,"/home/fy/Projects/etsmgr/etsmgr-src/Examples/etscounter/src/etscounter_srv.erl"},{line,160}]},{gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,637}]},{gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,711}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}
	2019-06-24T19:08:09.135366+01:00 error: crasher: initial call: etscounter_srv:init/1, pid: <0.165.0>, registered_name: etscounter_srv, error: {{badmatch,{ok,<0.169.0>,#Ref<0.3143534795.120979457.190595>}},[{etscounter_srv,handle_info,2,[{file,"/home/fy/Projects/etsmgr/etsmgr-src/Examples/etscounter/src/etscounter_srv.erl"},{line,160}]},{gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,637}]},{gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,711}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, ancestors: [etscounter_sup,<0.151.0>], message_queue_len: 0, messages: [], links: [<0.152.0>,<0.169.0>], dictionary: [], trap_exit: true, status: running, heap_size: 6772, stack_size: 27, reductions: 12889; neighbours:
	2019-06-24T19:08:09.136355+01:00 notice: etsmgr_srv:ETS-TRANSFER: for table=#Ref<0.3143534795.120979457.190595>, from pid=<0.165.0>, heir_data=etscounter.
	2019-06-24T19:08:09.136380+01:00 error: supervisor: {local,etscounter_sup}, errorContext: child_terminated, reason: {{badmatch,{ok,<0.169.0>,#Ref<0.3143534795.120979457.190595>}},[{etscounter_srv,handle_info,2,[{file,"/home/fy/Projects/etsmgr/etsmgr-src/Examples/etscounter/src/etscounter_srv.erl"},{line,160}]},{gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,637}]},{gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,711}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}, offender: [{pid,<0.165.0>},{id,etscounter_srv},{mfargs,{etscounter_srv,start_link,[]}},{restart_type,permanent},{shutdown,5000},{child_type,worker}]
	2019-06-24T19:08:09.136554+01:00 notice: etsmgr_srv:EXIT: from pid=<0.165.0>, reason={{badmatch,{ok,<0.169.0>,#Ref<0.3143534795.120979457.190595>}},[{etscounter_srv,handle_info,2,[{file,"/home/fy/Projects/etsmgr/etsmgr-src/Examples/etscounter/src/etscounter_srv.erl"},{line,160}]},{gen_server,try_dispatch,4,[{file,"gen_server.erl"},{line,637}]},{gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,711}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,249}]}]}.
	2019-06-24T19:08:09.137038+01:00 notice: etscounter_srv: got a table #Ref<0.3143534795.120979457.190595> from <0.169.0>, with data etsmgr.

	14> etscounter_srv:check().
	2019-06-24T19:08:15.101640+01:00 notice: etscounter_srv: Table: #Ref<0.3143534795.120979457.190595>, Data [{count,6}].
	ok

	15> etscounter_srv:count().
	2019-06-24T19:08:20.109055+01:00 notice: etscounter_srv: Counter 7.
	ok

	16> etscounter_srv:count().
	2019-06-24T19:08:21.813889+01:00 notice: etscounter_srv: Counter 8.
	ok
	17> 

