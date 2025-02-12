%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2022. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(distribution_SUITE).

-define(VERSION_MAGIC,       131).

-define(ATOM_EXT,            100).
-define(REFERENCE_EXT,       101).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).
-define(ATOM_UTF8_EXT,       118).
-define(SMALL_ATOM_UTF8_EXT, 119).

%% Tests distribution and the tcp driver.

-include_lib("common_test/include/ct.hrl").

%-define(Line, erlang:display({line,?LINE}),).
-define(Line,).

-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         ping/1, bulk_send_small/1,
         group_leader/1,
         optimistic_dflags/1,
         bulk_send_big/1, bulk_send_bigbig/1,
         local_send_small/1, local_send_big/1,
         local_send_legal/1, link_to_busy/1, exit_to_busy/1,
         lost_exit/1, link_to_dead/1, link_to_dead_new_node/1,
         ref_port_roundtrip/1, nil_roundtrip/1,
         trap_bif_1/1, trap_bif_2/1, trap_bif_3/1,
         stop_dist/1,
         dist_auto_connect_never/1, dist_auto_connect_once/1,
         dist_parallel_send/1,
         atom_roundtrip/1,
         unicode_atom_roundtrip/1,
         contended_atom_cache_entry/1,
         contended_unicode_atom_cache_entry/1,
         bad_dist_structure/1,
         bad_dist_ext_receive/1,
         bad_dist_ext_process_info/1,
         bad_dist_ext_control/1,
         bad_dist_ext_connection_id/1,
         bad_dist_ext_size/1,
	 start_epmd_false/1, no_epmd/1, epmd_module/1,
         bad_dist_fragments/1,
         exit_dist_fragments/1,
         message_latency_large_message/1,
         message_latency_large_link_exit/1,
         message_latency_large_monitor_exit/1,
         message_latency_large_exit2/1,
         message_latency_large_message/0,
         message_latency_large_link_exit/0,
         message_latency_large_monitor_exit/0,
         message_latency_large_exit2/0,
         dist_entry_refc_race/1,
         system_limit/1,
         hopefull_data_encoding/1,
         hopefull_export_fun_bug/1,
         huge_iovec/1]).

%% Internal exports.
-export([sender/3, receiver2/2, dummy_waiter/0, dead_process/0,
         group_leader_1/1,
         optimistic_dflags_echo/0, optimistic_dflags_sender/1,
         roundtrip/1, bounce/1, do_dist_auto_connect/1, inet_rpc_server/1,
         dist_parallel_sender/3, dist_parallel_receiver/0,
         derr_run/1,
         dist_evil_parallel_receiver/0, make_busy/2]).

%% epmd_module exports
-export([start_link/0, register_node/2, register_node/3, port_please/2, address_please/3]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 4}}].

all() ->
    [ping, {group, bulk_send}, {group, local_send},
     group_leader,
     optimistic_dflags,
     link_to_busy, exit_to_busy, lost_exit, link_to_dead,
     link_to_dead_new_node,
     ref_port_roundtrip, nil_roundtrip, stop_dist,
     {group, trap_bif}, {group, dist_auto_connect},
     dist_parallel_send, atom_roundtrip, unicode_atom_roundtrip,
     contended_atom_cache_entry, contended_unicode_atom_cache_entry,
     {group, message_latency}, exit_dist_fragments,
     {group, bad_dist}, {group, bad_dist_ext},
     dist_entry_refc_race,
     start_epmd_false, no_epmd, epmd_module, system_limit,
     hopefull_data_encoding, hopefull_export_fun_bug,
     huge_iovec].

groups() ->
    [{bulk_send, [], [bulk_send_small, bulk_send_big, bulk_send_bigbig]},
     {local_send, [],
      [local_send_small, local_send_big, local_send_legal]},
     {trap_bif, [], [trap_bif_1, trap_bif_2, trap_bif_3]},
     {dist_auto_connect, [],
      [dist_auto_connect_never, dist_auto_connect_once]},
     {bad_dist, [],
      [bad_dist_structure, bad_dist_fragments]},
     {bad_dist_ext, [],
      [bad_dist_ext_receive, bad_dist_ext_process_info,
       bad_dist_ext_size,
       bad_dist_ext_control, bad_dist_ext_connection_id]},
     {message_latency, [],
      [message_latency_large_message,
       message_latency_large_link_exit,
       message_latency_large_monitor_exit,
       message_latency_large_exit2]}
    ].

init_per_suite(Config) ->
    {ok, Apps} = application:ensure_all_started(os_mon),
    [{started_apps, Apps} | Config].

end_per_suite(Config) ->
    Apps = proplists:get_value(started_apps, Config),
    [application:stop(App) || App <- lists:reverse(Apps)],
    Config.

init_per_group(message_latency, Config) ->
    Free = free_memory(),
    if Free < 2048 ->
            {skip, "Not enough memory"};
       true ->
            Config
    end;
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

%% Tests pinging a node in different ways.
ping(Config) when is_list(Config) ->
    Times = 1024,

    %% Ping a non-existing node many times.  This used to crash the emulator
    %% on Windows.

    Host = hostname(),
    BadName = list_to_atom("__pucko__@" ++ Host),
    io:format("Pinging ~s (assumed to not exist)", [BadName]),
    test_server:do_times(Times, fun() -> pang = net_adm:ping(BadName)
                                end),

    %% Pings another node.

    {ok, OtherNode} = start_node(distribution_SUITE_other),
    io:format("Pinging ~s (assumed to exist)", [OtherNode]),
    test_server:do_times(Times, fun() -> pong = net_adm:ping(OtherNode) end),
    stop_node(OtherNode),

    %% Pings our own node many times.

    Node = node(),
    io:format("Pinging ~s (the same node)", [Node]),
    test_server:do_times(Times, fun() -> pong = net_adm:ping(Node) end),

    ok.

%% Test erlang:group_leader(_, ExternalPid), i.e. DOP_GROUP_LEADER
group_leader(Config) when is_list(Config) ->
    ?Line Sock = start_relay_node(group_leader_1, []),
    ?Line Sock2 = start_relay_node(group_leader_2, []),
    try
        ?Line Node2 = inet_rpc_nodename(Sock2),
        ?Line {ok, ok} = do_inet_rpc(Sock, ?MODULE, group_leader_1, [Node2])
    after
        ?Line stop_relay_node(Sock),
        ?Line stop_relay_node(Sock2)
    end,
    ok.

group_leader_1(Node2) ->
    ?Line ExtPid = spawn(Node2, fun F() ->
                                        receive {From, group_leader} ->
                                                From ! {self(), group_leader, group_leader()}
                                        end,
                                        F()
                                end),
    ?Line GL1 = self(),
    ?Line group_leader(GL1, ExtPid),
    ?Line ExtPid ! {self(), group_leader},
    ?Line {ExtPid, group_leader, GL1} = receive_one(),

    %% Kill connection and repeat test when group_leader/2 triggers auto-connect
    ?Line net_kernel:monitor_nodes(true),
    ?Line net_kernel:disconnect(Node2),
    ?Line {nodedown, Node2} = receive_one(),
    ?Line GL2 = spawn(fun() -> dummy end),
    ?Line group_leader(GL2, ExtPid),
    ?Line {nodeup, Node2} = receive_one(),
    ?Line ExtPid ! {self(), group_leader},
    ?Line {ExtPid, group_leader, GL2} = receive_one(),
    ok.

%% Test optimistic distribution flags toward pending connections (DFLAG_DIST_HOPEFULLY)
optimistic_dflags(Config) when is_list(Config) ->
    ?Line Sender = start_relay_node(optimistic_dflags_sender, []),
    ?Line Echo = start_relay_node(optimistic_dflags_echo, []),
    try
        ?Line {ok, ok} = do_inet_rpc(Echo, ?MODULE, optimistic_dflags_echo, []),

        ?Line EchoNode = inet_rpc_nodename(Echo),
        ?Line {ok, ok} = do_inet_rpc(Sender, ?MODULE, optimistic_dflags_sender, [EchoNode])
    after
        ?Line stop_relay_node(Sender),
        ?Line stop_relay_node(Echo)
    end,
    ok.

optimistic_dflags_echo() ->
    P = spawn(fun F() ->
                      receive {From, Term} ->
                              From ! {self(), Term}
                      end,
                      F()
              end),
    register(optimistic_dflags_echo, P),
    optimistic_dflags_echo ! {self(), hello},
    {P, hello} = receive_one(),
    ok.

optimistic_dflags_sender(EchoNode) ->
    ?Line net_kernel:monitor_nodes(true),

    optimistic_dflags_do(EchoNode, <<1:1>>),
    optimistic_dflags_do(EchoNode, fun lists:map/2),
    ok.

optimistic_dflags_do(EchoNode, Term) ->
    ?Line {optimistic_dflags_echo, EchoNode} ! {self(), Term},
    ?Line {nodeup, EchoNode} = receive_one(),
    ?Line {EchoPid, Term} = receive_one(),
    %% repeat with pid destination
    ?Line net_kernel:disconnect(EchoNode),
    ?Line {nodedown, EchoNode} = receive_one(),
    ?Line EchoPid ! {self(), Term},
    ?Line {nodeup, EchoNode} = receive_one(),
    ?Line {EchoPid, Term} = receive_one(),

    ?Line net_kernel:disconnect(EchoNode),
    ?Line {nodedown, EchoNode} = receive_one(),
    ok.


receive_one() ->
    receive M -> M after 1000 -> timeout end.


bulk_send_small(Config) when is_list(Config) ->
    bulk_send(64, 32).

bulk_send_big(Config) when is_list(Config) ->
    bulk_send(32, 64).

bulk_send(Terms, BinSize) ->
    ct:timetrap({seconds, 30}),

    io:format("Sending ~w binaries, each of size ~w K", [Terms, BinSize]),
    {ok, Node} = start_node(bulk_receiver),
    Recv = spawn(Node, erlang, apply, [fun receiver/2, [0, 0]]),
    Bin = binary:copy(<<253>>, BinSize*1024),
    Size = Terms*size(Bin),
    {Elapsed, {Terms, Size}} = test_server:timecall(?MODULE, sender,
                                                    [Recv, Bin, Terms]),
    stop_node(Node),
    {comment, integer_to_list(round(Size/1024/max(1,Elapsed))) ++ " K/s"}.

sender(To, _Bin, 0) ->
    To ! {done, self()},
    receive
        Any ->
            Any
    end;
sender(To, Bin, Left) ->
    To ! {term, Bin},
    sender(To, Bin, Left-1).

bulk_send_bigbig(Config) when is_list(Config) ->
    Terms = 32*5,
    BinSize = 4,
    {Rate1, MonitorCount1} = bulk_sendsend2(Terms, BinSize,   5),
    {Rate2, MonitorCount2} = bulk_sendsend2(Terms, BinSize, 995),
    Ratio = if MonitorCount2 == 0 -> MonitorCount1 / 1.0;
               true               -> MonitorCount1 / MonitorCount2
            end,
    Comment0 = io_lib:format("~p K/s, ~p K/s, "
                             "~p monitor msgs, ~p monitor msgs, "
                             "~.1f monitor ratio",
                             [Rate1,Rate2,MonitorCount1,
                              MonitorCount2,Ratio]),
    Comment = lists:flatten(Comment0),
    {comment,Comment}.

bulk_sendsend2(Terms, BinSize, BusyBufSize) ->
    ct:timetrap({seconds, 30}),

    io:format("\nSending ~w binaries, each of size ~w K",
              [Terms, BinSize]),
    {ok, NodeRecv} = start_node(bulk_receiver),
    Recv = spawn(NodeRecv, erlang, apply, [fun receiver/2, [0, 0]]),
    Bin = binary:copy(<<253>>, BinSize*1024),

    %% SLF LEFT OFF HERE.
    %% When the caller uses small hunks, like 4k via
    %% bulk_sendsend(32*5, 4), then (on my laptop at least), we get
    %% zero monitor messages.  But if we use "+zdbbl 5", then we
    %% get a lot of monitor messages.  So, if we can count up the
    %% total number of monitor messages that we get when running both
    %% default busy size and "+zdbbl 5", and if the 5 case gets
    %% "many many more" monitor messages, then we know we're working.

    {ok, NodeSend} = start_node(bulk_sender, "+zdbbl " ++
                                    integer_to_list(BusyBufSize)),
    _Send = spawn(NodeSend, erlang, apply,
                  [fun sendersender/4, [self(), Recv, Bin, Terms]]),
    {Elapsed, {_TermsN, SizeN}, MonitorCount} =
        receive
            %% On some platforms (Windows), the time taken is 0 so we
            %% simulate that some little time has passed.
            {sendersender, {0.0,T,MC}} ->
                {0.0015, T, MC};
            {sendersender, BigRes} ->
                BigRes
        end,
    stop_node(NodeRecv),
    stop_node(NodeSend),
    {round(SizeN/1024/Elapsed), MonitorCount}.

%% Sender process to be run on a slave node

sendersender(Parent, To, Bin, Left) ->
    erlang:system_monitor(self(), [busy_dist_port]),
    _ = spawn(fun() ->
                      sendersender_send(To, Bin, Left),
                      exit(normal)
              end),
    {USec, {Res, MonitorCount}} =
        timer:tc(fun() ->
                         sendersender_send(To, Bin, Left),
                         To ! {done, self()},
                         count_monitors(0)
                 end),
    Parent ! {sendersender, {USec/1000000, Res, MonitorCount}}.

sendersender_send(_To, _Bin, 0) ->
    ok;
sendersender_send(To, Bin, Left) ->
    To ! {term, Bin},
    sendersender_send(To, Bin, Left-1).

count_monitors(MonitorCount) ->
    receive
        {monitor, _Pid, _Type, _Info} ->
            count_monitors(MonitorCount + 1)
    after 0 ->
            receive
                {_,_}=Any ->
                    {Any,MonitorCount}
            end
    end.

%% Receiver process to be run on a slave node.

receiver(Terms, Size) ->
    receive
        {term, Bin} ->
            receiver(Terms+1, Size+byte_size(Bin));
        {done, ReplyTo} ->
            ReplyTo ! {Terms, Size}
    end.



%% Sends several big message to an non-registered process on the local node.
local_send_big(Config) when is_list(Config) ->
    Data0= ["Tests sending small and big messages to a non-existing ",
            "local registered process."],
    Data1=[Data0,[Data0, Data0, [Data0], Data0],Data0],
    Data2=Data0++lists:flatten(Data1)++
    list_to_binary(lists:flatten(Data1)),
    Func=fun() -> Data2= {arbitrary_name, node()} ! Data2 end,
    test_server:do_times(4096, Func),
    ok.

%% Sends a small message to an non-registered process on the local node.
local_send_small(Config) when is_list(Config) ->
    Data={some_stupid, "arbitrary", 'Data'},
    Func=fun() -> Data= {unregistered_name, node()} ! Data end,
    test_server:do_times(4096, Func),
    ok.

%% Sends data to a registered process on the local node, as if it was on another node.
local_send_legal(Config) when is_list(Config) ->
    Times=16384,
    Txt = "Some Not so random Data",
    Data={[Txt,Txt,Txt], [Txt,Txt,Txt]},
    Pid=spawn(?MODULE,receiver2, [0, 0]) ,
    true=register(registered_process, Pid),

    Func=fun() -> Data={registered_process, node()} ! Data end,
    TotalSize=size(Data)*Times,
    test_server:do_times(Times, Func),

    % Check that all msgs really came through.
    Me=self(),
    {done, Me}=
    {registered_process, node()} ! {done, Me},
    receive
        {Times, TotalSize} ->
            ok;
        _ ->
            ct:fail("Wrong number of msgs received.")
    end,
    ok.

receiver2(Num, TotSize) ->
    receive
        {done, ReplyTo} ->
            ReplyTo ! {Num, TotSize};
        Stuff ->
            receiver2(Num+1, TotSize+size(Stuff))
    end.

%% Test that link/1 to a busy distribution port works.
link_to_busy(Config) when is_list(Config) ->
    ct:timetrap({seconds, 60}),
    {ok, Node} = start_node(link_to_busy),
    Recv = spawn(Node, erlang, apply, [fun sink/1, [link_to_busy_sink]]),

    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
                 "true" -> start_busy_dist_port_tracer();
                 _ -> false
             end,

    %% We will spawn off a process which will try to link to the other
    %% node.  The linker process will not actually run until this
    %% process is suspended due to the busy distribution port (because
    %% of the big send).  When the link/1 is run, the linker
    %% process will block, too, because of the because busy port,
    %% and will later be restarted.

    do_busy_test(Node, fun () -> linker(Recv) end),

    %% Same thing, but we apply link/1 instead of calling it directly.

    do_busy_test(Node, fun () -> applied_linker(Recv) end),

    %% Same thing again, but we apply link/1 in the tail of a function.

    do_busy_test(Node, fun () -> tail_applied_linker(Recv) end),

    %% Done.
    stop_node(Node),
    stop_busy_dist_port_tracer(Tracer),
    ok.

linker(Pid) ->
    true = link(Pid),
    {links, Links} = process_info(self(), links),
    true = lists:member(Pid, Links).

applied_linker(Pid) ->
    true = apply(erlang, link, [Pid]),
    {links, Links} = process_info(self(), links),
    true = lists:member(Pid, Links).

tail_applied_linker(Pid) ->
    apply(erlang, link, [Pid]).

%% Test that exit/2 to a busy distribution port works.
exit_to_busy(Config) when is_list(Config) ->
    ct:timetrap({seconds, 60}),
    {ok, Node} = start_node(exit_to_busy),

    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
                 "true" -> start_busy_dist_port_tracer();
                 _ -> false
             end,

    %% We will spawn off a process which will try to exit a process on
    %% the other node.  That process will not actually run until this
    %% process is suspended due to the busy distribution port
    %% The process executing exit/2 will block,
    %% too, because of the busy distribution port, and will be allowed
    %% to continue when the port becomes non-busy.

    Recv1 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    M1 = erlang:monitor(process, Recv1),
    do_busy_test(Node, fun () -> joey_killer(Recv1) end),
    receive
        {'DOWN', M1, process, Recv1, R1} ->
            joey_said_die = R1
    end,

    %% Same thing, but tail call to exit/2.
    Recv2 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    M2 = erlang:monitor(process, Recv2),
    do_busy_test(Node, fun () -> tail_joey_killer(Recv2) end),
    receive
        {'DOWN', M2, process, Recv2, R2} ->
            joey_said_die = R2
    end,

    %% Same thing, but we apply exit/2 instead of calling it directly.
    Recv3 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    M3 = erlang:monitor(process, Recv3),
    do_busy_test(Node, fun () -> applied_joey_killer(Recv3) end),
    receive
        {'DOWN', M3, process, Recv3, R3} ->
            joey_said_die = R3
    end,

    %% Same thing again, but we apply exit/2 in the tail of a function.
    Recv4 = spawn(Node, fun () -> sink(exit_to_busy_sink) end),
    M4 = erlang:monitor(process, Recv4),
    do_busy_test(Node, fun () -> tail_applied_joey_killer(Recv4) end),
    receive
        {'DOWN', M4, process, Recv4, R4} ->
            joey_said_die = R4
    end,

    %% Done.
    stop_node(Node),
    stop_busy_dist_port_tracer(Tracer),
    ok.

make_busy_data() ->
    Size = 1024*1024,
    Key = '__busy__port__data__',
    case get(Key) of
        undefined ->
            Data = list_to_binary(lists:duplicate(Size, 253)),
            put(Key, Data),
            Data;
        Data ->
            true = is_binary(Data),
            true = size(Data) == Size,
            Data
    end.

make_busy(Node, Time) when is_integer(Time) ->
    Own = 500,
    freeze_node(Node, Time+Own),
    Data = make_busy_data(),
    DCtrl = dctrl(Node),
    %% first make port busy
    Pid = spawn_link(fun () ->
                             forever(fun () ->
                                             dctrl_dop_reg_send(Node,
                                                                '__noone__',
                                                                Data)
                                     end)
                     end),
    receive after Own -> ok end,
    until(fun () ->
                  case {DCtrl, process_info(Pid, status)} of
                      {DPrt, {status, suspended}} when is_port(DPrt) -> true;
                      {DPid, {status, waiting}} when is_pid(DPid) -> true;
                      _ -> false
                  end
          end),
    %% then dist entry
    make_busy(Node, [nosuspend], Data),
    Pid.

make_busy(Node, Opts, Data) ->
    case erlang:send({'__noone__', Node}, Data, Opts) of
        nosuspend -> nosuspend;
        _ -> make_busy(Node, Opts, Data)
    end.

unmake_busy(Pid) ->
    unlink(Pid),
    exit(Pid, bang).

do_busy_test(Node, Fun) ->
    Busy = make_busy(Node, 1000),
    {P, M} = spawn_monitor(Fun),
    receive after 100 -> ok end,
    Pinfo = process_info(P, [status, current_function]),
    unmake_busy(Busy),
    io:format("~p : ~p~n", [P, Pinfo]),
    case Pinfo of
        undefined ->
            receive
                {'DOWN', M, process, P, Reason} ->
                    io:format("~p died with exit reason ~p~n", [P, Reason])
            end,
            ct:fail(premature_death);
        _ ->
            %% Don't match arity; it is different in debug and
            %% optimized emulator
            [{status, suspended},
             {current_function, {Mod, Func, _}}] = Pinfo,
            if
                Mod =:= erlang andalso Func =:= bif_return_trap ->
                    true;
                Mod =:= erts_internal andalso Func =:= dsend_continue_trap ->
                    true;
                true ->
                    ct:fail({incorrect, pinfo, Pinfo})
            end,
            receive
                {'DOWN', M, process, P, Reason} ->
                    io:format("~p died with exit reason ~p~n", [P, Reason]),
                    verify_nc(node()),
                    verify_nc(Node),
                    normal = Reason
            end
    end.

remote_is_process_alive(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive,
             [Pid]).

joey_killer(Pid) ->
    exit(Pid, joey_said_die),
    until(fun () -> false == remote_is_process_alive(Pid) end).

tail_joey_killer(Pid) ->
    exit(Pid, joey_said_die).

applied_joey_killer(Pid) ->
    apply(erlang, exit, [Pid, joey_said_die]),
    until(fun () -> false == remote_is_process_alive(Pid) end).

tail_applied_joey_killer(Pid) ->
    apply(erlang, exit, [Pid, joey_said_die]).

sink(Name) ->
    register(Name, self()),
    sink1().

sink1() ->
    receive
        _Any -> sink1()
    end.

%% Test that EXIT and DOWN messages send to another node are not lost if
%% the distribution port is busy.
lost_exit(Config) when is_list(Config) ->
    {ok, Node} = start_node(lost_exit),

    Tracer = case os:getenv("TRACE_BUSY_DIST_PORT") of
                 "true" -> start_busy_dist_port_tracer();
                 _ -> false
             end,

    Self = self(),
    Die = make_ref(),
    R1 = spawn(fun () -> receive after infinity -> ok end end),
    MR1 = erlang:monitor(process, R1),

    {L1, ML1} = spawn_monitor(fun() ->
                                      link(R1),
                                      Self ! {self(), linked},
                                      receive
                                          Die ->
                                              exit(controlled_suicide)
                                      end
                              end),

    R2 = spawn(fun () ->
                       M = erlang:monitor(process, L1),
                       receive
                           {'DOWN', M, process, L1, R} ->
                               Self ! {self(), got_down_message, L1, R}
                       end
               end),

    receive {L1, linked} -> ok end,

    Busy = make_busy(Node, 2000),
    receive after 100 -> ok end,
    L1 ! Die,
    receive
        {'DOWN', ML1, process, L1, RL1} ->
            controlled_suicide = RL1
    end,
    receive after 500 -> ok end,
    unmake_busy(Busy),

    receive
        {'DOWN', MR1, process, R1, RR1} ->
            controlled_suicide = RR1
    end,

    receive
        {R2, got_down_message, L1, RR2} ->
            controlled_suicide = RR2
    end,

    %% Done.
    stop_busy_dist_port_tracer(Tracer),
    stop_node(Node),
    ok.

dummy_waiter() ->
    receive
    after infinity ->
              ok
    end.

%% Test that linking to a dead remote process gives an EXIT message
%% AND that the link is teared down.
link_to_dead(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Node} = start_node(link_to_dead),
    %    monitor_node(Node, true),
    net_adm:ping(Node), %% Ts_cross_server workaround.
    Pid = spawn(Node, ?MODULE, dead_process, []),
    receive
    after 5000 -> ok
    end,
    link(Pid),
    receive
        {'EXIT', Pid, noproc} ->
            ok;
        Other ->
            ct:fail({unexpected_message, Other})
    after 5000 ->
              ct:fail(nothing_received)
    end,
    {links, Links} = process_info(self(), links),
    io:format("Pid=~p, links=~p", [Pid, Links]),
    false = lists:member(Pid, Links),
    stop_node(Node),
    receive
        Message ->
            ct:fail({unexpected_message, Message})
    after 3000 ->
              ok
    end,
    ok.

dead_process() ->
    erlang:error(die).

%% Test that linking to a pid on node that has gone and restarted gives
%% the correct EXIT message (OTP-2304).
link_to_dead_new_node(Config) when is_list(Config) ->
    process_flag(trap_exit, true),

    %% Start the node, get a Pid and stop the node again.
    {ok, Node} = start_node(link_to_dead_new_node),
    Pid = spawn(Node, ?MODULE, dead_process, []),
    stop_node(Node),

    %% Start a new node with the same name.
    {ok, Node} = start_node(link_to_dead_new_node),
    link(Pid),
    receive
        {'EXIT', Pid, noproc} ->
            ok;
        Other ->
            stop_node(Node),
            ct:fail({unexpected_message, Other})
    after 5000 ->
              ct:fail(nothing_received)
    end,

    %% Make sure that the link wasn't created.
    {links, Links} = process_info(self(), links),
    io:format("Pid=~p, links=~p", [Pid, Links]),
    false = lists:member(Pid, Links),
    stop_node(Node),
    receive
        Message ->
            ct:fail({unexpected_message, Message})
    after 3000 ->
              ok
    end,
    ok.

%% Test that sending a port or reference to another node and back again
%% doesn't correct them in any way.
ref_port_roundtrip(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Port = make_port(),
    Ref = make_ref(),
    {ok, Node} = start_node(ref_port_roundtrip),
    net_adm:ping(Node),
    Term = {Port, Ref},
    io:format("Term before: ~p", [show_term(Term)]),
    Pid = spawn_link(Node, ?MODULE, roundtrip, [Term]),
    receive after 5000 -> ok end,
    stop_node(Node),
    receive
        {'EXIT', Pid, {Port, Ref}} ->
            io:format("Term after: ~p", [show_term(Term)]),
            ok;
        Other ->
            io:format("Term after: ~p", [show_term(Term)]),
            ct:fail({unexpected, Other})
    after 10000 ->
              ct:fail(timeout)
    end,
    ok.

make_port() ->
    hd(erlang:ports()).

roundtrip(Term) ->
    exit(Term).

%% Test that the smallest external term [] aka NIL can be sent to
%% another node node and back again.
nil_roundtrip(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    {ok, Node} = start_node(nil_roundtrip),
    net_adm:ping(Node),
    Pid = spawn_link(Node, ?MODULE, bounce, [self()]),
    Pid ! [],
    receive
        [] ->
            receive
                {'EXIT', Pid, []} ->
                    stop_node(Node),
                    ok
            end
    end.

bounce(Dest) ->
    receive Msg ->
                Dest ! Msg,
                exit(Msg)
    end.

show_term(Term) ->
    binary_to_list(term_to_binary(Term)).

%% Tests behaviour after net_kernel:stop (OTP-2586).
stop_dist(Config) when is_list(Config) ->
    Str = os:cmd(ct:get_progname()
                 ++ " -noshell -pa "
                 ++ proplists:get_value(data_dir, Config)
                 ++ " -s run"),
    %% The "true" may be followed by an error report, so ignore anything that
    %% follows it.
    "true\n"++_ = Str,

    %% "May fail on FreeBSD due to differently configured name lookup - ask Arndt",
    %% if you can find him.

    ok.


trap_bif_1(Config) when is_list(Config) ->
    {true} = tr1(),
    ok.

trap_bif_2(Config) when is_list(Config) ->
    {true} = tr2(),
    ok.

trap_bif_3(Config) when is_list(Config) ->
    {hoo} = tr3(),
    ok.

tr1() ->
    NonExisting = 'abc@boromir',
    X = erlang:monitor_node(NonExisting, true),
    {X}.

tr2() ->
    NonExisting = 'abc@boromir',
    X = apply(erlang, monitor_node, [NonExisting, true]),
    {X}.

tr3() ->
    NonExisting = 'abc@boromir',
    X = {NonExisting, glirp} ! hoo,
    {X}.




% This has to be done by nodes with differrent cookies, otherwise global
% will connect nodes, which is correct, but makes it hard to test.
% * Start two nodes, n1 and n2. n2 with the dist_auto_connect once parameter
% * n2 pings n1 -> connection
% * check that they now know each other
% * Kill n1
% * Make sure n2 gets pang when pinging n1
% * restart n1
% * Make sure n2 *still gets pang*!
% * Ping n2 from n1 -> pong
% * n2 now also gets pong when pinging n1
% * disconnect n2 from n1
% * n2 gets pang when pinging n1
% * n2 forces connection by using net_kernel:connect_node (ovverrides)
% * n2 gets pong when pinging n1.

%% Test the dist_auto_connect once kernel parameter
dist_auto_connect_once(Config) when is_list(Config) ->
    Sock = start_relay_node(dist_auto_connect_relay_node,[]),
    NN = inet_rpc_nodename(Sock),
    Sock2 = start_relay_node(dist_auto_connect_once_node,
                             "-kernel dist_auto_connect once"),
    NN2 = inet_rpc_nodename(Sock2),
    {ok,[]} = do_inet_rpc(Sock,erlang,nodes,[]),
    {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    {ok,[NN2]} = do_inet_rpc(Sock,erlang,nodes,[]),
    {ok,[NN]} = do_inet_rpc(Sock2,erlang,nodes,[]),
    [_,HostPartPeer] = string:lexemes(atom_to_list(NN),"@"),
    [_,MyHostPart] = string:lexemes(atom_to_list(node()),"@"),
    % Give net_kernel a chance to change the state of the node to up to.
    receive after 1000 -> ok end,
    case HostPartPeer of
        MyHostPart ->
            ok = stop_relay_node(Sock),
            {ok,pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]);
        _ ->
            {ok, true} = do_inet_rpc(Sock,net_kernel,disconnect,[NN2]),
            receive
            after 500 -> ok
            end
    end,
    {ok, []} = do_inet_rpc(Sock2,erlang,nodes,[]),
    Sock3 = case HostPartPeer of
                MyHostPart ->
                    start_relay_node(dist_auto_connect_relay_node,[]);
                _ ->
                    Sock
            end,
    TS1 = timestamp(),
    {ok, pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    TS2 = timestamp(),
    RefT = net_kernel:connecttime() - 1000,
    true = ((TS2 - TS1) < RefT),
    TS3 = timestamp(),
    {ok, true} = do_inet_rpc(Sock2,erlang,monitor_node,
                             [NN,true,[allow_passive_connect]]),
    TS4 = timestamp(),
    true = ((TS4 - TS3) > RefT),
    {ok, pong} = do_inet_rpc(Sock3,net_adm,ping,[NN2]),
    {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    {ok, true} = do_inet_rpc(Sock3,net_kernel,disconnect,[NN2]),
    receive
    after 500 -> ok
    end,
    {ok, pang} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    {ok, true} = do_inet_rpc(Sock2,net_kernel,connect_node,[NN]),
    {ok, pong} = do_inet_rpc(Sock2,net_adm,ping,[NN]),
    stop_relay_node(Sock3),
    stop_relay_node(Sock2).



%% Start a relay node and a lonely (dist_auto_connect never) node.
%% Lonely node pings relay node. That should fail. 
%% Lonely node connects to relay node with net_kernel:connect_node/1.
%% Result is sent here through relay node.
dist_auto_connect_never(Config) when is_list(Config) ->
    Self = self(),
    {ok, RelayNode} = start_node(dist_auto_connect_relay),
    spawn(RelayNode,
          fun() ->
                  register(dist_auto_connect_relay, self()),
                  dist_auto_connect_relay(Self)
          end),
    {ok, Handle} = dist_auto_connect_start(dist_auto_connect, never),
    Result = receive
                 {do_dist_auto_connect, ok} ->
                     ok;
                 {do_dist_auto_connect, Error} ->
                     {error, Error};
                 %% The io:formats in dos_dist_auto_connect will
                 %% generate port output messages that are ok
                 Other when not is_port(element(1, Other))->
                     {error, Other}
             after 32000 ->
                       timeout
             end,
    stop_node(RelayNode),
    Stopped = dist_auto_connect_stop(Handle),
    Junk = receive
               {do_dist_auto_connect, _} = J -> J
           after 0 -> ok
           end,
    {ok, ok, ok} = {Result, Stopped, Junk},
    ok.


do_dist_auto_connect([never]) ->
    Node = list_to_atom("dist_auto_connect_relay@" ++ hostname()),
    io:format("~p:do_dist_auto_connect([false]) Node=~p~n", [?MODULE, Node]),
    Ping = net_adm:ping(Node),
    io:format("~p:do_dist_auto_connect([false]) Ping=~p~n", [?MODULE, Ping]),
    Result = case Ping of
                 pang -> ok;
                 _ -> {error, Ping}
             end,
    io:format("~p:do_dist_auto_connect([false]) Result=~p~n", [?MODULE, Result]),
    net_kernel:connect_node(Node),
    catch {dist_auto_connect_relay, Node} ! {do_dist_auto_connect, Result};
%    receive after 1000 -> ok end,
%    halt();

do_dist_auto_connect(Arg) ->
    io:format("~p:do_dist_auto_connect(~p)~n", [?MODULE, Arg]),
    receive after 10000 -> ok end,
    halt().


dist_auto_connect_start(Name, Value) when is_atom(Name) ->
    dist_auto_connect_start(atom_to_list(Name), Value);
dist_auto_connect_start(Name, Value) when is_list(Name), is_atom(Value) ->
    Node = list_to_atom(lists:append([Name, "@", hostname()])),
    ModuleDir = filename:dirname(code:which(?MODULE)),
    ValueStr = atom_to_list(Value),
    Cookie = atom_to_list(erlang:get_cookie()),
    Cmd = lists:append(
            [%"xterm -e ",
             ct:get_progname(),
             %	     " -noinput ",
             " -detached ",
             long_or_short(), " ", Name,
             " -setcookie ", Cookie,
             " -pa ", ModuleDir,
             " -s ", atom_to_list(?MODULE),
             " do_dist_auto_connect ", ValueStr,
             " -kernel dist_auto_connect ", ValueStr]),
    io:format("~p:dist_auto_connect_start() cmd: ~p~n", [?MODULE, Cmd]),
    Port = open_port({spawn, Cmd}, [stream]),
    {ok, {Port, Node}}.


dist_auto_connect_stop({Port, Node}) ->
    Pid = spawn_link(fun() -> rpc:call(Node, erlang, halt, []) end),
    dist_auto_connect_stop(Port, Node, Pid, 5000).

dist_auto_connect_stop(Port, _Node, Pid, N) when is_integer(N), N =< 0 ->
    exit(Pid, normal),
    catch erlang:port_close(Port),
    Result = {error, node_not_down},
    io:format("~p:dist_auto_connect_stop() ~p~n", [?MODULE, Result]),
    Result;
dist_auto_connect_stop(Port, Node, Pid, N) when is_integer(N) ->
    case net_adm:ping(Node) of
        pong ->
            receive after 100 -> ok end,
            dist_auto_connect_stop(Port, Node, Pid, N-100);
        pang ->
            exit(Pid, normal),
            catch erlang:port_close(Port),
            io:format("~p:dist_auto_connect_stop() ok~n", [?MODULE]),
            ok
    end.


dist_auto_connect_relay(Parent) ->
    receive X ->
                catch Parent ! X
    end,
    dist_auto_connect_relay(Parent).


dist_parallel_send(Config) when is_list(Config) ->
    %% Disabled "connect all" so global wont interfere...
    {ok, RNode} = start_node(dist_parallel_receiver, "-connect_all false"),
    {ok, SNode} = start_node(dist_parallel_sender, "-connect_all false"),

    %% WatchDog = spawn_link(
    %%             fun () ->
    %%                     TRef = erlang:start_timer((2*60*1000), self(), oops),
    %%                     receive
    %%                         {timeout, TRef, _ } ->
    %%                             spawn(SNode, fun () -> abort(timeout) end),
    %%                             spawn(RNode, fun () -> abort(timeout) end)
    %%                              %%       rpc:cast(SNode, erlang, halt,
    %%                              %%		["Timetrap (sender)"]),
    %%                              %%       rpc:cast(RNode, erlang, halt,
    %%                              %%		["Timetrap (receiver)"])
    %%                      end
    %%              end),

    MkSndrs = fun (Receiver) ->
                      lists:map(fun (_) ->
                                        spawn_link(SNode,
                                                   ?MODULE,
                                                   dist_parallel_sender,
                                                   [self(), Receiver, 1000])
                                end, lists:seq(1, 64))
              end,
    Parent = self(),
    SndrsStart = fun (Sndrs) ->
                         spawn_link(SNode,
                           fun () ->
                                   lists:foreach(fun (P) ->
                                                         P ! {go, Parent}
                                                 end, Sndrs),
                                   unlink(Parent)
                           end)
                 end,
    SndrsWait = fun (Sndrs) ->
                        lists:foreach(fun (P) ->
                                              receive
                                                  {P, done} ->
                                                      unlink(P),
                                                      ok
                                              end
                                      end, Sndrs)
                end,
    DPR = spawn_link(RNode, ?MODULE, dist_parallel_receiver, []),
    Sndrs1 = MkSndrs(DPR),
    SndrsStart(Sndrs1),
    SndrsWait(Sndrs1),
    unlink(DPR),
    exit(DPR, bang),

    DEPR = spawn_link(RNode, ?MODULE, dist_evil_parallel_receiver, []),
    Sndrs2 = MkSndrs(DEPR),
    SndrsStart(Sndrs2),
    SndrsWait(Sndrs2),
    unlink(DEPR),
    exit(DEPR, bang),

    %% unlink(WatchDog),
    %% exit(WatchDog, bang),

    stop_node(RNode),
    stop_node(SNode),

    ok.

do_dist_parallel_sender(Parent, _Receiver, 0) ->
    Parent ! {self(), done};
do_dist_parallel_sender(Parent, Receiver, N) ->
    Receiver ! {self(), "Some data"},
    do_dist_parallel_sender(Parent, Receiver, N-1).

dist_parallel_sender(Parent, Receiver, N) ->
    receive {go, Parent} -> ok end,
    do_dist_parallel_sender(Parent, Receiver, N).

dist_parallel_receiver() ->
    receive {_Sender, _Data} -> ok end,
    dist_parallel_receiver().

dist_evil_parallel_receiver() ->
    receive {Sender, _Data} -> ok end,
    net_kernel:disconnect(node(Sender)),
    dist_evil_parallel_receiver().

atom_roundtrip(Config) when is_list(Config) ->
    AtomData = atom_data(),
    verify_atom_data(AtomData),
    {ok, Node} = start_node(Config),
    do_atom_roundtrip(Node, AtomData),
    stop_node(Node),
    ok.

unicode_atom_roundtrip(Config) when is_list(Config) ->
    AtomData = unicode_atom_data(),
    verify_atom_data(AtomData),
    {ok, Node} = start_node(Config),
    do_atom_roundtrip(Node, AtomData),
    stop_node(Node),
    ok.

do_atom_roundtrip(Node, AtomData) ->
    Parent = self(),
    Proc = spawn_link(Node, fun () -> verify_atom_data_loop(Parent) end),
    Proc ! {self(), AtomData},
    receive {Proc, AD1} -> AtomData = AD1 end,
    Proc ! {self(), AtomData},
    receive {Proc, AD2} -> AtomData = AD2 end,
    RevAtomData = lists:reverse(AtomData),
    Proc ! {self(), RevAtomData},
    receive {Proc, RAD1} -> RevAtomData = RAD1 end,
    unlink(Proc),
    exit(Proc, bang),
    ok.

verify_atom_data_loop(From) ->
    receive
        {From, AtomData} ->
            verify_atom_data(AtomData),
            From ! {self(), AtomData},
            verify_atom_data_loop(From)
    end.

atom_data() ->
    lists:map(fun (N) ->
                      ATxt = "a"++integer_to_list(N),
                      {list_to_atom(ATxt), ATxt}
              end,
              lists:seq(1, 2000)).

verify_atom_data(AtomData) ->
    lists:foreach(fun ({Atom, AtomTxt}) when is_atom(Atom) ->
                          AtomTxt = atom_to_list(Atom);
                      ({PPR, AtomTxt}) ->
                          % Pid, Port, or Ref
                          AtomTxt = atom_to_list(node(PPR))
                  end,
                  AtomData).

uc_atom_tup(ATxt) ->
    Atom = string_to_atom(ATxt),
    ATxt = atom_to_list(Atom),
    {Atom, ATxt}.

uc_pid_tup(ATxt) ->
    ATxtExt = string_to_atom_ext(ATxt),
    Pid = mk_pid({ATxtExt, 1}, 4711,17),
    true = is_pid(Pid),
    Atom = node(Pid),
    true = is_atom(Atom),
    ATxt = atom_to_list(Atom),
    {Pid, ATxt}.

uc_port_tup(ATxt) ->
    ATxtExt = string_to_atom_ext(ATxt),
    Port = mk_port({ATxtExt, 2}, 4711),
    true = is_port(Port),
    Atom = node(Port),
    true = is_atom(Atom),
    ATxt = atom_to_list(Atom),
    {Port, ATxt}.

uc_ref_tup(ATxt) ->
    ATxtExt = string_to_atom_ext(ATxt),
    Ref = mk_ref({ATxtExt, 3}, [4711,17, 4711]),
    true = is_reference(Ref),
    Atom = node(Ref),
    true = is_atom(Atom),
    ATxt = atom_to_list(Atom),
    {Ref, ATxt}.


unicode_atom_data() ->
    [uc_pid_tup(lists:seq(16#1f600, 16#1f600+249) ++ "@host"),
     uc_pid_tup(lists:seq(16#1f600, 16#1f600+30) ++ "@host"),
     uc_port_tup(lists:seq(16#1f600, 16#1f600+249) ++ "@host"),
     uc_port_tup(lists:seq(16#1f600, 16#1f600+30) ++ "@host"),
     uc_ref_tup(lists:seq(16#1f600, 16#1f600+249) ++ "@host"),
     uc_ref_tup(lists:seq(16#1f600, 16#1f600+30) ++ "@host"),
     uc_atom_tup(lists:seq(16#1f600, 16#1f600+254)),
     uc_atom_tup(lists:seq(16#1f600, 16#1f600+63)),
     uc_atom_tup(lists:seq(0, 254)),
     uc_atom_tup(lists:seq(100, 163)),
     uc_atom_tup(lists:seq(200, 354)),
     uc_atom_tup(lists:seq(200, 263)),
     uc_atom_tup(lists:seq(2000, 2254)),
     uc_atom_tup(lists:seq(2000, 2063)),
     uc_atom_tup(lists:seq(65500, 65754)),
     uc_atom_tup(lists:seq(65500, 65563))
     | lists:map(fun (N) ->
                         uc_atom_tup(lists:seq(64000+N, 64254+N))
                 end, lists:seq(1, 2000))].

contended_atom_cache_entry(Config) when is_list(Config) ->
    contended_atom_cache_entry_test(Config, latin1).

contended_unicode_atom_cache_entry(Config) when is_list(Config) ->
    contended_atom_cache_entry_test(Config, unicode).

contended_atom_cache_entry_test(Config, Type) ->
    TestServer = self(),
    ProcessPairs = 10,
    Msgs = 100000,
    {ok, SNode} = start_node(Config),
    {ok, RNode} = start_node(Config),
    Success = make_ref(),
    spawn_link(
      SNode,
      fun () ->
              Master = self(),
              CIX = get_cix(),
              TestAtoms = case Type of
                              latin1 ->
                                  get_conflicting_atoms(CIX,
                                                        ProcessPairs);
                              unicode ->
                                  get_conflicting_unicode_atoms(CIX,
                                                                ProcessPairs)
                          end,
              io:format("Testing with the following atoms all using "
                        "cache index ~p:~n ~w~n",
                        [CIX, TestAtoms]),
              Ps = lists:map(
                     fun (A) ->
                             Ref = make_ref(),
                             R = spawn_link(RNode,
                                   fun () ->
                                           Atom = receive
                                                      {Ref, txt, ATxt} ->
                                                          case Type of
                                                              latin1 ->
                                                                  list_to_atom(ATxt);
                                                              unicode ->
                                                                  string_to_atom(ATxt)
                                                          end
                                                  end,
                                           receive_ref_atom(Ref,
                                                            Atom,
                                                            Msgs),
                                           Master ! {self(), success}
                                   end),
                             S = spawn_link(SNode,
                                   fun () ->
                                           receive go -> ok end,
                                           R ! {Ref,
                                                txt,
                                                atom_to_list(A)},
                                           send_ref_atom(R, Ref, A, Msgs)
                                   end),
                             {S, R}
                     end,
                     TestAtoms),
              lists:foreach(fun ({S, _}) ->
                                    S ! go
                            end,
                            Ps),
              lists:foreach(fun ({_, R}) ->
                                    receive {R, success} -> ok end
                            end,
                            Ps),
              TestServer ! Success
      end),
    receive
        Success ->
            ok
    end,
    stop_node(SNode),
    stop_node(RNode),
    ok.

send_ref_atom(_To, _Ref, _Atom, 0) ->
    ok;
send_ref_atom(To, Ref, Atom, N) ->
    To ! {Ref, Atom},
    send_ref_atom(To, Ref, Atom, N-1).

receive_ref_atom(_Ref, _Atom, 0) ->
    ok;
receive_ref_atom(Ref, Atom, N) ->
    receive
        {Ref, Value} ->
            Atom = Value
    end,
    receive_ref_atom(Ref, Atom, N-1).

get_cix() ->
    get_cix(1000).

get_cix(CIX) when is_integer(CIX), CIX < 0 ->
    get_cix(0);
get_cix(CIX) when is_integer(CIX) ->
    get_cix(CIX,
            unwanted_cixs(),
            get_internal_state(max_atom_out_cache_index)).

get_cix(CIX, Unwanted, MaxCIX) when CIX > MaxCIX ->
    get_cix(0, Unwanted, MaxCIX);
get_cix(CIX, Unwanted, MaxCIX) ->
    case lists:member(CIX, Unwanted) of
        true -> get_cix(CIX+1, Unwanted, MaxCIX);
        false -> CIX
    end.

unwanted_cixs() ->
    lists:map(fun (Node) ->
                      get_internal_state({atom_out_cache_index,
                                          Node})
              end,
              nodes()).


get_conflicting_atoms(_CIX, 0) ->
    [];
get_conflicting_atoms(CIX, N) ->
    Atom = list_to_atom("atom" ++ integer_to_list(erlang:unique_integer([positive]))),
    case get_internal_state({atom_out_cache_index, Atom}) of
        CIX ->
            [Atom|get_conflicting_atoms(CIX, N-1)];
        _ ->
            get_conflicting_atoms(CIX, N)
    end.

get_conflicting_unicode_atoms(_CIX, 0) ->
    [];
get_conflicting_unicode_atoms(CIX, N) ->
    Atom = string_to_atom([16#1f608] ++ "atom" ++ integer_to_list(erlang:unique_integer([positive]))),
    case get_internal_state({atom_out_cache_index, Atom}) of
        CIX ->
            [Atom|get_conflicting_unicode_atoms(CIX, N-1)];
        _ ->
            get_conflicting_unicode_atoms(CIX, N)
    end.


%% The message_latency_large tests that small distribution messages are
%% not blocked by other large distribution messages. Basically it tests
%% that fragmentation of distribution messages works.
%%
%% Because of large problems to get reliable values from these testcases
%% they no longer fail when the latency is incorrect. However, they are
%% kept as they continue to find bugs in the distribution implementation.
message_latency_large_message(Config) when is_list(Config) ->
    measure_latency_large_message(?FUNCTION_NAME, fun(Dropper, Payload) -> Dropper ! Payload end).
message_latency_large_exit2(Config) when is_list(Config) ->
    measure_latency_large_message(?FUNCTION_NAME, fun erlang:exit/2).

message_latency_large_link_exit(Config) when is_list(Config) ->
    message_latency_large_exit(?FUNCTION_NAME, fun erlang:link/1).

message_latency_large_monitor_exit(Config) when is_list(Config) ->
    message_latency_large_exit(?FUNCTION_NAME,
                               fun(Dropper) ->
                                       Dropper ! {monitor, self()},
                                       receive ok -> ok end
                               end).

message_latency_large_message() ->
    [{timetrap, {minutes, 6}}].
message_latency_large_exit2() ->
    message_latency_large_message().
message_latency_large_link_exit() ->
    message_latency_large_message().
message_latency_large_monitor_exit() ->
    message_latency_large_message().

message_latency_large_exit(Nodename, ReasonFun) ->
    measure_latency_large_message(
      Nodename,
      fun(Dropper, Payload) ->
              Pid  = spawn(fun() ->
                                   receive go -> ok end,
                                   ReasonFun(Dropper),
                                   exit(Payload)
                           end),

              FlushTrace = fun F() ->
                                   receive
                                       {trace, Pid, _, _} ->
                                           F()
                                   after 0 ->
                                           ok
                                   end
                           end,

              erlang:trace(Pid, true, [exiting]),
              Pid ! go,
              receive
                  {trace, Pid, out_exited, 0} ->
                      FlushTrace()
              end
      end).

measure_latency_large_message(Nodename, DataFun) ->

    erlang:system_monitor(self(), [busy_dist_port]),

    {ok, N} = start_node(Nodename),

    Dropper = spawn(N, fun F() ->
                               process_flag(trap_exit, true),
                               receive
                                   {monitor,Pid} ->
                                       erlang:monitor(process, Pid),
                                       Pid ! ok;
                                   _ -> ok
                               end,
                               F()
                       end),

    Echo = spawn(N, fun F() -> receive {From, Msg} -> From ! Msg, F() end end),

    BuildType = erlang:system_info(build_type),
    WordSize = erlang:system_info(wordsize),

    if
        BuildType =/= opt; WordSize =:= 4 ->
            %% Test 3.2 MB and 32 MB and test the latency difference of sent messages
            Payloads = [{I, <<0:(I * 32 * 1024 * 8)>>} || I <- [1,10]];
        true ->
            %% Test 32 MB and 320 MB and test the latency difference of sent messages
            Payloads = [{I, <<0:(I * 32 * 1024 * 1024 * 8)>>} || I <- [1,10]]
    end,

    IndexTimes = [{I, measure_latency(DataFun, Dropper, Echo, P)}
                  || {I, P} <- Payloads],

    Times = [ Time || {_I, Time} <- IndexTimes],

    ct:pal("~p",[IndexTimes]),

    stop_node(N),

    case {lists:max(Times), lists:min(Times)} of
        {Max, Min} when Max * 0.25 > Min, BuildType =:= opt ->
            %% We only issue a comment for this failure as the
            %% testcases proved very difficult to run successfully
            %% on many platforms.
            ct:comment({incorrect_latency, IndexTimes}),
            ok;
        _ ->
            ok
    end.

measure_latency(DataFun, Dropper, Echo, Payload) ->

    TCProc = self(),

    flush(),

    Senders = [spawn_monitor(
                 fun F() ->
                         DataFun(Dropper, Payload),
                         F()
                 end) || _ <- lists:seq(1,2)],

    %% Link in order to cleanup properly if TC crashes
    [link(Sender) || {Sender,_} <- Senders],

    wait_for_busy_dist(2 * 60 * 1000, 10),

    {TS, Times} =
        timer:tc(fun() ->
                         [begin
                              T0 = erlang:monotonic_time(),
                              Echo ! {self(), hello},
                              receive hello -> ok end,
                              (erlang:monotonic_time() - T0) / 1000000
                          end || _ <- lists:seq(1,100)]
                 end),
    Avg = lists:sum(Times) / length(Times),
    StdDev = math:sqrt(lists:sum([math:pow(V - Avg,2) || V <- Times]) / length(Times)),
    ct:pal("Times: Avg: ~p Max: ~p Min: ~p Var: ~p",
           [Avg, lists:max(Times), lists:min(Times), StdDev]),
    [begin
         unlink(Sender),
         exit(Sender,die),
         receive
             {'DOWN', Ref, process, _, _} ->
                 ok
         end
     end || {Sender, Ref} <- Senders],
    TS.

wait_for_busy_dist(_Tmo, 0) ->
    ok;
wait_for_busy_dist(Tmo, N) ->
    T0 = erlang:monotonic_time(millisecond),
    receive
         {monitor, _Sender, busy_dist_port, _Info} ->
             wait_for_busy_dist(Tmo - (erlang:monotonic_time(millisecond) - T0), N - 1)
    after Tmo ->
            ct:log("Timed out waiting for busy_dist, ~p left",[N]),
            timeout
    end.

flush() ->
    receive
        _ ->
            flush()
    after 0 ->
            ok
    end.

system_limit(Config) when is_list(Config) ->
    case erlang:system_info(wordsize) of
        8 ->
            case proplists:get_value(system_total_memory,
                                     memsup:get_system_memory_data()) of
                Memory when is_integer(Memory),
                            Memory > 6*1024*1024*1024 ->
                    test_system_limit(Config),
                    garbage_collect(),
                    ok;
                _ ->
                    {skipped, "Not enough memory on this machine"}
            end;
        4 ->
            {skipped, "Only interesting on 64-bit builds"}
    end.

test_system_limit(Config) when is_list(Config) ->
    Bits = ((1 bsl 32)+1)*8,
    HugeBin = <<0:Bits>>,
    HugeListBin = [lists:duplicate(2000000,2000000), HugeBin],
    {ok, N1} = start_node(Config),
    monitor_node(N1, true),
    receive
        {nodedown, N1} ->
            ct:fail({unexpected_nodedown, N1})
    after 0 ->
            ok
    end,
    P1 = spawn(N1,
               fun () ->
                       receive after infinity -> ok end
               end),

    io:format("~n** distributed send **~n~n", []),
    try
        P1 ! HugeBin,
        exit(oops1)
    catch
        error:system_limit -> ok
    end,
    try
        P1 ! HugeListBin,
        exit(oops2)
    catch
        error:system_limit -> ok
    end,

    io:format("~n** distributed exit **~n~n", []),
    try
        exit(P1, HugeBin),
        exit(oops3)
    catch
        error:system_limit -> ok
    end,
    try
        exit(P1, HugeListBin),
        exit(oops4)
    catch
        error:system_limit -> ok
    end,

    io:format("~n** distributed registered send **~n~n", []),
    try
        {missing_proc, N1} ! HugeBin,
        exit(oops5)
    catch
        error:system_limit -> ok
    end,
    try
        {missing_proc, N1} ! HugeListBin,
        exit(oops6)
    catch
        error:system_limit -> ok
    end,
    receive
        {nodedown, N1} ->
            ct:fail({unexpected_nodedown, N1})
    after 0 ->
            ok
    end,

    %%
    %% system_limit in exit reasons brings the
    %% connection down...
    %%

    io:format("~n** distributed link exit **~n~n", []),
    spawn(fun () ->
                  link(P1),
                  exit(HugeBin)
          end),
    receive {nodedown, N1} -> ok end,

    {ok, N2} = start_node(Config),
    monitor_node(N2, true),
    P2 = spawn(N2,
               fun () ->
                       receive after infinity -> ok end
               end),
    spawn(fun () ->
                  link(P2),
                  exit(HugeListBin)
          end),
    receive {nodedown, N2} -> ok end,

    io:format("~n** distributed monitor down **~n~n", []),
    {ok, N3} = start_node(Config),
    monitor_node(N3, true),
    Go1 = make_ref(),
    LP1 = spawn(fun () ->
                        receive Go1 -> ok end,
                        exit(HugeBin)
                end),
    _ = spawn(N3,
               fun () ->
                       _ = erlang:monitor(process, LP1),
                       LP1 ! Go1,
                       receive after infinity -> ok end
               end),
    receive {nodedown, N3} -> ok end,

    {ok, N4} = start_node(Config),
    monitor_node(N4, true),
    Go2 = make_ref(),
    LP2 = spawn(fun () ->
                        receive Go2 -> ok end,
                        exit(HugeListBin)
                end),
    _ = spawn(N4,
              fun () ->
                      _ = erlang:monitor(process, LP2),
                      LP2 ! Go2,
                      receive after infinity -> ok end
              end),
    receive {nodedown, N4} -> ok end,
    ok.

-define(COOKIE, '').
-define(DOP_LINK,		1).
-define(DOP_SEND,		2).
-define(DOP_EXIT,		3).
-define(DOP_UNLINK,		4).
-define(DOP_REG_SEND,		6).
-define(DOP_GROUP_LEADER,	7).
-define(DOP_EXIT2,		8).

-define(DOP_SEND_TT,		12).
-define(DOP_EXIT_TT,		13).
-define(DOP_REG_SEND_TT,	16).
-define(DOP_EXIT2_TT,		18).

-define(DOP_MONITOR_P,		19).
-define(DOP_DEMONITOR_P,	20).
-define(DOP_MONITOR_P_EXIT,	21).

-define(DOP_SEND_SENDER, 22).
-define(DOP_SEND_SENDER_TT, 23).

-define(DOP_PAYLOAD_EXIT, 24).
-define(DOP_PAYLOAD_EXIT_TT, 25).
-define(DOP_PAYLOAD_EXIT2, 26).
-define(DOP_PAYLOAD_EXIT2_TT, 27).
-define(DOP_PAYLOAD_MONITOR_P_EXIT, 28).

start_monitor(Offender,P) ->
    Parent = self(),
    Q = spawn(Offender,
              fun () ->
                      Ref = erlang:monitor(process,P),
                      Parent ! {self(),ref,Ref},
                      receive
                          just_stay_alive -> ok
                      end
              end),
    Res = receive
              {Q,ref,R} ->
                  {Q, R}
          after  5000 ->
                     error
          end,
    io:format("Res is ~p~n",[Res]),
    Res.
start_link(Offender,P) ->
    Parent = self(),
    Q = spawn(Offender,
              fun () ->
                      process_flag(trap_exit,true),
                      link(P),
                      Parent ! {self(),ref,P},
                      receive
                          just_stay_alive -> ok
                      end
              end),
    Res = receive
              {Q,ref,R} ->
                  R
          after  5000 ->
                     error
          end,
    io:format("Res is ~p~n",[Res]),
    Res.

%% Test dist messages with valid structure (binary to term ok) but malformed control content
bad_dist_structure(Config) when is_list(Config) ->
    ct:timetrap({seconds, 15}),

    %% Disabled "connect all" so global wont interfere...
    {ok, Offender} = start_node(bad_dist_structure_offender, "-connect_all false"),
    {ok, Victim} = start_node(bad_dist_structure_victim, "-connect_all false"),
    start_node_monitors([Offender,Victim]),
    Parent = self(),
    P = spawn(Victim,
              fun () ->
                      process_flag(trap_exit,true),
                      Parent ! {self(), started},
                      receive check_msgs -> ok end,
                      bad_dist_struct_check_msgs([one,
                                                  two]),
                      Parent ! {self(), messages_checked},
                      receive done -> ok end
              end),
    receive {P, started} -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    start_monitor(Offender,P),
    P ! one,
    send_bad_structure(Offender, P,{?DOP_MONITOR_P_EXIT,'replace',P,normal},2),

    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_MONITOR_P_EXIT,'replace',P,normal,normal},2),

    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_LINK},0),

    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_UNLINK,'replace'},2),

    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_UNLINK,'replace',make_ref()},2),

    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_UNLINK,make_ref(),P},0),

    start_link(Offender,P),
    send_bad_structure(Offender, P,{?DOP_UNLINK,normal,normal},0),

    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_MONITOR_P,'replace',P},2),

    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_MONITOR_P,'replace',P,normal},2),

    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_DEMONITOR_P,'replace',P},2),

    start_monitor(Offender,P),
    send_bad_structure(Offender, P,{?DOP_DEMONITOR_P,'replace',P,normal},2),

    send_bad_structure(Offender, P,{?DOP_EXIT,'replace',P},2),
    send_bad_structure(Offender, P,{?DOP_EXIT,make_ref(),normal,normal},0),
    send_bad_structure(Offender, P,{?DOP_EXIT_TT,'replace',token,P},2),
    send_bad_structure(Offender, P,{?DOP_EXIT_TT,make_ref(),token,normal,normal},0),
    send_bad_structure(Offender, P,{?DOP_EXIT2,'replace',P},2),
    send_bad_structure(Offender, P,{?DOP_EXIT2,make_ref(),normal,normal},0),
    send_bad_structure(Offender, P,{?DOP_EXIT2_TT,'replace',token,P},2),
    send_bad_structure(Offender, P,{?DOP_EXIT2_TT,make_ref(),token,normal,normal},0),
    send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace'},2),
    send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace','atomic'},2),
    send_bad_structure(Offender, P,{?DOP_GROUP_LEADER,'replace',P},0),
    send_bad_structure(Offender, P,{?DOP_REG_SEND_TT,'replace','',name},2,{message}),
    send_bad_structure(Offender, P,{?DOP_REG_SEND_TT,'replace','',name,token},0,{message}),
    send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace',''},2,{message}),
    send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',P},0,{message}),
    send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',name},0,{message}),
    send_bad_structure(Offender, P,{?DOP_REG_SEND,'replace','',name,{token}},2,{message}),
    send_bad_structure(Offender, P,{?DOP_SEND_TT,'',P},0,{message}),
    send_bad_structure(Offender, P,{?DOP_SEND_TT,'',name,token},0,{message}),
    send_bad_structure(Offender, P,{?DOP_SEND,''},0,{message}),
    send_bad_structure(Offender, P,{?DOP_SEND,'',name},0,{message}),
    send_bad_structure(Offender, P,{?DOP_SEND,'',P,{token}},0,{message}),
    P ! two,
    P ! check_msgs,
    receive
        {P, messages_checked} -> ok
    after 5000 ->
              exit(victim_is_dead)
    end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    unlink(P),
    P ! done,
    stop_node(Offender),
    stop_node(Victim),
    ok.

%% Test various dist fragmentation errors
bad_dist_fragments(Config) when is_list(Config) ->
    ct:timetrap({seconds, 15}),

    %% Disabled "connect all" so global wont interfere...
    {ok, Offender} = start_node(bad_dist_fragment_offender, "-connect_all false"),
    {ok, Victim} = start_node(bad_dist_fragment_victim, "-connect_all false"),

    Msg = iolist_to_binary(dmsg_ext(lists:duplicate(255,255))),

    start_node_monitors([Offender,Victim]),
    Parent = self(),
    P = spawn(Victim,
              fun () ->
                      process_flag(trap_exit,true),
                      Parent ! {self(), started},
                      receive check_msgs -> ok end,
                      bad_dist_struct_check_msgs([one,
                                                  two]),
                      Parent ! {self(), messages_checked},
                      receive done -> ok end
              end),
    receive {P, started} -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    start_monitor(Offender,P),
    P ! one,

    start_monitor(Offender,P),
    send_bad_fragments(Offender, Victim, P,{?DOP_SEND,?COOKIE,P},3,
                      [{frg, 1, binary:part(Msg, 10,byte_size(Msg)-10)}]),

    start_monitor(Offender,P),
    send_bad_fragments(Offender, Victim, P,{?DOP_SEND,?COOKIE,P},3,
                      [{hdr, 3, binary:part(Msg, 0,10)},
                       {frg, 1, binary:part(Msg, 10,byte_size(Msg)-10)}]),

    start_monitor(Offender,P),
    send_bad_fragments(Offender, Victim, P,{?DOP_SEND,?COOKIE,P},3,
                      [{hdr, 3, binary:part(Msg, 0,10)},
                       {hdr, 3, binary:part(Msg, 0,10)}]),

    start_monitor(Offender,P),
    send_bad_fragments(Offender, Victim, P,{?DOP_SEND,?COOKIE,P,broken},3,
                      [{hdr, 1, binary:part(Msg, 10,byte_size(Msg)-10)}]),

    start_monitor(Offender,P),
    send_bad_fragments(Offender, Victim, P,{?DOP_SEND,?COOKIE,P},3,
                      [{hdr, 3, binary:part(Msg, 10,byte_size(Msg)-10)},
                       close]),

    ExitVictim = spawn(Victim, fun() ->
                                       receive
                                           {link, Proc} ->
                                               link(Proc),
                                               Parent ! {self(), linked}
                                       end,
                                       receive ok -> ok end
                               end),
    OP1 = start_link(Offender,ExitVictim),
    ExitVictim ! {link, OP1},
    receive {ExitVictim, linked} -> ok end,
    send_bad_fragments(Offender, Victim, ExitVictim,{?DOP_PAYLOAD_EXIT,OP1,ExitVictim},0,
                      [{hdr, 1, [131]}]),

    Exit2Victim = spawn(Victim, fun() -> receive ok -> ok end end),
    {OP2, _} = start_monitor(Offender,Exit2Victim),
    send_bad_fragments(Offender, Victim, Exit2Victim,{?DOP_PAYLOAD_EXIT2,OP2,Exit2Victim},0,
                      [{hdr, 1, [132]}]),

    DownVictim = spawn(Victim, fun() ->
                                       receive
                                           {monitor, Proc} ->
                                               DR = erlang:monitor(process, Proc),
                                               Parent ! {self(), DR}
                                       end,
                                       Parent ! {self, DR},
                                       receive ok -> ok end
                               end),
    {OP3, _} = start_monitor(Offender,DownVictim),
    DownVictim ! {monitor, OP3},
    DownRef = receive {DownVictim, DR} -> DR end,
    send_bad_fragments(Offender, Victim, DownVictim,{?DOP_PAYLOAD_MONITOR_P_EXIT,OP3,DownVictim,DownRef},0,
                      [{hdr, 1, [133]}]),

    P ! two,
    P ! check_msgs,
    receive
        {P, messages_checked} -> ok
    after 5000 ->
              exit(victim_is_dead)
    end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    unlink(P),
    P ! done,
    stop_node(Offender),
    stop_node(Victim),
    ok.

dmsg_frag_hdr(Frag) ->
    dmsg_frag_hdr(erlang:phash2(self()), Frag).
dmsg_frag_hdr(Seq, Frag) ->
    [131, $E, uint64_be(Seq), uint64_be(Frag), 0].

dmsg_frag(Frag) ->
    dmsg_frag(erlang:phash2(self()), Frag).
dmsg_frag(Seq, Frag) ->
    [131, $F, uint64_be(Seq), uint64_be(Frag)].

send_bad_fragments(Offender,VictimNode,Victim,Ctrl,WhereToPutSelf,Fragments) ->
    Parent = self(),
    Done = make_ref(),
    ct:pal("Send: ~p",[Fragments]),
    spawn_link(Offender,
          fun () ->
                  Node = node(Victim),
                  pong = net_adm:ping(Node),
                  erlang:monitor_node(Node, true),
                  DCtrl = dctrl(Node),
                  Ctrl1 = case WhereToPutSelf of
                             0 ->
                                 Ctrl;
                             N when N > 0 ->
                                 setelement(N,Ctrl,self())
                         end,

                  FragData = [case Type of
                                  hdr ->
                                      [dmsg_frag_hdr(FragId),
                                       dmsg_ext(Ctrl1), FragPayload];
                                  frg ->
                                      [dmsg_frag(FragId), FragPayload]
                              end || {Type, FragId, FragPayload} <- Fragments],

                  receive {nodedown, Node} -> exit("premature nodedown")
                  after 10 -> ok
                  end,

                  [ dctrl_send(DCtrl, D) || D <- FragData ],
                  [ erlang:port_close(DCtrl) || close <- Fragments],

                  receive {nodedown, Node} -> ok
                  after 5000 -> exit("missing nodedown")
                  end,
                  Parent ! {FragData,Done}
          end),
    receive
        {WhatSent,Done} ->
            io:format("Offender sent ~p~n",[WhatSent]),
            verify_nc(VictimNode),
            ok
    after 7000 ->
              exit(unable_to_send)
    end.

bad_dist_ext_receive(Config) when is_list(Config) ->
    %% Disabled "connect all" so global wont interfere...
    {ok, Offender} = start_node(bad_dist_ext_receive_offender, "-connect_all false"),
    {ok, Victim} = start_node(bad_dist_ext_receive_victim, "-connect_all false"),
    start_node_monitors([Offender,Victim]),

    Parent = self(),

    P = spawn_link(Victim,
                   fun () ->
                           Parent ! {self(), started},
                           receive check_msgs -> ok end,
                           bad_dist_ext_check_msgs([one,
                                                    two,
                                                    three]),
                           Parent ! {self(), messages_checked},
                           receive done -> ok end
                   end),

    receive {P, started} -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    P ! one,
    send_bad_msg(Offender, P),
    P ! two,
    verify_down(Offender, connection_closed, Victim, killed),
    {message_queue_len, 2}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    Suspended = make_ref(),
    S = spawn(Victim,
              fun () ->
                      erlang:suspend_process(P),
                      Parent ! Suspended,
                      receive after infinity -> ok end
              end),
    MS = erlang:monitor(process, S),
    receive Suspended -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    send_bad_msgs(Offender, P, 5),
    true = lists:member(Offender, rpc:call(Victim, erlang, nodes, [])),
    P ! three,
    send_bad_msgs(Offender, P, 5),

    %% Make sure bad msgs has reached Victim
    rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    verify_still_up(Offender, Victim),
    {message_queue_len, 13}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    exit(S, bang),
    receive {'DOWN', MS, process, S, bang} -> ok end,
    verify_down(Offender, connection_closed, Victim, killed),
    {message_queue_len, 3}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    P ! check_msgs,
    receive {P, messages_checked} -> ok end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    P ! done,
    unlink(P),
    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).


bad_dist_ext_process_info(Config) when is_list(Config) ->
    %% Disabled "connect all" so global wont interfere...
    {ok, Offender} = start_node(bad_dist_ext_process_info_offender, "-connect_all false"),
    {ok, Victim} = start_node(bad_dist_ext_process_info_victim, "-connect_all false"),
    start_node_monitors([Offender,Victim]),

    Parent = self(),
    P = spawn_link(Victim,
                   fun () ->
                           Parent ! {self(), started},
                           receive check_msgs -> ok end,
                           bad_dist_ext_check_msgs([one, two]),
                           Parent ! {self(), messages_checked},
                           receive done -> ok end
                   end),

    receive {P, started} -> ok end,
    P ! one,

    Suspended = make_ref(),
    S = spawn(Victim,
              fun () ->
                      erlang:suspend_process(P),
                      Parent ! Suspended,
                      receive after infinity -> ok end
              end),

    receive Suspended -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_msgs(Offender, P, 5),

    P ! two,
    send_bad_msgs(Offender, P, 5),

    %% Make sure bad msgs has reached Victim
    rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    verify_still_up(Offender, Victim),
    {message_queue_len, 12}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),
    verify_still_up(Offender, Victim),
    [{message_queue_len, 2},
     {messages, [one, two]}]
    = rpc:call(Victim, erlang, process_info, [P, [message_queue_len,
                                                  messages]]),
    verify_down(Offender, connection_closed, Victim, killed),

    P ! check_msgs,
    exit(S, bang),
    receive {P, messages_checked} -> ok end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    P ! done,
    unlink(P),
    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).

bad_dist_ext_control(Config) when is_list(Config) ->
    %% Disabled "connect all" so global wont interfere...
    {ok, Offender} = start_node(bad_dist_ext_control_offender, "-connect_all false"),
    {ok, Victim} = start_node(bad_dist_ext_control_victim, "-connect_all false"),
    start_node_monitors([Offender,Victim]),

    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_dhdr(Offender, Victim),
    verify_down(Offender, connection_closed, Victim, killed),

    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_ctl(Offender, Victim),
    verify_down(Offender, connection_closed, Victim, killed),

    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).

bad_dist_ext_connection_id(Config) when is_list(Config) ->
    %% Disabled "connect all" so global wont interfere...
    {ok, Offender} = start_node(bad_dist_ext_connection_id_offender, "-connect_all false"),
    {ok, Victim} = start_node(bad_dist_ext_connection_id_victim, "-connect_all false"),
    start_node_monitors([Offender,Victim]),

    Parent = self(),
    P = spawn_link(Victim,
                   fun () ->
                           Parent ! {self(), started},
                           receive check_msgs -> ok end,
                           bad_dist_ext_check_msgs([]),
                           Parent ! {self(), messages_checked},
                           receive done -> ok end
                   end),

    receive {P, started} -> ok end,
    Suspended = make_ref(),
    S = spawn(Victim,
              fun () ->
                      erlang:suspend_process(P),
                      Parent ! Suspended,
                      receive after infinity -> ok end
              end),
    MS = erlang:monitor(process, S),
    receive Suspended -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_msg(Offender, P),

    %% Make sure bad msg has reached Victim
    rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    {message_queue_len, 1}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    true = rpc:call(Offender, net_kernel, disconnect, [Victim]),
    verify_down(Offender, disconnect, Victim, connection_closed),
    pong = rpc:call(Offender, net_adm, ping, [Victim]),

    verify_up(Offender, Victim),
    %% We have a new connection between Offender and Victim, bad message
    %% should not bring it down.

    {message_queue_len, 1}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    exit(S, bang),
    receive {'DOWN', MS, process, S, bang} -> ok end,
    %% Wait for a while (if the connection is taken down it might take a
    %% while).
    receive after 2000 -> ok end,
    verify_still_up(Offender, Victim),

    P ! check_msgs,
    receive {P, messages_checked} -> ok end,

    {message_queue_len, 0}
    = rpc:call(Victim, erlang, process_info, [P, message_queue_len]),

    verify_still_up(Offender, Victim),
    P ! done,
    unlink(P),
    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).

%% OTP-14661: Bad message is discovered by erts_msg_attached_data_size
bad_dist_ext_size(Config) when is_list(Config) ->
    %% Disabled "connect all" so global wont interfere...
    {ok, Offender} = start_node(bad_dist_ext_process_info_offender, "-connect_all false"),
    %%Prog = "Prog=/home/uabseri/src/otp_new3/bin/cerl -rr -debug",
    Prog = [],
    {ok, Victim} = start_node(bad_dist_ext_process_info_victim, "-connect_all false", Prog),
    start_node_monitors([Offender,Victim]),

    Parent = self(),
    P = spawn_opt(Victim,
                   fun () ->
                           Parent ! {self(), started},
                           receive check_msgs -> ok end,  %% DID CRASH HERE
                           bad_dist_ext_check_msgs([one]),
                           Parent ! {self(), messages_checked}
                   end,
                 [link,
                  %% on_heap to force total_heap_size to inspect msg queue
                  {message_queue_data, on_heap}]),

    receive {P, started} -> ok end,
    P ! one,

    Suspended = make_ref(),
    S = spawn(Victim,
              fun () ->
                      erlang:suspend_process(P),
                      Parent ! Suspended,
                      receive after infinity -> ok end
              end),

    receive Suspended -> ok end,
    pong = rpc:call(Victim, net_adm, ping, [Offender]),
    verify_up(Offender, Victim),
    send_bad_msgs(Offender, P, 1, dmsg_bad_tag()),

    %% Make sure bad msgs has reached Victim
    rpc:call(Offender, rpc, call, [Victim, erlang, node, []]),

    verify_still_up(Offender, Victim),

    %% Let process_info(P, total_heap_size) find bad msg and disconnect
    rpc:call(Victim, erlang, process_info, [P, total_heap_size]),

    verify_down(Offender, connection_closed, Victim, killed),

    P ! check_msgs,
    exit(S, bang),  % resume Victim
    receive {P, messages_checked} -> ok end,

    unlink(P),
    verify_no_down(Offender, Victim),
    stop_node(Offender),
    stop_node(Victim).


bad_dist_struct_check_msgs([]) ->
    receive
        Msg ->
            exit({unexpected_message, Msg})
    after 0 ->
              ok
    end;
bad_dist_struct_check_msgs([M|Ms]) ->
    receive
        {'EXIT',_,_} = EM ->
            io:format("Ignoring exit message: ~p~n",[EM]),
            bad_dist_struct_check_msgs([M|Ms]);
        Msg ->
            M = Msg,
            bad_dist_struct_check_msgs(Ms)
    end.
bad_dist_ext_check_msgs([]) ->
    receive
        Msg ->
            exit({unexpected_message, Msg})
    after 0 ->
              ok
    end;
bad_dist_ext_check_msgs([M|Ms]) ->
    receive
        Msg ->
            M = Msg,
            bad_dist_ext_check_msgs(Ms)
    end.

ensure_dctrl(Node) ->
    case dctrl(Node) of
        undefined ->
            pong = net_adm:ping(Node),
            dctrl(Node);
        DCtrl ->
            DCtrl
    end.

dctrl_send(DPrt, Data) when is_port(DPrt) ->
    port_command(DPrt, Data);
dctrl_send(DPid, Data) when is_pid(DPid) ->
    Ref = make_ref(),
    DPid ! {send, self(), Ref, Data},
    receive {Ref, Res} -> Res end.

dctrl_dop_reg_send(Node, Name, Msg) ->
    dctrl_send(ensure_dctrl(Node),
               [dmsg_hdr(),
                dmsg_ext({?DOP_REG_SEND,
                          self(),
                          ?COOKIE,
                          Name}),
                dmsg_ext(Msg)]).

dctrl_dop_send(To, Msg) ->
    Node = node(To),
    dctrl_send(ensure_dctrl(Node),
               [dmsg_hdr(),
                dmsg_ext({?DOP_SEND, ?COOKIE, To}),
                dmsg_ext(Msg)]).

send_bad_structure(Offender,Victim,Bad,WhereToPutSelf) ->
    send_bad_structure(Offender,Victim,Bad,WhereToPutSelf,[]).
send_bad_structure(Offender,Victim,Bad,WhereToPutSelf,PayLoad) ->
    Parent = self(),
    Done = make_ref(),
    spawn_link(Offender,
          fun () ->
                  Node = node(Victim),
                  pong = net_adm:ping(Node),
                  erlang:monitor_node(Node, true),
                  DCtrl = dctrl(Node),
                  Bad1 = case WhereToPutSelf of
                             0 ->
                                 Bad;
                             N when N > 0 ->
                                 setelement(N,Bad,self())
                         end,
                  DData = [dmsg_hdr(),
                           dmsg_ext(Bad1)] ++
                  case PayLoad of
                      [] -> [];
                      _Other -> [dmsg_ext(PayLoad)]
                  end,

                  receive {nodedown, Node} -> exit("premature nodedown")
                  after 10 -> ok
                  end,

                  dctrl_send(DCtrl, DData),

                  receive {nodedown, Node} -> ok
                  after 5000 -> exit("missing nodedown")
                  end,
                  Parent ! {DData,Done}
          end),
    receive
        {WhatSent,Done} ->
            io:format("Offender sent ~p~n",[WhatSent]),
            ok
    after 5000 ->
              exit(unable_to_send)
    end.


%% send_bad_msgs():
%% Send a valid distribution header and control message
%% but an invalid message. This invalid message will be
%% enqueued in the receivers message queue.
send_bad_msg(BadNode, To) ->
    send_bad_msgs(BadNode, To, 1).

send_bad_msgs(BadNode, To, Repeat) ->
    send_bad_msgs(BadNode, To, Repeat, dmsg_bad_atom_cache_ref()).

send_bad_msgs(BadNode, To, Repeat, BadTerm) when is_atom(BadNode),
                                                 is_pid(To),
                                                 is_integer(Repeat) ->
    Parent = self(),
    Done = make_ref(),
    spawn_link(BadNode,
               fun () ->
                       Node = node(To),
                       pong = net_adm:ping(Node),
                       DCtrl = dctrl(Node),
                       DData = [dmsg_hdr(),
                                dmsg_ext({?DOP_SEND, ?COOKIE, To}),
                                BadTerm],
		       repeat(fun () -> dctrl_send(DCtrl, DData) end, Repeat),
                       Parent ! Done
               end),
    receive Done -> ok end.

%% send_bad_ctl():
%% Send a valid distribution header but an invalid control message.
send_bad_ctl(BadNode, ToNode) when is_atom(BadNode), is_atom(ToNode) ->
    Parent = self(),
    Done = make_ref(),
    spawn_link(BadNode,
               fun () ->
                       pong = net_adm:ping(ToNode),
                       %% We creat a valid ctl msg and replace an
                       %% atom with an invalid atom cache reference
                       <<131,Replace/binary>> = term_to_binary(replace),
                       Ctl = dmsg_ext({?DOP_REG_SEND,
                                       self(),
                                       ?COOKIE,
                                       replace}),
                       CtlBeginSize = size(Ctl) - size(Replace),
                       <<CtlBegin:CtlBeginSize/binary, Replace/binary>> = Ctl,
                       DCtrl = dctrl(ToNode),
                       Data = [dmsg_fake_hdr2(),
                               CtlBegin,
                               dmsg_bad_atom_cache_ref(),
                               dmsg_ext({a, message})],
                       dctrl_send(DCtrl, Data),
                       Parent ! Done
               end),
    receive Done -> ok end.

%% send_bad_dhr():
%% Send an invalid distribution header
send_bad_dhdr(BadNode, ToNode) when is_atom(BadNode), is_atom(ToNode) ->
    Parent = self(),
    Done = make_ref(),
    spawn_link(BadNode,
               fun () ->
                       pong = net_adm:ping(ToNode),
                       dctrl_send(dctrl(ToNode), dmsg_bad_hdr()),
                       Parent ! Done
               end),
    receive Done -> ok end.

dctrl(Node) when is_atom(Node) ->
    get_internal_state({dist_ctrl, Node}).

get_internal_state(Op) ->
    try erts_debug:get_internal_state(Op) of
        R -> R
    catch
        error:undef ->
            erts_debug:set_internal_state(available_internal_state, true),
            erts_debug:get_internal_state(Op)
    end.

set_internal_state(Op, Val) ->
    try erts_debug:set_internal_state(Op, Val) of
        R -> R
    catch
        error:undef ->
            erts_debug:set_internal_state(available_internal_state, true),
            erts_debug:set_internal_state(Op, Val)
    end.


dmsg_hdr() ->
    [131, % Version Magic
     $D,  % Dist header
     0].  % No atom cache referenses

dmsg_bad_hdr() ->
    [131, % Version Magic
     $D,  % Dist header
     255].  % 255 atom references


%% dmsg_fake_hdr1() ->
%%     A = <<"fake header atom 1">>,
%%     [131, % Version Magic
%%      $D, 1, 16#8, 0, size(A), A]. % Fake header

dmsg_fake_hdr2() ->
    A1 = <<"fake header atom 1">>,
    A2 = <<"atom 2">>,
    A3 = <<"atom 3">>,
    [131, % Version Magic
     $D,
     3,
     16#88, 16#08, % Flags
     0, size(A1), A1,
     1, size(A2), A2,
     2, size(A3), A3].

dmsg_ext(Term) ->
    <<131, Res/binary>> = term_to_binary(Term),
    Res.

dmsg_bad_atom_cache_ref() ->
    [$R, 137].

dmsg_bad_tag() ->  %% Will fail early at heap size calculation
    [$?, 66].

%% Test that processes exiting while sending a fragmented message works
%% as it should. We test that this works while the process doing the send
%% is suspended/resumed in order to trigger bugs in suspend/resume handling
%% while exiting.
%% We also make sure that the binary memory of the receiving node does not grow
%% without shrinking back as there used to be a memory leak on the receiving side.
exit_dist_fragments(_Config) ->
    {ok, Node} = start_node(?FUNCTION_NAME),
    try
        ct:log("Allocations before:~n~p",[erpc:call(Node,instrument,allocations, [])]),
        {BinInfo, BinInfoMref} =
            spawn_monitor(Node,
                          fun() ->
                                  (fun F(Acc) ->
                                           H = try erlang:memory(binary)
                                               catch _:_ -> 0 end,
                                           receive
                                               {get, Pid} ->
                                                   After = try erlang:memory(binary)
                                                           catch _:_ -> 0 end,
                                                   Pid ! lists:reverse([After,H|Acc])
                                           after 100 ->
                                                   F([H|Acc])
                                           end
                                   end)([])
                          end),
        {Tracer, Mref} = spawn_monitor(fun gather_exited/0),
        erlang:trace(self(), true, [{tracer, Tracer}, set_on_spawn, procs, exiting]),
        exit_suspend(Node),
        receive
            {'DOWN',Mref,_,_,_} ->
                BinInfo ! {get, self()},
                receive
                    {'DOWN',BinInfoMref,_,_,Reason} ->
                        ct:fail(Reason);
                    Info ->
                        Before = hd(Info),
                        Max = lists:max(Info),
                        After = lists:last(Info),
                        ct:log("Binary memory before: ~p~n"
                               "Binary memory max: ~p~n"
                               "Binary memory after: ~p",
                               [Before, Max, After]),
                        ct:log("Allocations after:~n~p",
                               [erpc:call(Node,instrument,allocations, [])]),
                        %% We check that the binary data used after is not too large
                        if
                            (After - Before) / (Max - Before) > 0.05 ->
                                ct:log("Memory ratio was ~p",[(After - Before) / (Max - Before)]),
                                ct:fail("Potential binary memory leak!");
                            true -> ok
                        end
                end
        end
    after
        stop_node(Node)
    end.

%% Make sure that each spawned process also has exited
gather_exited() ->
    process_flag(message_queue_data, off_heap),
    gather_exited(#{}).
gather_exited(Pids) ->
    receive
        {trace,Pid,spawned,_,_} ->
            gather_exited(Pids#{ Pid => true });
        {trace,Pid,exited_out,_,_} ->
            {true, NewPids} = maps:take(Pid, Pids),
            gather_exited(NewPids);
        _M ->
            gather_exited(Pids)
    after 1000 ->
            if Pids == #{} -> ok;
               true -> exit(Pids)
            end
    end.

exit_suspend(RemoteNode) ->
    exit_suspend(RemoteNode, 100).
exit_suspend(RemoteNode, N) ->
    Payload = case erlang:system_info(wordsize) of
                  8 ->
                      [<<0:100000/unit:8>> || _ <- lists:seq(1, 10)];
                  4 ->
                      [<<0:100000/unit:8>> || _ <- lists:seq(1, 2)]
              end,
    exit_suspend(RemoteNode, N, Payload).
exit_suspend(RemoteNode, N, Payload) ->
    Echo = fun F() ->
                   receive
                       {From, Msg} ->
                           From ! erlang:iolist_size(Msg),
                           F()
                   end
           end,
    Pinger =
        fun() ->
                false = process_flag(trap_exit, true),
                RemotePid = spawn_link(RemoteNode, Echo),
                Iterations = case erlang:system_info(debug_compiled) of
                                 false ->
                                     100;
                                 _ ->
                                     10
                             end,
                exit_suspend_loop(RemotePid, 2, Payload, Iterations)
        end,
    Pids = [spawn_link(Pinger) || _ <- lists:seq(1, N)],
    MRefs = [monitor(process, Pid) || Pid <- Pids],
    [receive {'DOWN',MRef,_,_,_} -> ok end || MRef <- MRefs],
    Pids.

exit_suspend_loop(RemotePid, _Suspenders, _Payload, 0) ->
    exit(RemotePid, die),
    receive
        {'EXIT', RemotePid, _} ->
            ok
    end;
exit_suspend_loop(RemotePid, Suspenders, Payload, N) ->
    LocalPid = spawn_link(
                 fun() ->
                         Parent = self(),
                         [spawn_link(
                            fun F() ->
                                    try
                                        begin
                                            erlang:suspend_process(Parent),
                                            erlang:yield(),
                                            erlang:suspend_process(Parent),
                                            erlang:yield(),
                                            erlang:resume_process(Parent),
                                            erlang:yield(),
                                            erlang:suspend_process(Parent),
                                            erlang:yield(),
                                            erlang:resume_process(Parent),
                                            erlang:yield(),
                                            erlang:resume_process(Parent),
                                            erlang:yield()
                                        end of
                                        _ ->
                                            F()
                                    catch _:_ ->
                                            ok
                                    end
                            end) || _ <- lists:seq(1, Suspenders)],
                         (fun F() ->
                                  RemotePid ! {self(), Payload},
                                  receive _IOListSize -> ok end,
                                  F()
                          end)()
                 end),
    exit_suspend_loop(LocalPid, RemotePid, Suspenders, Payload, N - 1).
exit_suspend_loop(LocalPid, RemotePid, Suspenders, Payload, N) ->
    receive
        {'EXIT', LocalPid, _} ->
            exit_suspend_loop(RemotePid, Suspenders, Payload, N);
        {'EXIT', _, Reason} ->
            exit(Reason)
    after 100 ->
            exit(LocalPid, die),
            exit_suspend_loop(LocalPid, RemotePid, Suspenders, Payload, N)
    end.

start_epmd_false(Config) when is_list(Config) ->
    %% Start a node with the option -start_epmd false.
    {ok, OtherNode} = start_node(start_epmd_false, "-start_epmd false"),
    %% We should be able to ping it, as epmd was started by us:
    pong = net_adm:ping(OtherNode),
    stop_node(OtherNode),

    ok.

no_epmd(Config) when is_list(Config) ->
    %% Trying to start a node with -no_epmd but without passing the
    %% --proto_dist option should fail.
    {error, timeout} = start_node(no_epmd, "-no_epmd").

epmd_module(Config) when is_list(Config) ->
    %% We need a relay node to test this, since the test node uses the
    %% standard epmd module.
    {N1,_,S1} = Sock1 = start_relay_node(
                          epmd_module_node1,
                          "-epmd_module " ++ ?MODULE_STRING,
                         [{"ERL_AFLAGS","-sname epmd_module_node1@dummy " ++ os:getenv("ERL_AFLAGS","")}]),
    Node1 = inet_rpc_nodename({N1,"dummy",S1}),
    %% Ask what port it's listening on - it won't have registered with
    %% epmd.
    {ok, {ok, Port1}} = do_inet_rpc(Sock1, application, get_env, [kernel, dist_listen_port]),

    %% Start a second node, passing the port number as a secret
    %% argument.
    {N2,_,S2} = Sock2 = start_relay_node(
                          epmd_module_node2,
                          "-epmd_module " ++ ?MODULE_STRING
                          ++ " -other_node_port " ++ integer_to_list(Port1),
                          [{"ERL_AFLAGS","-sname epmd_module_node2@dummy " ++ os:getenv("ERL_AFLAGS","")}]),
    Node2 = inet_rpc_nodename({N2,"dummy",S2}),
    %% Node 1 can't ping node 2
    {ok, pang} = do_inet_rpc(Sock1, net_adm, ping, [Node2]),
    {ok, []} = do_inet_rpc(Sock1, erlang, nodes, []),
    {ok, []} = do_inet_rpc(Sock2, erlang, nodes, []),
    %% But node 2 can ping node 1
    {ok, pong} = do_inet_rpc(Sock2, net_adm, ping, [Node1]),
    {ok, [Node2]} = do_inet_rpc(Sock1, erlang, nodes, []),
    {ok, [Node1]} = do_inet_rpc(Sock2, erlang, nodes, []),

    stop_relay_node(Sock2),
    stop_relay_node(Sock1).

%% epmd_module functions:

start_link() ->
    ignore.

register_node(Name, Port) ->
    register_node(Name, Port, inet_tcp).
register_node(_Name, Port, _Driver) ->
    %% Save the port number we're listening on.
    application:set_env(kernel, dist_listen_port, Port),
    Creation = rand:uniform(3),
    {ok, Creation}.

port_please(_Name, _Ip) ->
    case init:get_argument(other_node_port) of
	error ->
	    %% None specified.  Default to 42.
	    Port = 42,
	    Version = 5,
	    {port, Port, Version};
	{ok, [[PortS]]} ->
	    %% Port number given on command line.
	    Port = list_to_integer(PortS),
	    Version = 5,
	    {port, Port, Version}
    end.

address_please(_Name, "dummy", inet) ->
    %% Use localhost.
    {ok, {127,0,0,1}};
address_please(_Name, "dummy", inet6) ->
    {ok, {0,0,0,0,0,0,0,1}}.

hopefull_data_encoding(Config) when is_list(Config) ->
    test_hopefull_data_encoding(Config, true),
    test_hopefull_data_encoding(Config, false).

test_hopefull_data_encoding(Config, Fallback) when is_list(Config) ->
    {ok, ProxyNode} = start_node(hopefull_data_normal),
    {ok, BouncerNode} = start_node(hopefull_data_bouncer, "-hidden"),
    case Fallback of
        false ->
            ok;
        true ->
            rpc:call(BouncerNode, erts_debug, set_internal_state,
                     [available_internal_state, true]),
            false = rpc:call(BouncerNode, erts_debug, set_internal_state,
                            [remove_hopefull_dflags, true])
    end,
    Tester = self(),
    R1 = make_ref(),
    R2 = make_ref(),
    R3 = make_ref(),
    Bouncer = spawn_link(BouncerNode, fun () -> bounce_loop() end),
    Proxy = spawn_link(ProxyNode,
                       fun () ->
                               register(bouncer, self()),
                               %% We create the data on the proxy node in order
                               %% to create the correct sub binaries
                               HData = mk_hopefull_data(R1, Tester),
                               %% Verify same result between this node and tester
                               Tester ! [R1, HData],
                               %% Test when connection has not been setup yet
                               Bouncer ! {Tester, [R2, HData]},
                               Sync = make_ref(),
                               Bouncer ! {self(), Sync},
                               receive Sync -> ok end,
                               %% Test when connection is already up
                               Bouncer ! {Tester, [R3, HData]},
                               receive after infinity -> ok end
                       end),
    HData =
        receive
            [R1, HData1] ->
                HData1
        end,
    receive
        [R2, HData2] ->
            case Fallback of
                false ->
                    HData = HData2;
                true ->
                    check_hopefull_fallback_data(HData, HData2)
            end
    end,
    receive
        [R3, HData3] ->
            case Fallback of
                false ->
                    HData = HData3;
                true ->
                    check_hopefull_fallback_data(HData, HData3)
            end
    end,
    unlink(Proxy),
    exit(Proxy, bye),
    unlink(Bouncer),
    exit(Bouncer, bye),
    stop_node(ProxyNode),
    stop_node(BouncerNode),
    ok.

bounce_loop() ->
    receive
        {SendTo, Data} ->
            SendTo ! Data
    end,
    bounce_loop().

mk_hopefull_data(RemoteRef, RemotePid) ->
    HugeBs = list_to_bitstring([lists:duplicate(12*1024*1024, 85), <<6:6>>]),
    <<_:1/bitstring,HugeBs2/bitstring>> = HugeBs,
    mk_hopefull_data(list_to_binary(lists:seq(1,255))) ++
        [1234567890, HugeBs, fun gurka:banan/3, fun erlang:node/1,
         RemotePid, self(), fun erlang:self/0] ++
        mk_hopefull_data(list_to_binary(lists:seq(1,32))) ++
        [an_atom,
         fun lists:reverse/1, RemoteRef, make_ref(), HugeBs2,
         fun blipp:blapp/7].

mk_hopefull_data(BS) ->
    BSsz = bit_size(BS),
    lists:concat(
      [lists:map(fun (Offset) ->
                         <<NewBs:Offset/bitstring, _/bitstring>> = BS,
                         NewBs
                 end, lists:seq(1, 16)),
       lists:map(fun (Offset) ->
                         <<_:Offset/bitstring, NewBs/bitstring>> = BS,
                         NewBs
                 end, lists:seq(1, 16)),
       lists:map(fun (Offset) ->
                         <<NewBs:Offset/bitstring, _/bitstring>> = BS,
                         NewBs
                 end, lists:seq(BSsz-16, BSsz-1)),
       lists:map(fun (Offset) ->
                         PreOffset = Offset rem 16,
                         <<_:PreOffset/bitstring, NewBs:Offset/bitstring, _/bitstring>> = BS,
                         NewBs
                 end, lists:seq(BSsz-32, BSsz-17)),
       lists:map(fun (Offset) ->
                         <<NewBs:Offset/bitstring, _/bitstring>> = BS,
                         [NewBs]
                 end, lists:seq(1, 16)),
       lists:map(fun (Offset) ->
                         <<_:Offset/bitstring, NewBs/bitstring>> = BS,
                         [NewBs]
                 end, lists:seq(1, 16)),
       lists:map(fun (Offset) ->
                         <<NewBs:Offset/bitstring, _/bitstring>> = BS,
                         [NewBs]
                 end, lists:seq(BSsz-16, BSsz-1)),
       lists:map(fun (Offset) ->
                         PreOffset = Offset rem 16,
                         <<_:PreOffset/bitstring, NewBs:Offset/bitstring, _/bitstring>> = BS,
                         [NewBs]
                 end, lists:seq(BSsz-32, BSsz-17))]).

check_hopefull_fallback_data([], []) ->
    ok;
check_hopefull_fallback_data([X|Xs],[Y|Ys]) ->
    chk_hopefull_fallback(X, Y),
    check_hopefull_fallback_data(Xs,Ys).

chk_hopefull_fallback(Binary, FallbackBinary) when is_binary(Binary) ->
    Binary = FallbackBinary;
chk_hopefull_fallback([BitStr], [{Bin, BitSize}]) when is_bitstring(BitStr) ->
    chk_hopefull_fallback(BitStr, {Bin, BitSize});
chk_hopefull_fallback(BitStr, {Bin, BitSize}) when is_bitstring(BitStr) ->
    true = is_binary(Bin),
    true = is_integer(BitSize),
    true = BitSize > 0,
    true = BitSize < 8,
    Hsz = size(Bin) - 1,
    <<Head:Hsz/binary, I/integer>> = Bin,
    IBits = I bsr (8 - BitSize),
    FallbackBitStr = list_to_bitstring([Head,<<IBits:BitSize>>]),
    BitStr = FallbackBitStr,
    ok;
chk_hopefull_fallback(Func, {ModName, FuncName}) when is_function(Func) ->
    {M, F, _} = erlang:fun_info_mfa(Func),
    M = ModName,
    F = FuncName,
    ok;
chk_hopefull_fallback(Other, SameOther) ->
    Other = SameOther,
    ok.

%% ERL-1254
hopefull_export_fun_bug(Config) when is_list(Config) ->
    Msg = [1, fun blipp:blapp/7,
           2, fun blipp:blapp/7],
    {dummy, dummy@dummy} ! Msg.  % Would crash on debug VM

huge_iovec(Config) ->
    %% Make sure that we can pass a term that will produce
    %% an io-vector larger than IOV_MAX over the distribution...
    %% IOV_MAX is typically 1024. Currently we produce an
    %% element in the io-vector for all off heap binaries...
    NoBinaries = 1 bsl 14,
    BinarySize = 65,
    {ok, Node} = start_node(huge_iovec),
    P = spawn_link(Node,
                   fun () ->
                           receive {From, Data} ->
                                   From ! {self(), Data}
                           end
                   end),
    RBL = mk_rand_bin_list(BinarySize, NoBinaries),
    %% Check that it actually will produce a huge iovec...
    %% If we set a limit on the size of the binaries
    %% that will produce an element in the io-vector
    %% we need to adjust this testcase...
    true = length(term_to_iovec(RBL)) >= NoBinaries,
    P ! {self(), RBL},
    receive
        {P, EchoedRBL} ->
            stop_node(Node),
            RBL = EchoedRBL
    end,
    ok.

mk_rand_bin_list(Bytes, Binaries) ->
    mk_rand_bin_list(Bytes, Binaries, []).

mk_rand_bin_list(_Bytes, 0, Acc) ->
    Acc;
mk_rand_bin_list(Bytes, Binaries, Acc) ->
    mk_rand_bin_list(Bytes, Binaries-1, [mk_rand_bin(Bytes) | Acc]).

mk_rand_bin(Bytes) ->
    mk_rand_bin(Bytes, []).

mk_rand_bin(0, Data) ->
    list_to_binary(Data);
mk_rand_bin(N, Data) ->
    mk_rand_bin(N-1, [rand:uniform(256) - 1 | Data]).


%% Try provoke DistEntry refc bugs (OTP-17513).
dist_entry_refc_race(_Config) ->
    {ok, Node} = start_node(dist_entry_refc_race, "+zdntgc 1"),
    Pid = spawn_link(Node, ?MODULE, derr_run, [self()]),
    {Pid, done} = receive M -> M end,
    stop_node(Node),
    ok.

derr_run(Papa) ->
    inet_db:set_lookup([file]), % make connection attempt fail fast
    NScheds = erlang:system_info(schedulers_online),
    SeqList = lists:seq(1, 25 * NScheds),
    Nodes = [list_to_atom("none@host" ++ integer_to_list(Seq))
             || Seq <- SeqList],
    Self = self(),
    Pids = [spawn_link(fun () -> derr_sender(Self, Nodes) end)
            || _ <- SeqList],
    derr_count(1, 8000),
    [begin unlink(P), exit(P,kill) end || P <- Pids],
    Papa ! {self(), done},
    ok.

derr_count(Max, Max) ->
    done;
derr_count(N, Max) ->
    receive
        count -> ok
    end,
    case N rem 1000 of
        0 ->
            io:format("Total attempts: ~bk~n", [N div 1000]);
        _ -> ok
    end,
    derr_count(N+1, Max).


derr_sender(Main, Nodes) ->
    [{none, Node} ! msg || Node <- Nodes],
    Main ! count,
    derr_sender(Main, Nodes).


%%% Utilities

timestamp() ->
    erlang:monotonic_time(millisecond).

start_node(X) ->
    start_node(X, [], []).

start_node(X, Y) ->
    start_node(X, Y, []).

start_node(Name, Args, Rel) when is_atom(Name), is_list(Rel) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Cookie = atom_to_list(erlang:get_cookie()),
    RelArg = case Rel of
                 [] -> [];
                 _ -> [{erl,[{release,Rel}]}]
             end,
    test_server:start_node(Name, slave,
                           [{args,
                             Args++" -setcookie "++Cookie++" -pa \""++Pa++"\""}
                            | RelArg]);
start_node(Config, Args, Rel) when is_list(Config), is_list(Rel) ->
    Name = list_to_atom((atom_to_list(?MODULE)
                         ++ "-"
                         ++ atom_to_list(proplists:get_value(testcase, Config))
                         ++ "-"
                         ++ integer_to_list(erlang:system_time(second))
                         ++ "-"
                         ++ integer_to_list(erlang:unique_integer([positive])))),
    start_node(Name, Args, Rel).

stop_node(Node) ->
    verify_nc(Node),
    test_server:stop_node(Node).

verify_nc(Node) ->
    P = self(),
    Ref = make_ref(),
    Pid = spawn(Node,
                fun() ->
                        R = erts_test_utils:check_node_dist(fun(E) -> E end),
                        P ! {Ref, R}
                end),
    MonRef = monitor(process, Pid),
    receive
        {Ref, ok} ->
            demonitor(MonRef,[flush]),
            ok;
        {Ref, Error} ->
            ct:log("~s",[Error]),
            ct:fail(failed_nc_refc_check);
        {'DOWN', MonRef, _, _, _} = Down ->
            ct:log("~p",[Down]),
            ct:fail(crashed_nc_refc_check)
    end.

freeze_node(Node, MS) ->
    Own = 300,
    DoingIt = make_ref(),
    Freezer = self(),
    spawn_link(Node,
               fun () ->
                       dctrl_dop_send(Freezer, DoingIt),
                       receive after Own -> ok end,
                       set_internal_state(block, MS+Own)
               end),
    receive DoingIt -> ok end,
    receive after Own -> ok end.

inet_rpc_nodename({N,H,_Sock}) ->
    list_to_atom(N++"@"++H).

do_inet_rpc({_,_,Sock},M,F,A) ->
    Bin = term_to_binary({M,F,A}),
    gen_tcp:send(Sock,Bin),
    case gen_tcp:recv(Sock,0) of
        {ok, Bin2} ->
            T = binary_to_term(Bin2),
            {ok,T};
        Else ->
            {error, Else}
    end.

inet_rpc_server([Host, PortList]) ->
    Port = list_to_integer(PortList),
    {ok, Sock} = gen_tcp:connect(Host, Port,[binary, {packet, 4}, 
                                             {active, false}]),
    inet_rpc_server_loop(Sock).

inet_rpc_server_loop(Sock) ->
    case gen_tcp:recv(Sock,0) of
        {ok, Bin} ->
            {M,F,A} = binary_to_term(Bin),
            Res = (catch apply(M,F,A)),
            RB = term_to_binary(Res),
            gen_tcp:send(Sock,RB),
            inet_rpc_server_loop(Sock);
        _ ->
            erlang:halt()
    end.


start_relay_node(Node, Args) ->
    start_relay_node(Node, Args, []).
start_relay_node(Node, Args, Env) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Cookie = "NOT"++atom_to_list(erlang:get_cookie()),
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, 4}, {active, false}]),
    {ok, Port} = inet:port(LSock),
    {ok, Host} = inet:gethostname(),
    RunArg = "-run " ++ atom_to_list(?MODULE) ++ " inet_rpc_server " ++
    Host ++ " " ++ integer_to_list(Port),
    {ok, NN} = test_server:start_node(Node, peer,
                                      [{env,Env},
                                       {args, Args ++
                                        " -setcookie "++Cookie++" -pa "++Pa++" "++
                                        RunArg}]),
    [N,H] = string:lexemes(atom_to_list(NN),"@"),
    {ok, Sock} = gen_tcp:accept(LSock),
    pang = net_adm:ping(NN),
    {N,H,Sock}.

stop_relay_node({N,H,Sock}) ->
    catch do_inet_rpc(Sock,erlang,halt,[]),
    catch gen_tcp:close(Sock),
    wait_dead(N,H,10).

wait_dead(N,H,0) ->
    {error,{not_dead,N,H}};
wait_dead(N,H,X) ->
    case erl_epmd:port_please(N,H) of
        {port,_,_} ->
            receive
            after 1000 ->
                      ok
            end,
            wait_dead(N,H,X-1);
        noport ->
            ok;
        Else ->
            {error, {unexpected, Else}}
    end.


start_node_monitors(Nodes) ->
    Master = self(),
    lists:foreach(fun (Node) ->
                          spawn(Node,
                                fun () ->
                                        node_monitor(Master)
                                end)
                  end,
                  Nodes),
    ok.

node_monitor(Master) ->
    Opts = [nodedown_reason,{node_type,all}],
    Nodes0 = nodes(connected),
    net_kernel:monitor_nodes(true, Opts),
    Nodes1 = nodes(connected),
    case lists:sort(Nodes0) == lists:sort(Nodes1) of
        true ->
            lists:foreach(fun (Node) ->
                                  Master ! {nodeup, node(), Node}
                          end,
                          Nodes0),
            io:format("~p ~p: ~p~n", [node(), erlang:system_time(microsecond), Nodes0]),
            node_monitor_loop(Master);
        false ->
            net_kernel:monitor_nodes(false, Opts),
            flush_node_changes(),
            node_monitor(Master)
    end.

flush_node_changes() ->
    receive
        {NodeChange, _Node, _InfoList} when NodeChange == nodeup;
                                            NodeChange == nodedown ->
            flush_node_changes()
    after 0 ->
              ok
    end.

node_monitor_loop(Master) ->
    receive
        {nodeup, Node, _InfoList} = Msg ->
            Master ! {nodeup, node(), Node},
            io:format("~p ~p: ~p~n", [node(), erlang:system_time(microsecond), Msg]),
            node_monitor_loop(Master);
        {nodedown, Node, InfoList} = Msg ->
            Reason = case lists:keysearch(nodedown_reason, 1, InfoList) of
                         {value, {nodedown_reason, R}} -> R;
                         _ -> undefined
                     end,
            Master ! {nodedown, node(), Node, Reason},
            io:format("~p ~p: ~p~n", [node(), erlang:system_time(microsecond), Msg]),
            node_monitor_loop(Master)
    end.

verify_up(A, B) ->
    receive {nodeup, A, B} -> ok end,
    receive {nodeup, B, A} -> ok end.

verify_still_up(A, B) ->
    true = lists:member(B, rpc:call(A, erlang, nodes, [connected])),
    true = lists:member(A, rpc:call(B, erlang, nodes, [connected])),
    verify_no_down(A, B).

verify_no_down(A, B) ->
    receive
        {nodedown, A, B, _} = Msg0 ->
            ct:fail(Msg0)
    after 0 ->
              ok
    end,
    receive
        {nodedown, B, A, _} = Msg1 ->
            ct:fail(Msg1)
    after 0 ->
              ok
    end.

%% verify_down(A, B) ->
%%     receive {nodedown, A, B, _} -> ok end,
%%     receive {nodedown, B, A, _} -> ok end.

verify_down(A, ReasonA, B, ReasonB) ->
    receive
        {nodedown, A, B, _} = Msg0 ->
            {nodedown, A, B, ReasonA} = Msg0
    end,
    receive
        {nodedown, B, A, _} = Msg1 ->
            {nodedown, B, A, ReasonB} = Msg1
    end,
    ok.

hostname() ->
    from($@, atom_to_list(node())).

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_, []) -> [].

%% fun_spawn(Fun) ->
%%     fun_spawn(Fun, []).

%% fun_spawn(Fun, Args) ->
%%     spawn_link(erlang, apply, [Fun, Args]).


long_or_short() -> 
    case net_kernel:longnames() of
        true -> " -name ";
        false -> " -sname "
    end.

until(Fun) ->
    case Fun() of
        true ->
            ok;
        false ->
            receive after 10 -> ok end,
            until(Fun)
    end.

forever(Fun) ->
    Fun(),
    forever(Fun).

abort(Why) ->
    set_internal_state(abort, Why).


start_busy_dist_port_tracer() ->
    Tracer = spawn_link(fun () -> busy_dist_port_tracer() end),
    erlang:system_monitor(Tracer, [busy_dist_port]),
    Tracer.

stop_busy_dist_port_tracer(Tracer) when is_pid(Tracer) ->
    unlink(Tracer),
    exit(Tracer, bye);
stop_busy_dist_port_tracer(_) ->
    true.

busy_dist_port_tracer() ->
    receive
        {monitor, _SuspendedProcess, busy_dist_port, _Port} = M ->
            erlang:display(M),
            busy_dist_port_tracer()
    end.

repeat(_Fun, 0) ->
    ok;
repeat(Fun, N) ->
    Fun(),
    repeat(Fun, N-1).

string_to_atom_ext(String) ->
    Utf8List = string_to_utf8_list(String),
    Len = length(Utf8List),
    case Len < 256 of
        true ->
            [?SMALL_ATOM_UTF8_EXT, Len | Utf8List];
        false ->
            [?ATOM_UTF8_EXT, Len bsr 8, Len band 16#ff | Utf8List]
    end.

string_to_atom(String) ->
    binary_to_term(list_to_binary([?VERSION_MAGIC
                                   | string_to_atom_ext(String)])).

string_to_utf8_list([]) ->
    [];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
                                   0 =< CP,
                                   CP =< 16#7F ->
    [CP | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
                                   16#80 =< CP,
                                   CP =< 16#7FF ->
    [16#C0 bor (CP bsr 6),
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
                                   16#800 =< CP,
                                   CP =< 16#FFFF ->
    [16#E0 bor (CP bsr 12),
     16#80 bor (16#3F band (CP bsr 6)),
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)];
string_to_utf8_list([CP|CPs]) when is_integer(CP),
                                   16#10000 =< CP,
                                   CP =< 16#10FFFF ->
    [16#F0 bor (CP bsr 18),
     16#80 bor (16#3F band (CP bsr 12)),
     16#80 bor (16#3F band (CP bsr 6)),
     16#80 bor (16#3F band CP)
     | string_to_utf8_list(CPs)].

mk_pid({NodeName, Creation}, Number, Serial) when is_atom(NodeName) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_pid({NodeNameExt, Creation}, Number, Serial);
mk_pid({NodeNameExt, Creation}, Number, Serial) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
                                              ?PID_EXT,
                                              NodeNameExt,
                                              uint32_be(Number),
                                              uint32_be(Serial),
                                              uint8(Creation)])) of
        Pid when is_pid(Pid) ->
            Pid;
        {'EXIT', {badarg, _}} ->
            exit({badarg, mk_pid, [{NodeNameExt, Creation}, Number, Serial]});
        Other ->
            exit({unexpected_binary_to_term_result, Other})
    end.

mk_port({NodeName, Creation}, Number) when is_atom(NodeName) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_port({NodeNameExt, Creation}, Number);
mk_port({NodeNameExt, Creation}, Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
                                              ?PORT_EXT,
                                              NodeNameExt,
                                              uint32_be(Number),
                                              uint8(Creation)])) of
        Port when is_port(Port) ->
            Port;
        {'EXIT', {badarg, _}} ->
            exit({badarg, mk_port, [{NodeNameExt, Creation}, Number]});
        Other ->
            exit({unexpected_binary_to_term_result, Other})
    end.

mk_ref({NodeName, Creation}, [Number] = NL) when is_atom(NodeName),
                                                 is_integer(Creation),
                                                 is_integer(Number) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_ref({NodeNameExt, Creation}, NL);
mk_ref({NodeNameExt, Creation}, [Number]) when is_integer(Creation),
                                               is_integer(Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
                                              ?REFERENCE_EXT,
                                              NodeNameExt,
                                              uint32_be(Number),
                                              uint8(Creation)])) of
        Ref when is_reference(Ref) ->
            Ref;
        {'EXIT', {badarg, _}} ->
            exit({badarg, mk_ref, [{NodeNameExt, Creation}, [Number]]});
        Other ->
            exit({unexpected_binary_to_term_result, Other})
    end;
mk_ref({NodeName, Creation}, Numbers) when is_atom(NodeName),
                                           is_integer(Creation),
                                           is_list(Numbers) ->
    <<?VERSION_MAGIC, NodeNameExt/binary>> = term_to_binary(NodeName),
    mk_ref({NodeNameExt, Creation}, Numbers);
mk_ref({NodeNameExt, Creation}, Numbers) when is_integer(Creation),
                                              is_list(Numbers) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
                                              ?NEW_REFERENCE_EXT,
                                              uint16_be(length(Numbers)),
                                              NodeNameExt,
                                              uint8(Creation),
                                              lists:map(fun (N) ->
                                                                uint32_be(N)
                                                        end,
                                                        Numbers)])) of
        Ref when is_reference(Ref) ->
            Ref;
        {'EXIT', {badarg, _}} ->
            exit({badarg, mk_ref, [{NodeNameExt, Creation}, Numbers]});
        Other ->
            exit({unexpected_binary_to_term_result, Other})
    end.

uint64_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 64 ->
    [(Uint bsr 56) band 16#ff,
     (Uint bsr 48) band 16#ff,
     (Uint bsr 40) band 16#ff,
     (Uint bsr 32) band 16#ff,
     (Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint64_be(Uint) ->
    exit({badarg, uint64_be, [Uint]}).

uint32_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 32 ->
    [(Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint32_be(Uint) ->
    exit({badarg, uint32_be, [Uint]}).


uint16_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 16 ->
    [(Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint16_be(Uint) ->
    exit({badarg, uint16_be, [Uint]}).

uint8(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 8 ->
    Uint band 16#ff;
uint8(Uint) ->
    exit({badarg, uint8, [Uint]}).

free_memory() ->
    %% Free memory in MB.
    try
	SMD = memsup:get_system_memory_data(),
	{value, {free_memory, Free}} = lists:keysearch(free_memory, 1, SMD),
	TotFree = (Free +
		   case lists:keysearch(cached_memory, 1, SMD) of
		       {value, {cached_memory, Cached}} -> Cached;
		       false -> 0
		   end +
		   case lists:keysearch(buffered_memory, 1, SMD) of
		       {value, {buffered_memory, Buffed}} -> Buffed;
		       false -> 0
		   end),
	TotFree div (1024*1024)
    catch
	error : undef ->
	    ct:fail({"os_mon not built"})
    end.


