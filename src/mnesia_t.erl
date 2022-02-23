%%%----------------------------------------------------------------------
%%% File    : mnesia_t.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Mnesia debugging tools
%%% Created :  8 Oct 1999 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(mnesia_t).
-author('dgud@erix.ericsson.se').

-compile([export_all, nowarn_export_all]).
%%-export([Function/Arity, ...]).

lm() ->
    [{P, process_info(P, messages)} 
     || P <- processes(),
	{message_queue_len, 0} < process_info(P, message_queue_len)].
    

show(X) ->
    show(X, []).
show(F, A) when is_list(F) ->
    io:format(user, F, A);
show(Fd, F) when is_pid(Fd) ->
    io:format(Fd, F, []).
show(File, F, A) ->
    io:format(File, F, A).

core() ->
    Prefix = "MnesiaCore.",
    Filter = fun(F) -> lists:prefix(Prefix, F) end,
    {ok, Cwd} = file:get_cwd(),
    case file:list_dir(Cwd) of
	{ok, Files}->
	    CoreFiles = lists:sort(lists:zf(Filter, Files)),
	    show("Mnesia core files: ~p~n", [CoreFiles]),
	    [core(CoreF) || CoreF <- CoreFiles];
	Error ->
	    Error
    end.

logfiles() ->
    {ok, Cwd} = file:get_cwd(),
    {ok, Files} =  file:list_dir(Cwd),
    Filter = fun(F) ->
		     Rev = lists:reverse(F),
		     lists:prefix("DCD", Rev) or lists:prefix("LCD", Rev)
	     end,
    LogFiles = lists:zf(Filter, Files),
    {ok, Out} = file:open("LogFiles.txt", [write]),
    [viewlog(Out,LogF) || LogF <- LogFiles],
    file:close(Out).
	


core(File) ->
    OutFileStr = "TxT" ++ File ++ ".txt",
    {ok, Out} = file:open(OutFileStr, [write]),
    show("~n***** Mnesia core: ~p *****~n", [File]),
    case file:read_file(File) of
	{ok, Bin} ->
	    vcore(Bin, Out);
	_ ->
	    show("~n No Core File..exiting~n")
    end,
    file:close(Out).

vcore(Bin, Out) when is_binary(Bin) ->
    Core = binary_to_term(Bin),
    Fun = fun({Item, Info}) ->
		  show(Out, "***** ~p *****~n", [Item]),
		  case catch vcore_elem(Out, {Item, Info}) of
		      {'EXIT', Reason} ->
			  show(Out, "{'EXIT', ~p}~n", [Reason]);
		      _ -> ok
		  end
	  end,
    lists:foreach(Fun, Core).
    
vcore_elem(Fd, {schema_file, {ok, B}}) ->
    Fname = "/tmp/schema.DAT",
    file:write_file(Fname, B),
%    dets:view(Fname),
%    file:delete(Fname);
    show(Fd, "Dets file not viewied", []);

vcore_elem(Fd, {logfile, {ok, BinList}}) ->
    Fun = fun({F, Info}) ->
		  show(Fd,"----- logfile: ~p -----~n", [F]),
		  case Info of
		      {ok, B} ->
			  Fname = "/tmp/mnesia_vcore_elem.TMP",
			  file:write_file(Fname, B),
			  viewlog(Fd, Fname),
			  file:delete(Fname);
		      _ ->
			  show(Fd, "~p~n", [Info])
		  end
	  end,
    lists:foreach(Fun, BinList);

vcore_elem(Fd,{crashinfo, {Format, Args}}) ->
    show(Fd,Format, Args);
vcore_elem(Fd,{gvar, L}) ->
    show(Fd, "~p~n", [lists:sort(L)]);
vcore_elem(Fd, {transactions, Info}) ->
    mnesia_tm:display_info(Fd, Info);
vcore_elem(Fd, {_Item, Info}) ->
    show(Fd, "~p~n", [Info]).

viewlog(Out, File) ->
    show(Out, "*****  ~p ***** ~n", [File]),
    case mnesia_lib:exists(File) of
	false ->
	    show(Out, "No such file ~p~n", [File]),
	    nolog;
	true ->
	    Repair = mnesia_monitor:get_env(auto_repair),
	    N = view_only,
	    Args = [{file, File}, {name, N}, {repair, Repair}],
	    case disk_log:open(Args) of
		{ok, N} ->
		    view_file(Out, start, N);
		{repaired, _, _, _} ->
		    view_file(Out, start, N);
		{error, Reason} ->
		    show(Out, "Cannot open log ~p: ~p~n", [File, Reason])
	    end
    end.

view_file(Out, C, Log) ->
    case disk_log:chunk(Log, C) of
	{error, _Reason} ->
	    show(Out, "** Possibly truncated FILE ~n", []),
	    error;
	eof ->
	    disk_log:close(Log),
	    eof;
	{C2, Terms} ->
	    lists:foreach(fun(X) -> show(Out, "~p~n", [X]) end, Terms),
	    view_file(Out, C2, Log)
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nc() ->
    Mods = mnesia:ms(),
    nc(Mods).

nc(Mods) when is_list(Mods)->
    [Mod || Mod <- Mods, ok /= load(Mod, compile)].

ni() -> 
    Mods = mnesia:ms(),
    ni(Mods).

ni(Mods) when is_list(Mods) ->
    [Mod || Mod <- Mods, ok /= load(Mod, interpret)].

load(Mod, How) when is_atom(Mod) ->
    case try_load(Mod, How) of
	ok ->
	    ok;
	_ ->
	    mnesia_lib:show( "~n RETRY ~p FROM: ", [Mod]),
	    Abs = mod2abs(Mod),
	    load(Abs, How)
    end;
load(Abs, How) ->
    case try_load(Abs, How) of
	ok ->
	    ok;
	{error, Reason} ->
	    mnesia_lib:show( " *** ERROR *** ~p~n", [Reason]),
	    {error, Reason}
    end.

try_load(Mod, How) ->
    mnesia_lib:show( " ~p ", [Mod]),
    Flags = [{d, debug}],
    case How of
	compile ->
	    case catch c:nc(Mod, Flags) of
		{ok, _} -> ok;
		Other -> {error, Other}
	    end;
	interpret ->
	    case catch int:ni(Mod, Flags) of
		{module, _} -> ok;
		Other -> {error, Other}
	    end
    end.

mod2abs(Mod) ->
    ModString = atom_to_list(Mod),
    SubDir =
	case lists:suffix("test", ModString) of
	    true -> test;
	    false -> src
	end,
    filename:join([code:lib_dir(mnesia), SubDir, ModString]).



%%%%%%%%%%%%%%%

analyse() ->
    analyse(load_cores()).
analyse(Ns) ->
    ok(check_up(Ns), "Not connected: ~p~n~n"),
    ok(check_merged(Ns), "Not merged: ~p~n~n"),
    ok(check_lock_queue(Ns), "Locks Waiting:~n  ~s~n~n"),
    ok(check_held_locks(Ns), "Locks taken:~n  ~s~n~n"),
    ok.

check_up(Ns) ->
    Status = [check_up(Node, Ns) || Node <- Ns],
    [NI || {_, NA, NM} = NI <- Status, (NA =/= [] andalso NM =/= [])].

check_up(This, All0) ->
    All = lists:sort(All0),
    #{nodes:={Ns0,_}} = Core = get_core(This),
    ErlConnected = lists:sort([N || {N, _} <- Ns0]),
    NotAlive = All -- ErlConnected,
    MnesiaConnected = gvar(recover_nodes, Core),
    NotAttached = All -- MnesiaConnected,
    {This, NotAlive, NotAttached}.

check_merged(Ns) ->
    Check = fun(Node, Acc) ->
                    #{load_info:=Info} = get_core(Node),
                    case Info of
                        {info, State} when element(3, State) =:= false ->
                            [Node|Acc];
                        _ -> Acc
                    end
            end,
    lists:foldl(Check, [], Ns).

check_lock_queue(Ns) when is_list(Ns) ->
    Status = [check_lock_queue_1(Node) || Node <- Ns],
    SR = sofs:relation(lists:append(Status)),
    LockWait = sofs:to_external(sofs:relation_to_family(SR)),
    AddNode = fun(Tids) -> [{Tid, tid_to_node(Tid)} || Tid <- Tids] end,
    Str = [io_lib:format("~p ~p~n    have lock ~w~n     waiting: ~p~n    pid: ~p~n",
                         [tid_to_pid(Tid), tid_to_node(Tid), L, AddNode(Wait), pinfo(Tid)])
           || {{L, _, Tid}, Wait} <- LockWait],
    Str.

check_lock_queue_1(This) ->
    #{lock_queue := Q} = get_core(This),
    [{{L,Type, Tid}, Wait} || {L, Type, _, Tid, Wait} <- Q].

check_held_locks(Ns) when is_list(Ns) ->
    Status = [check_held_locks_1(Node) || Node <- Ns],
    SR = sofs:relation(lists:append(Status)),
    Locked = sofs:to_external(sofs:relation_to_family(SR)),
    Report = fun({{L, _, Tid}, Reporter}) ->
                     case pinfo(Tid) of
                         {locking, {_, [undefined|undefined]}} ->
                             io_lib:format("~p ~p~n    have released ~w~n",
                                           [tid_to_pid(Tid), tid_to_node(Tid), L]);
                         PInfo ->
                             io_lib:format("~p ~p~n    ~p have lock ~w~n   know: ~w~n    pid: ~p~n",
                                           [tid_to_pid(Tid), tid_to_node(Tid), Tid, L, Reporter, PInfo])
                     end
             end,
    Str = [Report(L) || L <- Locked],
    Str.

check_held_locks_1(This) ->
    #{held_locks := Q} = get_core(This),
    [{L, This} || L <- Q].


gvar(Key, #{gvar := Gvar}) ->
    proplists:get_value(Key, Gvar).

tid_to_node(Tid) ->
    node(tid_to_pid(Tid)).

tid_to_pid({tid,_,Pid}) ->
    Pid.

pinfo(Pid, #{processes:=Ps, relatives:=Rs, workers:=Ws, locking_procs := LPs}) ->
    [{senders,Ss}, {loader, Ls}] = Ws,
    pinfo_1(Pid, [{LPs,1, locking}, {Rs, 2, mnesia},
                  {Ss, 1, sender}, {Ls, 1, loader}, {Ps, 1, procs}]).

pinfo_1(Pid, [{Ls, Key, What}|Rest]) ->
    case lists:keyfind(Pid, Key, Ls) of
        false -> pinfo_1(Pid, Rest);
        Info  -> {What, Info}
    end;
pinfo_1(Pid, []) ->
    {not_found, node(Pid), Pid}.

pinfo(Pid) when is_pid(Pid) ->
    Node = node(Pid),
    Core = get_core(Node),
    pinfo(Pid, Core);
pinfo(Tid) ->
    pinfo(tid_to_pid(Tid)).

ok(ok, _) -> ok;
ok([], _) -> ok;
ok(Other, Format) ->
    io:format(Format, [Other]).

load_cores() ->
    io:format("If crashes with out of memory, increase with: 'erl +MIscs 2048'"),
    CoresFiles = core_files(),
    Load = fun(File) ->
                   {ok, Bin} = file:read_file(File),
                   Cs0 = binary_to_term(Bin),
                   Cs1 = lists:keydelete(applications, 1, Cs0),
                   Cs2 = lists:keydelete(code_path, 1, Cs1),
                   Cs3 = lists:keydelete(code_loaded, 1, Cs2),
                   Cs4 = lists:keydelete(etsinfo, 1, Cs3),
                   Cs5 = lists:keydelete(schema, 1, Cs4),
                   Cs = Cs5,
                   Core = maps:from_list(Cs),
                   #{nodes:={[{Node,_}|_],_}} = Core,
                   io:format("~w   ~w~n",[Node, maps:keys(Core)]),
                   persistent_term:put({core, Node}, Core),
                   Node
           end,
    Ns = lists:map(Load, CoresFiles),
    io:format("Imported ~p files to persistent_term~n",[length(Ns)]),
    lists:sort(Ns).

get_core(Node) ->
    persistent_term:get({core,Node}).

core_files() ->
    Prefix = "MnesiaCore.",
    Filter = fun(F) -> lists:prefix(Prefix, F) end,
    {ok, Cwd} = file:get_cwd(),
    case file:list_dir(Cwd) of
        {ok, Files}->
            lists:sort(lists:zf(Filter, Files));
        _ ->
            error
    end.
