%%%----------------------------------------------------------------------
%%% File    : mnesia_t.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : Mnesia debugging tools
%%% Created :  8 Oct 1999 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(mnesia_t).
-author('dgud@erix.ericsson.se').

-compile(export_all).
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
