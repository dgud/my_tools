%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%% shell_default is intended for 'rolling you own'
%% add or remove code as you feel free
%% This is just a empty template which calls routoines in the modulke c
%% to do all the work!

-module(shell_default).

-export([help/0,lc/1,c/1,c/2,nc/1,nl/1,l/1,i/0,pid/3,i/3,m/0,m/1,
         memory/0,memory/1,
	 erlangrc/1,bi/1, regs/0, flush/0,pwd/0,ls/0,ls/1,cd/1, 
	 zi/0, bt/1, q/0,
	 ni/0, nregs/0]).

-export([ih/0,iv/0,im/0,ii/1,ii/2,iq/1,ini/1,ini/2,inq/1,ib/2,ib/3,
	 ir/2,ir/3,ibd/2,ibe/2,iba/3,ibc/3,
	 ic/0,ir/1,ir/0,il/0,ipb/0,ipb/1,iaa/1,iaa/2,ist/1,ia/1,ia/2,ia/3,
	 ia/4,ip/0]).

%% MY own stuff !!
-export([pi/1, pi/3, pid/1, mi/0, ti/1, p2n/0, p2nhelp/0, st/1]).
-export([make/0]).

-import(io, [format/1]).

help() ->
    format('** shell internal commands **~n'),
    format('b()        -- display all variable bindings~n'),
    format('e(N)       -- repeat the expression in query <N>~n'),
    format('f()        -- forget all variable bindings~n'),
    format('f(X)       -- forget the binding of variable X~n'),
    format('h()        -- history~n'),    
    format('history(N) -- set how many previous commands to keep\n'),
    format('results(N) -- set how many previous command results to keep\n'),
    format('v(N)       -- use the value of query <N>~n'),
    format('** commands in module c **~n'),
    c:help(),
    format('** commands in module i (interpreter interface) **~n'),
    format('ih()       -- print help for the i module~n'),
    %% format('** private commands ** ~n'),
    %% format('myfunc()   -- does my operation ...\n'),
    format('~n My own stuff~n'),
    format('pi(N | reg_name)  -- process_info(Pid)~n'),
    format('mi()              -- mnesia:info()~n'),
    true.


make() ->
    {ok, Dir} = file:get_cwd(),
    Opts = case filename:basename(Dir) of
	       "src" -> 
		   Ebin = filename:join(filename:dirname(Dir), ebin),
		   case filelib:is_dir(Ebin) of
		       true -> [{outdir, Ebin}];
		       false -> []
		   end;
	       _ -> 
		   []
	   end,
    case make:all([load,debug_info|Opts]) of
	up_to_date -> ok;
	error -> exit(compile_failed)
    end.	    

pi(Pid)  ->
    catch process_info(pid(Pid)).
pi(A,B,C) ->
    catch process_info(pid(A,B,C)).

pid(Pid) when is_pid(Pid) ->
    Pid;
pid(RegName) when is_atom(RegName) ->
    whereis(RegName);
pid(N) when is_number(N) ->
    c:pid(0,N,0);
pid([A,B,C]) ->
    c:pid(A,B,C);
pid(Pid) when is_list(Pid) ->
    erlang:list_to_pid(Pid);
pid({A,B,C}) ->
    c:pid(A,B,C).

st(Pid) ->
    {_, List} = erlang:process_info(pid(Pid), current_stacktrace),
    List.

mi() ->
    mnesia:info().

ti(Tab) ->
    [io:format("~p~n", [I]) || I <- mnesia:table_info(Tab, all)],
    ok.

p2n() ->
    Ns = [node()|nodes()],
    lists:sort(rpc:multicall(Ns, ?MODULE, p2nhelp, [])).

p2nhelp() ->
    {node(), self()}.

%% these are in alphabetic order it would be nice if they
%% were to *stay* so!

bi(I) 		-> c:bi(I).
bt(Pid)		-> c:bt(pid(Pid)).
c(File) 	-> c:c(File).
c(File, Opt)    -> c:c(File, Opt).
cd(D)           -> c:cd(D).
erlangrc(X) 	-> c:erlangrc(X).
flush()         -> c:flush().
i() 		-> c:i().
i(X,Y,Z) 	-> c:i(X,Y,Z).
l(Mod)       	-> c:l(Mod).
lc(X)  		-> c:lc(X).
ls()            -> c:ls().
ls(S)           -> c:ls(S).
m() 		-> c:m().
m(Mod) 		-> c:m(Mod).
memory()        -> c:memory().
memory(Type)    -> c:memory(Type).
nc(X)     	-> c:nc(X).
ni()            -> c:ni().
nl(Mod) 	-> c:nl(Mod).
nregs()         -> c:nregs().
pid(X,Y,Z) 	-> c:pid(X,Y,Z).
pwd()           -> c:pwd().
q()		-> c:q().
regs()          -> c:regs().
zi() 		-> c:zi().

iaa(Flag)       -> i:iaa(Flag).
iaa(Flag,Fnk)   -> i:iaa(Flag,Fnk).
ist(Flag)       -> i:ist(Flag).
ia(Pid)         -> i:ia(pid(Pid)).
ia(X,Y,Z)       -> i:ia(X,Y,Z).
ia(Pid,Fnk)     -> i:ia(pid(Pid),Fnk).
ia(X,Y,Z,Fnk)   -> i:ia(X,Y,Z,Fnk).
ib(Mod,Line)    -> i:ib(Mod,Line).
ib(Mod,Fnk,Arity) -> i:ib(Mod,Fnk,Arity).
ibd(Mod,Line)   -> i:ibd(Mod,Line).
ibe(Mod,Line)   -> i:ibe(Mod,Line).
iba(M,L,Action) -> i:iba(M,L,Action).
ibc(M,L,Cond)   -> i:ibc(M,L,Cond).
ic()            -> i:ic().
ih()            -> i:help().
ii(Mod)         -> i:ii(Mod).
ii(Mod,Op)      -> i:ii(Mod,Op).
il()            -> i:il().
im()            -> i:im().
ini(Mod)        -> i:ini(Mod).
ini(Mod,Op)     -> i:ini(Mod,Op).
inq(Mod)        -> i:inq(Mod).
ip()            -> i:ip().
ipb()           -> i:ipb().
ipb(Mod)        -> i:ipb(Mod).
iq(Mod)         -> i:iq(Mod).
ir(Mod,Line)    -> i:ir(Mod,Line).
ir(Mod,Fnk,Arity) -> i:ir(Mod,Fnk,Arity).
ir(Mod)         -> i:ir(Mod).
ir()            -> i:ir().
iv()            -> i:iv().
