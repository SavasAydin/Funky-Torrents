%%%-------------------------------------------------------------------
%%% File    : sup.erl
%%% Author  : mysko <mysko@mysko-laptop>
%%% Description : 
%%%
%%% Created : 16 Dec 2010 by mysko <mysko@mysko-laptop>
%%%-------------------------------------------------------------------
-module(gen_servror_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,test_in_shell/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link()->	
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
		      

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    AChild = {tag1,{gen_servror,start_link,[]},
	      permanent,2000,worker,[gen_servror]},
    {ok,{{one_for_all,0,1}, [AChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================

test_in_shell()->
    {ok, Pid} =  supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).		      
		       
