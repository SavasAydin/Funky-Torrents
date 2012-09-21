%%% File    : gen_servror.erl
%%% Author  : Michal Musialik
%%% Description : gen_server 
%%% Created : 12 Dec 2010 by mysko <mysko@mysko-laptop>

%-------------------------------------------------------------------------------
% Books:
% A concurrent approach to software development
% Francesco Cesarini
% ISBN: 978-0-596-51818-9
%
% Programming Erlang
% Joe Amstrong
% ISBN-10: 1-9343560-0-X
%-------------------------------------------------------------------------------



-module(gen_servror).
-behaviour(gen_server).

%% API
-export([start_link/0]).



%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).



%----------------Mine Functions---------------------------------------


-export([stop/0,put/2,get/1]).
%--------------------------------------------------------------------

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------


start_link()->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

stop()->
   gen_server:cast(?MODULE,stop).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------

init([])->
    {ok,[]}.

put(Key, Value) ->
    gen_server:cast(?MODULE, {put, Key, Value}).

get(Key)->
    gen_server:call(?MODULE,{get,Key}).


%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get,Key}, _From, Db) ->
   {reply, read(Key,Db),Db}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, Db) ->
    {stop, normal, Db};
handle_cast({put, Key, Value}, Db) ->
    {noreply, write(Key, Value, Db)}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason,Db) ->
    destroy(Db).


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



destroy(_db) ->
    ok.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
write(Key,Element,Db) ->
    [{Key,Element}|Db].


read(Key,[{Key,Element}|_Db])->
		    {ok,Element};
read(Key,[_|Db]) ->
		    read(Key,Db);
read(_Key,[])->
		    {error,instance}.
