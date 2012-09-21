%%% name: Magnus Bergqvist
%%% email: gusbermaal@student.gu.se
%%% personal number: 830410-4998

%%I created this gen server using a skeleton provided by Emacs

-module(piece_server).
%% replace/3 looks up a tuple pair containing the given key and deletes
%% it if it exists. Returns the rest of the data base.
-behaviour(gen_server).

%% API
-export([start_link/0,stop/0,put/2,get/1]).

%% gen_server callbacks
-export([init/1, handle_call/2, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================

%% start_link/0 spawns the server process
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% stop/0 casts the message stop, since we don't need a response
stop() ->
    gen_server:cast(?MODULE,stop).

%% put/2 casts a message passing on Key and Value to the server
put(Value) ->
    gen_server:call(?MODULE,put,Value}).

%% get/1 calls a message passing on the Key-value of the element
%% the user wants to get
get() ->
    gen_server:call(?MODULE,{get}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% Description: Initiates the server  - comment from Emacs
%%--------------------------------------------------------
%% Gets called automatically by start_link/0 
init([]) -> 
    {ok,new()}. %%responds by calling new/0 which returns an empty 
                %%list as a base for the data base.



%% Description: Handling call messages - comment from Emacs
%%---------------------------------------------------------
%% Here calls from get/1 is handled, replies with the result from 
%% get/2
handle_call({get},_From,DB) ->
    {reply,get(DB),DB};
%% when a put message is received, put/3 is called to add the new
%% record to the data base.
handle_call(put,Value,DB) ->
    {noreply, put(Value,DB)}.
    

%% Description: Handling cast messages - comment from Emacs
%%---------------------------------------------------------
%% the reply from the stop call handling causes the server process to 
%% terminate
handle_cast(stop,DB) ->
    {stop,normal,DB};




%%Following three functions are from Emacs as a part of the gen_server behaviour:
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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.






%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Here are my database functions:

new() -> %% returns an empty list as base for the database
    [].

%% put/3 adds given Key and Value as a tuple to existing data base
%% after looking up if the key already exists via a call to replace/3.
put(Value,DB) ->
    [Value|DB].

%% get/2 looks up a tuple pair containing the given key and returns
%% corresponding Value in a tuple together with the atom value.
get([]) -> 
  list_empty;
get([H|_DB]) ->
  H.


