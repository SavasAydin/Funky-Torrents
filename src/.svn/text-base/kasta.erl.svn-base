-module(kasta).
-compile(export_all).

% -include_lib("orber/COSS/CosNaming/CosNaming.hrl").

test()->
    Announce_URL = "http://85.228.185.132:8001/tracker/announce.php",
    Info_hash = "%CB%40%AC%7B%29%8F%00%0B%2C%6A%E0%8E%BC%31%9D%88%A0%FC%91%E9",
    Peer_id = "-TR1330-znmphhbrij37",
    Port = 6888,
    Uploaded = 0,
    Downloaded = 0,
    Left = 0,%%225338,

    URL = lists:flatten(io_lib:format("~s?info_hash=~s&peer_id=~s&port=~p&uploaded=~p&downloaded=~p&left=~p",[Announce_URL,Info_hash,Peer_id,Port,Uploaded,Downloaded,Left])),
    
    inets:start(),
    {ok,Tuple} = httpc:request(URL),
    Tuple.
   

cut(List)->
    cut(List,[]).

cut([],NewList)->
   final_fix(escape_html(lists:reverse(NewList)));

cut([H1|[H2|OldList]],NewList) ->	    
    cut(OldList, [httpd_util:hexlist_to_integer([H1]++[H2])|NewList]).


%% --------------------------------------------
%% OBS! STOLEN!!! 
%% http://paste.lisp.org/display/59691
%% 2010-12-08
%% ---------------------------------------------

escape_html(Data)->
    D = lists:map(fun(X)->
			  Hash2 = integer_to_list(X, 16),
			  lists:concat(["%"|[Hash2]])
			      end, Data),
    lists:concat(D).
%% ------------------------------------------------------------------------------
%% This one we constructed our selves after some "thinking-beers"
%% / BjöMag
%% ------------------------------------------------------------------------------
final_fix(Sentence)->
    final_fix(string:tokens(Sentence,"%"),[]).

final_fix([],NewList)->
    "%" ++ string:join(lists:reverse(NewList),"%");

final_fix([H|OldList],NewList)->
    case length(H) == 2 of
	true->
	    final_fix(OldList,[H|NewList]);
	false ->
	    NewH = [48|H],
	    final_fix(OldList,[NewH|NewList])
    end.
	

