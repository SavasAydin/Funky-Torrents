%%%-------------------------------------------------------------------
%%% File    : commfunc.erl
%%% Author  : Björn Eriksson, Magnus Bergqvist and Savas Aydin
%%%
%%% References: http://wiki.theory.org/BitTorrentSpecification,
%%%             https://github.com/logaan/erlang-bittorrent,
%%%             https://github.com/jlouis/etorrent/tree/master/apps/etorrent/src.
%%%            
%%% Tests by: Ali Issa
%%%
%%% Description : commfunc contains all functions for requesting and
%%%               sending data between to peers. Processes will be
%%%               spawned from these functions.
%%%
%%%
%%% Version : 1.0
%%% Updated : 17 Dec 2010
%%%-------------------------------------------------------------------
-module(commfunc).
-export([escape_html/1, get_pieceInfo/1, get_peerInfo/1,
	 incomming/1, interested/0, request/3, cut/2, cut/1,
	get_pieceNumber/1, get_piece/2]).

%% ------------------------------------------------------------------------
%% sending standard bittorrent messages with with the 4 first numbers 
%% removed in connector inet:setopts
%% ------------------------------------------------------------------------

interested()->
    <<2>>.

request(Index,Offset,Length)->
    <<6,Index/binary,Offset/binary,Length/binary>>.


%% ------------------------------------------------------------------------
%% This functions are just comments because we don't have any upload yet
%% ------------------------------------------------------------------------


%keep_alive()->
%    <<>>.

%choke()->
%    <<0>>.

%unchoke()->
%    <<1>>.



%not_interested()->
%    <<3>>.

%have()-> 
%    <<4>>.

%bitfield(_BitLength,BitField)-> 
%    <<5,BitField/binary>>.


%piece(BlockLength,Index,Offset,Block)->
%    <<9,BlockLength/binary,7,Index/binary,Offset/binary,Block/binary>>.

%cancel(Index,Begin,Length)->
%    <<8,Index/binary,Begin/binary,Length/binary>>.

%port(ListenPort)->
%    <<9,ListenPort>>.



%% ------------------------------------------------------------------------
%% incomming, handling all the bittorrent messages 
%% received from other cliens
%% ------------------------------------------------------------------------

%% keep alive
incomming(<<>>) ->
    keep_alive;

%% choke
incomming(<<0>>) -> 
    choke;

%% unchoke
incomming(<<1>>) ->
    unchoke;

%% interested
incomming(<<2>>) -> 
    interested;

%% not_interested
incomming(<<3>>) ->
    not_interested;

%% have
incomming(<<4, PieceNumber:32>>) ->
    io:format("have piece nr: ~w~n", [PieceNumber]),
    PieceNumber;
    
%% piece, the data recieved
incomming(<<7, _Index:32, _Offset:32, Block/binary>>) ->
    {data,Block};

%% request
incomming(<<6,Index:32, Offset:32, Length:32>>) -> 
      io:format("Received request for ~p bytes ~p bytes into piece ~p~n", [
      Length, Offset, Index]),
    io:format("request~n"),
    {Index, Offset, Length};

%% cancel
incomming(<<8,Index:32, Offset:32, Length:32>>) -> 
      io:format("Received request for ~p bytes ~p bytes into piece ~p~n", [
      Length,Offset,Index]),
    {Index, Offset, Length};

%% bitfield
incomming(<<5, Bitfield/binary>>) ->
    {Bitfield};

%% other information recieved
incomming(Other) ->
    io:format("can't recognise this information:~w~n", [Other]).
    
%% -----------------------------------------------------------------------------
%% cut the info_hash and convert it to hexadecimal
%% calls the escape_html function and convert it to URL format
%% calls the final_fix to fix the problem if the hexadecimal if lower than 16
%% -----------------------------------------------------------------------------
cut(List)->
    cut(List,[]).

cut([],NewList)->
    lists:reverse(NewList);

cut([H1|[H2|OldList]],NewList) ->	    
    cut(OldList, [httpd_util:hexlist_to_integer([H1]++[H2])|NewList]).

%% ---------------------------------------------------------------------
%% OBS! STOLEN!!! 
%% http://paste.lisp.org/display/59691
%% 2010-12-08
%% convert the hexadecimal to URL format
%% created final_fix ourself because the info_hash did not work properly
%% ---------------------------------------------------------------------

escape_html(Data)->
    D = lists:map(fun(X)->
			  Hash2 = erlang:integer_to_list(X, 16),
			  lists:concat(["%"|[Hash2]])
			      end, Data),
    final_fix(lists:concat(D)).

%% ------------------------------------------------------------------------------
%% the fix in case that one zero is missing in the URL format
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

%% ------------------------------------------------------------------------------
%% get_pieceInfo retrieves data from the torrent-file and can be passed on to
%% the downloading processes so that they can keep track on what data they should
%% look for.
%% ------------------------------------------------------------------------------

get_pieceInfo(File)->
%% standard hardcoded length
    ChunkLength = 16384,
    FileLength = pars:get_length(File),
    PieceLength =pars:get_piece_length(File),
    LastPieceLength = FileLength - (PieceLength * (FileLength div PieceLength)),
    NoOfChunksInFullPiece = PieceLength div ChunkLength,
    NoOfFullChunksInLastPiece = LastPieceLength div ChunkLength,
    LastChunkLength = LastPieceLength-(ChunkLength * NoOfFullChunksInLastPiece),
    OutputFilename = pars:get_file_name(File),
    
%% nice div method the calculate how many full pieces =)

    NoOfPieces = case (FileLength/PieceLength)==(FileLength div PieceLength) of
		     true ->
			 FileLength div PieceLength;
		     false ->
			 (FileLength div PieceLength)+1
		 end,

    {NoOfPieces,LastChunkLength,ChunkLength,NoOfChunksInFullPiece,NoOfFullChunksInLastPiece,OutputFilename}.

%% ------------------------------------------------------------------------------
%% get_pieceNumber keeps track of the picenumbers for the downloading processes.
%% Is supposed to be spawned and kept as a simple database.
%% ------------------------------------------------------------------------------
get_pieceNumber(DB)->
    receive
	{get,From} ->
	    get_piece(DB,From);
	{put,PieceNumber} ->
	    get_pieceNumber([PieceNumber|DB])
    end.
get_piece([],From)->
    From ! empty_list;
get_piece([H|DB],From)->
    From ! {piece,H},
    get_pieceNumber(DB).

%%-------------------------------------------------------------------------
%% Björns peer information master func
%% parsing the information recieved from the tracker (only works if it's in dict format) called from connector:tracker_connect/3
%% 
%% ------------------------------------------------------------------------

get_peerInfo(String) ->
    Temp = element(1,pars:bencode_reader(String)),
    List = element(2,Temp),
    {_,Peers} = lists:split(2, List),
    Temp2 = element(1, list_to_tuple(Peers)),
    {list, PeerList} = element(2, Temp2),
    chop(PeerList, []).

%% -----------------------------------------------------------------------
%% chop the PeerList and return a list of tuples with ip, port and peerID
%% sending the list of peer information with a max length of 8 peers
%% and then return to peerList 
%% -----------------------------------------------------------------------

chop([], Accum) -> peerList(lists:reverse(Accum), [], 8);

chop([H|T], Accum)->
    {dic, [{{string, "ip"}, {string, IP}}, {_, {string,PeerID}},
	   {{string, "port"},{integer, Port}}]} = H, 
    
    chop(T, [{IP, Port, PeerID}|Accum]).

%% ------------------------------------------------------------------------
%% recieve a list of peers and return them to connector:tracker_connect/3
%% ------------------------------------------------------------------------

peerList(_PeerInfo, List, 0) -> lists:reverse(List);
 
peerList([], List, _Counter) -> lists:reverse(List);
 
peerList([PeerInfo|Next], List, Counter) ->
    peerList(Next, [PeerInfo|List], Counter - 1).
