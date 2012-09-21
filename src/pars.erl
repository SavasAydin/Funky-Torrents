%%%------------------------------------------------------------------------------
%%% File    : pars.erl
%%% Author  : Michal Musialik
%%% Description : This module decode bencode and hash
%%% Created : 25 Oct 2010 by mysko <mysko@mysko-laptop>
%%% Version : 1.0
%%%------------------------------------------------------------------------------
%%-----------------------References for module-----------------------------------
%% http://www.erlang.org/
%% http://steve.vinoski.net/blog/2009/01/03/more-sha-in-erlang/
%% http://paste.lisp.org/display/59691
%% http://rhoden.id.au/doc/BEncodingSpecification.html
%% http://wiki.theory.org/BitTorrentSpecification
%% http://www.bittorrent.org/beps/bep_0003.html
%%-------------------------------------------------------------------------------

-module(pars).
-compile(export_all).

%%------------------------------------------------------------------------------
%% External Interfaces
%% Getters for connector
%%------------------------------------------------------------------------------

get_announce(File)->
    Announce = element(1,start(File)),
    Announce.

get_hash(File)->
    Info_Hash = element(3,start(File)),
    Info_Hash.

get_comment(File)->
    Comment = element(2,start(File)),
    Comment.

get_pieces(File)->
    Pieces = element(4,start(File)),
    Pieces.

get_length(File)->
    Length = element(5,start(File)),
    Length.

get_piece_length(File)->
    Piece_Length = element(6,start(File)),
    Piece_Length.

get_file_name(File)->
    File_name = element(7,start(File)),
    File_name.

%%------------------------------------------------------------------------------
%% Module start function where bencode and raw data is recived and various parts 
%% of torrent file information is gathered
%%------------------------------------------------------------------------------
start(File) ->
    {A,_} = filedata:read(File),search(A).

search(Data)->
    Announce = announce(Data),
    Comment = comment(Data),
    Info_hash = info_hash(Data), 
    Pieces = search_info(Data,"pieces"),
    File_Name = search_info(Data,"name"),
    Length = search_info(Data,"length"),
    Piece_Length = search_info(Data,"piece length"),
    {Announce,Comment,Info_hash,Pieces,Length,Piece_Length,File_Name}.

search_info(Data,String)->
    {dic, List} = Data,
    Test = lists:sublist(List,length(List)),
    Test,
    {value, {_, Pieces}} = lists:keysearch({string, "info"},1,Test),
    {dic,List2} = Pieces,
    {value, {{_,_},{_,Length}}} = lists:keysearch({string, String},1,List2),
    Length.

file_length(Info)->
    {dic, Int} = Info,
    {value, {_,Value}} = lists:keysearch({string, "length"},1,Int),
    Value.

comment(X)->
    {dic, List} = X,
    {value, {_, Show}} = lists:keysearch({string, "comment"},1,List), Show.

announce(Info) ->
    {dic, Announce} = Info,
    {value, {_,{string,Value}}} = lists:keysearch({string, "announce"},1,Announce),
   Value.

%%------------------------------------------------------------------------------
%% Hash function with encoding for verification of data.
%%------------------------------------------------------------------------------
info_hash(Info) ->
    {dic, Data} = Info,
    {value, {_,Value}} = lists:keysearch({string, "info"},1,Data),
    sha1(encoder:enc(Value)).
  
%%------------------------------------------------------------------------------
%% Sha1 decoder crypto
%%------------------------------------------------------------------------------
sha1(X)->
   crypto:start(),
    S = crypto:sha(X),
    binary_to_list(S).    
%%------------------------------------------------------------------------------
%% Following functions are parts for bencode decoding. Bencode data is devided 
%% four different kinds of data strings, dictioneries, lists and integrs.
%% At the begining os every data type there is a marker for wich kind of data 
%% it is. 
%%------------------------------------------------------------------------------
bencode_reader([H|T])->
    bencode_reader([H|T], []).
bencode_reader([H|Rest], Acc)->
    case H of
	$l -> list_decode(Rest,[]);
	$i -> int_decode(Rest,[]);
      	$d -> dic_decode(Rest,[]);
	$e -> {ends, Rest};
	S  -> string_decode([S|Rest],Acc)
    end.

%-------------------------------------------------------------------------------
% Decoding integers
%-------------------------------------------------------------------------------

read_int([H|T], Acc) when (H >= $0) and (H =< $9) ->
    read_int(T, [H|Acc]);
read_int(T, Acc)  ->   
    Value = list_to_integer(lists:reverse(Acc)),
    {{integer, Value}, T}.

int_decode(Input, []) ->
    {{integer, Value}, [$e|T]} = read_int(Input,[]),
    {{integer, Value}, T}.

%%-----------------------------------------------------------------------------
%% Decoding list
%%-----------------------------------------------------------------------------

list_decode([$e|T], Acc)->
    {{list, lists:reverse(Acc)}, T};
list_decode(Input, Acc) ->
    {Element, Rest} = bencode_reader(Input),
    list_decode(Rest, [Element|Acc]).
%%------------------------------------------------------------------------------
%% Decoding dictionary
%%------------------------------------------------------------------------------
dic_decode([],Acc) -> Acc;
dic_decode(List, Acc)-> 
    {Key,Rest1} = bencode_reader(List),
    {Value,Rest2} = bencode_reader(Rest1),
    NewAcc = [{Key, Value}|Acc],
   
    case Rest2 of
	[$e|NotParsed] ->
	    {{dic, lists:reverse(NewAcc)},NotParsed};
	_ ->
	    dic_decode(Rest2, NewAcc)
    end.
%%------------------------------------------------------------------------------
%% Decoding string
%%------------------------------------------------------------------------------
string_decode([H|Rest], Acc)->
    if ((H >= $0) and (H =< $9)) ->  
      string_decode(Rest, [H|Acc]);
      H == $: ->
	    Int = list_to_integer(lists:reverse(Acc)),
	    {String, NotParsed} = lists:split(Int, Rest), 
	    {{string, String}, NotParsed}
    end.

