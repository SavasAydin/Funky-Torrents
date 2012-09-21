%%% File    : parsor.erl
%%% Author  : Michal Musialik & Björn Eriksson
%%% Description : 
%%% Created : 25 Oct 2010 by mysko <mysko@mysko-laptop>

%-----------------------References----------------------------------------------
% Web pages:
% http://www.erldocs.com
% http://www.erlang.org/
% http://steve.vinoski.net/blog/2009/01/03/more-sha-in-erlang/
% http://rhoden.id.au/doc/BEncodingSpecification.html
% http://wiki.theory.org/BitTorrentSpecification
% http://www.bittorrent.org/beps/bep_0003.html
% Books:
% A concurrent approach to software development
% Francesco Cesarini
% ISBN: 978-0-596-51818-9
%
% Programming Erlang
% Joe Amstrong
% ISBN-10: 1-9343560-0-X
%-------------------------------------------------------------------------------
-module(parsor).
-compile(export_all).

%-------------------------------------------------------------------------------
% This module decode bencode and hash, varaibels are stored in a gen_server
%-------------------------------------------------------------------------------

test()->
    start("our.torrent").


start(File) ->
    {A,_} = filedator:read(File),search(A).

% Search function for different parts of bencode and storing them in a gen_sevrer

search(Data)->
   
    gen_servror:put(announce,announce(Data)),
    gen_servror:put(comment, comment(Data)),
    gen_servror:put(hash,info_hash(Data)),
    Pieces = search_info(Data,"pieces"),
    File_name =  search_info(Data,"name"),
    Length = search_info(Data,"length"),
    Piece_Length = search_info(Data,"piece length"),
    gen_servror:put(pieces,Pieces), 
    gen_servror:put(file_name,File_name),
    gen_servror:put(file_length,Length),
    gen_servror:put(piece_length,Piece_Length).
    
% Search function for reading parsed bencode

search_info(Data,String)->
    {dic, List} = Data,
    Test = lists:sublist(List,length(List)),
    Test,
    {value, {_, Value}} = lists:keysearch({string, "info"},1,Test),
    {dic,List2} = Value,
    {value, {{_,_},{_,Value1}}} = lists:keysearch({string, String},1,List2),Value1.




file_length(Info)->
    {dic, Int} = Info,
    {value, {_,Value}} = lists:keysearch({string, "length"},1,Int),
   Value.

comment(Info)->
    {dic, List} = Info,
    {value, {_, Value}} = lists:keysearch({string, "comment"},1,List), Value.

announce(Info) ->
    {dic, Announce} = Info,
    {value, {_,Value}} = lists:keysearch({string, "announce"},1,Announce),Value.
%--------------------------------------------------------------------------
% Hash decoding 
% This part of code was inspired from:
% http://paste.lisp.org/display/59691
%--------------------------------------------------------------------------

info_hash(Hash_info) ->
    {dic, Data} = Hash_info,
    {value, {_,Value}} = lists:keysearch({string, "info"},1,Data),
    X = escape_html(sha1(encodor:enc(Value))),
    X.
  
escape_html(Data) ->
    D = lists:map(fun(X) ->
                    Hash = erlang:integer_to_list(X, 16),
                    lists:concat(["%"|[Hash]])
                  end, Data),
    lists:concat(D).


%-------------------------End of inspiration------------------------------------



%-------------------------------------------------------------------------------
% Sha1 encoder
%-------------------------------------------------------------------------------
sha1(X)->
   crypto:start(),
    S = crypto:sha(X),
    binary_to_list(S).    
%-------------------------------------------------------------------------------
% bencode decoder
%-------------------------------------------------------------------------------
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
% decoding integers
%-------------------------------------------------------------------------------

read_int([H|T], Acc) when (H >= $0) and (H =< $9) ->
    read_int(T, [H|Acc]);
read_int(T, Acc)  ->   
    Value = list_to_integer(lists:reverse(Acc)),
    {{integer, Value}, T}.

int_decode(Input, []) ->
    {{integer, Value}, [$e|T]} = read_int(Input,[]),
    {{integer, Value}, T}.

%-------------------------------------------------------------------------------
% decoding list
%-------------------------------------------------------------------------------

list_decode([$e|T], Acc)->
    {{list, lists:reverse(Acc)}, T};
list_decode(Input, Acc) ->
    {Element, Rest} = bencode_reader(Input),
    list_decode(Rest, [Element|Acc]).
%-------------------------------------------------------------------------------
% decoding dictionary
%-------------------------------------------------------------------------------
dic_decode([],Acc) -> Acc;
dic_decode(List, Acc)-> 
    {Key,Rest1} = bencode_reader(List),
    {Value,Rest2} = bencode_reader(Rest1),
    Acc1 = [{Key, Value}|Acc],
   
    case Rest2 of
	[$e|NotParsed] ->
	    {{dic, lists:reverse(Acc1)},NotParsed};
	_ ->
	    dic_decode(Rest2, Acc1)
    end.
%-------------------------------------------------------------------------------
% decoding string
%-------------------------------------------------------------------------------
string_decode([H|Rest], Acc)->
    if ((H >= $0) and (H =< $9)) ->  
      string_decode(Rest, [H|Acc]);
      H == $: ->
	    Int = list_to_integer(lists:reverse(Acc)),
	    {String, NotParsed} = lists:split(Int, Rest), 
	    {{string, String}, NotParsed}
    end.

