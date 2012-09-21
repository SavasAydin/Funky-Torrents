%%% File    : encodor.erl
%%% Author  : Michal Musialik, Ali Issa
%%% Description : Bencode Encoder
%%% Created :  6 Nov 2010 by mysko <mysko@mysko-laptop>

%--------------------Refernaces-------------------------------------------------
% Web pages:
% http://www.erlang.org/
% http://rhoden.id.au/doc/BEncodingSpecification.html
% http://wiki.theory.org/BitTorrentSpecification
% http://www.bittorrent.org/beps/bep_0003.html
% Books:
% Programming Erlang
% Joe Amstrong
% ISBN-10: 1-9343560-0-X
%-------------------------------------------------------------------------------

-module(encodor).
-compile(export_all).

%-------------------------------------------------------------------------------
% Start case of function where we pick out diffrent part of the elements to be
% encoded. 
%-------------------------------------------------------------------------------

enc(Input)->
    case Input of 
	{integer, Int} -> enc_int(Int);
	{string, String} -> enc_str(String);
	{list, List}-> enc_list([enc(I)|| I <- List]);
	{dic, Dic} -> enc_dic(Dic);
	false -> wrong_input
    end.
	
	
%-------------------------------------------------------------------------------
% Bencode encoder for integer,string, list and dictionaries
%-------------------------------------------------------------------------------



% Encoding integer
enc_int(X)->
    Integer = is_integer(X),
    case Integer of
	Integer = true ->
	    lists:concat(['i',X,'e']);
	false -> wrong_format
    end.

% Encoding string

enc_str(X)->
    String = is_list(X),
    case String of 
	String = true ->
	    lists:concat([length(X),':',X]);
	false ->
	    wrong_format
    end.

% Encode list

enc_list(X)->
    C = lists:concat(X),  
    if	C == "wrong_format"-> wrong_format;
 	true->
	    "l" ++ C ++ "e"
    end.

% Encoding dictionary

enc_dic(X)->
    Dic =  lists:map(fun ({A,B})-> enc(A) ++ enc(B) end, X),
     "d" ++ lists:concat(Dic) ++ "e".
