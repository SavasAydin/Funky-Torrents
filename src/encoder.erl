%%%-----------------------------------------------------------------------------
%%% File    : encoder.erl
%%% Author  : Michal Musialik, Ali Issa
%%% Description : Bencode encoder
%%% Created :  6 Nov 2010 by mysko <mysko@mysko-laptop>
%%% Version : 1.0
%%%-----------------------------------------------------------------------------

%%-----------------------------Reference for module-----------------------------
%% http://www.erlang.org/
%% http://steve.vinoski.net/blog/2009/01/03/more-sha-in-erlang/
%% http://paste.lisp.org/display/59691
%% http://rhoden.id.au/doc/BEncodingSpecification.html
%% http://wiki.theory.org/BitTorrentSpecification
%% http://www.bittorrent.org/beps/bep_0003.html
%%------------------------------------------------------------------------------

-module(encoder).
-compile(export_all).

%%------------------------------------------------------------------------------
%% Start case of function where we pick out diffrent part of the 
%% elements to be encoded. Listcomes with preset list comprehention.
%%------------------------------------------------------------------------------

enc(Input)->
    case Input of 
	{integer, Int} -> enc_int(Int);
	{string, String} -> enc_str(String);
	{list, List}-> enc_list([enc(I)|| I <- List]);
	{dic, Dic} -> enc_dic(Dic);
	false -> wrong_input
    end.
	
%%------------------------------------------------------------------------------
%% Bencode encoder for integer,string, list and dictionaries
%%------------------------------------------------------------------------------


enc_int(X)->
    Integer = is_integer(X),
    case Integer of
	Integer = true ->
	    lists:concat(['i',X,'e']);
	false -> wrong_format end.

enc_str(X)->
    String = is_list(X),
    case String of 
	String = true ->
	    lists:concat([length(X),':',X]);
	false -> wrong_format
    end.

enc_list(X)->
    C = lists:concat(X),  
    if	C == "wrong_format"-> wrong_format;
	true-> "l" ++ C ++ "e"
    end.

enc_dic(X)->
    Dic =  lists:map(fun ({A,B})-> enc(A) ++ enc(B) end, X),
    "d" ++ lists:concat(Dic) ++ "e".
