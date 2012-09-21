%%% File    : filedator.erl
%%% Author  : Michal Musialik
%%% Description : File reader and writer
%%% Created : 13 Oct 2010 by mysko <mysko@mysko-laptop>

%------------------------------Refernces----------------------------------------
% Web pages:
% http://www.erldocs.com
% http://www.erlang.org/
% Books:
% A concurrent approach to software development
% Francesco Cesarini
% ISBN: 978-0-596-51818-9
%
% Programming Erlang
% Joe Amstrong
% ISBN-10: 1-9343560-0-X
%-------------------------------------------------------------------------------

-module(filedator).
-compile(export_all).
	    
%this function reads in file and send it to parser
%ex filedata:read("test.torrent").
read(File)->
    {ok, S} = file:open(File, read),
    Result = parsor:bencode_reader(read_lines(S)),
    file:close(S),
    Result.


read_lines(S) -> 
read_lines(S, []).
read_lines(S, Acc) ->
    Line = io:get_line(S, ''),
    case Line of
        eof -> lists:concat(lists:reverse([Line|Acc]));
        _   -> read_lines(S, [Line|Acc])
    end.

%Create a dir 
%Example
%Windows - "C:/Temp"
%Linux - "/home/XXX/Desktop/"
%You need to "" when you write in terminal
dir(X)->
    file:make_dir(X).

% check if file is aviable true/false
% isfile("file")
isfile(X)->
    filelib:is_file(X).
   

writer(Filename,Position,String)->
{ok, S} = file:open(Filename,[read,write,raw,binary]),
    if Position >= 0 ->
	    file:pwrite(S,Position,String),
	    file:close(S);	
       Position < 0 -> error
    end.


