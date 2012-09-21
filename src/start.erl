-module(start).
-export([a/0,b/0,c/0]).

a()->
    io:format("You have selected to download SoftwareArchitectureHandin.zip~n"),
    connector:download("SoftwareArchitectureHandin.torrent").
b()->
    io:format("You have selected to download TheParty.zip~n"),
    connector:download("TheParty.torrent").
c()->
    io:format("You have selected to download Total.zip~n"),
    connector:download("Total.torrent").
