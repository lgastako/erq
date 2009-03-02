-module(erqutils).
-export([debug/2, chomp/1]).

%%-define(debug(_, _), ok).
%%-define(debug(Format, Args), io:format("DEBUG: " ++ Format, Args), ok).
debug(_, _) -> ok.
%%debug(Format, Args) -> io:format("DEBUG: " ++ Format, Args), ok.

%% Remove trailing \r's and \n's from a string.  Probably better ways to do this in
%% erlang, but for now, this works.
chomp([]) -> [];
chomp(S) ->
    Len = length(S),
    LastChar = string:substr(S, Len),
    case LastChar of
        "\n" -> chomp(string:left(S, Len-1));
        "\r" -> chomp(string:left(S, Len-1));
        _ -> S
    end.


