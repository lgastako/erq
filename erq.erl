-module(erq).
-export([start/0, start/1]).

-define(DEFAULT_PORT, 2345).

%% Read a line of data from the socket, for the control messages (get, set, etc).
read_line_of_data(Socket) ->
    %% Active lets us poll for data so we can switch between line and buffer mode.
    inet:setopts(Socket, [{active, false},
                          {packet, line}]),
    gen_tcp:recv(Socket, 0).


%% Read a fixed size buffer of data from the socket, for the values.
read_fixed_data(Socket, Size) ->
    inet:setopts(Socket, [{packet, raw}]),
    Result = gen_tcp:recv(Socket, Size),
    read_line_of_data(Socket),
    Result.


start() ->
    start(?DEFAULT_PORT).


start(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [list, {reuseaddr, true}, {packet, line}]),
    spawn(fun() -> serve(Listen) end),
    self().


serve(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> serve(Listen) end),
    loop(Socket).


handle_set(Args, Socket) ->
    [QueueName, _FlagsString, _ExpiryString, SizeString] = Args,
    Size = list_to_integer(SizeString),
    erqutils:debug("Reading data of size ~p.~n", [Size]),
    case read_fixed_data(Socket, Size) of
        {ok, Data} ->
            case mqueue:enqueue(QueueName, Data) of
                ack -> "STORED\r\n";
                {error, ErrorMessage} -> lists:flatten(io_lib:format("SERVER_ERROR ~p\r\n",
                                                                     [ErrorMessage]));
                _ -> "SERVER_ERROR invalid response from queue process\r\n"
            end;
        Other -> io:format("Got other, dunno what to do: ~p~n", [Other])
    end.


handle_get(Args) ->
    [QueueName] = Args,
    erqutils:debug("get requested from queue: ~p~n", [QueueName]),
    case mqueue:dequeue(QueueName) of
        {ok, Data} ->
            lists:flatten(io_lib:format("VALUE ~s 0 ~.10B\r\n",
                                        [QueueName, length(Data)]) ++ Data ++ "\r\nEND\r\n");
        {empty} -> "END\r\n";
        _ -> "ERROR\r\n" %% should we use SERVER_ERROR with a message?
    end.


loop(Socket) ->
    case read_line_of_data(Socket) of
        {ok, StrWithNewline} ->
            Str = erqutils:chomp(StrWithNewline),
            erqutils:debug("Server received: = ~p~n", [Str]),
            [Command|Args] = string:tokens(Str, " "),
            erqutils:debug("command: ~p~n", [Command]),
            Response = case Command of
                "get" -> handle_get(Args);
                "set" -> handle_set(Args, Socket);
                _ -> "ERROR\r\n"
            end,
            gen_tcp:send(Socket, Response),
            loop(Socket);
        {tcp_closed, Socket} ->
            erqutils:debug("Server socket closed~n", []);
        {error, closed} -> {ok, closed};
        Thing ->
            io:format("Dunno whats going on: ~p~n", [Thing])
    end.
