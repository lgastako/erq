-module(erq).
-export([start/0, start/1]).

-define(DEFAULT_PORT, 2345).

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


%% Helper for display socket options for debug purposes.
%% TODO: Remove me once everything is working.
report_socket_opts(_Socket, _Message) ->
%%    debug("Socket options (~p): ~p~n", [Message, inet:getopts(Socket, inet:options())]).
    debug("socket reporting supressed...", []),
    ok.


%% Read a line of data from the socket, for the control messages (get, set, etc).
read_line_of_data(Socket) ->
    report_socket_opts(Socket, "read_line_of_data, before reset."),
    %% Active lets us poll for data so we can switch between line and buffer mode.
    inet:setopts(Socket, [{active, false},
                          {packet, line}]),
    report_socket_opts(Socket, "read_line_of_data, after reset."),
    gen_tcp:recv(Socket, 0).


%% Read a fixed size buffer of data from the socket, for the values.
read_fixed_data(Socket, Size) ->
    report_socket_opts(Socket, "read_fixed_data, before reset."),
    inet:setopts(Socket, [{packet, raw}]),
    report_socket_opts(Socket, "read_fixed_data, after reset."),
    Result = gen_tcp:recv(Socket, Size),
    % not sure if this is right yet, but I think there is a blank line after the data.
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


manage_queue(Q) ->
%%    debug("Q is now length ~p and contains: ~p~n", [queue:len(Q), Q]),
    debug("Q is now length ~p.", [queue:len(Q)]),
    receive
        {add, Item, Pid} ->
            NewQ = queue:cons(Item, Q),
            Result = ack;
        {get, Pid} ->
            case queue:is_empty(Q) of
                true ->
                    NewQ = Q,
                    Result = {ok, "empty"}; %% TODO: Block?
                false ->
                    NewQ = queue:init(Q),
                    Result = {ok, queue:last(Q)}
            end;
        Msg ->
            io:format("ERROR! Could not handle msg: ~p~n", [Msg]),
            Pid = -1,
            Result = {error, "could not handle msg"},
            NewQ = Q
    end,
    if
        Pid =/= -1 ->
            Pid ! Result
    end,
    manage_queue(NewQ).


get_queue_pid(QueueName) ->
    QueueNameAtom = list_to_atom(QueueName),
    case whereis(QueueNameAtom) of
        undefined -> register(QueueNameAtom,
                              spawn(fun() -> manage_queue(queue:new()) end));
        ExistingQueuePid -> ExistingQueuePid
    end.


enqueue(QueueName, Data) ->
    QueuePid = get_queue_pid(QueueName),
    QueuePid ! {add, Data, self()},
    receive
        X -> X
    end.


dequeue(QueueName) ->
    QueuePid = get_queue_pid(QueueName),
    QueuePid ! {get, self()},
    receive
        X -> X
    end.


formulate_response(Header, Data) ->
    lists:flatten(Header ++ Data  ++ "\r\nEND\r\n").


handle_set(Args, Socket) ->
    [QueueName, _FlagsString, _ExpiryString, SizeString] = Args,
    Size = list_to_integer(SizeString),
    debug("Reading data of size ~p.~n", [Size]),
    case read_fixed_data(Socket, Size) of
        {ok, Data} ->
            case enqueue(QueueName, Data) of
                ack -> "STORED\r\n";
                {error, ErrorMessage} -> lists:flatten(io_lib:format("SERVER_ERROR ~p\r\n",
                                                                     [ErrorMessage]));
                _ -> "SERVER_ERROR invalid response from queue process\r\n"
            end;
        Other -> io:format("Got other, dunno what to do: ~p~n", [Other])
    end.


handle_get(Args) ->
    [QueueName] = Args,
    debug("get requested from queue: ~p~n", [QueueName]),
    case dequeue(QueueName) of
        {ok, Data} ->
            formulate_response(io_lib:format("VALUE ~s 0 ~.10B\r\n",
                                             [QueueName, length(Data)]), Data);
        _ -> "ERROR\r\n" %% should we use SERVER_ERROR with a message?
    end.


loop(Socket) ->
    case read_line_of_data(Socket) of
        {ok, StrWithNewline} ->
            Str = chomp(StrWithNewline),
            debug("Server received: = ~p~n", [Str]),
            [Command|Args] = string:tokens(Str, " "),
            debug("command: ~p~n", [Command]),
            Response = case Command of
                "get" -> handle_get(Args);
                "set" -> handle_set(Args, Socket);
                _ -> "ERROR\r\n"
            end,
            gen_tcp:send(Socket, Response),
            loop(Socket);
        {tcp_closed, Socket} ->
            debug("Server socket closed~n", []);
        {error, closed} -> {ok, closed};
        Thing ->
            io:format("Dunno whats going on: ~p~n", [Thing])
    end.
