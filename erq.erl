-module(erq).
-export([start/0, start/1, serve/1]).

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
    {ok, Listen} = gen_tcp:listen(Port, [list,
                                         {reuseaddr, true},
                                         {packet, line},
                                         {ip, {127, 0, 0, 1}}
                                         ]),
    spawn(fun() -> erq:serve(Listen) end),
    self().


serve(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> erq:serve(Listen) end),
    loop(Socket, none).


handle_set(Args, Socket) ->
    [QueueName, _FlagsString, _ExpiryString, SizeString] = Args,
    Size = list_to_integer(SizeString),
    erqutils:debug("Reading data of size ~p.", [Size]),
    case read_fixed_data(Socket, Size) of
        {ok, Data} ->
            case mqueue:enqueue(QueueName, Data) of
                ack -> "STORED\r\n";
                {error, ErrorMessage} -> lists:flatten(io_lib:format("SERVER_ERROR ~p\r\n",
                                                                     [ErrorMessage]));
                _ -> "SERVER_ERROR invalid response from queue process\r\n"
            end;
        Result ->
            erqutils:unexpected_result(Result, "from read_fixed_data in handle_set")
    end.


is_open_requested(Args) ->
    lists:any(fun(X) -> X == "open" end, Args).


is_close_requested(Args) ->
   lists:any(fun(X) -> X == "close" end, Args).


get_timeout_from_args([]) ->
    nonblocking;
get_timeout_from_args(Args) ->
    [First|Rest] = Args,
    erqutils:debug("First/Rest: ~p/~p", [First,Rest]),
    case string:sub_string(First, 1, 2) of
        "t=" -> 
            {Timeout, _Rest} = string:to_integer(string:substr(First, 3)),
            Timeout;
        _ ->
            get_timeout_from_args(Rest)
    end.


parse_queuename_with_args(QueueNameWithArgs) ->
    [QueueName|Args] = string:tokens(QueueNameWithArgs, "/"),
    {QueueName, is_open_requested(Args), is_close_requested(Args), get_timeout_from_args(Args)}.


handle_get(_QueueName, false, true, _Timeout, _) ->
    {"END\r\n", none};
handle_get(_QueueName, true, false, _Timeout, CheckedOutMessage) when CheckedOutMessage =/= none ->
    {"ERROR must close old message before opening a new one\r\n", CheckedOutMessage}; % TODO: Is this a legit way to report the specific error?
handle_get(QueueName, OpenRequested, CloseRequested, Timeout, CheckedOutMessage) ->
    case Timeout of
        nonblocking ->
            Result = mqueue:dequeue(QueueName);
        _ ->
            Result = mqueue:dequeue(QueueName, Timeout)
    end,
    erqutils:debug("get requested from queue: ~p", [QueueName]),
    case Result of
        {ok, Data} ->
            erqutils:debug("Dequeued data: ~p", [Data]),
            case OpenRequested of
                true ->
                    NewCheckedOutMessage = {QueueName, Data};
                false ->
                    case CloseRequested of
                        true -> NewCheckedOutMessage = none;
                        false -> NewCheckedOutMessage = CheckedOutMessage
                    end
            end,
            Response = lists:flatten(io_lib:format("VALUE ~s 0 ~.10B\r\n",
                                        [QueueName, length(Data)]) ++ Data ++ "\r\nEND\r\n");
        empty ->
            erqutils:debug("Queue was empty, so nothing to dequeue.", []),
            NewCheckedOutMessage = CheckedOutMessage,
            Response = "END\r\n";
        Other ->
            erqutils:unexpected_result(Other, "mqueue:dequeue in handle_get"),
            NewCheckedOutMessage = CheckedOutMessage,
            Response = "ERROR\r\n"
            %% should we use SERVER_ERROR with a message?
    end,
    {Response, NewCheckedOutMessage}.


handle_get(Args, CheckedOutMessage) ->
    [QueueNameWithArgs] = Args,
    {QueueName, OpenRequested, CloseRequested, Timeout} = parse_queuename_with_args(QueueNameWithArgs),
    erqutils:debug("QueueName/OpenRequested/CloseRequested/Timeout: ~p/~p/~p/~p", [QueueName, OpenRequested, CloseRequested, Timeout]),
    handle_get(QueueName, OpenRequested, CloseRequested, Timeout, CheckedOutMessage).


return_checked_out_message_if_necessary(none) -> ok;
return_checked_out_message_if_necessary(CheckedOutMessage) ->
    {QueueName, Data} = CheckedOutMessage,
    mqueue:return(QueueName, Data).


loop(Socket, CheckedOutMessage) ->
    case read_line_of_data(Socket) of
        {ok, StrWithNewline} ->
            Str = erqutils:chomp(StrWithNewline),
            erqutils:debug("Server received: = ~p", [Str]),
            [Command|Args] = string:tokens(Str, " "),
            erqutils:debug("command: ~p", [Command]),
            case Command of
                "get" -> 
                    {Response, NewCheckedOutMessage} = handle_get(Args, CheckedOutMessage);
                "set" ->
                    NewCheckedOutMessage = CheckedOutMessage,
                    Response = handle_set(Args, Socket);
                _ ->
                    NewCheckedOutMessage = CheckedOutMessage,
                    Response = "ERROR\r\n"
            end,
            gen_tcp:send(Socket, Response),
            loop(Socket, NewCheckedOutMessage);
        {tcp_closed, Socket} ->
            return_checked_out_message_if_necessary(CheckedOutMessage),
            erqutils:debug("Server socket closed", []);
        {error, closed} ->
            return_checked_out_message_if_necessary(CheckedOutMessage),
            {ok, closed};
        Other ->
            return_checked_out_message_if_necessary(CheckedOutMessage),
            io:format("Dunno whats going on: ~p~n", [Other])
    end.
