-module(pqueue).
-export([enqueue/2, dequeue/1]).


get_queue_pid(QueueName) ->
    QueueNameAtom = list_to_atom(QueueName),
    case whereis(QueueNameAtom) of
        undefined -> register(QueueNameAtom,
                              spawn(fun() -> manage_queue(queue:new()) end));
        ExistingQueuePid -> ExistingQueuePid
    end.



manage_queue(Q) ->
%%    erqutils:debug("Q is now length ~p and contains: ~p~n", [queue:len(Q), Q]),
    erqutils:debug("Q is now length ~p.", [queue:len(Q)]),
    receive
        {add, Item, Pid} ->
            NewQ = queue:cons(Item, Q),
            Result = ack;
        {get, Pid} ->
            case queue:is_empty(Q) of
                true ->
                    NewQ = Q,
                    Result = {empty};
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


