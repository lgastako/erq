-module(mqueue).
-export([enqueue/2, dequeue/1]).


get_queue_pid(QueueName) ->
    QueueNameAtom = list_to_atom("queue" ++ QueueName),
    case whereis(QueueNameAtom) of
        undefined -> register(QueueNameAtom,
                              spawn(fun() -> setup_queue(QueueName) end));
        ExistingQueuePid -> ExistingQueuePid
    end.


setup_queue(QueueName) ->
    manage_queue(QueueName,
                 replay_journal_items(journal:replay(QueueName),
                                      queue:new())).


replay_journal_items([], Q) -> Q;
replay_journal_items([Head|Tail], Q) ->
    replay_journal_items(Tail, queue:cons(Q, Head)).


manage_queue(QueueName, Q) ->
    %% erqutils:debug("Q is now length ~p and contains: ~p", [queue:len(Q), Q]),
    erqutils:debug("Q is now length ~p.", [queue:len(Q)]),
    receive
        {add, Item, Pid} ->
            NewQ = queue:cons(Item, Q),
            %% TODO: confirm success
            journal:enqueue(QueueName, Item),
            Result = ack;
        {get, Pid} ->
            case queue:is_empty(Q) of
                true ->
                    NewQ = Q,
                    %% TODO: should we journal:dequeue just in case?
                    Result = {empty};
                false ->
                    NewQ = queue:init(Q),
                    %% TODO: confirm success
                    journal:dequeue(QueueName),
                    Result = {ok, queue:last(Q)}
            end;
        Other ->
            io:format("ERROR! Could not handle msg: ~p", [Other]),
            Pid = -1,
            Result = {error, "could not handle msg"},
            NewQ = Q
    end,
    if
        Pid =/= -1 ->
            Pid ! Result
    end,
    manage_queue(QueueName, NewQ).


enqueue(QueueName, Data) ->
    erqutils:debug("mqueue:enqueue(~p, ~p)", [QueueName, Data]),
    QueuePid = get_queue_pid(QueueName),
    erqutils:debug("mqueue:enqueue :: got QueuePid: ~p", [QueuePid]),
    QueuePid ! {add, Data, self()},
    erqutils:debug("mqueue:enqueue :: waiting for response...", []),
    receive
        X -> X
    end.


dequeue(QueueName) ->
    QueuePid = get_queue_pid(QueueName),
    QueuePid ! {get, self()},
    receive
        X -> X
    end.


