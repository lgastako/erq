-module(mqueue).
-export([enqueue/2, dequeue/1]).


get_queue_pid(QueueName) ->
    %% erqutils:debug("Getting pid for queue: ~p", [QueueName]),
    QueueNameAtom = list_to_atom("queue" ++ QueueName),
    Pid = case whereis(QueueNameAtom) of
              undefined -> register(QueueNameAtom,
                                    spawn(fun() -> setup_queue(QueueName) end)),
                           whereis(QueueNameAtom);
              ExistingQueuePid -> ExistingQueuePid
          end,
    %% erqutils:debug("Got pid: ~p", [Pid]),
    Pid.


setup_queue(QueueName) ->
    erqutils:debug("Setting up queue: ~p", [QueueName]),
    Q = case journal:replay(QueueName) of
            {ok, Items} ->
                erqutils:debug("Got ~p journal items to replay.", [length(Items)]),
                replay_journal_items(Items, queue:new());
            {error, Reason} -> {error, Reason};
            Result ->
                erqutils:unexpected_result(Result, "from journal:replay in mqueue:setup_queue"),
                Result
        end,
    manage_queue(QueueName, Q).


replay_journal_item(Item, Q) ->
    case Item of
        {set, Data} ->
            erqutils:debug("Replaying a set with data: ~p", [Data]),
            queue:cons(Data, Q);
        get ->
            erqutils:debug("Replaying a get.", []),
            case queue:is_empty(Q) of
                true -> Q;
                false -> queue:init(Q)
            end;
        Result ->
            erqutils:unexpected_result(Result, "Item in replay_journal_item")
    end.


replay_journal_items([], Q) ->
    erqutils:debug("Done with replay.", []),
    Q;
replay_journal_items([Head|Tail], Q) ->
    erqutils:debug("Replaying ~p more journal items.", [length(Tail) + 1]),
    replay_journal_items(Tail, replay_journal_item(Head, Q)).


manage_queue(QueueName, Q) ->
    %% erqutils:debug("Q is now length ~p and contains: ~p", [queue:len(Q), Q]),
    erqutils:debug("Q is now length ~p.", [queue:len(Q)]),
    receive
        {set, Item, Pid} ->
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
        Result ->
            erqutils:unexpected_result(Result, "receive in mqueue:manage_queue"),
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
    QueuePid ! {set, Data, self()},
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


