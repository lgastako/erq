-module(mqueue).
-export([enqueue/2, dequeue/1, dequeue/2]).


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
    BlockingClients = [],
    manage_queue(QueueName, Q, BlockingClients).


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


next_from_non_empty_queue(QueueName, Q) ->
    NewQ = queue:init(Q),
    %% TODO: confirm success
    journal:dequeue(QueueName),
    Result = {ok, queue:last(Q)},
    {Result, NewQ}.


remove_last(List) ->
    % TODO: If we are going to stick with a list for this, make this more
    % effcicient.
    [Last|Rest] = lists:reverse(List),
    {Last, lists:reverse(Rest)}.


pop_blocked_consumer(BlockingClients) ->
    % We could do some priotiziation based on timeouts or whatever but that
    % is a) probably not necessary, b) something that should be configurable
    % if we do support it (and I don't want to deal with configuration just
    % yet) and c) something that requires a better data structure (priorty
    % queue?) which I don't want to deal with just yet either.

    % So for now we just grab the oldest one, which requires a complete list
    % reshuffle.  Maybe we should be using a queue for this... ? Oh well, for
    % now it's fine...
    {Last, Rest} = remove_last(BlockingClients),
    {Pid, TimerRef} = Last,
    {Pid, TimerRef, Rest}.


manage_queue_set(Q, QueueName, [], Item) ->
    erqutils:debug("got a queue set with no blocking consumers, so enqueueing it...", []),
    NewQ = queue:cons(Item, Q),
    %% TODO: confirm success
    journal:enqueue(QueueName, Item),
    {ack, NewQ, []};
manage_queue_set(Q, _QueueName, BlockingClients, Item) ->
    erqutils:debug("got a queue set with a blocking consumer, so passing item on directly...", []),
    {BlockedPid, TimerRef, NewBlockingClients} = pop_blocked_consumer(BlockingClients),
    % Is there a race condition here where we get the ref above, then the timer fires, putting the {timeout, ...} message in the mailbox of this process,
    % then we get another blocking get request from the same Pid which causes us to put this same BlockedPid back into the BlockingClients list, THEN
    % we cancel the timer, and as a result eventually that mailbox message gets processed and this stops blocking early?  I am guessing the answer is yes.
    % The best way I can think to solve this is to reate some sort of uniqnue ID that is stored for each request and the {timeout} only causes removal of
    % the right Pid, Id combo.... so TODO: this.
    % (For now it's probably extremely unlikely this would happen... it would depend on the consumer processing the response and asking for another
    % ridiculously quickly).
    timer:cancel(TimerRef),
    % Immediately send the new item to the selected blocked client.
    BlockedPid ! {ok, Item},
    % ack the set command
    {ack, Q, NewBlockingClients}.


manage_queue_get_blocking(Q, QueueName, BlockingClients, Pid, BlockTime) ->
    case queue:is_empty(Q) of
        true ->
            Result = deferred,
            NewQ = Q,
            % We start a timer that will send us the Pid back when the timer has
            % expired.  We store the timer reference with it, so that if we
            % end up servicing this request we can cancel the timer so as to
            % not have lingering timers that interfere with future requests.
            TimerRef = erlang:start_timer(BlockTime, self(), Pid),
            NewBlockingClients = [{Pid, TimerRef}|BlockingClients];
        false ->
            {Result, NewQ} = next_from_non_empty_queue(QueueName, Q),
            NewBlockingClients = BlockingClients
    end,
    {Result, NewQ, NewBlockingClients}.


manage_queue_get(Q, QueueName, BlockingClients) ->
    case queue:is_empty(Q) of
        true ->
            NewQ = Q,
            %% TODO: should we journal:dequeue just in case?
            Result = empty;
        false ->
            {Result, NewQ} = next_from_non_empty_queue(QueueName, Q)
    end,
    {Result, NewQ, BlockingClients}.


manage_queue_unexpected_result(Q, BlockingClients, UnexpectedResult) ->
    erqutils:unexpected_result(UnexpectedResult, "receive in mqueue:manage_queue"),
    {error, Q, BlockingClients}.


manage_queue_timeout(Q, BlockingClients, Pid) ->
    % Remove the specified Pid form the blocking clients list
    NewBlockingClients = lists:filter(fun(X) ->
        case X of
            {Pid, _} -> false;
            _ -> true
        end
    end, BlockingClients),
    {empty, Q, NewBlockingClients}.


manage_queue(QueueName, Q, BlockingClients) ->
    %% erqutils:debug("Q is now length ~p and contains: ~p", [queue:len(Q), Q]),
    erqutils:debug("Q is now length ~p.", [queue:len(Q)]),
    receive
        {set, Item, Pid} ->
            {Result, NewQ, NewBlockingClients} = manage_queue_set(Q, QueueName, BlockingClients, Item);
        {get, Pid, {block, BlockTime}} ->
            {Result, NewQ, NewBlockingClients} = manage_queue_get_blocking(Q, QueueName, BlockingClients, Pid, BlockTime);
        {get, Pid} ->
            {Result, NewQ, NewBlockingClients} = manage_queue_get(Q, QueueName, BlockingClients);
        {timeout, _, Pid} ->
            erqutils:debug("got a timeout for Pid ~p", [Pid]),
            {Result, NewQ, NewBlockingClients} = manage_queue_timeout(Q, BlockingClients, Pid);
        UnexpectedResult ->
            Pid = -1,
            {Result, NewQ, NewBlockingClients} = manage_queue_unexpected_result(Q, BlockingClients, UnexpectedResult)
    end,
    case Result of
        deferred -> ok; % We just queued up a blocking listening
        error -> ok; % We have no pid to reply to
        Result when Pid =/= -1
            -> Pid ! Result % Real result
    end,
    manage_queue(QueueName, NewQ, NewBlockingClients).


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

dequeue(QueueName, Timeout) ->
    QueuePid = get_queue_pid(QueueName),
    QueuePid ! {get, self(), {block, Timeout}},
    receive
        X -> X
    % maybe we should have an "after Timeout + N" here as a failsafe
    % in case no response ever comes?  If so, should probably make it
    % configurable.  On the other hand, if there are no bugs, this should
    % never happen, except maybe in cases of load so extreme we're screwed
    % anyway... and if there are bugs, the solution is to fix the bugs, not
    % to have a failsafe....
    end.
