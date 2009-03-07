-module(pqueue).
-export([enqueue/2, dequeue/1]).


enqueue(QueueName, Data) ->
    erqutils:debug("pqueue:enqueue(~p, ~p)", [QueueName, Data]),
    mqueue:enqueue(mangle(QueueName), Data),
    erqutils:debug("pqueue:enqueue :: mqueue:enqueue complete.", []),
    journal:enqueue(mangle(QueueName), Data),
    erqutils:debug("pqueue:enqueue :: journal:enqueue complete.", []).


dequeue(QueueName) ->
    erqutils:debug("pqueue:dequeue(~p)", [QueueName]),
    Result = mqueue:dequeue(mangle(QueueName)),
    journal:remove(),
    Result.


mangle(QueueName) ->
    "__PERSISTENT__" ++ QueueName.
