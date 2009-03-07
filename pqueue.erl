-module(pqueue).
-export([enqueue/2, dequeue/1]).


enqueue(QueueName, Data) ->
    mqueue:enqueue(mangle(QueueName), Data),
    journal:enqueue(mangle(QueueName), Data).


dequeue(QueueName) ->
    Result = mqueue:dequeue(mangle(QueueName)),
    journal:remove(),
    Result.


mangle(QueueName) ->
    "__PERSISTENT__" ++ QueueName.
