-module(erqnesia).
-include("queues.hrl").
-export([create_database/0, start/0, enqueue/3]).

%% Run this once to setup the database.  
%% TODO: Auto detect database existence and create it automatically.
create_database() ->
    create_schema(),
    create_tables().


start() ->
    mnesia:start().


create_schema() ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]).


create_tables() ->
    mnesia:delete_table(queues),
    mnesia:create_table(message,
                        [
                         {disc_copies, [node()]},
                         {type, ordered_set},
                         {attributes, record_info(fields, message)}
                        ]).

enqueue(QueueName, Data, Flags) ->
    mnesia:transaction(fun() -> 
                               mnesia:write(#message{queue_name=QueueName,
                                                     data=Data,
                                                     flags=Flags}) 
                       end).

%% dequeue(QueueName) ->
%%      mnesia:transaction(fun() ->
%%                                 Key = mnesia:first(messages),

