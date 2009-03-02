-module(erqnesia).
-export([create_database/0, start/0]).

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
    mnesia:create_table(queues, 
                        [
                         {disc_copies, [node()]},
                         {type, ordered_set}
                        ]).
