-module(journal).
-export(...).

start_journal_for_queue_path(QueuePath) ->
    spawn(fun() -> manage_journal(QueuePath) end).

manage_journal(QueuePath) ->
    receive
%%         {cmd, open} -> do_open(QueuePath);
%%         {cmd, erase} -> do_erase(QueuePath);
%%         {cmd, roll} -> do_roll(QueuePath);
%%         {cmd, close} -> do_close(QueuePath);
%%         {cmd, add} -> do_add(QueuePath);
%%         {cmd, add_with_xid} -> do_add_with_xid(QueuePath);
%%         remove
%% remove_tentative
%% unremove
%% confirm ->

erase(Journal) ->
    close it and delete it.

write_items(Journal, Items) ->
    ...


