-module(journal).
-export([enqueue/2, dequeue/1]).


enqueue(QueueName, Data) ->
    JournalPid = get_journal_pid(QueueName),
    JournalPid ! {set, Data, self()},
    %% I bet you can just use "receive" as the last statement. TODO: Try this.
    %% If not, there's surely a better idiom for this in Erlang, so figure it out.
    receive
        X -> X
    end.


dequeue(QueueName) ->
    JournalPid = get_journal_pid(QueueName),
    JournalPid ! {get, self()},
    receive
        X -> X
    end.


replay(QueueName) ->
    {ok, Terms} = file:consult(File),
    Terms.


get_queue_pid(QueueName) ->
    JournalNameAtom = list_to_atom("journal:" ++ QueueName),
    case whereis(JournalNameAtom) of
        undefined -> register(JournalNameAtom,
                              spawn(fun() -> setup_journal(QueueName) end));
        ExistingJournalPid -> ExistingJournalPid
    end.


%% From p. 235 of Programming Erlang
unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n",[X]) end, L),
    file:close(S).



setup_journal(QueueName) ->
    % Open the files,
    File = file:open(File, [write, append])
    manage_journal(File).


manage_journal(File) ->
    receive
        {set, Data, ReplyPid} ->
            write_op(File, {set, Data}),
            %% TODO: Actually check results
            ReplyPid ! ok;
        {get, ReplyPid} ->
            write_op(File, get),
            %% TODO: Actually check results
            ReplyPid ! ok;
        Other ->
            io:format("Unexpected message: ~p~n", [Other])
    end,
    manage_journal(File).
