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


journal_path(QueueName) ->
    %% TODO: Un-hardcode path
    "/tmp/erq_" ++ QueueName ++ ".ej".


replay(QueueName) ->
    {ok, File} = file:open(journal_path(QueueName), read),
    {ok, Terms} = file:consult(File),
    file:close(File),
    Terms.


get_journal_pid(QueueName) ->
    JournalNameAtom = list_to_atom("journal:" ++ QueueName),
    case whereis(JournalNameAtom) of
        undefined -> register(JournalNameAtom,
                              spawn(fun() -> setup_journal(QueueName) end));
        ExistingJournalPid -> ExistingJournalPid
    end.


setup_journal(QueueName) ->
    File = file:open(journal_path(QueueName), [write, append]),
    manage_journal(File).


write_term(File, Term) ->
    io:format(File, "~p.~n", [Term]).


manage_journal(File) ->
    receive
        {set, Data, ReplyPid} ->
            write_term(File, {set, Data}),
            %% TODO: Actually check results
            ReplyPid ! ok;
        {get, ReplyPid} ->
            write_term(File, get),
            %% TODO: Actually check results
            ReplyPid ! ok;
        Other ->
            io:format("Unexpected message: ~p~n", [Other])
    end,
    manage_journal(File).
