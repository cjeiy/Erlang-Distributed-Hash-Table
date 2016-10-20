-module(storage).
-compile(export_all).

create() ->
    [].

add(Key, Value, Store) ->
    [{Key, Value} | Store].

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    lists:partition(fun({Key, _}) -> 
			       key:between(Key, From, To) end, Store).

merge(Entries, Store) ->
    Store ++ Entries.
