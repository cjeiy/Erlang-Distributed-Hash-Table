-module(node2).
-compile(export_all).

-define(Stabilize, 1000).
-define(Timeout, 5000).


start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
	{Qref, Skey} ->
	    {ok, {Skey, Peer}}
    after ?Timeout ->
	    io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor, Store) ->
    receive
	{key, Qref, Peer} ->
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor, Store);

	{notify, New} ->
	    {Pred, Store1} = notify(New, Id, Predecessor, Store),
	    node(Id, Pred, Successor, Store1);

	{request, Peer} ->
	    request(Peer, Predecessor),
	    node(Id, Predecessor, Successor, Store);

	{status, Pred} ->
	    Succ = stabilize(Pred, Id, Successor),
	    node(Id, Predecessor, Succ, Store);

	stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor, Store);


	{add, Key, Value, Qref, Client} ->
	    Store1 = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Store1);
	{lookup, Key, Qref, Client} ->
	    lookup(Key, Qref, Client, Id,Predecessor, Successor, Store),
	    node(Id, Predecessor, Successor, Store);

	{handover, Elements} ->
	    Merged = storage:merge(Store, Elements),
	    node(Id, Predecessor, Successor, Merged);

	probe ->
	    io:format("probe initialized ~n"),
	    create_probe(Id, Successor),
	    node(Id, Predecessor, Successor, Store);

	{probe, Id, Nodes, T} ->
	    io:format("I should stop ffs ~n"),
	    remove_probe(T, Nodes),
	    node(Id, Predecessor, Successor, Store);

	{probe, Ref, Nodes, T} ->
	    forward_probe(Ref, T, Nodes, Id, Successor),
	    node(Id, Predecessor, Successor, Store);

	status ->
	    io:format("Id: ~w~n", [Id]),
	    io:format("P: ~p, S: ~p~n", [Predecessor, Successor]),
	    io:format("Storage: ~p~n", [Store]),
	    node(Id, Predecessor, Successor, Store);

	{error,Error} ->
	    io:format("strange message ~w~n", [Error]),
	    node(Id, Predecessor, Successor, Store);
	
	stop ->
	    ok
	
    end.





handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.


add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
	true ->
	    Client ! {Qref, ok},
	    storage:add(Key, Value, Store);
	false ->
	    Spid ! {add, Key, Value, Qref, Client},
	    Store

    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
	true ->
	    Result = storage:lookup(Key, Store),
	    Client ! {Qref, Result};
	false ->
	    Spid ! {lookup, Key, Qref, Client} 
    end.


notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
	nil ->
	    Keep = handover(Id, Store, Nkey, Npid),
	    {{Nkey,Npid}, Keep};
	{Pkey, _} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    Keep = handover(Id, Store, Nkey, Npid),
		    {{Nkey,Npid}, Keep};
		false ->
		    {Predecessor, Store}

	    end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
	nil ->
	    Peer ! {status, nil};
	{Pkey, Ppid} ->
	    Peer ! {status, {Pkey, Ppid}}
    end.


stabilize({_, Spid}) ->
    Spid ! {request, self()}.

stabilize(Pred, Id, {Skey, Spid}) ->
    case Pred of
	nil ->
	    Spid ! {notify,{Id, self()}},
	    {Skey, Spid};
	{Id, _} ->
	    {Skey, Spid};
	{Skey, _} ->
	    Spid ! {notify,{Id, self()}},
	    {Skey, Spid};
	{Pkey, Ppid} ->
	    case key:between(Pkey, Id, Skey) of
		true ->
		    Ppid ! {request, self()},
		    Pred;
		false ->
		    Spid ! {notify, {Id,self()}},
		    {Skey, Spid}

	    end
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

create_probe(Id, {_, Spid}) ->
    Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

forward_probe(Ref,Time,Nodes,Id,{_, Spid}) ->
    %io:format("~w: Forwarded ~n", [Id]),
    Spid ! {probe, Ref, Nodes ++ [Id], Time }.

remove_probe(Time, Nodes) ->
    T2 = erlang:system_time(micro_seconds),
    T = T2 - Time,
    Nodes_visited = length(Nodes),
    io:format("Iteration Time: ~w micro seconds, visited ~w nodes ~n",[T,Nodes_visited]).

