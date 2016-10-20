-module(node1).
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
    node(Id, Predecessor, Successor).

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

status(From)->
    [].

node(Id, Predecessor, Successor) ->
    receive
	{key, Qref, Peer} ->
	    Peer ! {Qref, Id},
	    node(Id, Predecessor, Successor);

	{notify, New} ->
	    Pred = notify(New, Id, Predecessor),
	    node(Id, Pred, Successor);

	{request, Peer} ->
	    request(Peer, Predecessor),
	    node(Id, Predecessor, Successor);

	{status, Pred} ->
	    Succ = stabilize(Pred, Id, Successor),
	    node(Id, Predecessor, Succ);

	stabilize ->
	    stabilize(Successor),
	    node(Id, Predecessor, Successor);

	probe ->
	    io:format("probe initialized ~n"),
	    create_probe(Id, Successor),
	    node(Id, Predecessor, Successor);

	{probe, Id, Nodes, T} ->
	    io:format("I should stop ffs ~n"),
	    remove_probe(T, Nodes),
	    node(Id, Predecessor, Successor);

	{probe, Ref, Nodes, T} ->
	    forward_probe(Ref, T, Nodes, Id, Successor),
	    node(Id, Predecessor, Successor);


	{error,Error} ->
	    io:format("strange message ~w~n", [Error]),
	    node(Id, Predecessor, Successor);
	
	stop ->
	    ok
	
    end.


notify({Nkey, Npid}, Id, Predecessor) ->
    case Predecessor of
	nil ->
	    {Nkey,Npid};
	{Pkey, _} ->
	    case key:between(Nkey, Pkey, Id) of
		true ->
		    {Nkey, Npid};
		false ->
		    Predecessor

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
    io:format("~w: Forwarded ~n", [Id]),
    Spid ! {probe, Ref, Nodes ++ [Id], Time }.

remove_probe(Time, Nodes) ->
    Tot_Time = erlang:system_time(micro_seconds) - Time,
    Nodes_visited = length(Nodes),
    io:format("Iteration Time: ~w, visited ~w nodes ~n",[Tot_Time,Nodes_visited]).
    
