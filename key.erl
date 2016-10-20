-module(key).
-compile(export_all).

-define(_30bit,1000000).%math:pow(2,30)).

generate() ->
    random:uniform(?_30bit).

between(Key, From, To) ->
    if
	From < To   -> (Key>From) and (Key<To);
	To   < From -> (Key>From) or (Key<To);
	From == To  ->  true
    end.
    
