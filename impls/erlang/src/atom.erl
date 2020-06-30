-module(atom).

-export([deref/1, reset/2, swap/3, run/1, new/1]).

run(V) ->
    receive
        {From, deref} ->
            From ! {ok, V},
            run(V);
        {From, reset, NewV} ->
            From ! {ok},
            run(NewV);
        {From, swap, F, Args} ->
            NextV = core:call(F, [V] ++ Args),
            From ! {ok, NextV},
            run(NextV)
    end.

deref(Pid) ->
    Pid ! {self(), deref},
    receive
        {ok, V} -> V
    after 100 ->
        throw({malerr, atom_deref_timeout})
    end.

reset(V, Pid) ->
    Pid ! {self(), reset, V},
    receive
        {ok} -> ok
    after 100 ->
        throw({malerr, atom_reset_timeout})
    end.

swap(F, Args, Pid) ->
    Pid ! {self(), swap, F, Args},
    receive
        {ok, V} -> V
    after 100 ->
        throw({malerr, atom_swap_timeout})
    end.

new(V) ->
    spawn(?MODULE, run, [V]).
