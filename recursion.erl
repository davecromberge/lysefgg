-module(recursion).
%% -export([len/1]).
-compile(export_all).

%% Recursion is the only looping construct in Erlang, together with list
%% comprehensions.
fac(0) -> 1;
fac(N) when N > 0 -> N * fac(N-1).

%% Using an accumulator, the factorial function can be converted to a tail
%% recursive function - linear to iterative process.
tail_fac(N) -> tail_fac(N,1).
tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1, N*Acc).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_H|T], Acc) -> tail_len(T, Acc+1).

%% This function is the same as lists:seq/2
duplicate(0, _) ->
    [];
duplicate(N,Term) when N > 0 ->
    [Term | duplicate(N-1,Term)].

duplicate_tail_rec(N,Term) ->
    duplicate_tail_rec(N,Term,[]).
duplicate_tail_rec(0,_Term,Acc) -> Acc;
duplicate_tail_rec(N,Term,Acc) ->
    duplicate_tail_rec(N-1,Term,[Term|Acc]).

%% This function is inefficient because it needs to make a traversal of the entire
%% list on every invocation while attempting to preserve the correct ordering 
%% on the cons operations.
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

%% Consing an element onto the Acc automatically reverses the list.
tail_reverse(L) -> tail_reverse(L,[]).
tail_reverse([],Acc) ->
    Acc;
tail_reverse([H|T],Acc) ->
    tail_reverse(T,[H|Acc]).

%% This is equivalent to a take function
sublist(L, N) ->
    tail_reverse(sublist(L, N, [])). %% we need to reverse the list because the
                                     %% cons operation would mess up the
                                     %% ordering.
sublist([], _N, Acc) -> Acc;
sublist(_L, 0, Acc) -> Acc;
sublist([H|T],N,Acc) when N > 0 ->
    sublist(T, N-1, [H|Acc]).

%% Last call optimization / tail call elimination avoids storing the stack
%% frame and inlines the function definition into the current stack.
zip(L1,L2) -> tail_reverse(zip(L1,L2,[])).
zip([],_L2,Acc) -> Acc;
zip(_L1,[],Acc) -> Acc;
zip([H1|T1], [H2|T2], Acc) ->
    zip(T1, T2, [{H1, H2} | Acc]).

%% an example is as follows, note the heterogenous list.
%% recursion:quicksort([2, 1, 4, apple, fun(X) -> X < 3 end]).
%% output:
%% [1,2,4,apple,#Fun<erl_eval.6.54118792>]
quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smaller,Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) ->
    {Smaller, Larger};
partition(Pivot, [H|T], Smaller, Larger) ->
    if (H =< Pivot) -> partition(Pivot, T, [H|Smaller], Larger);
       (H > Pivot)  -> partition(Pivot, T, Smaller, [H|Larger])
    end.

%% This version is less efficient but has greater clarity.
lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
    lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
    ++ [Pivot] ++
    lc_quicksort([Bigger || Bigger <- Rest, Bigger > Pivot]).


