-module(hhfuns).
-compile(export_all).

%% add is a higher order function that takes two funs as arguments.
%% hhfuns:add(fun hhfuns:one/0, fun hhfuns:two/0).
%% if functions were referred to by their name only, this would be interpreted
%% as an atom.
one() -> 1.
two() -> 2.
add(X, Y) -> X() + Y().

%% map takes another function as an argument.
%%
%% hhfuns:map(fun hhfuns:incr/1, lists:seq(1,10)).
%% [2,3,4,5,6,7,8,9,10,11]
%% hhfuns:map(fun hhfuns:decr/1, lists:seq(1,10)).
%% [0,1,2,3,4,5,6,7,8,9]
incr(N) -> N+1.
decr(N) -> N-1.

map(_F, []) -> [];
map(F, [H|T]) ->
    [F(H) | map(F,T)].

%% anonymous functions are a convenience that allow the use of functions without
%% having to name them.
anonFun() ->
    Fn = fun() -> a end,
    Fn.

decrList(L) ->
    map(fun(E) -> E-1 end, L).

%% anonymous functions form a closure over their surrounding lexical scope
%% hhfuns:b(hhfuns:a()).
%% "a/0's password isallyourbase"
a() ->
    Secret = "allyourbase",
    fun() -> Secret end.

b(F) ->
    "a/0's password is" ++ F().

%% The scope serves as a private state that is carried with the function, however it is
%% not possible to redefine this state, otherwise known as shadowing.
shadowing() ->
    A = 1,
    fun(A) -> A = 2 end. %% This results in a warning that A is shadowed in fun.

%% Anonymous functions with an internal name are also supported, this allows
%% the anonfun to refer to itself for recursive steps.
anonFun2() ->
    (fun Loop() ->
        io:format("Loop invocation... ~n"),
        timer:sleep(500),
        Loop()
    end)().

%% hhfuns:filter(fun(X) -> X rem 2 =:= 0 end, lists:seq(1,10)).
%% [2,4,6,8,10]
filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).
filter(_Pred, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H|Acc]);
        false -> filter(Pred, T, Acc)
    end.

%% A fold produces a single value using a binary combining function F.
%% hhfuns:fold(fun(A,B) when A > B -> A; (_A, B) -> B end, 1, lists:seq(1,10)).
%% 10
fold(_F, Seed, []) -> Seed;
fold(F, Seed, [H|T]) -> fold(F, F(H, Seed), T).

%% folds can also use lists as an accumulator, making them general enough to
%% implement other HOFs such as reverse, filter, map etc.
reverse(L) ->
    fold(fun(A,B) -> [A|B] end, [], L).

%% hhfuns:map2(fun(X) -> X+1 end, lists:seq(1,10)).
%% [2,3,4,5,6,7,8,9,10,11]
map2(F, L) ->
    reverse(fold(fun(A,B) -> [F(A)|B] end, [], L)).
