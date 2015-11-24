-module(what_the_if).
-export([heh_fine/0, oh_gosh/1, help_me/1, insert/2, beach/1]).

%% If statements share guard's syntax, but operate outside of a function
%% clause's head.

%% should check if this actually works (hint: an error will be thrown)
%% ; or orelse is equivalent to short circuit or
%% , or andalso is equivalent to short circuit and
heh_fine() ->
    if 1 =:= 1 -> %% =:= denotes exact or strict equality, as opposed to ==
        works
    end,
    if 1 =:= 2; 1 =:= 1 ->
        works
    end,
    if 1 =:= 2, 1 =:= 1 ->  %% we have a warning here that this is inexhaustive
        fails
    end.

%% else / true should always be avoided, as it is clearer to be exhaustive in your
%% if expressions.
oh_gosh(N) ->
    if N =:= 2 -> might_succeed;
       true -> always_does       %% this is Erlang's version of the else clause
    end.

help_me(Animal) ->
    Talk = if Animal == cat -> "meow";
              Animal == dog -> "bark";
              Animal == beef -> "moo";
              Animal == tree -> "bark";
              true -> "fdagnafa"
           end,
    {Animal, "says " ++ Talk ++ "!"}.

%% This is an inefficient set insertion function, but it illustrates case...of expressions.
insert(X, []) -> [X];
insert(X, Set) ->
    case lists:member(X, Set) of 
        true -> Set;
        false -> [X|Set]
    end.

%% guards may also be used with case...of expressions, 
%% , acts like andalso, where ; acts like orelse
beach(Temperature) ->
    case Temperature of 
        {celcius, N} when N >= 20, N =< 45 -> 
            'favourable'; %% these are not strings but atoms with spaces
        {kelvin, N} when N >= 293, N =< 318 ->
            'scientifically favourable';
        {fahrenheit, N} when N >= 68, N =< 113 ->
            'favourable in the US';
        _ -> 
            'avoid beach'
    end.
