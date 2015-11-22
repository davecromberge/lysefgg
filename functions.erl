%% this is the name of the module, it is compiled with c(functions)
-module(functions).

%% this should be changed to exports([Function/arity])
-compile(export_all).

%% This demonstrates positional pattern matching in a list, with the cons
%% Bound vs unbound variables: Bound variables are variables that already contain a value
%% It's not possible to assign a different value to an already bound variable - this is called invariable variables
head([H | _]) -> H.
second([_,S | _]) -> S.
third([_,_,T | _]) -> T.
fourth([_,_,_,F | _]) -> F.

greeting(male, Name) ->
  io:format("Hello mr ~s", [Name]); %% the semi colon denotes a function fragment
greeting(female, Name) ->
  io:format("Hello mrs ~s", [Name]); %% this is a comment
greeting(_, Name) ->
  io:format("Hello ~s", [Name]). %% this is a wildcard match

%% binding to an already bound variable should produce the same value
same(X,X) ->
  true;
same(_,_) ->
  false.

%% this is similar to Haskell's "at matches", because you can store the original 
%% binding before destructuring it using equality
valid_time({{D,M,Y} = Date, {H,Min,S} = Time}) ->
  io:format("The date tuple (~p) today is: ~p/~p/~p~n", [Date, D, M, Y]),
  io:format("The time tuple (~p) indicates: ~p:~p:~p~n", [Time, H, Min, S]);
valid_time(_) ->
  io:format("Stop feeding me the wrong data.~n").

%% guards are additional clauses that go in a functions head and make pattern matching
%% more expressive
old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

%% in guards, the comma serves the same purpose as andalso 
%% in guards, the semi-colon serves the same purpose as orelse (either one guard succeeds or they%%            all fail)
right_age(X) when X >= 16, X =< 104 -> true;
right_age(_) -> false.
