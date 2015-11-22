-module(recursion).
-export([len/1]).

%% this is an example of a recursive function, not tail recursive
len([]) -> 0;
len([_|T]) -> 1 + len(T).
