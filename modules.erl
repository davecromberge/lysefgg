%% a module can be compiled in the shell like this:
%% c(modules, [debug_info, export_all]). The hipe module may be used to compile
%% to native code.
-module(modules). %% name of the module should be an atom and match the filename
-export([add/2, hello/0, greet_and_add_two/1]). %% module interface, list of functions with their
                                                %% arity included, as the name can be common across
                                                %% multiple functions.
-import(io, [format/1]).

-define(sub(X,Y), X-Y). %% this is a macro, to use it: ?sub(11, 5)

add(A,B) ->
    A + B.

%% Shows greetings
%% io:format is used for text formatting, it is part of includes attribute.
hello() ->
    format("Hello, world!~n").

greet_and_add_two(A) ->
    hello(),
    add(A,2).

%% It is possible to inspect the metadata about this module using the
%% module_info/1 function. Vsn is a unique generated reference number that 
%% differentiates each version of the code.
%%
%% modules:module_info().
%%
%% [{module,modules},
%%  {exports,[{add,2},
%%            {hello,0},
%%            {greet_and_add_two,1},
%%            {module_info,0},
%%            {module_info,1}]},
%%  {attributes,[{vsn,[290671497036023282662673398103357907093]}]},
%%  {compile,[{options,[debug_info,export_all]},
%%            {version,"6.0.1"},
%%            {time,{2015,11,23,15,38,27}},
%%            {source,"/Users/davecromberge/development/lysefgg/modules.erl"}]},
%%  {native,false},
%%  {md5,<<218,173,75,68,97,107,78,161,204,250,105,146,70,
%%         233,224,149>>}]
