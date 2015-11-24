%% A simple recursive data structure for a tree where a node consists of a Key, 
%% Value, and two child nodes where the left subtree's keys are less than the 
%% Key and the right subtree's keys are greater than the key.
-module(tree).
-export([empty/0, insert/3, lookup/2]).

empty() -> {node, 'nil'}.

%% Sample usage of insert:
%%
%% 45> T1 = tree:insert("Jim Woodland", "jim.woodland@gmail.com", tree:empty()).
%% {node,{"Jim Woodland","jim.woodland@gmail.com",
%%        {node,nil},
%%        {node,nil}}}
%%
%% 46> T2 = tree:insert("Mark Anderson", "mark.anderson@msn.com", T1).
%% {node,{"Jim Woodland","jim.woodland@gmail.com",
%%        {node,nil},
%%        {node,{"Mark Anderson","mark.anderson@msn.com",
%%               {node,nil},
%%               {node,nil}}}}}
%%
%% 47> Addresses = tree:insert("Anita Bath", "anita@someuni.edu", tree:insert("Kevin Robert", "myfairy@y
%% ahoo.com", tree:insert("Wilson Longbrow", "longwil@gmail.com", T2))).
%% {node,{"Jim Woodland","jim.woodland@gmail.com",
%%        {node,{"Anita Bath","anita@someuni.edu",
%%               {node,nil},
%%               {node,nil}}},
%%        {node,{"Mark Anderson","mark.anderson@msn.com",
%%               {node,{"Kevin Robert","myfairy@yahoo.com",
%%                      {node,nil},
%%                      {node,nil}}},
%%               {node,{"Wilson Longbrow","longwil@gmail.com",
%%                      {node,nil},
%%                      {node,nil}}}}}}}
insert(Key, Val, {node, 'nil'}) ->
    {node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key ->
    {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key ->
    {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(Key, _, {node, {Key, Val, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.

%% Sample usage of lookups:
%% 
%% 57> tree:lookup("Anita Bath", Addresses).
%% {node,{"Anita Bath","anita@someuni.edu",
%%        {node,nil},
%%        {node,nil}}}
%% 
%% 58> tree:lookup("Jaques Requin", Addresses).
%% undefined
lookup(_Key, {node, 'nil'}) ->
    undefined;
lookup(SKey, {node, {Key, _Val, Smaller, _Larger}}) when SKey < Key ->
    lookup(SKey, Smaller);
lookup(SKey, {node, {Key, _Val, _Smaller, Larger}}) when SKey > Key ->
    lookup(SKey, Larger);
lookup(Key, {node, {Key, Val, Smaller, Larger}}) ->
    {node, {Key, Val, Smaller, Larger}}.
