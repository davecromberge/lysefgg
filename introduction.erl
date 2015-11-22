-module(introduction).

%% shell commands in user switch command (^G)

%% h  - gets help
%% ic - interrupt current job and return to responsive shell
%% j  - list running processes
%% k  - kill the shell instead of interrupting it
%% s  - starts a new shell

%% Numbers can be expressed in bases other than 10, use Base#Value

%% Bin = 2#101010,
%% Oct = 8#0677,
%% Hex = 16#AE.

%% Atoms are constants and literals

%% Used to tag or qualify data associated with it
%% Tagged = {request, {header, Name, Id}, {payload, Binary}}.

%% Equality tests

%% =:=  is used to denote equal to
%% =/=  is used to denote not equal to
%% =< and >= are comparison operators

%% Tuples and pattern matching

%% PreciseTemperature = {celcius, 10}.
%% {celcius, Temp} = PreciseTemperature.

%% Lists do not have to be homogenous 

%% List = [dog, {cat, Leo}].
%% hd(List).        gets the head of the list
%% tl(List).        gets the tail of the list
%% l1 ++ l2         catenates two lists
%% l1 -- l2         list difference
%% [H | [1 | []]]   list cons
%% [N || N <- lists:seq(1,5), N rem 2 =:= 0]  list comprehensions/generators

%% Bit syntax encloses binary data with << >>, segments (sequences of bits) are
%% separated by commas. Bitstrings may be used to store string data in a more 
%% C like manner, allowing better efficiency.
%% Binary segments can be described in more than one way: 
%%
%% Value
%% Value:Size - size does not have to be fixed to byte boundaries
%% Value/TypeSpecifierList 
%% Value:Size/TypeSpecifierList
%%
%% Type specifier list can consist of Type, Signedness, EndianNess, Unit
%% Type can be bistring, binary, float, boolean, bits etc.

%% White = 16#FFFFFF.
%% Pixel = <<White:24>>.
%% <<R:8, G:8, B:8>> = Pixel.
%% <<R:8, Rest/binary>> = Pixel.
%% <<X2/integer-signed-little>> =  <<-44>>.

%% Binary comprehensions, use <<>> instead of [], <= instead of <-
%% Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
%% yields <<213,45,132,64,76,32,76,0,0,234,32,15>>
%%
%% RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ].
%% yields [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]
%%
%% If the generator returns binary, a fixed size is required.
%% << <<R:8, G:8, B:8>> ||  {R,G,B} <- RGB >>.
%% yields <<213,45,132,64,76,32,76,0,0,234,32,15>>
