-module(records).
-compile(export_all).

%% Records are an extension to the language on top of tuples

%% Working with records in the shell:
%%
%% rd lets us define a record in the shell
%% rd(Name, Definition).
%% rp(Term) lets us convert a tuple to a record
%% rf(Name) or rf([Names]) allows the shell to forget specific definitions
%% rf() unloads all record definitions
%% rl() will print all the record definitions that have been added to the shell

-record(robot, {name,
                type=industrial,
                hobbies, %% this defaults to undefined when there is no default set
                details=[]}).

-record(user, {id, name, group, age}).

%% To get the position of a field element within a tuple,
%% #robot.type.
%% 3

first_robot() ->
    #robot{name="Mechatron",
           type=handmade,
           details=["Moved by a small man inside"]}.

car_factory(CorpName) ->
    #robot{name=CorpName, hobbies=["Building cars"]}.

%% Extracting values from a record can be done using the dot syntax:
%% records:get_hobbies(records:car_factory("bmw")).
%% ["Building cars"]
get_hobbies(Robot) when is_record(Robot, robot) ->
    Robot#robot.hobbies.

%% Extracting values from a record can also be done using pattern matching and the = operator
%% Matching on a tuple is awkward because a function head would look like this:
%% function({_, _, _, Field4, _}) ->
admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed!".

old_enough(U = #user{}) when U#user.age > 18 ->
    allowed;
old_enough(_) ->
    forbidden.

%% updating a record appears like an in-place update, but a copy is made:
repairman(Rob) when is_record(Rob, robot) ->
    Details = Rob#robot.details,
    NewRob = #robot{details = ["Repaired by repairman"|Details]},
    {repaired, NewRob}.

%% records may also be defined in header files that end in a .hrl extension,
%% and these may be included via the include attribute:
%% -include(records.hrl).
