-module(records).
-compile(export_all).

-record(robot, {name, type=industrial, hobbies, details=[]}).

first_rebot() ->
    #robot{name="Blabla", 
           type=handmade,
           details=["Moved by a small man inside"]}.
