-module(name_server).

-export([init/0, handle/2, add/2, find/1]).

%% Client routines

add(Name, Place) -> server1:rpc(?MODULE, {add, Name, Place}).
find(Name) -> server1:rpc(?MODULE, {find, Name}).

%% Callback routines

init() ->
    dict:new().

handle({add, Name, Place}, Dict) ->
    {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) ->
    {dict:find(Name, Dict), Dict}.
