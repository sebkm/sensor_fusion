-module(measure_counter).

-behaviour(hera_measure).

-export([init/1, measure/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({Id, Names, Sync, Timeout}) ->
    Name = lists:nth(Id, Names),
    Spec = #{
        name => Name,
        iter => infinity,
        sync => Sync,
        timeout => Timeout
    },
    T0 = hera:timestamp(),
    {ok, {Names, Sync, T0, Timeout}, Spec}.


measure({_,true,T0,Timeout}) ->
    timer:sleep(Timeout),
    T1 = hera:timestamp(),
    {ok, [T1-T0], {undefined, true, T1, Timeout}}; 

measure({Names, false, T0, Timeout}) ->
    T1 = hera:timestamp(),
    L = [0 || Name <- Names, {_,_,T,[_, Age]} <- hera_data:get(Name),
        T > T1-3/2*Timeout, Age > 2*Timeout],
    {ok, [length(L), T1-T0], {Names, false, T0, Timeout}}.

