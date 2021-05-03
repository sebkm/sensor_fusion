-module(sonar).

-behaviour(hera_measure).

-export([range/0]).
-export([init/1, measure/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% unsafe
range() ->
    pmod_maxsonar:get()*0.0254.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init({MaxRange, X, Y}) ->
    Spec = #{
        name => ?MODULE,
        iter => infinity,
        timeout => 100,
        sync => true
    },
    {ok, {MaxRange, X, Y}, Spec}.


measure(State={MaxRange, X, Y}) ->
    case pmod_maxsonar:get() of
        Range when Range*0.0254 =< MaxRange ->
            {ok, [Range*0.0254, X, Y], State};
        _ ->
            {undefined, State}
    end.
    