-module(e4).

-behaviour(hera_measure).

-export([init/1, measure/1]).

-define(VAR_A, 0.8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    Spec = #{
        name => ?MODULE,
        iter => 1000,
        timeout => 10
    },
    X = [[1]],
    P = [[1]],
    State = {-1, X, P},
    {ok, State, Spec}.

measure({SA0, X0, P0}) ->
    case hera_data:get(nav, node()) of
        [{_,SA,_,[Ay,_]}] when SA > SA0 ->
            Radius = 0.57,
            F = fun(X) -> X end,
            Jf = fun(_) -> [[1]] end,
            H = fun([[X11]]) -> [[Radius * math:pow(X11, 2)]] end,
            Jh = fun([[X11]]) -> [[2*Radius*X11]] end,
            Q = [[0]],
            R = [[?VAR_A]],
            Z = [[-Ay]],

            {X1,P1} = kalman:ekf({X0, P0}, {F,Jf}, {H,Jh}, Q, R, Z),
            Values = lists:append(X1),
            {ok, Values, {SA, X1, P1}};
        _ ->
            {undefined, {SA0, X0, P0}}
    end.
