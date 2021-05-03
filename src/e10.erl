-module(e10).

-behaviour(hera_measure).

-export([init/1, measure/1]).

-define(VAR_S, 0.01). % (0.2/2)^2
-define(VAR_P, 0.0025). % (0.1/2)^2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(_) ->
    Spec = #{
        name => ?MODULE,
        iter => infinity,
        timeout => 100
    },
    X = mat:zeros(3, 1),
    P = mat:eye(3),
    State = {hera:timestamp(), X, P},
    {ok, State, Spec}.


measure({T0, X0, P0}) ->
    DataSonars = hera_data:get(sonar),
    T1 = hera:timestamp(),
    Sonars = [{Node,Data} || {Node,_,Ts,Data} <- DataSonars,
        T0 < Ts, T1-Ts < 500],
    {XS,YS,ZS} = xyzS(Sonars),
    if
        length(Sonars) == 0 -> % no measure
            {undefined, {T0, X0, P0}};
        true ->
            F = mat:eye(3),
            Q = mat:diag([?VAR_P,?VAR_P,?VAR_P]),
            H = [[1,0,0] || _ <- XS] ++
                [[0,1,0] || _ <- YS] ++
                [[0,0,1] || _ <- ZS],
            Z = [[X] || X <- XS] ++
                [[Y] || Y <- YS] ++
                [[Z] || Z <- ZS],
            R = mat:diag([?VAR_S || _ <- Sonars]),

            {X1,P1} = kalman:kf({X0,P0}, F, H, Q, R, Z),
            Values = lists:append(X1),
            {ok, Values, {T1, X1, P1}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xyzS(Sonars) ->
    X = [Pos + R*Dir || {sensor_fusion@sonar_1,[R,Pos,Dir]} <- Sonars],
    Y = [Pos + R*Dir || {sensor_fusion@sonar_2,[R,Pos,Dir]} <- Sonars],
    Z = [Pos + R*Dir || {sensor_fusion@sonar_3,[R,Pos,Dir]} <- Sonars],
    {X,Y,Z}.
