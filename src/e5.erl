-module(e5).

-behaviour(hera_measure).

-export([init/1, measure/1]).

-define(VAR_S, 0.0625). % (0.5/2)^2
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
    X = [[0], [0], [0], [0.5]],
    P = mat:diag([2,2,math:pi(),1]),
    State = {hera:timestamp(), -1, -1, X, P},
    {ok, State, Spec}.

measure({T0, SA0, SB0, X0, P0}) ->
    case hera_data:get(nav, node()) of
        [{_,SA,_,[Ay,_]}] when SA > SA0 ->
            T1 = hera:timestamp(),
            Dt = (T1 - T0)/1000,
            Radius = 0.605,
            F = fun([_, _, [O], [W]]) ->
                [[Radius*math:cos(O)], [Radius*math:sin(O)], [O+W*Dt], [W]] end,
            Jf = fun([_, _, [O], _]) -> [
                [0,0,-Radius*math:sin(O),0],
                [0,0,Radius*math:cos(O),0],
                [0,0,1,Dt],
                [0,0,0,1]
            ] end,
            Q = mat:zeros(4,4),

            case hera_data:get(bilateration, sensor_fusion@sonar_1) of
                [{_,SB,_,[Px,Py]}] when SB > SB0 -> % nav + pos
                    H = fun([[X], [Y], _, [W]]) ->
                        [[X], [Y], [Radius*W*W]] end,
                    Jh = fun([_, _, _, [W]]) ->
                        [[1,0,0,0], [0,1,0,0], [0,0,0,2*Radius*W]] end,
                    Z = [[Px], [Py], [-Ay]],
                    R = [[0.2,0,0], [0,0.2,0], [0,0,0.8]],

                    {X1,P1} = kalman:ekf({X0,P0}, {F,Jf}, {H,Jh}, Q, R, Z),
                    Values = lists:append(X1),
                    {ok, Values, {T1, SA, SB, X1, P1}};

                _ -> % nav only
                    H = fun([_, _, _, [W]]) -> [[Radius*W*W]] end,
                    Jh = fun([_, _, _, [W]]) -> [[0,0,0,2*Radius*W]] end,
                    Z = [[-Ay]],
                    R = [[0.8]],

                    {X1,P1} = kalman:ekf({X0, P0}, {F,Jf}, {H,Jh}, Q, R, Z),
                    Values = lists:append(X1),
                    {ok, Values, {T1, SA, SB0, X1, P1}}
            end;
        _ ->
            {undefined, {T0, SA0, SB0, X0, P0}}
    end.
