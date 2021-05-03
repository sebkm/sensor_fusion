-module(e11).

-behaviour(hera_measure).

-export([calibrate/1]).
-export([init/1, measure/1]).

-define(VAR_Q, 0.001).
-define(VAR_R, 0.01).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calibrate({MBx,MBy,MBz}) ->
    _ = io:get_line("Place the pmod_nav at 0° then press enter"),
    [Ax,Ay,Az] = calibrate(acc, [out_x_xl, out_y_xl, out_z_xl], 100),
    [Mx,My,Mz] = calibrate(mag, [out_x_m, out_y_m, out_z_m], 10),
    R0 = ahrs([Ax,Ay,-Az], [-(Mx-MBx),My-MBy,-(Mz-MBz)]),
    mat:tr(R0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(R0) ->
    Spec = #{
        name => ?MODULE,
        iter => 1000,
        timeout => 10
    },
    X = [[1],[0],[0],[0]],
    P = mat:diag([10,10,10,10]),
    State = {hera:timestamp(), X, P, R0},
    {ok, State, Spec}.


measure({T0, X0, P0, R0}) ->
    DataNav = hera_data:get(nav3, sensor_fusion@nav_2),
    T1 = hera:timestamp(),
    Nav = [Data || {_,_,Ts,Data} <- DataNav, T0 < Ts, T1-Ts < 500],
    if
        length(Nav) == 0 ->
            {undefined, {T0, X0, P0, R0}};
        true ->
            {Acc, Gyro, Mag} = process_nav(Nav),
            R1 = ahrs(Acc, Mag),
            Quat = dcm2quat(mat:'*'(R1, R0)),
            
            % {ok, Quat, {T1, X0, P0, R0}} % acc_mag only

            Dt = (T1-T0)/1000,
            [Wx,Wy,Wz] = Gyro,

            Omega = [
                [0,Wx,Wy,Wz],
                [-Wx,0,-Wz,Wy],
                [-Wy,Wz,0,-Wx],
                [-Wz,-Wy,Wx,0]
            ],
            F = mat:'+'(mat:eye(4), mat:'*'(0.5*Dt, Omega)),
            Q = mat:diag([?VAR_Q,?VAR_Q,?VAR_Q,?VAR_Q]),
            H = mat:eye(4),
            Z = mat:tr([Quat]),
            R = mat:diag([?VAR_R,?VAR_R,?VAR_R,?VAR_R]),

            {Xp, Pp} = kalman:kf_predict({X0,P0}, F, Q),
            {X1, P1} = case qdot(Z, Xp) > 0 of
                true ->
                    kalman:kf_update({Xp, Pp}, H, R, Z);
                false ->
                    kalman:kf_update({mat:'*'(-1,Xp), Pp}, H, R, Z)
            end,
            % {X1, P1} = {Xp, Pp}, % gyro only
            Values = unit([X || [X] <- X1]),
            X1Norm = [[X] || X <- Values],
            {ok, Values, {T1, X1Norm, P1, R0}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qdot([[Q11], [Q12], [Q13], [Q14]], [[Q21], [Q22], [Q23], [Q24]]) ->
    Q11*Q21 + Q12*Q22 + Q13*Q23 + Q14*Q24.


process_nav([Nav]) ->
    {Acc, Next} = lists:split(3, Nav),
    {Gyro, Mag} = lists:split(3, Next),
    {Acc, Gyro, Mag}.


unit(V) ->
    Norm = math:sqrt(lists:sum([X*X || X <- V])),
    [X/Norm || X <- V].


scale(List, Factor) ->
    [X*Factor || X <- List].


calibrate(Comp, Registers, N) ->
    Data = [list_to_tuple(pmod_nav:read(Comp, Registers))
        || _ <- lists:seq(1,N)],
    {X, Y, Z} = lists:unzip3(Data),
    [lists:sum(X)/N, lists:sum(Y)/N, lists:sum(Z)/N].


ahrs(Acc, Mag) ->
    Down = unit([-A || A <- Acc]),
    East = unit(cross_product(Down, unit(Mag))),
    North = unit(cross_product(East, Down)),
    mat:tr([North, East, Down]).


cross_product([U1,U2,U3], [V1,V2,V3]) -> 
    [U2*V3-U3*V2, U3*V1-U1*V3, U1*V2-U2*V1].


dcm2quat(R) ->
    [[R11,R12,R13],
     [R21,R22,R23],
     [R31,R32,R33]
    ] = R,
    Q12 = 0.25*(1+R11+R22+R33),
    Q1 = math:sqrt(Q12),
    V = [
        4*Q12,
        R32-R23,
        R13-R31,
        R21-R12
    ],
    scale(V, (0.25/Q1)).
