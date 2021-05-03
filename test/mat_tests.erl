-module(mat_tests).

-include_lib("eunit/include/eunit.hrl").


tr_test() ->
    M1 = [[1]],
    ?assertEqual(M1, mat:tr(M1)),

    M2 = [[1,2], [3,4]],
    ?assertEqual([[1,3], [2,4]], mat:tr(M2)),

    M3 = [[1,2,3], [4,5,6], [7,8,9]],
    M3t = [[1,4,7], [2,5,8], [3,6,9]],
    ?assertEqual(M3t, mat:tr(M3)),

    M12 = [[1,2]],
    ?assertEqual([[1], [2]], mat:tr(M12)),

    M21 = [[1], [2]],
    ?assertEqual([[1,2]], mat:tr(M21)),

    M23 = [[1,2,3], [4,5,6]],
    M23t = [[1,4], [2,5], [3,6]],
    ?assertEqual(M23t, mat:tr(M23)),

    M32 = [[1,2], [3,4], [5,6]],
    M32t = [[1,3,5], [2,4,6]],
    ?assertEqual(M32t, mat:tr(M32)).


'+_test'() ->
    S1 = mat:'+'([[1]], [[1]]),
    ?assertEqual([[2]], S1),

    S2 = mat:'+'([[1,2], [3,4]], [[5,6], [7,8]]),
    ?assertEqual([[6,8], [10,12]], S2),

    S3 = mat:'+'([[1], [2]], [[3], [4]]),
    ?assertEqual([[4], [6]], S3).


'-_test'() ->
    S1 = mat:'-'([[1]], [[1]]),
    ?assertEqual([[0]], S1),

    S2 = mat:'-'([[1,2], [3,4]], [[5,6], [7,8]]),
    ?assertEqual([[-4,-4], [-4,-4]], S2),

    S3 = mat:'-'([[1], [2]], [[3], [4]]),
    ?assertEqual([[-2], [-2]], S3).


'==_test'() ->
    M1 = [[1]],
    ?assert(mat:'=='(M1, M1)),

    M2 = [[1,2], [3,4]],
    ?assert(mat:'=='(M2, M2)),

    M3 = [[1,2,3], [4,5,6]],
    ?assert(mat:'=='(M3, M3)),

    ?assertNot(mat:'=='(M1, M2)),
    ?assertNot(mat:'=='(M2, M1)).


'N*M_test'() ->
    M0 = [[0,0], [0,0]],
    ?assert(mat:'=='(M0, mat:'*'(5, M0))),

    M1 = [[1,1], [1,1]],
    NM1 = [[3,3], [3,3]],
    ?assert(mat:'=='(NM1, mat:'*'(3, M1))),

    M2 = [[1,2,3], [-4,-5,-6], [7, -8, 9]],
    NM2 = [[-1,-2,-3], [4,5,6], [-7, 8, -9]],
    ?assert(mat:'=='(NM2, mat:'*'(-1, M2))).


'M*M_test'() ->
    P1 = mat:'*'([[1]], [[2]]),
    ?assertEqual([[2]], P1),

    P2 = mat:'*'([[1,2], [3,4]], [[5,6], [7,8]]),
    ?assertEqual([[19,22], [43,50]], P2),

    P3 = mat:'*'([[1,2], [3,4]], [[5], [6]]),
    ?assertEqual([[17], [39]], P3),

    P4 = mat:'*'([[5,6]], [[1,2], [3,4]]),
    ?assertEqual([[23,34]], P4),

    P5 = mat:'*'([[1,2]], [[3], [4]]),
    ?assertEqual([[11]], P5),

    P6 = mat:'*'([[1], [2]], [[3,4]]),
    ?assertEqual([[3,4], [6,8]], P6).


'*´_test'() ->
    P1 = mat:'*´'([[1]], [[2]]),
    ?assertEqual([[2]], P1),

    P2 = mat:'*´'([[1,2], [3,4]], [[5,7], [6,8]]),
    ?assertEqual([[19,22], [43,50]], P2),

    P3 = mat:'*´'([[1,2], [3,4]], [[5,6]]),
    ?assertEqual([[17], [39]], P3),

    P4 = mat:'*´'([[5,6]], [[1,3], [2,4]]),
    ?assertEqual([[23,34]], P4),

    P5 = mat:'*´'([[1,2]], [[3,4]]),
    ?assertEqual([[11]], P5),

    P6 = mat:'*´'([[1], [2]], [[3], [4]]),
    ?assertEqual([[3,4], [6,8]], P6).


row_test() ->
    M1 = [[1]],
    ?assertEqual([[1]], mat:row(1, M1)),

    M2 = [[1, 2], [3, 4]],
    ?assertEqual([[1,2]], mat:row(1, M2)),
    ?assertEqual([[3,4]], mat:row(2, M2)).


col_test() ->
    M1 = [[1]],
    ?assertEqual([[1]], mat:col(1, M1)),

    M2 = [[1, 2], [3, 4]],
    ?assertEqual([[1], [3]], mat:col(1, M2)),
    ?assertEqual([[2], [4]], mat:col(2, M2)).


get_test() ->
    M = [[1, 2], [3, 4]],
    ?assertEqual(1, mat:get(1, 1, M)),
    ?assertEqual(2, mat:get(1, 2, M)),
    ?assertEqual(3, mat:get(2, 1, M)),
    ?assertEqual(4, mat:get(2, 2, M)).


zeros_test() ->
    Z1 = [[0]],
    ?assertEqual(Z1, mat:zeros(1,1)),

    Z2 = [[0,0], [0,0]],
    ?assertEqual(Z2, mat:zeros(2,2)),

    Z12 = [[0,0]],
    ?assertEqual(Z12, mat:zeros(1,2)),

    Z21 = [[0], [0]],
    ?assertEqual(Z21, mat:zeros(2,1)).


eye_test() ->
    I1 = [[1]],
    ?assertEqual(I1, mat:eye(1)),

    I2 = [[1,0], [0,1]],
    ?assertEqual(I2, mat:eye(2)),

    I3 = [[1,0,0], [0,1,0], [0,0,1]],
    ?assertEqual(I3, mat:eye(3)).


diag_test() ->
    D1 = [[5]],
    ?assertEqual(D1, mat:diag([5])),

    D2 = [[1,0,0], [0,1,0], [0,0,1]],
    ?assertEqual(D2, mat:diag([1,1,1])),

    D3 = [[7,0,0], [0,8,0], [0,0,-2]],
    ?assertEqual(D3, mat:diag([7,8,-2])).


inv_test() ->
    M1 = [[1]],
    ?assert(mat:'=='(M1, mat:inv(M1))),

    I2 = mat:eye(2),
    M2 = [[1,2], [3,4]],
    ?assert(mat:'=='(I2, mat:'*'(M2, mat:inv(M2)))),

    I3 = mat:eye(3),
    M3 = [[2,-1,0], [-1,2,-1], [0,-1,2]],
    ?assert(mat:'=='(I3, mat:'*'(M3, mat:inv(M3)))),

    I4 = mat:eye(4),
    M4 = [[1,1,1,0], [0,3,1,2], [2,3,1,0], [1,0,2,1]],
    ?assert(mat:'=='(I4, mat:'*'(M4, mat:inv(M4)))).