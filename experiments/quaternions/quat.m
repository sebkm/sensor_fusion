clear all;
close all;
clc;

function q = qmul(q1, q2)
    q = [
        q1(1)*q2(1) - q1(2)*q2(2) - q1(3)*q2(3) - q1(4)*q2(4);
        q1(1)*q2(2) + q1(2)*q2(1) + q1(3)*q2(4) - q1(4)*q2(3);
        q1(1)*q2(3) - q1(2)*q2(4) + q1(3)*q2(1) + q1(4)*q2(2);
        q1(1)*q2(4) + q1(2)*q2(3) - q1(3)*q2(2) + q1(4)*q2(1)
    ];
end

function q = DCM2q(R)
    q12 = 0.25*(1+R(1,1)+R(2,2)+R(3,3));
    q1 = sqrt(q12);
    q = (0.25/q1)*[
        4*q12;
        R(3,2)-R(2,3);
        R(1,3)-R(3,1);
        R(2,1)-R(1,2);
    ];
end

function R = q2DCM(q)
    q0 = q(1);
    q1 = q(2);
    q2 = q(3);
    q3 = q(4);
     
    # First row of the rotation matrix
    r00 = 2 * (q0 * q0 + q1 * q1) - 1;
    r01 = 2 * (q1 * q2 - q0 * q3);
    r02 = 2 * (q1 * q3 + q0 * q2);
     
    # Second row of the rotation matrix
    r10 = 2 * (q1 * q2 + q0 * q3);
    r11 = 2 * (q0 * q0 + q2 * q2) - 1;
    r12 = 2 * (q2 * q3 - q0 * q1);
     
    # Third row of the rotation matrix
    r20 = 2 * (q1 * q3 - q0 * q2);
    r21 = 2 * (q2 * q3 + q0 * q1);
    r22 = 2 * (q0 * q0 + q3 * q3) - 1;

    R = [
        r00 r01 r02;
        r10 r11 r12;
        r20 r21 r22
    ];
end

function res = qrot(point, q)
    p = [0 point];
    qc = [q(1) -q(2:end)];
    pq = qmul(qmul(q, p), qc);
    res = pq(2:end);
end

function pR = rot(q, p)
    R = q2DCM(q);
    pR = p*R;
end

function unit = unitq(q)
    norm = sqrt(sum(q.^2));
    unit = q ./ norm;
end

function drawBox(xyz, dim, q)
    x = xyz(1);
    y = xyz(2);
    z = xyz(3);

    p = rot(q, eye(3));
    plot3(x+[0 p(1,1)], y+[0 p(1,2)], z+[0 p(1,3)]);
    plot3(x+[0 p(2,1)], y+[0 p(2,2)], z+[0 p(2,3)]);
    plot3(x+[0 p(3,1)], y+[0 p(3,2)], z+[0 p(3,3)]);

    points = [
        -1 -1 -1;
        1 -1 -1;
        1 1 -1;
        -1 1 -1;
        -1 -1 1;
        1 -1 1;
        1 1 1;
        -1 1 1
    ];
    vertices = rot(q, points.*dim./2) + ones(8,3).*xyz ;
    faces = [
        1 2 6 5;
        4 3 7 8;
        5 6 7 8;
        1 2 3 4;
        2 3 7 6;
        1 4 8 5;
    ];

    patch("Vertices", vertices, "Faces", faces(1:2,:), "FaceColor", "red");
    patch("Vertices", vertices, "Faces", faces(3:4,:), "FaceColor", "yellow");
    patch("Vertices", vertices, "Faces", faces(5:6,:), "FaceColor", "blue");
end

function Var = deg2QuatVar(degree)
    o = degree*pi/180;
    Rx = [1 0 0; 0 cos(o) -sin(o); 0 sin(o) cos(o)];
    q0 = [1; 0; 0; 0];
    q = qmul(q0, DCM2q(Rx));
    Var = q(2)^2;
end

figure();
hold on;
title("f = 5 [Hz]");
view([-45 30]);
axis([-1 1 -1 1 -1 1]);
set (gca (), "ydir", "reverse");
set (gca (), "zdir", "reverse");

w = [0 pi/2 0];
wx = [0 w(3) -w(2); -w(3) 0 w(1); w(2) -w(1) 0];
Omega = [0 -w; w' wx];

f = 100;
dt = 1/f;
q0 = [1; 0; 0; 0];
drawBox([0 0 0], [1 0.5 0.1], q0);
for i = 1:f
    F = eye(4) + 0.5*Omega*dt;
    % q1 = unitq(F*q0);
    q1 = F*q0;
    q0 = q1;
end
drawBox([0 0 0], [1 0.5 0.1], q1);
