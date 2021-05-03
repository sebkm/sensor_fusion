close all;
clear all;

function [X1, P1] = kalmanFilter(X0, P0, F, B, H, Q, R, u, Z)
    % Prediction
    Xp = F*X0 + B*u;
    Pp = F*P0*F' + Q;

    % Update
    K = Pp*H' / (R + H*Pp*H');
    X1 = Xp + K*(Z - H*Xp);
    P1 = Pp - K*H*Pp;
end

############################
# sonars signal preprocess #
############################

sonar1 = csvread("sonar_1.csv");
sonar2 = csvread("sonar_2.csv");
r1 = sonar1(:, 2)/100;
r2 = sonar2(:, 2)/100;
t_tmp = sonar1(:, 3)/1000;
n = length(t_tmp);

u = 2.7;
xs_tmp = (r1.^2 - r2.^2 + u^2) / (2*u);
ys_tmp = sqrt(r1.^2 - (r1.^2 - r2.^2 + u^2).^2 / (4*u^2));

xs = zeros(n, 1);
ys = zeros(n, 1);
ts = zeros(n, 1);

idx = 1;
for i=1:n
    if !imag(ys_tmp(i)) && r1(i) < 3 && r2(i) < 4
        xs(idx, 1) = xs_tmp(i);
        ys(idx, 1) = ys_tmp(i);
        ts(idx, 1) = t_tmp(i);
        idx++;
    end
end

xs = xs(18:51, 1);
ys = ys(18:51, 1);
ts = ts(18:51, 1);
ts -= ts(1, 1);

figure();
hold on;
grid on;
title("Trilateration");
ylabel("Position y");
xlabel("Position x");
plot(xs, ys, '.', "markersize", 20);
plot([0, u], [0, 0], '^', "markersize", 20)
legend("trilateration", "sonars", "location", "northwest");

figure();
hold on;
title("X")
plot(ts, xs, '.', "markersize", 20);

figure();
hold on;
title("Y");
plot(ts, ys, '.', "markersize", 20);

##################
# nav preprocess #
##################

nav = csvread("NAV.CSV");
ax = nav(70:165, 1);
ay = nav(70:165, 2);
ta = nav(70:165, 3);
ta -= ta(1, 1);

figure()
hold on;
title("Ax")
plot(ta, ax, '.', "markersize", 20);

figure()
hold on;
title("Ay")
plot(ta, ay, '.', "markersize", 20);

##########
# kalman #
##########

# params
varA = 1e3;
varTx = 0.2;
varTy = 0.2;
Sigma_a = 0.1;

# init
X0 = [0.4; 0; 0; 0.9; 0; 0];
P0 = zeros(6, 6);
P0(1, 1) = 0.1;
P0(4, 4) = 0.1;

n = length(ta);
kalx = zeros(1, n);
kaly = zeros(1, n);
kalx(1) = X0(1);
kaly(1) = X0(4);

si = 2;
for i = 2:n
    dt = ta(i) - ta(i-1);
    update = si < length(ts) && ts(si) < ta(i);

    F = [
        1 dt 0.5*dt^2 0 0 0;
        0 1 dt 0 0 0;
        0 0 1 0 0 0;
        0 0 0 1 dt 0.5*dt^2;
        0 0 0 0 1 dt;
        0 0 0 0 0 1
    ];

    G = zeros(6, 2);
    G(:, 1) = F(:, 3);
    G(:, 2) = F(:, 6);
    Q = G*G'*Sigma_a^2;

    if update
        R = eye(4).*[varTx; varA; varTy; varA];
        H = [
            1 0 0 0 0 0;
            0 0 1 0 0 0;
            0 0 0 1 0 0;
            0 0 0 0 0 1
        ];
        Z = [xs(si); ax(i); ys(si); ay(i)];
        si++;
    else
        R = eye(2).*[varA; varA];
        H = [
            0 0 1 0 0 0;
            0 0 0 0 0 1
        ];
        Z = [ax(i); ay(i)];
    end

    [X1, P1] = kalmanFilter(X0, P0, F, 0, H, Q, R, 0, Z);
    kalx(i) = X1(1);
    kaly(i) = X1(4);
    X0 = X1;
    P0 = P1;
end

figure();
hold on;
grid on;
title("E2");
xlabel("x [m]");
ylabel("y [m]");
plot(kalx, kaly);
plot(xs, ys, '.', "markersize", 10);
plot([0, u], [0, 0], '^', "markersize", 20)
legend("kalman filter", "bilateration", "sonars", "location", "northwest");

figure();
hold on;
grid on;
title("E2");
xlabel("t [s]");
ylabel("x [m]");
plot(ta, kalx);
plot(ts, xs, '.', "markersize", 10);
legend("kalman filter", "bilateration", "location", "northwest");

figure();
hold on;
grid on;
title("E2");
xlabel("t [s]");
ylabel("y [m]");
plot(ta, kaly);
plot(ts, ys, '.', "markersize", 10);
legend("kalman filter", "bilateration", "location", "northwest");
