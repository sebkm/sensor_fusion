close all;
clear all;
pkg load statistics;

function [X1, P1] = kalmanFilter(X0, P0, F, B, H, Q, R, u, Z)
    % Prediction
    Xp = F*X0 + B*u;
    Pp = F*P0*F' + Q;

    % Update
    K = Pp*H' / (R + H*Pp*H');
    X1 = Xp + K*(Z - H*Xp);
    P1 = Pp - K*H*Pp;
end

# params
u = 2;
varA = 0.2;
varS = 0.1;
varTx = 0.2;
varTy = 0.2;
ea = 1e-3;
dt = 0.1;
A = 0.1;
t = [0:dt:10];
n = length(t);

# actual
Shift = 1/5*pi;
ax = A*sin(t*Shift);
ay = zeros(1, n);
x = -A/Shift^2*sin(t*Shift) + A/Shift*t;
y = ones(1, n);

# accelerometers
accx = ax + normrnd(0, varA, 1, n);
accy = ay + normrnd(0, varA, 1, n);

# sonars
r1 = sqrt(x.^2 + y.^2) + normrnd(0, sqrt(varS), 1, n);
r2 = sqrt((u-x).^2 + y.^2) + normrnd(0, sqrt(varS), 1, n);

# trilateration
xs = (r1.^2 - r2.^2 + u^2) / (2*u);
ys = sqrt(r1.^2 - (r1.^2 - r2.^2 + u^2).^2 / (4*u^2));
xs(1) = x(1);
ys(1) = y(1);
for i=2:n
    if imag(ys(i))
        xs(i) = xs(i-1);
        ys(i) = ys(i-1);
    end
end

# kalman filter
kalx = zeros(1, n);
kaly = zeros(1, n);
kaly(1) = 1;
ex = zeros(1, n);
ey = zeros(1, n);

X0 = [0; 0; 0; 1; 0; 0];
P0 = zeros(6, 6);

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
Q = G*G'*ea;

for i = 2:n
    R = eye(4).*[varTx; varA; varTy; varA];
    H = [
        1 0 0 0 0 0;
        0 0 1 0 0 0;
        0 0 0 1 0 0;
        0 0 0 0 0 1
    ];
    Z = [xs(i); accx(i); ys(i); accy(i)];

    [X1, P1] = kalmanFilter(X0, P0, F, 0, H, Q, R, 0, Z);
    kalx(i) = X1(1);
    kaly(i) = X1(4);
    ex(i) = P1(1, 1);
    ey(i) = P1(4, 4);
    X0 = X1;
    P0 = P1;
end

ekx = mean(abs(x-kalx));
eky = mean(abs(y-kaly));
etx = mean(abs(x-xs));
ety = mean(abs(y-ys));
printf("Kalman error (x, y) =  (%f, %f)\n", ekx, eky);
printf("Trilateration error (x, y) =  (%f, %f)\n", etx, ety);

figure();
hold on;
grid on;
title("Motion simulation");
xlabel("x [m]");
ylabel("y [m]");
plot(x, y);
plot(kalx, kaly);
plot(xs, ys, '.', "markersize", 10);
plot([0, u], [0, 0], '^', "markersize", 20)
legend("reality", "kalman filter", "trilateration", "sonars", "location", "northwest");

figure();
hold on;
grid on;
title("Motion simulation");
xlabel("t [s]");
ylabel("x [m]");
plot(t, x);
plot(t, kalx);
plot(t, xs, '.', "markersize", 10);
legend("reality", "kalman filter", "trilateration", "location", "northwest");

figure();
hold on;
grid on;
title("Motion simulation");
xlabel("t [s]");
ylabel("y [m]");
plot(t, y);
errorbar(t, kaly, ey);
plot(t, ys, '.', "markersize", 10);
legend("reality", "kalman filter", "trilateration", "location", "northwest");