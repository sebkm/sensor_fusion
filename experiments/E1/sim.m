close all;
clear all;

pkg load statistics;

function [X1, P1] = kalmanFilter(X0, P0, F, B, H, Q, R, u, Z, update)
    % Prediction
    Xp = F*X0 + B*u;
    Pp = F*P0*F' + Q;

    % Update
    if update
        K = Pp*H' / (R + H*Pp*H');
        X1 = Xp + K*(Z - H*Xp);
        P1 = Pp - K*H*Pp;
    else
        X1 = Xp;
        P1 = Pp;
    end
end

# simulation

varA = 0.2;
varS = 0.01;
ea = 0.01;
DT = 0.1;

t = [0:0.1*DT:20];
A = 0.5;
Shift = 0.1*pi;
a = A*sin(t*Shift);
v = A/Shift - A*cos(t*Shift)/Shift;
p = -A/Shift^2*sin(t*Shift) + A/Shift*t;

idx = [1:10:length(t)];
T = t(idx);
n = length(T);
accelerometer = a(idx) + normrnd(0, sqrt(varA), 1, n);
sonar = p(idx) + normrnd(0, sqrt(varS), 1, n);

for i=1:(n/4)
    idx = 4*(i-1)+2;
    m = sonar(idx-1);
    sonar(idx:(idx+2)) = m;
end

sim_p = zeros(1, n);
sim_v = zeros(1, n);
sim_a = zeros(1, n);

dt = DT;
F = [1 dt 0.5*dt^2; 0 1 dt; 0 0 1];

X0 = [0; 0; 0];
P0 = zeros(3, 3);
G = F(:, 3);
Q = G*G'*ea;        

s0 = sonar(1);
for i = 2:n
    s1 = sonar(i);
    update = s0 != s1;
    s0 = sonar(i);

    if update
        R = [varS 0; 0 varA];
        H = [1 0 0; 0 0 1];
        Z = [sonar(i); accelerometer(i)];
    else
        R = varA;
        H = [0 0 1];
        Z = accelerometer(i);
    end

    [X1, P1] = kalmanFilter(X0, P0, F, 0, H, Q, R, 0, Z, true);
    sim_p(i) = X1(1,1);
    sim_v(i) = X1(2,1);
    sim_a(i) = X1(3,1);
    X0 = X1;
    P0 = P1;
end

# accelerometer only
x0 = 0;
v0 = 0;
acc_p = zeros(1, n);
acc_v = zeros(1, n);
for i = 2:n
    acc_v(i) = v0 + accelerometer(i) * DT;
    acc_p(i) = x0 + v0 * DT;
    x0 = acc_p(i);
    v0 = acc_v(i);
end


figure();
hold on;
grid on;
title("Motion simulation");
ylabel("Position [m]");
xlabel("Time [s]");
plot(t, p);
plot(T, sim_p);
plot(T, acc_p);
plot(T, sonar);
legend("reality", "kalman filter", "inertial", "sonar", "location", "northwest");

figure();
hold on;
grid on;
title("Motion simulation");
ylabel("Velocity [m/s]");
xlabel("Time [s]");
plot(t, v);
plot(T, sim_v);
plot(T, acc_v);
legend("reality", "kalman filter", "inertial", "location", "northwest");

figure();
hold on;
grid on;
title("Motion simulation");
ylabel("Acceleration [m/s²]");
xlabel("Time [s]");
plot(t, a);
plot(T, sim_a);
legend("reality", "kalman filter", "location", "northwest");
