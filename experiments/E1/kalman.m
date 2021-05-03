close all;
clear all;


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


sonar = csvread("SONAR.CSV")(61:end, :);
nav = csvread("NAV.CSV")(38:203, :);
n = size(nav)(1);

% time shift
nav(:, 2) -= nav(1, 2);
sonar(:, 2) -= sonar(1, 2);

sonar(:, 1) *= 0.01; % [cm] -> [m]
nav(:, 1) *= -1; % direction of the sensor
t = nav(:, 2);

p = zeros(1, n);
sim_p = zeros(1, n);
sim_v = zeros(1, n);
sim_a = zeros(1, n);

varA = 0.2;
varS = 0.01;
Sigma_e = 0.1;

X0 = [0; 0; 0];
P0 = zeros(3, 3); % assuming I know exactly the initial state
t0 = 0;
si = 1;
for i = 1:n
    t1 = t(i);
    update = sonar(si, 2) <= t1; 
    DT = t1 - t0;
    p(i) = sonar(si, 1);

    F = [1 DT 0.5*DT^2; 0 1 DT; 0 0 1];
    G = F(:, 3);
    Q = G*Sigma_e^2*G';
    
    if update
        R = [varS 0; 0 varA];
        H = [1 0 0; 0 0 1];
        Z = [sonar(si, 1); nav(i, 1)];
    else
        R = varA;
        H = [0 0 1];
        Z = nav(i, 1);
    end

    [X1, P1] = kalmanFilter(X0, P0, F, 0, H, Q, R, 0, Z, true);
    sim_p(i) = X1(1,1);
    sim_v(i) = X1(2,1);
    sim_a(i) = X1(3,1);
    X0 = X1;
    P0 = P1;

    t0 = t1;
    si += update; % +1 if true +0 otherwise
end

% inertial only
x0 = 0;
v0 = 0;
t0 = 0;
acc_p = zeros(1, n);
acc_v = zeros(1, n);
for i = 1:n
    t1 = nav(i, 2);
    DT = t1-t0;
    acc_v(i) = v0 + nav(i, 1) * DT;
    acc_p(i) = x0 + v0 * DT;
    x0 = acc_p(i);
    v0 = acc_v(i);
    t0 = t1;
end


figure();
hold on;
grid on;
ylabel("Position [m]");
xlabel("Time [s]");
plot(t, sim_p);
plot(t, p);
plot(t, acc_p);
legend("kalman filter", "sonar", "inertial", "location", "northwest");

figure();
hold on;
grid on;
ylabel("Velocity [m/s]");
xlabel("Time [s]");
plot(t, sim_v);
plot(t, acc_v);
legend("kalman filter", "inertial", "location", "northwest");

figure();
hold on;
grid on;
ylabel("Acceleration [m/s^2]");
xlabel("Time [s]");
plot(t, sim_a);
plot(t, nav(:, 1), "marker", "*", "markersize", 5);
legend("kalman filter", "measure", "location", "northwest");
