close all;
clear all;
pkg load statistics;

dt = 0.1;
t = [0:dt:10];
n = length(t);
A = 0.1;
Shift = 1/5*pi;
a = A*sin(t*Shift);
x = -A/Shift^2*sin(t*Shift) + A/Shift*t;
y = ones(1, n);

varS = 0.0625;
u = 2;
r1 = sqrt(x.^2 + y.^2) + normrnd(0, sqrt(varS), 1, n);
r2 = sqrt((u-x).^2 + y.^2) + normrnd(0, sqrt(varS), 1, n);

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

printf("varTx = %f\n", var(x-xs));
printf("varTy = %f\n", var(y-ys));

figure();

subplot(2, 2, 1);
hold on;
ylabel("x [m]");
plot(t, x);
plot(t, xs);
legend("reality", "bilateration", "location", "northwest");

subplot(2, 2, 3);
hold on;
ylabel("y [m]");
xlabel("t [s]");
plot(t, y);
plot(t, ys);

subplot(2, 2, [2 4]);
hold on;
title("Top view");
xlabel("y [m]");
ylabel("x [m]");
plot(y, x);
plot(ys, xs, '.', "markersize", 10);
plot([0,0], [0,u], '*', "markersize", 20);
legend("reality", "bilateration", "sonars");

