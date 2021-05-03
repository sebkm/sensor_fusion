close all;
clear all;

nav = csvread("NAV.CSV")(17:255, :);
ax = nav(:, 1);
ay = nav(:, 2);
ax *= -1;
ay *= -1;
t = nav(:, 3);
t -= t(1);

T = 5/4;
A = 4;
X0 = 0.4;
Y0 = 0.3+0.2;

x = -ax*(0.5*T/pi)^2 + X0;
y = -ay*(0.5*T/pi)^2 + Y0;

figure()
hold on;
grid on;
title("Circular Motion");
plot(x, y);
xlabel("x");
ylabel("y");




