close all;
clear all;
clc;

t = @(x) (x-x(1))/1000;

e4 = csvread("../E4/e4.csv");
e6 = csvread("../E6/e6.csv");
e7 = csvread("../E7/e7.csv");
e8 = csvread("../E8/e8.csv");
e9 = csvread("../E9/e9.csv");

figure();

subplot(2,1,1);
hold on;
set(gca(), "fontsize", 20);
ylabel('\omega [rad/s]');
axis([0 50 0.8 1]);
plot(t(e4(:,2)), e4(:,3));
plot(t(e7(:,2)), e7(:,6));
plot(t(e9(:,2)), e9(:,6));
plot([0 50], [1 1]*2*pi/7.1);
legend("a_c only", "+ gyro", "+ mag", 'true \omega');

subplot(2,1,2);
hold on;
set(gca(), "fontsize", 20);
ylabel('\omega [rad/s]');
xlabel("time [s]");
axis([0 50 0.8 1]);
plot(t(e6(:,2)), e6(:,6));
plot(t(e8(:,2)), e8(:,6));
plot([0 50], [1 1]*2*pi/6.85);
legend("+ sonars", "+ radius estimation", 'true \omega');
