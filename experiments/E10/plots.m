close all;
clear all;
clc;


e10 = csvread("e10.csv");
t = (e10(:,2)-e10(1,2))/1000;

figure();
hold on;
set(gca(), "fontsize", 20);
plot(t, e10(:,3));
plot(t, e10(:,4));
plot(t, e10(:,5));
legend("x", "y", "z");
axis([0 t(end) -0.6 0.6]);
ylabel("position [m]");
xlabel("time [s]");