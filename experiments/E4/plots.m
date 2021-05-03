clear all;
close all;
clc;

nav = csvread("nav.csv");
e4 = csvread("e4.csv");

t0 = min([nav(1,2) e4(1,2)]);
tend = min([nav(end,2) e4(end,2)]);
select = @(x) x(x(:,2) <= tend,:);
t = @(x) (x-t0)/1000;
w = 2*pi/(71/10);

nav = select(nav);
e4 = select(e4);

tnav = t(nav(:,2));
te = t(e4(:,2));


figure();
hold on;
title("Centripetal acceleration");
xlabel("t [s]");
ylabel("ac [m/s^2]");
plot(tnav, -nav(:,3));
plot(tnav, ones(length(nav),1)*-mean(nav(:,3)));
legend("ac(t)", "average ac");

figure();
hold on;
set(gca(), "fontsize", 20);
% title("Angular velocity estimation")
xlabel("t [s]");
ylabel('\omega [rad/s]');
plot(te, e4(:,3));
plot(te, ones(length(te),1)*w);
axis([0 50 0.5 1.5]);
legend('{\omega}(t)', 'true \omega');
