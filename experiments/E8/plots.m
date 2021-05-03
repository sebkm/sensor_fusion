clear all;
close all;
clc;

nav = csvread("nav.csv");
s1 = csvread("sonar1.csv");
s2 = csvread("sonar2.csv");
e8 = csvread("e8.csv");

t0 = min([nav(1,2) s1(1,2) s2(1,2) e8(1,2)]);
tend = min([nav(end,2) s1(end,2) s2(end,2) e8(end,2)]);
select = @(x) x(x(:,2) <= tend,:);
t = @(x) (x-t0)/1000;
w = 2*pi/6.84;

nav = select(nav);
s1 = select(s1);
s2 = select(s2);
e8 = select(e8);

tnav = t(nav(:,2));
te = t(e8(:,2));


figure();
hold on;
title("centripetal acceleration");
xlabel("t [s]");
ylabel("ac [m/s^2]");
plot(tnav, -nav(:,3));
plot(tnav, ones(length(nav),1)*-mean(nav(:,3)));
legend("ac(t)", "average ac");

figure();
hold on;
title("Angular velocity");
xlabel("t [s]");
ylabel('\omega [rad/s]');
plot(tnav, nav(:,4));
plot(tnav, ones(length(nav),1)*mean(nav(:,4)));
legend('{\omega}(t)', 'average \omega');

figure();
hold on;
title("Sonar's view");
xlabel("t [s]");
ylabel("range [m]");
plot(t(s1(:,2)), s1(:,3), '-o');
plot(t(s2(:,2)), s2(:,3), '-x');
axis([0 40 0 1.5]);
legend(sprintf("sonar1 at (%d,%d)", s1(1,4), s1(1,5)),
    sprintf("sonar2 at (%d,%d)", s2(1,4), s2(1,5)));

figure();
% subplot(1,2,1);
%     plot(te, e8(:,3:4), '-x');
%     axis([0 40 -1 1]);
%     title("Position estimation");
%     xlabel("t [s]");
%     ylabel("position [m]");
%     legend("x", "y");
% subplot(1,2,2);
%     plot(e8(te <= 40,3), e8(te <= 40,4));
%     axis([-1 1 -1 1]);
%     title("Top view over 40 [s]");
%     xlabel("x [m]");
%     ylabel("y [m]");
plot(te, e8(:,3:4), '-x');
axis([0 20 -1 1]);
title("Position estimation");
xlabel("t [s]");
ylabel("position [m]");
legend("x", "y");

figure();
hold on;
set(gca(), "fontsize", 20);
% title("Angular velocity estimation")
xlabel("t [s]");
ylabel('\omega [rad/s]');
plot(te, e8(:,6));
plot(te, ones(length(te),1)*w);
axis([0 50 0.5 1.5]);
legend('{\omega}(t)', 'true \omega');

figure();
hold on;
set(gca(), "fontsize", 20);
% title("Radius estimation")
xlabel("t [s]");
ylabel("r [m]");
plot(te, e8(:,7));
plot([0 50], 0.57*[1 1]);
plot([0 50], 0.615*[1 1]);
axis([0 50 0 1]);
legend("r(t)", "r_{in}", "r_{out}");
