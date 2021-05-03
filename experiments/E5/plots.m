clear all;
close all;
clc;

nav = csvread("nav.csv");
s1 = csvread("sonar1.csv");
s2 = csvread("sonar2.csv");
bil = csvread("bilateration.csv");
e5 = csvread("e5.csv");

t0 = min([nav(1,2) s1(1,2) s2(1,2) bil(1,2) e5(1,2)]);
tend = min([nav(end,2) s1(end,2) s2(end,2) bil(end,2) e5(end,2)]);
select = @(x) x(x(:,2) <= tend,:);
t = @(x) (x-t0)/1000;
w = 2*pi/6.7533;

nav = select(nav);
s1 = select(s1);
s2 = select(s2);
bil = select(bil);
e5 = select(e5);

tnav = t(nav(:,2));
tb = t(bil(:,2));
te = t(e5(:,2));


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
title("Sonar's view");
xlabel("t [s]");
ylabel("range [m]");
plot(t(s1(:,2)), s1(:,3), '-o');
plot(t(s2(:,2)), s2(:,3), '-x');
axis([0 40 0.2 1.5]);
legend(sprintf("sonar1 at (%d,%d)", s1(1,4), s1(1,5)),
    sprintf("sonar2 at (%d,%d)", s2(1,4), s2(1,5)));

figure();
subplot(1,2,1);
    plot(tb, bil(:,3:4), '-x');
    axis([0 40 -1 1]);
    title("Bilateration");
    xlabel("t [s]");
    ylabel("position [m]");
    legend("x", "y");
subplot(1,2,2);
    plot(bil(tb <= 40,3), bil(tb <= 40,4), 'x');
    axis([-1 1 -1 1]);
    title("Top view over 40 [s]");
    xlabel("x [m]");
    ylabel("y [m]");

figure();
subplot(1,2,1);
    plot(te, e5(:,3:4), '-x');
    axis([0 40 -1 1]);
    title("Position estimation");
    xlabel("t [s]");
    ylabel("position [m]");
    legend("x", "y");
subplot(1,2,2);
    plot(e5(te <= 40,3), e5(te <= 40,4));
    axis([-1 1 -1 1]);
    title("Top view over 40 [s]");
    xlabel("x [m]");
    ylabel("y [m]");

figure();
hold on;
title("Angular velocity estimation")
xlabel("t [s]");
ylabel('\omega [rad/s]');
plot(te, e5(:,6));
plot(te, ones(length(te),1)*w);
axis([0 50 0.5 1.5]);
legend('{\omega}(t)', 'true \omega');