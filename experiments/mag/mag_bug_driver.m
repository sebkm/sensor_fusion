clear all;
close all;
clc;

mag = csvread("mag_bug_driver.csv");
t = (mag(:,2) - mag(1,2))*1e-3;

figure();
hold on;
plot(t, mag(:,3));
plot(t, mag(:,4));
plot(t, ones(length(t),1)*4);
plot(t, ones(length(t),1)*-4);
axis([0 8 -10 4]);
xlabel("Time [s]");
ylabel("Magnetic field [gauss]");
title("Magnetometer output");
legend("x axis", "y axis", "Max", "Min");

figure();
hold on;
plot(t, atan2(mag(:,4), mag(:,3))*180/pi);
xlabel("Time [s]");
ylabel("Angle [degree]");
title("Heading from magnetometer");
