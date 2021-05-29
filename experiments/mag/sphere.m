clear all;
close all;
clc;

nav = csvread("sphere.csv");
mag = nav(:,9:11);
cal = csvread("cal.csv");
bias = 0.5*(cal(1,:) + cal(2,:));
corrected = mag - bias;

figure();
hold on;
plot3(mag(:,1), mag(:,2), mag(:,3));
plot3(corrected(:,1), corrected(:,2), corrected(:,3));
legend("raw", "corrected");
title("Magnetometer output");
xlabel("x [gauss]");
ylabel("y [gauss]");
zlabel("z [gauss]");
