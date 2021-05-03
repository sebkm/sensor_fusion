clear all;
close all;
clc;

nav = csvread("converge_nav.csv");
n = size(nav)(1);
avg = zeros(n, 1);

for i=1:n
    avg(i,1) = mean(nav(1:i, 1));
end

figure();
hold on;
grid on;
title("Offset convergence at rest")
xlabel("Number of measures")
ylabel("Average offset x [9.81=1]")
plot([1:n], avg(:, 1))
