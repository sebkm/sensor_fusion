close all;
clear all;
clc;

function R = q2DCM(q)
    q0 = q(1);
    q1 = q(2);
    q2 = q(3);
    q3 = q(4);
     
    # First row of the rotation matrix
    r00 = 2 * (q0 * q0 + q1 * q1) - 1;
    r01 = 2 * (q1 * q2 - q0 * q3);
    r02 = 2 * (q1 * q3 + q0 * q2);
     
    # Second row of the rotation matrix
    r10 = 2 * (q1 * q2 + q0 * q3);
    r11 = 2 * (q0 * q0 + q2 * q2) - 1;
    r12 = 2 * (q2 * q3 - q0 * q1);
     
    # Third row of the rotation matrix
    r20 = 2 * (q1 * q3 - q0 * q2);
    r21 = 2 * (q2 * q3 + q0 * q1);
    r22 = 2 * (q0 * q0 + q3 * q3) - 1;

    R = [
        r00 r01 r02;
        r10 r11 r12;
        r20 r21 r22
    ];
end

function pR = rot(p, q)
    R = q2DCM(q);
    pR = p*R;
end

function plotFile1(fileName)
    data = csvread(fileName);

    figure();

    subplot(3, 1, 1);
    hold on;
    set(gca(), "fontsize", 20);
    % title("Position of point (1,0,0) from the body frame in the reference frame");
    ylabel("x [m]");

    subplot(3, 1, 2);
    hold on;
    set(gca(), "fontsize", 20);
    ylabel("y [m]");

    subplot(3, 1, 3);
    hold on;
    set(gca(), "fontsize", 20);
    xlabel("time [s]");
    ylabel("z [m]");
    

    n = size(data)(1);
    t = (data(:,2) - data(1,2)) / 1000;
    for i = 1:1:n
        q = data(i, 3:6);
        p = rot([1 0 0], q);

        subplot(3, 1, 1);
        plot([t(i) t(i)], [0 p(1,1)], "color", "black");
        plot(t(i), p(1,1), '.', "markersize", 10, "color", "black");

        subplot(3, 1, 2);
        plot([t(i) t(i)], [0 p(1,2)], "color", "black");
        plot(t(i), p(1,2), '.', "markersize", 10, "color", "black");

        subplot(3, 1, 3);
        plot([t(i) t(i)], [0 p(1,3)], "color", "black");
        plot(t(i), p(1,3), '.', "markersize", 10, "color", "black");
    end

    subplot(3, 1, 1);
    axis([0 t(n)]);
    subplot(3, 1, 2);
    axis([0 t(n)]);
    subplot(3, 1, 3);
    axis([0 t(n)]);

end

function plotFile2(fileName)
    data = csvread(fileName);

    n = size(data)(1); 
    points = zeros(n, 3);

    t = (data(:,2) - data(1,2)) / 1000;
    for i = 1:1:n
        q = data(i, 3:6);
        p = rot([1 0 0], q);
        points(i,:) = p;
    end

    figure();
    hold on;
    grid on;
    set(gca(), "fontsize", 20);
    % title(
    %     "Position of point (1,0,0) from the body frame in the reference frame");
    xlabel("x [m]");
    ylabel("y [m]");
    zlabel("z [m]");
    view([45, 30])
    colormap("jet");
    colors = jet(n);
    for i=2:n
        line(
            [points(i-1,1), points(i,1)],
            [points(i-1,2), points(i,2)],
            [points(i-1,3), points(i,3)],
            "color", colors(i,:));
    end
    scatter3(points(:,1), points(:,2), points(:,3), 10, t, "filled");
    h = colorbar();
    set(h, "fontsize", 20);
    ylabel(h, "time [s]");
end

function shaking()
    files = {"acc_mag", "gyro", "acc_mag_gyro"};

    figure();
    hold on;
    set(gca(), "fontsize", 20);
    % title("Deviation from previous position");
    xlabel("time [s]");
    ylabel("%");
    axis([0 8 0 100]);

    for file = files
        data = csvread([file{1,1} "_shaking.csv"]);
        n = size(data)(1);
        t = (data(:,2) - data(1,2)) / 1000;

        points = zeros(n, 3);
        for i = 1:1:n
            q = data(i, 3:6);
            points(i,:) = rot([1 0 0], q);
        end

        delta = points(2:end,:) - points(1:end-1,:);
        deviation = sqrt(sum(delta.^2, 2)) / 2 * 100;
        plot(t(2:end), deviation, '-*');
    end

    legend("acc+mag", "gyro", "kalman");
end

benchmark = "slow";
plotFile1(["acc_mag_" benchmark ".csv"]);
plotFile2(["gyro_" benchmark ".csv"]);
plotFile1(["acc_mag_gyro_" benchmark ".csv"]);

benchmark = "fast";
plotFile1(["acc_mag_" benchmark ".csv"]);
plotFile2(["gyro_" benchmark ".csv"]);
plotFile1(["acc_mag_gyro_" benchmark ".csv"]);

shaking();