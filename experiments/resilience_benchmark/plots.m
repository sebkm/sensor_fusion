clear all;
close all;
clc;

function ret = t(timestamps)
    ret = (timestamps - timestamps(1)) / 1000;
end

function hera_measure()
    figure();

    fault25 = csvread("hera_measure_4_1.csv");
    fault50 = csvread("hera_measure_4_2.csv");
    fault75 = csvread("hera_measure_4_3.csv");
    fault100 = csvread("hera_measure_4_4.csv");

    subplot(2,1,1);
    hold on;
    grid on;
    set(gca(), "fontsize", 20);

    plot(t(fault25(:,4)), fault25(:,3), '-*');
    plot(t(fault50(:,2)), fault50(:,3), '-*');
    plot(t(fault75(:,2)), fault75(:,3), '-*');
    plot(t(fault100(:,2)), fault100(:,3), '-*');
    plot([5 5], [0 20], '-.');

    legend("25%", "50%", "75%", "100%", "fault injection",
        "location", "southeast");
    xlabel("time [s]");
    ylabel("count");

    fault25 = csvread("hera_sync_measure_1.csv");
    fault50 = csvread("hera_sync_measure_2.csv");
    fault75 = csvread("hera_sync_measure_3.csv");
    fault100 = csvread("hera_sync_measure_4.csv");

    subplot(2,1,2);
    hold on;
    grid on;
    set(gca(), "fontsize", 20);

    plot(t(fault25(:,2)), fault25(:,3)/1000, '-*');
    plot(t(fault50(:,2)), fault50(:,3)/1000, '-*');
    plot(t(fault75(:,2)), fault75(:,3)/1000, '-*');
    plot(t(fault100(:,2)), fault100(:,3)/1000, '-*');
    plot([4 4], [0 1], '-.');

    xlabel("time [s]");
    ylabel('{\Delta}t [s]');
end

function hera_com_data()
    data = csvread("hera_data_4_6.csv");
    com = csvread("hera_com_4_6.csv");
    figure();

    subplot(2, 1, 1);
    hold on;
    grid on;
    set(gca(), "fontsize", 20);
    axis([0 20 0 20]);
    xlabel("time [s]");
    ylabel("count");
    plot(t(data(:,2)), data(:,3), '-*', "color", "black");
    for i = 1:6
        x = 4+i;
        p = plot([x x], [0 20], '-.', "color", "red");
    end
    legend(p, "fault injection", "location", "southeast");

    Colors = jet(3);
    subplot(2, 1, 2);
    hold on;
    grid on;
    set(gca(), "fontsize", 20);
    axis([0 5 0 20]);
    xlabel("time [s]");
    ylabel("count");
    plot(com(:,4)/1000, com(:,3), '-*', "color", "black");
    for i = 1:6
        x = 1.4+i*0.1;
        p = plot([x x], [0 20], '-.', "color", "red");
    end
    legend(p, "fault injection", "location", "southeast");
end

function hera_sync_sub()
    figure();

    sync1 = csvread("hera_sync_1.csv");
    sync2 = csvread("hera_sync_2.csv");
    sync3 = csvread("hera_sync_3.csv");
    sync4 = csvread("hera_sync_4.csv");
    sync5 = csvread("hera_sync_5.csv");

    tof = min([sync1(1,2) sync2(1,2) sync3(1,2) sync4(1,2) sync5(1,2)]);
    t = @(x) (x-tof)/1000;

    subplot(2, 1, 1);
    hold on;
    grid on;
    set(gca(), "fontsize", 20);
    xlabel("time [s]");
    ylabel('{\Delta}t [s]');
    plot(t(sync1(:,2)), sync1(:,3)/1000, '-*', "color", "black");
    plot(t(sync2(:,2)), sync2(:,3)/1000, '-*', "color", "black");
    plot(t(sync3(:,2)), sync3(:,3)/1000, '-*', "color", "black");
    plot(t(sync4(:,2)), sync4(:,3)/1000, '-*', "color", "black");
    plot(t(sync5(:,2)), sync5(:,3)/1000, '-*', "color", "black");
    p = plot([5 5], [0 1.4], '-.', "color", "red");
    legend(p, "fault injection", "location", "northwest");


    sub1 = csvread("hera_sub_1.csv");
    sub2 = csvread("hera_sub_2.csv");
    sub3 = csvread("hera_sub_3.csv");
    sub4 = csvread("hera_sub_4.csv");
    sub5 = csvread("hera_sub_5.csv");

    tof = min([sub1(1,2) sub2(1,2) sub3(1,2) sub4(1,2) sub5(1,2)]);
    t = @(x) (x-tof)/1000;

    subplot(2, 1, 2);
    hold on;
    grid on;
    set(gca(), "fontsize", 20);
    xlabel("time [s]");
    ylabel('{\Delta}t [s]');
    plot(t(sub1(:,2)), sub1(:,3)/1000, '-*', "color", "black");
    plot(t(sub2(:,2)), sub2(:,3)/1000, '-*', "color", "black");
    plot(t(sub3(:,2)), sub3(:,3)/1000, '-*', "color", "black");
    plot(t(sub4(:,2)), sub4(:,3)/1000, '-*', "color", "black");
    plot(t(sub5(:,2)), sub5(:,3)/1000, '-*', "color", "black");
    p = plot([5 5], [0 3], '-.', "color", "red");
    legend(p, "fault injection", "location", "northwest");
end

function hera_synchronization()
    s1 = csvread("hera_synchronization_2_not_app.csv");
    s2 = csvread("hera_synchronization_1_app.csv");

    figure();
    hold on;
    grid on;
    set(gca(), "fontsize", 20);
    axis([0 120 0 14]);

    tv = t(s1(:,2));
    plot(tv, s1(:,3)/1000, '-*');
    plot(tv(end)+t(s2(:,2)), s2(:,3)/1000, '-*');
    plot(tv(end)*[1 1], [0 14], '-.');

    xlabel("time [s]");
    ylabel('{\Delta}t [s]');
    legend("2 power-off, 0 failover", "1 power-off, 1 failover",
        "restart with remaining nodes",
        "location", "northwest");

end

hera_measure();
hera_com_data();
hera_sync_sub();
hera_synchronization();
