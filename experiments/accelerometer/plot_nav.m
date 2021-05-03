function plot_nav(name)
    nav = csvread(strcat(name, ".csv"));
    p = nav(:, 1);
    v = nav(:, 2);
    a = nav(:, 3);
    t = nav(:, 4);
    T0 = t(1);
    t = (t - T0) / 1000;

    figure();
    hold on;
    title(name, 'Interpreter', 'none');
    ylabel("position [m]")
    xlabel("time [s]")
    plot(t, p);

    figure();
    hold on;
    title(name, 'Interpreter', 'none');
    ylabel("velocity [m/s]")
    xlabel("time [s]")
    plot(t, v);

    figure();
    hold on;
    title(name, 'Interpreter', 'none');
    ylabel("acceleration [m/s^2]")
    xlabel("time [s]")
    plot(t, a);

end
