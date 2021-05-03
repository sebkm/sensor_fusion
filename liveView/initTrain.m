function handles = initTrain()
    set(gcf(), "position", [0 0 1 1]);
    handles = zeros(1,3);

    handles(1,1) = axes("outerposition", [0 .25 .5 .5]);
    hold(gca(), "on");
    title("Position");
    xlabel("x [m]");
    ylabel("y [m]");
    axis([-1 1 -1 1]);

    handles(1,2) = axes(
        "outerposition", [.5 .5 .5 .5],
        "xtick", []
    );
    hold(gca(), "on");
    title("Speed");
    ylabel("[rad/s]");
    axis([0 1 0 1.5]);

    handles(1,3) = axes(
        "outerposition", [.5 0 .5 .5],
        "xtick", []
    );
    hold(gca(), "on");
    title("Radius");
    ylabel("[m]");
    axis([0 1 0 1]);
end