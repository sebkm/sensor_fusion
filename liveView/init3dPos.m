function handles = init3dPos()
    set(gcf(), "position", [2/3 0 1/3 1]);
    handles = zeros(1,3);

    handles(1,1) = axes("outerposition", [0 2/3 1 1/3]);
    hold(gca(), "on");
    title("Position");
    xlabel("Sample number");
    ylabel("x [m]");
    axis([1 10 -1 1]);

    handles(1,2) = axes("outerposition", [0 1/3 1 1/3]);
    hold(gca(), "on");
    xlabel("Sample number");
    ylabel("y [m]");
    axis([1 10 -1 1]);

    handles(1,3) = axes("outerposition", [0 0 1 1/3]);
    hold(gca(), "on");
    xlabel("Sample number");
    ylabel("z [m]");
    axis([1 10 -1 1]);
end
