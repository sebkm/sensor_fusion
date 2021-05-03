function handles = initBox()
    set(gcf(), "position", [0 0 2/3 1]);
    handles = zeros(1,4);

    handles(1,1) = axes(
        % "outerposition", [0 0 1 1],
        "xtick", [],
        "ytick", [],
        "ztick", []);
    hold(gca(), "on");
    title("Orientation");
    xlabel("Roll");
    ylabel("Pitch");
    zlabel("Yaw");
    view([-45 30]);
    axis([-1 1 -1 1 -1 1]);
    set (gca (), "ydir", "reverse");
    set (gca (), "zdir", "reverse");
end
