function handles = initSonar()
    handles = zeros(1,1);
    handles(1,1) = axes(
        "outerposition", [0 0 1 1],
        "xtick", []
    );
    hold(gca(), "on");
    title("Range");
    ylabel("[m]");
    axis([0 1 0 2]);
end