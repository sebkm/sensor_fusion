function initMain()
    global replay view views fileName;

    set(gcf(), "position", [.3 .3 .3 .5]);
    options = uipanel(
        "title", "Options",
        "titleposition", "centertop",
        "fontsize", 15,
        "position", [0 .3 1 .7]
    );

    viewSelection = uipanel(options,
        "title", "View selection",
        "fontsize", 10,
        "position", [0 .6 1 .3]
    );
    uicontrol(viewSelection,
        "style", "listbox",
        "tooltipstring", "Select the view",
        "string", views,
        "value", view,
        "units", "normalized",
        "position", [.1 0 .8 .85],
        "callback", (@(h,~) setView(get(h, "value")))
    );

    sourceSelection = uipanel(options,
        "title", "Source selection",
        "fontsize", 10,
        "position", [0 .35 1 .2]
    );
    uicontrol(sourceSelection,
        "tooltipstring", "Click to select the source file",
        "string", fileName,
        "units", "normalized",
        "position", [.1 0 .8 .8],
        "callback", @setFile
    );

    modeSelection = uipanel(options,
        "title", "Mode selection",
        "fontsize", 10,
        "position", [0 .1 1 .2]
    );
    uicontrol(modeSelection,
        "style", "togglebutton",
        "tooltipstring", "Select the mode",
        "string", "Replay",
        "units", "normalized",
        "position", [.1 0 .8 .8],
        "value", replay,
        "callback", (@(h,~) setMode(get(h, "value")))
    );

    uicontrol(
        "string", "Start",
        "units", "normalized",
        "position", [.4 .1 .2 .1],
        "callback", @loop
    );
end