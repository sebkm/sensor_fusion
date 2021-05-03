close all;
clear all;
clc;

global running = false;
global replay = false;
global view = 1;
global fileName = "data.csv";
global filePath = -1;

global initView;
global updateView;
global views;
global data = [];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function stop()
    global running data;
    running = false;
    data = [];
end

function setMode(bool)
    global replay
    replay = bool;
end

function setView(idx)
    global view;
    view = idx;
end

function setFile(handle)
    global fileName filePath;
    [file, folder] = uigetfile('*.csv','Select the source file');
    if ~file
        return;
    end
    set(handle, "string", file);
    fileName = file;
    filePath = folder;
end

function file = openFile(measureName)
    global fileName filePath;
    if filePath == -1
        fprintf("No source file selected\n");
        file = -1;
        return;
    end
    file = fopen([filePath fileName]);
    if file == -1
        fprintf("Could not open %s\n", fileName);
    end
end

function liveHandle = initCallbackUi()
    backHandle = uicontrol(
        "string", "<--",
        "units", "normalized",
        "position", [0 1 0 0],
        "callback", @stop
    );
    set(backHandle, "units", "centimeters");
    pos = get(backHandle, "position");
    set(backHandle, "position", [0 pos(2)-1 1 1]);

    liveHandle = uicontrol(
        "units", "normalized",
        "position", [1 1 0 0]
    );
    set(liveHandle, "units", "centimeters");
    pos = get(liveHandle, "position");
    set(liveHandle, "position", [pos(1)-1 pos(2)-1 1 1]);
    set(liveHandle, "backgroundcolor", "red");
end

function busyWait(dt)
    global running;
    c = dt/0.01;
    while(running && c > 0)
        pause(.01);
        --c;
    end
end

function loop()
    global running replay view initView updateView;

    idx = view;
    t0 = 0;
    running = true;

    clf();
    handles = initView{1,idx}();
    liveHandle = initCallbackUi();
    refresh();

    file = openFile();
    if file == -1
        running = false;
    elseif !replay
        fseek(file, 0, SEEK_END());
    end

    offlineCount = 0;
    while (running)
        line = fgetl(file);
        if !replay && line == -1
            fseek(file, ftell(file));
            if ++offlineCount == 50
                set(liveHandle, "backgroundcolor", "red");
            end
            pause(.01);
        elseif !replay
            X = textscan(line, "%f", "Delimiter", ","){1,1}';
            offlineCount = 0;
            set(liveHandle, "backgroundcolor", "green");
            updateView{1,idx}(X, handles);
            drawnow();
        elseif replay && line == -1
            stop();
        else
            X = textscan(line, "%f", "Delimiter", ","){1,1}';
            T = X(2);
            dt = (t0!=0)*(T-t0);
            t0 = T;
            busyWait(dt/1000);
            updateView{1,idx}(X, handles);
            drawnow();
        end
    end

    if file != -1
        fclose(file);
    end

    if isfigure(1)
        clf();
        initMain();
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initView = {@initTrain, @initSonar, @initBox, @init3dPos};
updateView = {@updateTrain, @updateSonar, @updateBox, @update3dPos};
views = {"train tracking", "sonar range", "3d orientation", "3d position"};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Script
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

graphics_toolkit("qt");

fig = figure(
    "name", "Sensor fusion live view",
    "numbertitle", "off",
    "menubar", "none",
    "resize", "off",
    "units", "normalized",
    "position", [.3 .3 .3 .3],
    "deletefcn", @stop
);

running = false;
initMain();
waitfor(fig);
