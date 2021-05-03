function updateTrain(Data, handles)
    X = zeros(1,7);
    X(1:length(Data)) = Data; % E5->E7 do not provide radius

    axes(handles(1));
    cla(handles(1));
    plot(X(3), X(4), '.', "markersize", 20);

    axes(handles(2));
    cla(handles(2));
    plot([0 1], [X(6) X(6)]);

    axes(handles(3));
    cla(handles(3));
    plot([0 1], [X(7) X(7)]);
end