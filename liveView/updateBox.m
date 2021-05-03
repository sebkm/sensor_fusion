function updateBox(X, handles)
    axes(handles(1));
    cla(handles(1));
    drawBox([0 0 0], [1 .5 .1], X(3:6));
end
