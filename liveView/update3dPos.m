function update3dPos(X, handles)
    global data;

    l = size(data)(1);
    data = [data(max(1,l-9):l,:); X(3:5)];
    for i=1:3
        axes(handles(i));
        cla(handles(i));
        plot(data(:,i));
    end
end
