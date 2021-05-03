function drawBox(xyz, dim, q)
    x = xyz(1);
    y = xyz(2);
    z = xyz(3);

    p = rot(eye(3), q);
    plot3(x+[0 p(1,1)], y+[0 p(1,2)], z+[0 p(1,3)]);
    plot3(x+[0 p(2,1)], y+[0 p(2,2)], z+[0 p(2,3)]);
    plot3(x+[0 p(3,1)], y+[0 p(3,2)], z+[0 p(3,3)]);

    points = [
        -1 -1 -1;
        1 -1 -1;
        1 1 -1;
        -1 1 -1;
        -1 -1 1;
        1 -1 1;
        1 1 1;
        -1 1 1
    ];
    vertices = rot(points.*dim./2, q) + ones(8,3).*xyz ;
    faces = [
        1 2 6 5;
        4 3 7 8;
        5 6 7 8;
        1 2 3 4;
        2 3 7 6;
        1 4 8 5;
    ];

    patch("Vertices", vertices, "Faces", faces(1:2,:), "FaceColor", "red");
    patch("Vertices", vertices, "Faces", faces(3:4,:), "FaceColor", "yellow");
    patch("Vertices", vertices, "Faces", faces(5:6,:), "FaceColor", "blue");
end