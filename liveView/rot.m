function pR = rot(p, q)
    % ox = angles(1);
    % oy = angles(2);
    % oz = angles(3);
    % Rx = [1 0 0; 0 cos(ox) -sin(ox); 0 sin(ox) cos(ox)];
    % Ry = [cos(oy) 0 sin(oy); 0 1 0; -sin(oy) 0 cos(oy)];
    % Rz = [cos(oz) -sin(oz) 0; sin(oz) cos(oz) 0; 0 0 1];
    % R = Rz*Ry*Rx;
    % pR = p*R;
    R = q2DCM(q);
    pR = p*R;
end

