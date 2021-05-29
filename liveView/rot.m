function pR = rot(p, q)
    R = q2DCM(q);
    pR = p*R;
end

