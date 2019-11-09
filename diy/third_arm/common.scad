/// Rotate child nodes towards endpoint as if they were placed at the
/// `start` position
module look_at(
     start, ///< Starting position
     end ///< Aim point
     ) {
    rotate([
        -acos((end[2] - start[2]) / norm(start - end)),
        0,
        -atan2(end[0] - start[0], end[1] - start[1])
    ]) {
        children();
    }
}

/// Translate child nodes to start point and rotate them towards end
/// point
module move_look_at(start, end) {
    translate(start) look_at(start, end) {
        children();
    }
}


/// Rotate child nodes so that the vector [0,0,1] (normal to the XY
/// plane) would would point towards `end` point.
module look_at0(end) {
    look_at([ 0, 0, 0 ], [ end[0], end[1], end[2] ]) {
        children();
    }
}


module move_look_at_test() {
    for (i = [0:10:360]) {
        r = 80;
        x = r * cos(i);
        y = r * sin(i);
        move_look_at(start = [ x, y, 0 ], end = [ 0, 0, 0 ]) {
            cylinder(4, 4);
        }
    }

    look_at0([ 1, 2, 10 ]) {
         cylinder(10, 10);
    }
}
