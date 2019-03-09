module key(position = [ 0, 0 ], dimensions = [ 1, 1 ]) {
    x_offset = position[0];
    y_offset = position[1];

    x_size = dimensions[0];
    y_size = dimensions[1];

    translate([ x_offset, y_offset, 0 ]) {
        difference() {
            cube([ x_size * 0.8, y_size * 0.8, 1 * 0.8 ]);
            translate([ x_size * 0.25, y_size * 0.25, 0.1 ]) {
                cube([ x_size * 0.3, y_size * 0.3, 1 * 0.9 ]);
            }
        }
    }
}

module section(rowlist, tilt_angle, rotation_angle, rotation_center) {
    translate(rot_center) {
        rotate(tilt_angle, [ 1, 0, 0 ]) {
            rotate(rot_angle, [ 0, 0, 1 ]) {
                translate(-rot_center) {
                    for (row = rowlist) {
                        for (key_arr = row) {
                            key(position = key_arr[0],
                                position = key_arr[1]);
                        }
                    }
                }
            }
        }
    }
}

section(
    rowlist =
        [
            [
                [
                    [ 0, 0 ], // row-start-relative position
                    [ 1, 1 ]  // dimensions height/width
                ],            // key 1
                [
                    [ 0, 0 ], // row-start-relative position
                    [ 1, 1 ]  // dimensions height/width
                ],            // key 2
            ],                // row 1
            [
                [
                    [ 0, 0 ], // row-start-relative position
                    [ 1, 1 ]  // dimensions height/width
                ],            // key 1
                [
                    [ 0, 0 ], // row-start-relative position
                    [ 1, 1 ]  // dimensions height/width
                ],            // key 2
            ]                 // row 2
        ],
    tilt_angle = 10,
    rot_angle  = 15,
    rot_center = [ 0, 0, 1 ]);
