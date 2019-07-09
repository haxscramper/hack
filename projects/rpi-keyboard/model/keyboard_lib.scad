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

/// keylist is a nested vector of tuples of four numbers.
/// First two numbesr is a position of key, second pair
/// is a dumensions
module row(keylist, rot_angle = 0, rot_center = [ 0, 0, 1 ]) {
    translate(rot_center) {
        rotate(rot_angle, [ 0, 0, 1 ]) {
            translate(-rot_center) {
                for (key_arr = keylist) {
                    key([ key_arr[0], key_arr[1] ],
                        [ key_arr[2], key_arr[3] ]);
                }
            }
        }
    }
}
