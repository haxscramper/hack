module key(position = [ 0, 0 ], dimensions = [ 1, 1 ]) {
    translate([
        position[0] + 0.1 * dimensions[0],
        position[1] + 0.1 * dimensions[1],
        0
    ]) {
        cube([ dimensions[0] * 0.8, dimensions[1] * 0.8, 1 * 0.8 ]);
    }
}

function addl_idx(list, idx = 0, c = 0) = c < len(list) - 1
                                              ? list[c][idx]
                                                    + addl(
                                                          list,
                                                          idx,
                                                          c + 1)
                                              : list[c][idx];

/// keylist is a nested vector of tuples of four numbers.
/// First two numbesr is a position of key, second pair
/// is a dumensions
module row(keylist, rot_angle = 0, rot_center = [ 0, 0, 1 ]) {
    translate(rot_center) {
        rotate(rot_angle, [ 0, 0, 1 ]) {
            translate(-rot_center) {
                for (key_arr = keylist) {
                    key(position   = [ current_x, 0 ],
                        dimensions = [ key_arr[0], key_arr[1] ]);
                }
            }
        }
    }
}

row(keylist = [ [ 2, 2 ], [ 0.5, 0.5 ] ]);
