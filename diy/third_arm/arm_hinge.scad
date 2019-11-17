// clang-format off
use <common.scad>;
// clang-format on

teeth_count  = 16;
teeth_length = 15;
teeth_height = 1.5;

center_cutout_r    = 2 - 0.25;
connector_cutour_r = 2.25;
bottom_thickness   = 5;

teeth_angle = 360 / (teeth_count * 2);


module line_bevel(start, end, radius, rot = 180) {
    length        = norm(end - start);
    edge_distance = radius;

    move_look_at(start, end) rotate([ 0, 0, rot ])
        translate([ -edge_distance, -edge_distance, 0 ]) {
        difference() {
            cube([ radius + 1, radius + 1, length ]);
            translate([ 0, 0, -1 ]) {
                cylinder(length + 2, radius, radius, [ 0, 0, 0 ]);
            }
        }
    }
}

module triangle_beveled(angle, height, length) {
    neg_point = [ length, length * tan(angle / 2) ];
    pos_point = [ length, -length * tan(angle / 2) ];

    difference() {
        linear_extrude(height) {
            polygon([ [ 0, 0 ], neg_point, pos_point ]);
        }
        line_bevel(
            start  = [ 0, 0, height ],
            end    = [ neg_point[0], neg_point[1], height ],
            radius = height);

        line_bevel(
            start  = [ 0, 0, height ],
            end    = [ pos_point[0], pos_point[1], height ],
            radius = height,
            rot    = -90);
    }
}


module flat_teeth() {
    difference () {
        for (idx = [0:2:teeth_count * 2]) {
            angle = (360 / (teeth_count * 2)) * idx;
            rotate([ 0, 0, angle ]) {
                triangle_beveled(
                    angle  = 360 / (teeth_count * 2),
                    height = teeth_height,
                    length = teeth_length);
            }
        }
        cylinder(
        teeth_height, 
        r1 = center_cutout_r * 2, 
        r2 = center_cutout_r * 2);
    }
}


module cube_cylinder_cutout(height, radius) {
    difference() {
        translate([ -radius, -radius, 0 ]) {
            cube([ 2 * radius, 2 * radius, height ]);
        }

        translate([ 0, 0, -1 ]) {
            cylinder(height + 2, r1 = radius, r2 = radius);
        }
    }
}


module cutting_cylinder(radius, height) {
    translate([ 0, 0, -1 ]) cylinder(
    height + 2, 
    r1 = radius, 
    r2 = radius,
    $fn = radius * 8
    );
}


module teeth_gear_half() {
    difference() {
        union() {
            flat_teeth();
            translate([ 0, 0, -bottom_thickness ]) {
                cylinder(
                    bottom_thickness,
                    r1 = teeth_length,
                    r2 = teeth_length);
            }
        }
        translate([ 0, 0, -1 ]) //
            cube_cylinder_cutout(2 * bottom_thickness, teeth_length);

        translate([ 0, 0, -bottom_thickness ]) //
            cutting_cylinder(center_cutout_r, 2 * bottom_thickness);
    }
}


module main(even_teeth = true) {
    l             = teeth_length;
    holder_depth  = l * 0.6;
    holder_height = (l - center_cutout_r) * 0.8;
    difference() {
        union() {
            translate([ -l, -l, -holder_depth ]) //
                cube([ 2 * l, holder_height, holder_depth ]);

            if (even_teeth) {
                teeth_gear_half();
            } else {
                rotate([ 0, 0, teeth_angle ]) teeth_gear_half();
            }
        }


        translate([ -l, -holder_height, -holder_depth / 2 ]) //
            rotate([ 0, 90, 0 ])                             //
            cutting_cylinder(connector_cutour_r, 2 * l);
    }
}

main(false);
