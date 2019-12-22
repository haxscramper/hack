module key_core(height, width, length) {
    sizeX = length;
    sizeY = width;
    sizeZ = height + 0.2;

    centerX = sizeX / 2;
    centerY = sizeY / 2;
    centerZ = sizeZ / 2;

    difference() {
        color ("Orange") cube([ sizeX, sizeY, sizeZ ]);

        translate([ centerX, centerY, centerZ ]) {
            color("Red") cube(
                [
                    sizeX * 0.7, //
                    sizeY * 0.7, //
                    sizeZ + 0.1
                ],
                center = true);
        }
    }
}

module key_boundary(height, width, length, length) {
    shift = 0.1;
    translate([ 0, 0, -shift ]) {
        cube([ length, width, height + 2 * shift ]);
    }
}

module row_boundary(width, height, length) {
    shift = 0.1;
    translate([ 0, 0, -shift ]) {
        cube([ length, width, height + 2 * shift ]);
    }
}
