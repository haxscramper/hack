module key_core(height, width, length) {
    sizeX = length;
    sizeY = width;
    sizeZ = height + 0.2;

    centerX = sizeX / 2;
    centerY = sizeY / 2;
    centerZ = sizeZ / 2;

    difference() {
        color("Orange") cube([ sizeX, sizeY, sizeZ ]);

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

module interlock(
    height,   // float
    width,    // float
    oddHoles, // bool
    depth,    // float
    sectionWidth,
    lockWidth,
    offsetSize,
    outerDirection) {

    test = false;
    if (test) {
        step = width / 10;
        if (oddHoles) {
            color("red") {
                cube([ step, depth, height ]);
                translate([ step * 9, 0, 0 ]) {
                    cube([ step, depth, height ]);
                }
            }
        } else {
            translate([ step * 2, 0, 0 ]) color("blue")
                cube([ 6 * step, depth, height ]);
        }

    } else {
        baseWidth = sectionWidth - lockWidth;
        baseTmp   = outerDirection ? baseWidth : lockWidth;
        lockTmp   = outerDirection ? lockWidth : baseWidth;
        plunge    = 2 * offsetSize;
        let(baseWidth = baseTmp, lockWidth = lockTmp) {
            lockCount = floor(width / sectionWidth);
            shift     = (width - (lockCount * sectionWidth)) / 2;
            if (!oddHoles && shift > offsetSize) {
                padding = shift - offsetSize;
                translate([ offsetSize, -plunge, 0 ])
                    cube([ padding, depth + offsetSize, height ]);
                translate([ width - padding - offsetSize, -plunge, 0 ])
                    cube([ padding, depth + offsetSize, height ]);
            }
            translate([ shift, 0, 0 ]) {
                if (oddHoles) {
                    for (lock = [0:lockCount - 1]) {
                        translate([ lock * sectionWidth, 0, 0 ]) {
                            linear_extrude(height = height + 0.01) {
                                offset(r = -offsetSize) polygon(
                                    [[baseWidth / 2, 0],
                                     [lockWidth / 2, depth + plunge],
                                     [lockWidth / 2 + baseWidth,
                                      depth + plunge],
                                     [baseWidth / 2 + lockWidth, 0]]);
                            }
                        }
                    }
                } else {
                    for (lock = [0:lockCount - 1]) {
                        translate([
                            lock * (lockWidth + baseWidth),
                            0,
                            -0.005
                        ]) {
                            linear_extrude(height = height) {
                                polygon([
                                    [ 0, -plunge ],
                                    [ 0, depth - offsetSize ],
                                    [
                                        (sectionWidth - baseWidth) / 2,
                                        depth -
                                        offsetSize
                                    ],
                                    [
                                        (sectionWidth - lockWidth) / 2,
                                        -plunge
                                    ]
                                ]);
                            }

                            linear_extrude(height = height) {
                                polygon([
                                    [
                                        (sectionWidth - baseWidth) / 2
                                            + baseWidth,
                                        depth -
                                        offsetSize
                                    ],
                                    [
                                        (sectionWidth - lockWidth) / 2
                                            + lockWidth,
                                        -plunge
                                    ],
                                    [ sectionWidth, -plunge ],
                                    [ sectionWidth, depth - offsetSize ]
                                ]);
                            }
                        }
                    }
                }
            }
        }
    }
}


height       = 1.1;
width        = 9;
depth        = 2.0;
lockWidth    = 2;
sectionWidth = 3;
offsetSize   = 0.05;

interlock(
    height       = height,
    width        = width,
    oddHoles     = true,
    depth        = depth,
    lockWidth    = lockWidth,
    sectionWidth = sectionWidth,
    offsetSize   = offsetSize);

interlock(
    height       = height,
    width        = width,
    oddHoles     = false,
    depth        = depth,
    lockWidth    = lockWidth,
    sectionWidth = sectionWidth,
    offsetSize   = offsetSize);

translate([ 0, 2 * depth, 0 ]) {
    interlock(
        height         = height,
        width          = width,
        oddHoles       = true,
        depth          = depth,
        lockWidth      = lockWidth,
        sectionWidth   = sectionWidth,
        offsetSize     = offsetSize,
        outerDirection = true);

    interlock(
        height         = height,
        width          = width,
        oddHoles       = false,
        depth          = depth,
        lockWidth      = lockWidth,
        sectionWidth   = sectionWidth,
        offsetSize     = offsetSize,
        outerDirection = true);
}
