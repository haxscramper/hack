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

function canWeHaveFuckingIfElse(condition, ifTrue, ifFalse) = condition
                                                                  ? ifTrue
                                                                  : ifFalse;

module interlockTeeth(
    height, // float
    width,  // float
    depth,  // float
    baseAngles,
    lockWidth,
    offsetSize,
    outerDirection) {

    baseWidth = lockWidth + 2 * tan(baseAngles - 90);

    baseTmp = outerDirection ? baseWidth : lockWidth;
    lockTmp = outerDirection ? lockWidth : baseWidth;

    let(baseWidth = baseTmp, lockWidth = lockTmp) {
        sectionWidth = baseWidth + lockWidth;
        lockCount    = floor(width / (baseWidth * 2));
        shift        = (width - (lockCount * baseWidth)) / 2;
        translate([ shift + lockWidth / 2, 0, 0 ]) {
            for (lock = [0:lockCount - 1]) {
                translate([ lock * (lockWidth + baseWidth), 0, -0.005 ]) {
                    linear_extrude(height = height + 0.01) {
                        translate([ 0, offsetSize, 0 ])
                            offset(r = offsetSize) polygon(
                                [[baseWidth / 2, 0],
                                 [lockWidth / 2, depth],
                                 [lockWidth / 2 + baseWidth, depth],
                                 [baseWidth / 2 + lockWidth, 0]]);
                    }
                }
            }
        }
    }
}

module interlock(
    height,   // float
    width,    // float
    oddHoles, // bool
    depth,    // float
    baseAngles,
    lockWidth,
    offsetSize,
    outerDirection) {

    if (oddHoles) {
        interlockTeeth(
            height         = height,
            width          = width,
            depth          = depth,
            baseAngles     = baseAngles,
            lockWidth      = lockWidth,
            offsetSize     = -offsetSize,
            outerDirection = outerDirection);
    } else {
        difference() {
            translate([ 0, offsetSize, 0 ])
                cube([ width, depth - offsetSize, height ]);
            interlockTeeth(
                height         = height,
                width          = width,
                depth          = depth,
                baseAngles     = baseAngles,
                lockWidth      = lockWidth,
                offsetSize     = offsetSize,
                outerDirection = outerDirection);
        }
    }
}

interlock(
    height     = 1.1,
    width      = 8.5,
    oddHoles   = true,
    depth      = 2.0,
    lockWidth  = 2,
    baseAngles = 115,
    offsetSize = 0.05);


interlock(
    height     = 1.1,
    width      = 8.5,
    oddHoles   = false,
    depth      = 2.0,
    lockWidth  = 2,
    baseAngles = 115,
    offsetSize = 0.05);
