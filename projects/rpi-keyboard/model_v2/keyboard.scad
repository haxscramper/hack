module key_core(height, width, length) {
      difference () {
           cube([height, width, height + 0.2]);
      }
}

module key_boundary(height, width, length, length) {
     shift = 0.1;
     translate([0,0, -shift]) {
          cube([length, width, height + 2 * shift]);
     }
}

module row_boundary(width, height, length) {
     shift = 0.1;
     translate([0,0, -shift]) {
          cube([width, length, height + 2 * shift]);
     }
}

