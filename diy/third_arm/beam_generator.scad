cell_w = 1;
cell_d = 4;
cell_h = 2;

mult = 10;
hole_r = 2.3;
over = 0.1;
do_bolt_cutouts = false;
cut_radius = 3;
cut_depth = mult * 0.2;
cylinder_fragments = 12;


module pipe(length, radius) {
  cylinder(length,      //
           r1 = radius, //
           $fn = cylinder_fragments, r2 = radius);

  if (do_bolt_cutouts) {

    translate([ 0, 0, -over ]) {
      cylinder(cut_depth,       //
               r1 = cut_radius, //
               $fn = cylinder_fragments, r2 = cut_radius);
    }

    translate([ 0, 0, length - cut_depth ]) {
      cylinder(cut_depth,       //
               r1 = cut_radius, //
               $fn = cylinder_fragments, r2 = cut_radius);
    }
  }
}

module bars(bar_rows, bar_cols, bar_length) {
  translate([ mult, 0, mult / 2 ]) {
    rotate(a = [ -90, 0, 0 ]) {
      for (row = [0:bar_rows - 1]) {
        for (col = [0:bar_cols - 1]) {
          translate([
            mult * row - mult / 2, //
            -col * mult,           //
            -over
          ]) {
            pipe(bar_length * mult + over * 2, hole_r);
          }
        }
      }
    }
  }
}

module brick() {
  difference() {
    cube(
        [
          cell_w * 10, //
          cell_d * 10, //
          cell_h * 10
        ],
        [ 0, 0, 0 ]);
    union() {
      bars(bar_rows = cell_w, //
           bar_cols = cell_h, //
           bar_length = cell_d);

      translate([ 0, cell_d * mult, 0 ]) {
        rotate([ 90, 0, 0 ]) {
          bars(bar_rows = cell_w, //
               bar_cols = cell_d, //
               bar_length = cell_h);
        }
      }

      translate([ 0, (cell_d)*mult, 0 ]) {
        rotate([ 0, 0, -90 ]) {
          bars(bar_rows = cell_d, //
               bar_cols = cell_h, //
               bar_length = cell_w);
        }
      }
    }
  }
}

brick();
