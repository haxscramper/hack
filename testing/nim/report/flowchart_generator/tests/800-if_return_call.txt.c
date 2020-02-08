if (row == 0) {
    return getByteRowCell(col);
} else if (row == 1) {
    if (col == 0) {
        return "№ Бита";
    } else {
        col = col - 1;
        return 0;
    }
} else if (row == 2) {
    if (col == 0) {
        return "Вес бита";
    } else {
        col = col - 1;
        col -= fltBits;
        if (col < 0) {
            return Math.pow(2, fltBits - col);
        } else {
            return (int)Math.pow(2, fltBits - col);
        }
    }
} else if (row == 3) {
    return getBitRowCell(col);
}
return 0;
