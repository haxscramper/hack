#ifndef BASE_16_COLORS_HPP
#define BASE_16_COLORS_HPP

#include <QColor>
#include <QString>

static QString getColor(int num) {
    switch (num) {
            // clang-format off
    /* 8 normal colors */
    case 0 : return "#171717"; /* black   */
    case 1 : return "#d81765"; /* red     */
    case 2 : return "#97d01a"; /* green   */
    case 3 : return "#ffa800"; /* yellow  */
    case 4 : return "#16b1fb"; /* blue    */
    case 5 : return "#ff2491"; /* magenta */
    case 6 : return "#0fdcb6"; /* cyan    */
    case 7 : return "#ebebeb"; /* white   */

    /* 8 bright colors */
    case 8 : return "#38252c"; /* black   */
    case 9 : return "#ff0000"; /* red     */
    case 10 : return "#76b639"; /* green   */
    case 11 : return "#e1a126"; /* yellow  */
    case 12 : return "#289cd5"; /* blue    */
    case 13 : return "#ff2491"; /* magenta */
    case 14 : return "#0a9b81"; /* cyan    */
    case 15 : return "#f8f8f8"; /* white   */

    /* special colors */
    case 256 : return "#171717"; /* background */
    case 257 : return "#f8f8f8"; /* foreground */

        default: return "";
            // clang-format on
    }
}


struct Colors {
    static QColor getBlack() {
        return QColor(getColor(0));
    }
    static QColor getRed() {
        return QColor(getColor(1));
    }
    static QColor getGreen() {
        return QColor(getColor(2));
    }
    static QColor getYellow() {
        return QColor(getColor(3));
    }
    static QColor getBlue() {
        return QColor(getColor(4));
    }
    static QColor getMagenta() {
        return QColor(getColor(5));
    }
    static QColor getCyan() {
        return QColor(getColor(6));
    }
    static QColor getWhite() {
        return QColor(getColor(7));
    }
    static QColor getBrightBlack() {
        return QColor(getColor(8));
    }
    static QColor getBrightRed() {
        return QColor(getColor(9));
    }
    static QColor getBrightGreen() {
        return QColor(getColor(10));
    }
    static QColor getBrightYellow() {
        return QColor(getColor(11));
    }
    static QColor getBrightBlue() {
        return QColor(getColor(12));
    }
    static QColor getBrightMagenta() {
        return QColor(getColor(13));
    }
    static QColor getBrightCyan() {
        return QColor(getColor(14));
    }
    static QColor getBrightWhite() {
        return QColor(getColor(15));
    }
    static QColor getBackground() {
        return QColor(getColor(256));
    }
    static QColor getForeground() {
        return QColor(getColor(257));
    }
};

#endif // BASE_16_COLORS_HPP
