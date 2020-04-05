#include <X11/Xlib.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#include <unistd.h> // usleep

// V1: https://www.lemoda.net/c/xlib-text-box/

/* The window which contains the text. */

Display* display;
Window   window;
GC       gc;

const size_t window_width  = 300;
const size_t window_height = 300;

std::vector<std::string> linebuffer;

struct {
    XFontStruct*  font;
    unsigned long background_pixel;
    unsigned long foreground_pixel;
} text_box;

/* Connect to the display, set up the basic variables. */

void x_connect() {
    display       = XOpenDisplay(NULL);
    int    screen = DefaultScreen(display);
    Window root   = RootWindow(display, screen);

    text_box.background_pixel = BlackPixel(display, screen);
    text_box.foreground_pixel = WhitePixel(display, screen);

    // Create window
    window = XCreateSimpleWindow(
        display,
        root,
        0, /* x */
        0, /* y */
        window_width,
        window_height,
        0,                         /* border width */
        text_box.background_pixel, /* border pixel */
        text_box.background_pixel /* background */);

    XSelectInput(display, window, ExposureMask);
    XMapWindow(display, window);

    // Set up graphics context
    gc = XCreateGC(display, window, 0, 0);
    XSetBackground(display, gc, text_box.foreground_pixel);
    XSetForeground(display, gc, text_box.foreground_pixel);

    // Configure font
    text_box.font = XLoadQueryFont(display, "fixed");
    XSetFont(display, gc, text_box.font->fid);
}

/* Draw the window. */

void draw_screen() {
    int         direction;
    int         ascent;
    int         descent;
    XCharStruct overall;

    XClearWindow(display, window);
    XTextExtents(
        text_box.font, "x", 1, &direction, &ascent, &descent, &overall);

    int line_shift = ascent;

    for (const auto& line : linebuffer) {
        XDrawString(
            display,
            window,
            gc,
            /* X */ 0,
            /* Y */ line_shift,
            line.c_str(),
            line.size());
        line_shift += ascent;
    }
}

/* Loop over events. */

void event_loop() {
    while (1) {
        XEvent e;
        XNextEvent(display, &e);
        if (e.type == Expose) {
            draw_screen();
            usleep(1000);
        }
    }
}


int main(int argc, char** argv) {
    linebuffer.push_back(" 111");
    linebuffer.push_back(" 11001");
    x_connect();

    event_loop();

    return 0;
}
