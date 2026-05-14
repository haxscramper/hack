from dynamic_window_view.atspi_walker import get_x11_windows, populate_splits, print_split_tree


def main():
    windows = get_x11_windows()
    print(f"Found {len(windows)} windows\n")

    for win in windows:
        populate_splits(win)
        print(f"Window 0x{win.wid:08x}: \"{win.title}\"")
        print(f"  Class: {win.wm_class}")
        print(f"  PID: {win.pid}")
        print(
            f"  Rect: ({win.rect.x},{win.rect.y}) {win.rect.width}x{win.rect.height}"
        )
        if win.splits:
            print(f"  Splits:")
            print_split_tree(win.splits)
        else:
            print(f"  Splits: (none detected)")
        print()


if __name__ == "__main__":
    main()
