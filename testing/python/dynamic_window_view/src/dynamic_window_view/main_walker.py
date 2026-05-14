from dynamic_window_view.atspi_walker import populate_splits, TagInfo, Rect, SplitPane, WindowInfo
from dynamic_window_view.awesome_wm_walker import get_awesome_tags


def get_structure() -> list[TagInfo]:
    tags = get_awesome_tags()
    for tag in tags:
        for win in tag.windows:
            populate_splits(win)

    return tags


def print_split_tree(splits: list[SplitPane], indent: int = 4) -> list[str]:
    res = list()
    prefix = " " * indent
    for s in splits:
        res.append(
            f"{prefix}[{s.role}] \"{s.name}\" @ ({s.rect.x},{s.rect.y}) {s.rect.width}x{s.rect.height}"
        )
        if s.children:
            res.extend(print_split_tree(s.children, indent + 4))

    return res


def print_tags(tags: list[TagInfo]) -> str:
    res = list()
    res.append(f"Found {len(tags)} tags\n")

    for tag in tags:
        res.append(f'Tag "{tag.name}"')
        res.append(f"  Screen: {tag.screen}")
        res.append(f"  Selected: {tag.selected}")
        res.append(f"  Activated: {tag.activated}")
        res.append(f"  Index: {tag.index}")
        res.append(f"  Layout: {tag.layout}")
        res.append(f"  Master count: {tag.master_count}")
        res.append(f"  Column count: {tag.column_count}")
        res.append(f"  Master width factor: {tag.master_width_factor}")
        res.append(f"  Gap: {tag.gap}")
        res.append(f"  Volatile: {tag.volatile}")
        res.append(f"  Windows: {len(tag.windows)}")

        for win in tag.windows:

            res.append(f'    Window 0x{win.wid:08x}: "{win.title}"')
            res.append(f"      Class: {win.wm_class}")
            res.append(f"      PID: {win.pid}")
            res.append(f"      Screen: {win.screen}")
            res.append(f"      Tags: {', '.join(win.tags)}")
            res.append(
                f"      Visible on selected tag: {win.visible_on_selected_tag}"
            )
            res.append(
                f"      Rect: ({win.rect.x},{win.rect.y}) {win.rect.width}x{win.rect.height}"
            )
            if win.splits:
                res.append("      Splits:")
                res.extend(print_split_tree(win.splits, indent=8))
            else:
                res.append("      Splits: (none detected)")
        res.append("")

    return "\n".join(res)
