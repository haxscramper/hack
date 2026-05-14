from dynamic_window_view.atspi_walker import WindowInfo, Rect, TagInfo

from plumbum import local
from pathlib import Path
import json


def _window_from_awesome_json(data: dict) -> WindowInfo:
    return WindowInfo(
        wid=int(data["wid"]),
        title=data.get("title", ""),
        wm_class=data.get("wm_class", ""),
        pid=int(data.get("pid", -1)),
        rect=Rect(
            x=int(data["x"]),
            y=int(data["y"]),
            width=int(data["width"]),
            height=int(data["height"]),
        ),
        screen=int(data.get("screen", -1)),
        tags=list(data.get("tags", [])),
        visible_on_selected_tag=bool(data.get("visible_on_selected_tag",
                                              False)),
        floating=bool(data.get("floating", False)),
        maximized=bool(data.get("maximized", False)),
        minimized=bool(data.get("minimized", False)),
        fullscreen=bool(data.get("fullscreen", False)),
        urgent=bool(data.get("urgent", False)),
        hidden=bool(data.get("hidden", False)),
        ontop=bool(data.get("ontop", False)),
        sticky=bool(data.get("sticky", False)),
        above=bool(data.get("above", False)),
        below=bool(data.get("below", False)),
    )


def get_awesome_tags() -> list[TagInfo]:
    awesome_client = local["awesome-client"]
    script_path = Path(__file__).parent / "get_windows_and_tags.lua"

    result = awesome_client < str(script_path)
    cmd_result = result.run()

    raw = str(cmd_result[1].strip())[len('string "'):-len('"')]
    print(raw)
    tag_items = json.loads(raw)

    result = [
        TagInfo(
            name=tag_data.get("name", ""),
            screen=int(tag_data.get("screen", -1)),
            selected=bool(tag_data.get("selected", False)),
            activated=bool(tag_data.get("activated", False)),
            index=int(tag_data.get("index", -1)),
            layout=tag_data.get("layout", ""),
            master_count=int(tag_data.get("master_count", 0)),
            column_count=int(tag_data.get("column_count", 0)),
            master_width_factor=float(tag_data.get("master_width_factor",
                                                   0.0)),
            gap=int(tag_data.get("gap", 0)),
            volatile=bool(tag_data.get("volatile", False)),
            windows=[
                _window_from_awesome_json(window_data)
                for window_data in tag_data.get("windows", [])
            ],
        ) for tag_data in tag_items
    ]

    return [t for t in result if 0 < len(t.windows)]
