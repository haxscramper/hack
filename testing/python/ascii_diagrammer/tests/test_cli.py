import json
import subprocess
import sys


def test_cli_outputs_resolved_json(tmp_path):
    input_path = tmp_path / "input.json"
    input_path.write_text(
        json.dumps(
            {
                "canvas_width": 100,
                "canvas_height": 100,
                "shapes": [
                    {
                        "id": "a",
                        "shape_type": "rect",
                        "size": {
                            "type": "fixed",
                            "w": {"type": "axis-value", "fixed": 10},
                            "h": {"type": "axis-value", "fixed": 10},
                        },
                        "position": {"type": "absolute", "x": 1, "y": 2},
                    }
                ],
            }
        )
    )

    proc = subprocess.run(
        [sys.executable, "-m", "diagram_layout", str(input_path)],
        capture_output=True,
        text=True,
        check=True,
    )

    payload = json.loads(proc.stdout)
    shape = {s["id"]: s for s in payload["shapes"]}["a"]
    assert shape["x"] == 1
    assert shape["y"] == 2
