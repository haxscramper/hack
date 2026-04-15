from beartype import beartype
import subprocess
import tempfile
from pathlib import Path

import pytest


@beartype
class TestCLI:

    def test_basic_cli(self, tmp_path: Path):
        input_file = tmp_path / "input.dsl"
        output_file = tmp_path / "output.txt"
        input_file.write_text("rect(0, 0, width=5, height=3)")

        result = subprocess.run(
            [
                "uv", "run", "diagrammer",
                str(input_file), "-o",
                str(output_file)
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        content = output_file.read_text()
        lines = content.strip().split("\n")
        assert lines[0] == "┌───┐"
        assert lines[1] == "│   │"
        assert lines[2] == "└───┘"

    def test_cli_ascii_charset(self, tmp_path: Path):
        input_file = tmp_path / "input.dsl"
        output_file = tmp_path / "output.txt"
        input_file.write_text("rect(0, 0, width=5, height=3)")

        result = subprocess.run(
            [
                "uv",
                "run",
                "diagrammer",
                str(input_file),
                "-o",
                str(output_file),
                "--charset",
                "ascii",
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        content = output_file.read_text()
        lines = content.strip().split("\n")
        assert lines[0] == "+---+"

    def test_cli_with_scale(self, tmp_path: Path):
        input_file = tmp_path / "input.dsl"
        output_file = tmp_path / "output.txt"
        input_file.write_text("rect(0, 0, width=3, height=2)")

        result = subprocess.run(
            [
                "uv",
                "run",
                "diagrammer",
                str(input_file),
                "-o",
                str(output_file),
                "--scale",
                "2.0",
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        content = output_file.read_text()
        lines = content.strip().split("\n")
        assert len(lines[0]) == 6
        assert len(lines) == 4

    def test_cli_with_canvas_size(self, tmp_path: Path):
        input_file = tmp_path / "input.dsl"
        output_file = tmp_path / "output.txt"
        input_file.write_text("rect(0, 0, width=5, height=3)")

        result = subprocess.run(
            [
                "uv",
                "run",
                "diagrammer",
                str(input_file),
                "-o",
                str(output_file),
                "--canvas-width",
                "20",
                "--canvas-height",
                "10",
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0


@beartype
class TestEndToEnd:

    def test_complex_diagram(self, tmp_path: Path):
        input_file = tmp_path / "input.dsl"
        output_file = tmp_path / "output.txt"
        input_file.write_text(
            "let header = rect(0, 0, width=30, height=3) {\n"
            '  text(1, 1, "Dashboard")\n'
            "}\n"
            "let body = rect(@below(header), width=30, height=10) {\n"
            "  rect(1, 1, width=13, height=4)\n"
            "  rect(15, 1, width=13, height=4)\n"
            "}\n")

        result = subprocess.run(
            [
                "uv", "run", "diagrammer",
                str(input_file), "-o",
                str(output_file)
            ],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0
        content = output_file.read_text()
        assert "Dashboard" in content
        lines = content.strip().split("\n")
        assert len(lines) >= 13
