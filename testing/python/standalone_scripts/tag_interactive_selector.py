#!/usr/bin/env python

# /// script
# requires-python = ">=3.12"
# dependencies = ["textual", "PyYAML", "Jinja2", "beartype"]
# ///

import argparse
from dataclasses import dataclass
from pathlib import Path

import yaml
from beartype import beartype
from jinja2 import Environment, StrictUndefined, Template
from textual.app import App, ComposeResult
from textual.widgets import Footer, Header, Label, Select, Static


@beartype
@dataclass(frozen=True)
class Character:
    name: str
    clothes: list[str]
    appearances: list[str]


@beartype
@dataclass(frozen=True)
class Configuration:
    templates: list[str]
    characters: list[Character]


@beartype
def read_choices(value: object, character: str, field: str) -> list[str]:
    match value:
        case None:
            return [""]
        case str():
            return [value]
        case list():
            if not value:
                raise ValueError(
                    f"Character {character!r} has an empty {field!r} list."
                )

            for index, item in enumerate(value):
                if not isinstance(item, str):
                    raise ValueError(
                        f"Character {character!r}, field {field!r}, item "
                        f"{index}: expected a string, received {item!r}."
                    )

            return value
        case _:
            raise ValueError(
                f"Character {character!r}, field {field!r}: expected a "
                f"string or list of strings, received {value!r}."
            )


@beartype
def load_configuration(path: Path) -> Configuration:
    with path.open(encoding="utf-8") as stream:
        document = yaml.safe_load(stream)

    match document:
        case {"templates": list() as templates, "characters": list() as entries}:
            pass
        case _:
            raise ValueError(
                f"{path}: expected 'templates' and 'characters' lists."
            )

    if not templates:
        raise ValueError(f"{path}: 'templates' must contain at least one entry.")

    for index, template in enumerate(templates):
        if not isinstance(template, str):
            raise ValueError(
                f"{path}: template {index} must be a string, "
                f"received {template!r}."
            )

    if not entries:
        raise ValueError(f"{path}: 'characters' must contain at least one entry.")

    characters: list[Character] = []

    for index, entry in enumerate(entries):
        match entry:
            case {"character": str() as name}:
                characters.append(
                    Character(
                        name=name,
                        clothes=read_choices(entry.get("clothes"), name, "clothes"),
                        appearances=read_choices(
                            entry.get("appearance"), name, "appearance"
                        ),
                    )
                )
            case _:
                raise ValueError(
                    f"{path}: character entry {index} must contain a string "
                    f"'character' field, received {entry!r}."
                )

    return Configuration(templates=templates, characters=characters)


@beartype
class PromptApp(App[None]):
    TITLE = "Character prompts"
    BINDINGS = [("q", "quit", "Quit")]
    CSS = """
    Screen {
        padding: 1 2;
    }

    Label {
        margin-top: 1;
    }

    Select {
        width: 100%;
    }

    #preview {
        height: 1fr;
        overflow-y: auto;
        border: round $accent;
        padding: 1 2;
        margin-top: 1;
    }
    """

    def __init__(self, template_path: Path, output: Path) -> None:
        super().__init__()
        self.template_path = template_path
        self.template_modified = template_path.stat().st_mtime_ns
        self.configuration = load_configuration(template_path)
        self.output = output
        self.character_index = 0
        self.clothes_index = 0
        self.appearance_index = 0
        self.templates = self.compile_templates(self.configuration.templates)

    def compile_templates(self, sources: list[str]) -> list[Template]:
        environment = Environment(
            undefined=StrictUndefined,
            autoescape=False,
            keep_trailing_newline=True,
        )
        return [environment.from_string(source) for source in sources]

    def reload_templates(self) -> None:
        modified = self.template_path.stat().st_mtime_ns

        if modified == self.template_modified:
            return

        configuration = load_configuration(self.template_path)
        templates = self.compile_templates(configuration.templates)
        selected_name = self.configuration.characters[self.character_index].name

        selected_index = next(
            (
                index
                for index, character in enumerate(configuration.characters)
                if character.name == selected_name
            ),
            0,
        )

        self.configuration = configuration
        self.templates = templates

        selector = self.query_one("#character", Select)
        selector.set_options(
            [
                (character.name, index)
                for index, character in enumerate(configuration.characters)
            ]
        )
        selector.value = selected_index
        self.update_character(selected_index)
        self.save_output()
        self.template_modified = modified

        
    def compose(self) -> ComposeResult:
        character = self.configuration.characters[0]

        yield Header()
        yield Label("Character")
        yield Select[int](
            [
                (entry.name, index)
                for index, entry in enumerate(self.configuration.characters)
            ],
            value=0,
            allow_blank=False,
            id="character",
        )
        yield Label("Clothes")
        yield Select[int](
            [
                (value or "(none)", index)
                for index, value in enumerate(character.clothes)
            ],
            value=0,
            allow_blank=False,
            disabled=len(character.clothes) == 1,
            id="clothes",
        )
        yield Label("Appearance")
        yield Select[int](
            [
                (value or "(none)", index)
                for index, value in enumerate(character.appearances)
            ],
            value=0,
            allow_blank=False,
            disabled=len(character.appearances) == 1,
            id="appearance",
        )
        yield Static("", markup=False, id="preview")
        yield Footer()

    def on_mount(self) -> None:
        self.save_output()
        self.set_interval(0.5, self.reload_templates)

    def update_character(self, index: int) -> None:
        self.character_index = index
        self.clothes_index = 0
        self.appearance_index = 0
        character = self.configuration.characters[index]

        clothes = self.query_one("#clothes", Select)
        clothes.set_options(
            [
                (value or "(none)", position)
                for position, value in enumerate(character.clothes)
            ]
        )
        clothes.value = 0
        clothes.disabled = len(character.clothes) == 1

        appearance = self.query_one("#appearance", Select)
        appearance.set_options(
            [
                (value or "(none)", position)
                for position, value in enumerate(character.appearances)
            ]
        )
        appearance.value = 0
        appearance.disabled = len(character.appearances) == 1

    def on_select_changed(self, event: Select.Changed) -> None:
        value = event.select.value

        if value is Select.BLANK:
            return

        if not isinstance(value, int):
            raise TypeError(
                f"Selector {event.select.id!r} returned {value!r}; "
                f"expected an integer option index."
            )

        match event.select.id:
            case "character":
                if value != self.character_index:
                    self.update_character(value)
            case "clothes":
                self.clothes_index = value
            case "appearance":
                self.appearance_index = value

        self.save_output()

    def save_output(self) -> None:
        character = self.configuration.characters[self.character_index]
        appearance = character.appearances[self.appearance_index]
        clothes = character.clothes[self.clothes_index]

        prompts = [
            template.render(
                character=character.name,
                clothes=clothes,
                appearance=appearance,
                appearnace=appearance,
            ).strip()
            for template in self.templates
        ]
        text = "\n".join(f"- {prompt}" for prompt in prompts) + "\n"

        self.output.write_text(text, encoding="utf-8")
        self.query_one("#preview", Static).update(text)


@beartype
def main() -> None:
    parser = argparse.ArgumentParser(
        description="Interactively select character prompts and save every change."
    )
    parser.add_argument("templates", type=Path)
    parser.add_argument("output", type=Path)
    arguments = parser.parse_args()

    PromptApp(arguments.templates, arguments.output).run()


if __name__ == "__main__":
    main()
