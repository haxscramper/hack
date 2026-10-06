#!/usr/bin/env python

# /// script
# requires-python = ">=3.12"
# dependencies = ["textual", "PyYAML", "Jinja2", "pydantic>=2"]
# ///

import argparse
import random
from decimal import Decimal
from pathlib import Path
from typing import Self

import yaml
from jinja2 import Environment, StrictUndefined, Template
from pydantic import BaseModel, ConfigDict, Field, model_validator
from textual.app import App, ComposeResult
from textual.widgets import Button, Footer, Header, Label, Select, Static


SAMPLES_PER_PROMPT = 2


class StrictModel(BaseModel):
    model_config = ConfigDict(
        extra="forbid",
        strict=True,
        allow_inf_nan=False,
    )


class Choice(StrictModel):
    value: str
    chance: float | None = Field(default=None, ge=0, le=100)


class Choices(StrictModel):
    clothes: list[Choice] = Field(default_factory=list)
    appearance: list[Choice] = Field(default_factory=list)


class Character(Choices):
    character: str = Field(min_length=1)


def choice_weights(
    choices: list[Choice],
    *,
    context: str,
) -> list[float]:
    """Resolve percentage weights for a combined common/character pool."""
    if not choices:
        return []

    specified_total = sum(
        (
            Decimal(str(choice.chance))
            for choice in choices
            if choice.chance is not None
        ),
        start=Decimal(0),
    )

    if specified_total > 100:
        raise ValueError(
            f"{context}: explicitly specified chances sum to "
            f"{specified_total}%, exceeding 100%."
        )

    unspecified_count = sum(
        choice.chance is None for choice in choices
    )
    remaining = Decimal(100) - specified_total

    if unspecified_count == 0 and remaining != 0:
        raise ValueError(
            f"{context}: all entries specify a chance, but their sum is "
            f"{specified_total}%. Either make the sum 100% or omit a chance "
            "from at least one entry to receive the remaining probability."
        )

    unspecified_weight = (
        float(remaining / unspecified_count)
        if unspecified_count
        else 0.0
    )

    return [
        choice.chance
        if choice.chance is not None
        else unspecified_weight
        for choice in choices
    ]


class Configuration(StrictModel):
    templates: list[str] = Field(min_length=1)
    common: Choices = Field(default_factory=Choices)
    characters: list[Character] = Field(min_length=1)

    @model_validator(mode="after")
    def validate_choice_pools(self) -> Self:
        names: set[str] = set()

        for character in self.characters:
            if character.character in names:
                raise ValueError(
                    f"Duplicate character name: {character.character!r}."
                )
            names.add(character.character)

            choice_weights(
                self.common.clothes + character.clothes,
                context=f"Character {character.character!r}, clothes",
            )
            choice_weights(
                self.common.appearance + character.appearance,
                context=f"Character {character.character!r}, appearance",
            )

        return self


def load_configuration(path: Path) -> Configuration:
    with path.open(encoding="utf-8") as stream:
        document = yaml.safe_load(stream)

    return Configuration.model_validate(document)


def compile_templates(sources: list[str]) -> list[Template]:
    environment = Environment(
        undefined=StrictUndefined,
        autoescape=False,
        keep_trailing_newline=True,
    )
    result = [environment.from_string(source) for source in sources]
    random.shuffle(result)
    return result


def sample_choice(choices: list[Choice], *, context: str) -> str:
    # Missing both common and character-specific entries means no value.
    if not choices:
        return ""

    weights = choice_weights(choices, context=context)
    return random.choices(choices, weights=weights, k=1)[0].value


def generate_prompts(
    configuration: Configuration,
    templates: list[Template],
    character_index: int,
) -> str:
    character = configuration.characters[character_index]
    clothes_pool = configuration.common.clothes + character.clothes
    appearance_pool = (
        configuration.common.appearance + character.appearance
    )

    prompts: list[str] = []

    for template in templates:
        for _ in range(SAMPLES_PER_PROMPT):
            clothes = sample_choice(
                clothes_pool,
                context=f"Character {character.character!r}, clothes",
            )
            appearance = sample_choice(
                appearance_pool,
                context=f"Character {character.character!r}, appearance",
            )

            prompt = template.render(
                character=character.character,
                clothes=clothes,
                appearance=appearance,
                # Compatibility with templates using the original typo.
                appearnace=appearance,
            ).strip()

            prompts.append(prompt)

    return "\n".join(f"- {prompt}" for prompt in prompts) + "\n"


class PromptApp(App[None]):
    TITLE = "Random character prompts"

    BINDINGS = [
        ("r", "resample", "Resample"),
        ("q", "quit", "Quit"),
    ]

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

    #resample {
        margin-top: 1;
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
        self.output = output
        self.template_modified = template_path.stat().st_mtime_ns

        self.configuration = load_configuration(template_path)
        self.templates = compile_templates(self.configuration.templates)
        self.character_index = 0

        # Avoid repeating the same notification every polling interval.
        self.last_reload_error: str | None = None

    def compose(self) -> ComposeResult:
        yield Header()
        yield Label("Character")
        yield Select[int](
            [
                (character.character, index)
                for index, character in enumerate(
                    self.configuration.characters
                )
            ],
            value=self.character_index,
            allow_blank=False,
            id="character",
        )
        yield Button("Resample", id="resample", variant="primary")
        yield Static("", markup=False, id="preview")
        yield Footer()

    def on_mount(self) -> None:
        self.action_resample()
        self.set_interval(0.5, self.reload_templates)

    def save_output(self, text: str) -> None:
        self.output.write_text(text, encoding="utf-8")
        self.query_one("#preview", Static).update(text)

    def action_resample(self) -> None:
        try:
            text = generate_prompts(
                self.configuration,
                self.templates,
                self.character_index,
            )
            self.save_output(text)
        except Exception as error:
            self.notify(
                str(error),
                title="Could not generate prompts",
                severity="error",
                timeout=10,
            )

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "resample":
            self.action_resample()

    def on_select_changed(self, event: Select.Changed) -> None:
        if event.select.id != "character":
            return

        value = event.value

        if value is Select.BLANK:
            return

        # Ignore queued events for a selection that has since changed.
        if value != event.select.value:
            return

        if not isinstance(value, int):
            raise TypeError(
                f"Character selector returned {value!r}; "
                "expected an integer option index."
            )

        if value == self.character_index:
            return

        self.character_index = value
        self.action_resample()

    def reload_templates(self) -> None:
        try:
            modified = self.template_path.stat().st_mtime_ns

            if modified == self.template_modified:
                self.last_reload_error = None
                return

            configuration = load_configuration(self.template_path)
            templates = compile_templates(configuration.templates)

            selected_name = self.configuration.characters[
                self.character_index
            ].character

            selected_index = next(
                (
                    index
                    for index, character in enumerate(
                        configuration.characters
                    )
                    if character.character == selected_name
                ),
                0,
            )

            # Validate rendering and write the new output before replacing
            # the currently working configuration.
            text = generate_prompts(
                configuration,
                templates,
                selected_index,
            )
            self.save_output(text)

            self.configuration = configuration
            self.templates = templates
            self.character_index = selected_index

            selector = self.query_one("#character", Select)
            selector.set_options(
                [
                    (character.character, index)
                    for index, character in enumerate(
                        configuration.characters
                    )
                ]
            )
            selector.value = selected_index

            self.template_modified = modified
            self.last_reload_error = None

        except Exception as error:
            message = str(error)

            if message != self.last_reload_error:
                self.notify(
                    message,
                    title="Could not reload configuration",
                    severity="error",
                    timeout=10,
                )
                self.last_reload_error = message


def main() -> None:
    if (
        isinstance(SAMPLES_PER_PROMPT, bool)
        or not isinstance(SAMPLES_PER_PROMPT, int)
        or SAMPLES_PER_PROMPT < 1
    ):
        raise ValueError("SAMPLES_PER_PROMPT must be a positive integer.")

    parser = argparse.ArgumentParser(
        description=(
            "Generate random prompt combinations for a selected character "
            "and save every resample."
        )
    )
    parser.add_argument("templates", type=Path)
    parser.add_argument("output", type=Path)
    arguments = parser.parse_args()

    try:
        app = PromptApp(arguments.templates, arguments.output)
    except Exception as error:
        parser.exit(1, f"Error: {error}\n")

    app.run()


if __name__ == "__main__":
    main()
