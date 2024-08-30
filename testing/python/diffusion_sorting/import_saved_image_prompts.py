#!/usr/bin/env python

from pathlib import Path
from pydantic import BaseModel, Field
import json
from beartype.typing import Optional, Union, List
import logging

class TextEntity(BaseModel):
    type: str
    text: str

class Message(BaseModel):
    id: int
    type: str
    date: str
    date_unixtime: str
    from_: str = Field(alias="from")
    from_id: str
    file: Optional[str] = None
    file_name: Optional[str] = None
    thumbnail: Optional[str] = None
    mime_type: Optional[str] = None
    text: Union[str, List[Union[str, TextEntity]]]
    text_entities: list

    def get_text(self) -> str:
        result = ""
        if isinstance(self.text, str):
            result = self.text

        else:
            for it in self.text:
                if isinstance(it, str):
                    result += it
                else: 
                    result += it.text

        return result


class SavedMessages(BaseModel):
    type: str
    id: int
    messages: list[Message]

def load_json_file(filepath: str) -> SavedMessages:
    with open(filepath, "r", encoding="utf-8") as file:
        data = json.load(file)
    return SavedMessages(**data)

def save_prompts(data: SavedMessages):
    base_dir = Path("/tmp/tg_saved_import")
    base_dir.mkdir(parents=True, exist_ok=True)

    for message in data.messages:
        if "Prompt:" in message.get_text():
            if message.file_name:
                file_name = Path(message.file_name).stem
                target_path = base_dir / f"{file_name}.txt"
                with open(target_path, "w", encoding="utf-8") as file:
                    file.write(message.get_text())

            else:
                logging.warning(f"{message}")

if __name__ == "__main__":
    data = load_json_file("/tmp/messages.json")
    save_prompts(data)
