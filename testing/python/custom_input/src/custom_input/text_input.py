from threading import Lock
from beartype import beartype
import pyautogui
import pyperclip

clipboard_lock = Lock()


@beartype
def type_text(text: str) -> None:
    with clipboard_lock:
        original_clipboard = pyperclip.paste()
        pyperclip.copy(text)

        try:
            pyautogui.hotkey("ctrl", "v")
        finally:
            pyperclip.copy(original_clipboard)
