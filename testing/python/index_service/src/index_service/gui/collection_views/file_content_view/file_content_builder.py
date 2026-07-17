from abc import ABC, abstractmethod

from PyQt6.QtWidgets import QWidget
from beartype import beartype


@beartype
class FileContentViewBuilder(ABC):

    @abstractmethod
    def can_build(self, mime: str) -> bool:
        raise NotImplementedError

    @abstractmethod
    def build(self, absolute_path: str) -> QWidget:
        raise NotImplementedError