from dataclasses import dataclass

from beartype import beartype
from beartype.typing import Dict, List
from index_service.harness import ResourceHarness


@beartype
@dataclass
class ReverserResult:
    """Result of reversing the order of input lines."""

    lines: List[str]
    """Input lines in reversed order."""


@beartype
def handle(payload: Dict[str, object]) -> ReverserResult:
    lines = payload["lines"]
    return ReverserResult(lines=list(reversed(lines)))


if __name__ == "__main__":
    ResourceHarness("file-reverser", handle).run()
