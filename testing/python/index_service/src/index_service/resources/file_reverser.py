from index_service.harness import BaseResourceActor
from pydantic import BaseModel


class ReverseLinesRequest(BaseModel):
    lines: list[str]


class ReverserResult(BaseModel):
    lines: list[str]


class FileReverserResourceActor(BaseResourceActor):
    actor_id = "file-reverser"

    def handle(self, request: ReverseLinesRequest) -> ReverserResult:
        return ReverserResult(lines=list(reversed(request.lines)))
