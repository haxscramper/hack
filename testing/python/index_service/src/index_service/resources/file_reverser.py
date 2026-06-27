from index_service.harness import BaseResource
from pydantic import BaseModel


class ReverseLinesRequest(BaseModel):
    lines: list[str]


class ReverserResult(BaseModel):
    lines: list[str]


class FileReverserResource(BaseResource):
    resource_key = "file_reverser"

    def handle(self, request: ReverseLinesRequest) -> ReverserResult:
        return ReverserResult(lines=list(reversed(request.lines)))
