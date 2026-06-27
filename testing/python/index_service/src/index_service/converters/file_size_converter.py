from pathlib import Path

from index_service.harness import BaseConverterActor
from index_service.protocol import ConverterOutput, ConverterRequest
from pydantic import BaseModel


class FileSizeConverterResult(BaseModel):
    sizes: dict[str, int]
    total_size: int


class FileSizeConverterActor(BaseConverterActor):
    actor_id = "file-size-converter"

    def handle(self, request: ConverterRequest) -> ConverterOutput:
        sizes = {p: Path(p).stat().st_size for p in request.input_files}
        return ConverterOutput(
            converter_id=self.actor_id,
            output_files=[],
            return_value=FileSizeConverterResult(
                sizes=sizes,
                total_size=sum(sizes.values()),
            ),
        )
