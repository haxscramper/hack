from pathlib import Path

from index_service.harness import BaseConverter
from index_service.protocol import ConverterOutput, ConverterRequest
from pydantic import BaseModel


class FileSizeConverterResult(BaseModel):
    sizes: dict[str, int]
    total_size: int


class FileSizeConverter(BaseConverter):
    converter_id = "file-size-converter"
    result_model = FileSizeConverterResult

    def run(self, request: ConverterRequest,
            **resources: object) -> ConverterOutput:
        sizes = {p: Path(p).stat().st_size for p in request.input_files}
        return ConverterOutput(
            converter_id=self.converter_id,
            output_files=[],
            return_value=FileSizeConverterResult(
                sizes=sizes,
                total_size=sum(sizes.values()),
            ),
        )
