from pathlib import Path

from index_service.services.harness import BaseConverter
from index_service.services.types import ConverterOutput, ConverterRequest
from pydantic import BaseModel
import logging


class FileSizeConverterResult(BaseModel, extra="forbid"):
    sizes: dict[str, int]
    total_size: int


class FileSizeConverter(BaseConverter):
    converter_id = "file_size_converter"
    result_model = FileSizeConverterResult

    def run(
        self,
        request: ConverterRequest,
        resources: dict[str, object],
        assets: dict[str, object],
    ) -> ConverterOutput:
        logging.info("running file size converter")
        sizes = {
            str(p.path): Path(p.path).stat().st_size
            for p in request.input_files
        }

        return ConverterOutput(
            converter_id=self.converter_id,
            output_files=[],
            return_value=FileSizeConverterResult(
                sizes=sizes,
                total_size=sum(sizes.values()),
            ),
        )
