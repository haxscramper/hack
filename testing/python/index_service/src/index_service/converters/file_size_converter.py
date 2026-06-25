from pathlib import Path

from beartype import beartype
from beartype.typing import Dict
from index_service.harness import RpcContext, RpcHarness
from index_service.protocol import ConverterOutput


@beartype
def handle(payload: Dict[str, object], ctx: RpcContext) -> ConverterOutput:
    input_files = payload["input_files"]
    sizes = {p: Path(p).stat().st_size for p in input_files}
    total = sum(sizes.values())
    return ConverterOutput(
        converter_id="file-size-converter",
        output_files=[],
        return_value={
            "sizes": sizes,
            "total_size": total
        },
    )


if __name__ == "__main__":
    RpcHarness(handle).run()
