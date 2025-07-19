#!/usr/bin/env python

import logging
from pathlib import Path
from typing import Any, Dict, List, Union, Tuple, Optional, Generator
from contextlib import contextmanager

logging.basicConfig(
    level=logging.INFO,
    format='%(message)s',
    filename="/tmp/msgpack_result.log",
    filemode="w+",
)
logger = logging.getLogger(__name__)


class MsgPackDebugParser:

    def __init__(self, data: bytes):
        self.data = data
        self.pos = 0
        self.depth = 0

    @contextmanager
    def depth_context(self) -> Generator[None, None, None]:
        self.depth += 1
        try:
            yield
        finally:
            self.depth -= 1

    def read_byte(self) -> int:
        if self.pos >= len(self.data):
            raise EOFError(f"Unexpected end of data at position {self.pos}")
        byte = self.data[self.pos]
        self.pos += 1
        return byte

    def read_bytes(self, count: int) -> bytes:
        if self.pos + count > len(self.data):
            raise EOFError(f"Cannot read {count} bytes at position {self.pos}")
        result = self.data[self.pos:self.pos + count]
        self.pos += count
        return result

    def read_uint8(self) -> int:
        return self.read_byte()

    def read_uint16(self) -> int:
        return int.from_bytes(self.read_bytes(2), 'big')

    def read_uint32(self) -> int:
        return int.from_bytes(self.read_bytes(4), 'big')

    def read_uint64(self) -> int:
        return int.from_bytes(self.read_bytes(8), 'big')

    def read_int8(self) -> int:
        return int.from_bytes(self.read_bytes(1), 'big', signed=True)

    def read_int16(self) -> int:
        return int.from_bytes(self.read_bytes(2), 'big', signed=True)

    def read_int32(self) -> int:
        return int.from_bytes(self.read_bytes(4), 'big', signed=True)

    def read_int64(self) -> int:
        return int.from_bytes(self.read_bytes(8), 'big', signed=True)

    def read_float32(self) -> float:
        import struct
        return struct.unpack('>f', self.read_bytes(4))[0]

    def read_float64(self) -> float:
        import struct
        return struct.unpack('>d', self.read_bytes(8))[0]

    def indent(self) -> str:
        return "  " * self.depth

    def parse_object(self) -> Any:
        with self.depth_context():
            if self.pos >= len(self.data):
                logger.error(f"Reached end of data at position {self.pos}")
                return None

            try:
                format_byte = self.read_byte()
                logger.info(
                    f"{self.indent()}Format byte: 0x{format_byte:02x} at position {self.pos-1}"
                )

                if format_byte <= 0x7f:
                    logger.info(
                        f"{self.indent()}Positive fixint: {format_byte}")
                    return format_byte
                elif format_byte >= 0xe0:
                    value = format_byte - 0x100
                    logger.info(f"{self.indent()}Negative fixint: {value}")
                    return value
                elif format_byte & 0xf0 == 0x80:
                    length = format_byte & 0x0f
                    logger.info(
                        f"{self.indent()}Fixmap with {length} elements")
                    return self.parse_map(length)
                elif format_byte & 0xf0 == 0x90:
                    length = format_byte & 0x0f
                    logger.info(
                        f"{self.indent()}Fixarray with {length} elements")
                    return self.parse_array(length)
                elif format_byte & 0xe0 == 0xa0:
                    length = format_byte & 0x1f
                    tmp = self.parse_string(length)
                    logger.info(
                        f"{self.indent()}Fixstr with {length} bytes: '{tmp.replace("\n", "\\n")}'")
                    return tmp
                elif format_byte == 0xc0:
                    logger.info(f"{self.indent()}Nil")
                    return None
                elif format_byte == 0xc2:
                    logger.info(f"{self.indent()}False")
                    return False
                elif format_byte == 0xc3:
                    logger.info(f"{self.indent()}True")
                    return True
                elif format_byte == 0xc4:
                    length = self.read_uint8()
                    logger.info(f"{self.indent()}Bin8 with {length} bytes")
                    return self.read_bytes(length)
                elif format_byte == 0xc5:
                    length = self.read_uint16()
                    logger.info(f"{self.indent()}Bin16 with {length} bytes")
                    return self.read_bytes(length)
                elif format_byte == 0xc6:
                    length = self.read_uint32()
                    logger.info(f"{self.indent()}Bin32 with {length} bytes")
                    return self.read_bytes(length)
                elif format_byte == 0xca:
                    value = self.read_float32()
                    logger.info(f"{self.indent()}Float32: {value}")
                    return value
                elif format_byte == 0xcb:
                    value = self.read_float64()
                    logger.info(f"{self.indent()}Float64: {value}")
                    return value
                elif format_byte == 0xcc:
                    value = self.read_uint8()
                    logger.info(f"{self.indent()}Uint8: {value}")
                    return value
                elif format_byte == 0xcd:
                    value = self.read_uint16()
                    logger.info(f"{self.indent()}Uint16: {value}")
                    return value
                elif format_byte == 0xce:
                    value = self.read_uint32()
                    logger.info(f"{self.indent()}Uint32: {value}")
                    return value
                elif format_byte == 0xcf:
                    value = self.read_uint64()
                    logger.info(f"{self.indent()}Uint64: {value}")
                    return value
                elif format_byte == 0xd0:
                    value = self.read_int8()
                    logger.info(f"{self.indent()}Int8: {value}")
                    return value
                elif format_byte == 0xd1:
                    value = self.read_int16()
                    logger.info(f"{self.indent()}Int16: {value}")
                    return value
                elif format_byte == 0xd2:
                    value = self.read_int32()
                    logger.info(f"{self.indent()}Int32: {value}")
                    return value
                elif format_byte == 0xd3:
                    value = self.read_int64()
                    logger.info(f"{self.indent()}Int64: {value}")
                    return value
                elif format_byte == 0xd9:
                    length = self.read_uint8()
                    logger.info(f"{self.indent()}Str8 with {length} bytes")
                    return self.parse_string(length)
                elif format_byte == 0xda:
                    length = self.read_uint16()
                    logger.info(f"{self.indent()}Str16 with {length} bytes")
                    return self.parse_string(length)
                elif format_byte == 0xdb:
                    length = self.read_uint32()
                    logger.info(f"{self.indent()}Str32 with {length} bytes")
                    return self.parse_string(length)
                elif format_byte == 0xdc:
                    length = self.read_uint16()
                    logger.info(
                        f"{self.indent()}Array16 with {length} elements")
                    return self.parse_array(length)
                elif format_byte == 0xdd:
                    length = self.read_uint32()
                    logger.info(
                        f"{self.indent()}Array32 with {length} elements")
                    return self.parse_array(length)
                elif format_byte == 0xde:
                    length = self.read_uint16()
                    logger.info(f"{self.indent()}Map16 with {length} elements")
                    return self.parse_map(length)
                elif format_byte == 0xdf:
                    length = self.read_uint32()
                    logger.info(f"{self.indent()}Map32 with {length} elements")
                    return self.parse_map(length)
                else:
                    logger.error(
                        f"{self.indent()}Unknown format byte: 0x{format_byte:02x}"
                    )
                    return f"<unknown:0x{format_byte:02x}>"

            except Exception as e:
                logger.error(
                    f"{self.indent()}Error parsing at position {self.pos}: {e}"
                )
                return f"<error:{e}>"

    def parse_string(self, length: int) -> str:
        try:
            data = self.read_bytes(length)
            return data.decode('utf-8')
        except UnicodeDecodeError as e:
            logger.error(f"{self.indent()}Unicode decode error: {e}")
            return f"<invalid_utf8:{data.hex()}>"

    def parse_array(self, length: int) -> List[Any]:
        logger.info(f"{self.indent()}Parsing array of {length} elements")
        self.depth += 1
        result = []
        for i in range(length):
            logger.info(f"{self.indent()}Array element {i}:")
            try:
                element = self.parse_object()
                result.append(element)
            except Exception as e:
                logger.error(
                    f"{self.indent()}Error parsing array element {i}: {e}")
                result.append(f"<error:{e}>")
        self.depth -= 1
        return result

    def parse_map(self, length: int) -> Dict[Any, Any]:
        with self.depth_context():
            logger.info(f"{self.indent()}Parsing map of {length} pairs")
            result = {}
            for i in range(length):
                with self.depth_context():
                    try:
                        logger.info(f"{self.indent()}Map pair {i} key:")
                        key = self.parse_object()
                        logger.info(f"{self.indent()}Map pair {i} value:")
                        value = self.parse_object()
                        result[key] = value
                    except Exception as e:
                        logger.error(
                            f"{self.indent()}Error parsing map pair {i}: {e}")
                        result[f"<error_key_{i}>"] = f"<error:{e}>"

        return result


def debug_msgpack_file() -> None:
    input_path = Path("/tmp/dir1_msgpack.bin")

    if not input_path.exists():
        logger.error(f"File {input_path} does not exist")
        return

    with input_path.open("rb") as f:
        data = f.read()

    logger.info(f"File size: {len(data)} bytes")

    parser = MsgPackDebugParser(data)

    try:
        result = parser.parse_object()
        logger.info(
            f"Parsing completed. Remaining bytes: {len(data) - parser.pos}")

        if parser.pos < len(data):
            logger.warning(
                f"Extra data found: {data[parser.pos:parser.pos+20].hex()}...")

        import pprint
        logger.info(pprint.pformat(result, width=120, depth=10))
        import json
        Path("/tmp/msgpack_result_debug.json").write_text(json.dumps(result, indent=2))
        print("Parsed result:")

    except Exception as e:
        logger.error(f"Fatal error during parsing: {e}")
        logger.info(f"Parser stopped at position {parser.pos}/{len(data)}")


if __name__ == "__main__":
    debug_msgpack_file()
