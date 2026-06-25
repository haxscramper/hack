import enum
import json
from dataclasses import dataclass

from beartype import beartype
from beartype.typing import Dict, List


class MessageType(str, enum.Enum):
    INIT = "init"
    PROGRESS = "progress"
    LOG = "log"
    RESULT = "result"
    ERROR = "error"
    STATUS = "status"
    REQUEST = "request"
    RESPONSE = "response"


@beartype
@dataclass
class Message:
    """Wire envelope exchanged over every zmq link."""

    type: MessageType
    """Discriminator describing how to interpret the payload."""

    payload: Dict[str, object]
    """Free-form JSON body whose shape depends on the message type."""

    def to_bytes(self) -> bytes:
        return json.dumps({
            "type": self.type.value,
            "payload": self.payload
        }).encode("utf-8")

    @staticmethod
    def from_bytes(data: bytes) -> "Message":
        obj = json.loads(data.decode("utf-8"))
        return Message(type=MessageType(obj["type"]), payload=obj["payload"])


@beartype
@dataclass
class ResourceInfo:
    """Connection details for a long-running resource service."""

    endpoint: str
    """zmq endpoint the indexer should connect to in order to reach the resource."""

    status: str
    """Reported liveness/readiness of the resource at scheduling time."""

    @staticmethod
    def from_dict(data: Dict[str, object]) -> "ResourceInfo":
        return ResourceInfo(endpoint=data["endpoint"], status=data["status"])


@beartype
@dataclass
class IndexerOutput:
    """Result emitted by an ephemeral indexer subprocess."""

    indexer_id: str
    """Stable identity of the indexer, used as part of the result primary key."""

    result_type: str
    """Category of the produced metadata (only one per (file, type))."""

    result: Dict[str, object]
    """Indexer specific metadata payload."""

    @staticmethod
    def from_dict(data: Dict[str, object]) -> "IndexerOutput":
        return IndexerOutput(
            indexer_id=data["indexer_id"],
            result_type=data["result_type"],
            result=data["result"],
        )


@beartype
@dataclass
class ConverterOutput:
    """Result emitted by an ephemeral converter subprocess."""

    converter_id: str
    """Stable identity of the converter."""

    output_files: List[str]
    """Paths of files produced by the conversion."""

    return_value: Dict[str, object]
    """Converter specific return value describing the operation outcome."""

    @staticmethod
    def from_dict(data: Dict[str, object]) -> "ConverterOutput":
        return ConverterOutput(
            converter_id=data["converter_id"],
            output_files=data["output_files"],
            return_value=data["return_value"],
        )
