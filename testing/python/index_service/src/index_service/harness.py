import argparse
import dataclasses
import logging
import time

import zmq
from beartype import beartype
from beartype.typing import Callable, Dict, Union
from index_service.log_config import configure_logging
from index_service.protocol import (
    ConverterOutput,
    IndexerOutput,
    Message,
    MessageType,
    ResourceInfo,
)

log = logging.getLogger(__name__)


@beartype
class RpcContext:

    def __init__(
        self,
        socket: zmq.Socket,
        zmq_context: zmq.Context,
        resources: Dict[str, ResourceInfo],
    ) -> None:
        self._socket = socket
        self._zmq = zmq_context
        self._resources = resources

    def send_progress(self, payload: Dict[str, object]) -> None:
        self._socket.send(Message(MessageType.PROGRESS, payload).to_bytes())

    def send_log(self, payload: Dict[str, object]) -> None:
        self._socket.send(Message(MessageType.LOG, payload).to_bytes())

    def resource_request(self, resource_name: str,
                         payload: Dict[str, object]) -> Message:
        info = self._resources[resource_name]
        sock = self._zmq.socket(zmq.DEALER)
        sock.setsockopt(zmq.LINGER, 1000)
        sock.connect(info.endpoint)
        log.debug("requesting resource %s at %s", resource_name, info.endpoint)
        sock.send(Message(MessageType.REQUEST, payload).to_bytes())
        reply = Message.from_bytes(sock.recv())
        sock.close()
        return reply


@beartype
class RpcHarness:

    def __init__(
        self,
        handler: Callable[[Dict[str, object], RpcContext],
                          Union[IndexerOutput, ConverterOutput]],
    ) -> None:
        self._handler = handler

    def run(self) -> None:
        configure_logging()
        parser = argparse.ArgumentParser()
        parser.add_argument("--socket", required=True)
        args = parser.parse_args()

        zmq_ctx = zmq.Context.instance()
        sock = zmq_ctx.socket(zmq.DEALER)
        sock.setsockopt(zmq.LINGER, 2000)
        sock.connect(args.socket)
        log.debug("connected to orchestrator at %s", args.socket)

        init_msg = Message.from_bytes(sock.recv())
        if init_msg.type != MessageType.INIT:
            raise RuntimeError(f"expected init message, got {init_msg.type}")

        payload = init_msg.payload
        resources = {
            name: ResourceInfo.from_dict(info)
            for name, info in payload.get("available_resources", {}).items()
        }
        ctx = RpcContext(sock, zmq_ctx, resources)
        try:
            output = self._handler(payload, ctx)
            sock.send(
                Message(MessageType.RESULT,
                        dataclasses.asdict(output)).to_bytes())
            log.debug("sent result")
        except Exception as exc:
            sock.send(
                Message(MessageType.ERROR, {
                    "message": str(exc)
                }).to_bytes())
            raise
        finally:
            time.sleep(0.05)
            sock.close()


@beartype
class ResourceHarness:

    def __init__(self, name: str, handler: Callable[[Dict[str, object]],
                                                    object]) -> None:
        self._name = name
        self._handler = handler

    def run(self) -> None:
        configure_logging()
        parser = argparse.ArgumentParser()
        parser.add_argument("--socket", required=True)
        args = parser.parse_args()

        zmq_ctx = zmq.Context.instance()
        sock = zmq_ctx.socket(zmq.ROUTER)
        sock.bind(args.socket)
        log.debug("resource %s bound at %s", self._name, args.socket)

        while True:
            frames = sock.recv_multipart()
            identity = frames[0]
            msg = Message.from_bytes(frames[-1])
            if msg.type == MessageType.STATUS:
                reply = Message(MessageType.RESPONSE, {
                    "status": "ready",
                    "name": self._name
                })
            elif msg.type == MessageType.REQUEST:
                output = self._handler(msg.payload)
                reply = Message(MessageType.RESPONSE,
                                dataclasses.asdict(output))
            else:
                raise RuntimeError(f"unexpected message type {msg.type}")
            sock.send_multipart([identity, reply.to_bytes()])
