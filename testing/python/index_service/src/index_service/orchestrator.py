import logging
import os
import subprocess
import sys
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path

import zmq
from beartype import beartype
from beartype.typing import Dict, List
from index_service.protocol import Message, MessageType

log = logging.getLogger(__name__)

REPO_ROOT = Path(__file__).resolve().parents[1]


@beartype
@dataclass
class ResourceHandle:
    """Handle to a running long-lived resource process managed by the orchestrator."""

    name: str
    """Identity of the resource."""

    endpoint: str
    """zmq endpoint the resource is bound to."""

    process: subprocess.Popen
    """Operating system process backing the resource."""


@beartype
@dataclass
class RpcSubprocessResult:
    """Outcome of driving an ephemeral indexer/converter subprocess to completion."""

    result: Dict[str, object]
    """Final RESULT payload returned by the subprocess."""

    progress_messages: List[Dict[str, object]] = field(default_factory=list)
    """Ordered list of PROGRESS payloads observed during the run."""

    log_messages: List[Dict[str, object]] = field(default_factory=list)
    """Ordered list of LOG payloads observed during the run."""


@beartype
def _child_env() -> Dict[str, str]:
    env = dict(os.environ)
    existing = env.get("PYTHONPATH")
    env["PYTHONPATH"] = (str(REPO_ROOT) if existing is None else
                         f"{REPO_ROOT}{os.pathsep}{existing}")
    return env


@beartype
def make_socket_path(socket_dir: Path, prefix: str) -> str:
    socket_dir.mkdir(parents=True, exist_ok=True)
    name = f"{prefix}-{uuid.uuid4().hex}.sock"
    return f"ipc://{socket_dir / name}"


@beartype
def _unlink_ipc(endpoint: str) -> None:
    prefix = "ipc://"
    if endpoint.startswith(prefix):
        path = Path(endpoint[len(prefix):])
        if path.exists():
            path.unlink()


@beartype
def start_resource(
    name: str,
    module: str,
    socket_dir: Path,
    ready_timeout: float = 15.0,
) -> ResourceHandle:
    endpoint = make_socket_path(socket_dir, f"resource-{name}")
    proc = subprocess.Popen(
        [sys.executable, "-m", module, "--socket", endpoint],
        env=_child_env(),
    )
    zmq_ctx = zmq.Context.instance()
    sock = zmq_ctx.socket(zmq.DEALER)
    sock.setsockopt(zmq.LINGER, 1000)
    sock.connect(endpoint)
    poller = zmq.Poller()
    poller.register(sock, zmq.POLLIN)

    deadline = time.monotonic() + ready_timeout
    try:
        while time.monotonic() < deadline:
            if proc.poll() is not None:
                raise RuntimeError(
                    f"resource {name} exited during startup with {proc.returncode}"
                )
            sock.send(Message(MessageType.STATUS, {}).to_bytes())
            events = dict(poller.poll(timeout=500))
            if sock in events:
                reply = Message.from_bytes(sock.recv())
                if reply.payload.get("status") == "ready":
                    log.debug("resource %s ready at %s", name, endpoint)
                    return ResourceHandle(name=name,
                                          endpoint=endpoint,
                                          process=proc)
        proc.kill()
        raise TimeoutError(
            f"resource {name} did not become ready within {ready_timeout}s")
    finally:
        sock.close()


@beartype
def stop_resource(handle: ResourceHandle) -> None:
    handle.process.terminate()
    try:
        handle.process.wait(timeout=5.0)
    except subprocess.TimeoutExpired:
        handle.process.kill()
        handle.process.wait(timeout=5.0)
    _unlink_ipc(handle.endpoint)


@beartype
def run_rpc_subprocess(
    module: str,
    init_payload: Dict[str, object],
    socket_dir: Path,
    timeout: float,
) -> RpcSubprocessResult:
    endpoint = make_socket_path(socket_dir, "rpc")
    zmq_ctx = zmq.Context.instance()
    sock = zmq_ctx.socket(zmq.DEALER)
    sock.setsockopt(zmq.LINGER, 2000)
    sock.bind(endpoint)

    proc = subprocess.Popen(
        [sys.executable, "-m", module, "--socket", endpoint],
        env=_child_env(),
    )
    sock.send(Message(MessageType.INIT, init_payload).to_bytes())

    poller = zmq.Poller()
    poller.register(sock, zmq.POLLIN)

    progress: List[Dict[str, object]] = []
    logs: List[Dict[str, object]] = []
    deadline = time.monotonic() + timeout
    try:
        while True:
            if deadline < time.monotonic():
                proc.kill()
                raise TimeoutError(
                    f"subprocess {module} timed out after {timeout}s")
            events = dict(poller.poll(timeout=200))
            if sock in events:
                msg = Message.from_bytes(sock.recv())
                if msg.type == MessageType.PROGRESS:
                    progress.append(msg.payload)
                elif msg.type == MessageType.LOG:
                    logs.append(msg.payload)
                elif msg.type == MessageType.RESULT:
                    _reap(proc)
                    return RpcSubprocessResult(
                        result=msg.payload,
                        progress_messages=progress,
                        log_messages=logs,
                    )
                elif msg.type == MessageType.ERROR:
                    _reap(proc)
                    raise RuntimeError(
                        f"subprocess {module} failed: {msg.payload.get('message')}"
                    )
                else:
                    raise RuntimeError(f"unexpected message type {msg.type}")
            else:
                code = proc.poll()
                if code is not None:
                    raise RuntimeError(
                        f"subprocess {module} exited with {code} before sending result"
                    )
    finally:
        sock.close()
        _unlink_ipc(endpoint)


@beartype
def _reap(proc: subprocess.Popen) -> None:
    try:
        proc.wait(timeout=5.0)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait(timeout=5.0)
