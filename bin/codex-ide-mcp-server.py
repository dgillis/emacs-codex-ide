#!/usr/bin/env python3
"""Minimal MCP bridge backed by a running Emacs instance."""

from __future__ import annotations

import argparse
import base64
from dataclasses import dataclass
import json
import os
import subprocess
import sys
from typing import Any


PROTOCOL_VERSION = "2024-11-05"
SERVER_INFO = {"name": "codex-ide-emacs-bridge", "version": "0.1.0"}
DEBUG_LOG_PATH = "/tmp/codex-ide-mcp-debug.log"


@dataclass(frozen=True)
class EmacsBridgeCommand:
    name: str
    description: str
    inputSchema: dict[str, Any]


COMMANDS = [
    EmacsBridgeCommand(
        name="get_all_open_file_buffers",
        description="List all currently open file-backed buffers in Emacs.",
        inputSchema={
            "type": "object",
            "properties": {},
            "additionalProperties": False,
        },
    ),
    EmacsBridgeCommand(
        name="get_buffer_info",
        description="Return metadata for a named Emacs buffer.",
        inputSchema={
            "type": "object",
            "properties": {"buffer": {"type": "string"}},
            "required": ["buffer"],
            "additionalProperties": False,
        },
    ),
    EmacsBridgeCommand(
        name="get_buffer_text",
        description="Return the full contents of a named Emacs buffer as a string.",
        inputSchema={
            "type": "object",
            "properties": {"buffer": {"type": "string"}},
            "required": ["buffer"],
            "additionalProperties": False,
        },
    ),
    EmacsBridgeCommand(
        name="get_diagnostics",
        description="Return Flymake or Flycheck diagnostics for a buffer name.",
        inputSchema={
            "type": "object",
            "properties": {"buffer": {"type": "string"}},
            "required": ["buffer"],
            "additionalProperties": False,
        },
    ),
    EmacsBridgeCommand(
        name="get_window_list",
        description="List visible windows in the selected frame and their buffers.",
        inputSchema={
            "type": "object",
            "properties": {},
            "additionalProperties": False,
        },
    ),
    EmacsBridgeCommand(
        name="ensure_file_buffer_open",
        description="Ensure a file-backed buffer exists without displaying it in a window.",
        inputSchema={
            "type": "object",
            "properties": {"path": {"type": "string"}},
            "required": ["path"],
            "additionalProperties": False,
        },
    ),
    EmacsBridgeCommand(
        name="view_file_buffer",
        description="Display a file-backed buffer in a non-selected window and optionally jump to line and column.",
        inputSchema={
            "type": "object",
            "properties": {
                "path": {"type": "string"},
                "line": {"type": "integer", "minimum": 1},
                "column": {"type": "integer", "minimum": 1},
            },
            "required": ["path"],
            "additionalProperties": False,
        },
    ),
    EmacsBridgeCommand(
        name="kill_file_buffer",
        description="Kill the buffer visiting a file, prompting if it has unsaved changes.",
        inputSchema={
            "type": "object",
            "properties": {"path": {"type": "string"}},
            "required": ["path"],
            "additionalProperties": False,
        },
    ),
    EmacsBridgeCommand(
        name="lisp_check_parens",
        description="Check a Lisp source file for mismatched parentheses and report the mismatch location when found.",
        inputSchema={
            "type": "object",
            "properties": {"path": {"type": "string"}},
            "required": ["path"],
            "additionalProperties": False,
        },
    ),
]
COMMANDS_BY_NAME = {command.name: command for command in COMMANDS}


def json_dumps(value: Any) -> bytes:
    return json.dumps(value, separators=(",", ":"), ensure_ascii=True).encode("utf-8")


def debug_log(*parts: object) -> None:
    try:
        with open(DEBUG_LOG_PATH, "a", encoding="utf-8") as handle:
            print(*parts, file=handle)
    except OSError:
        pass


def read_message() -> dict[str, Any] | None:
    while True:
        line = sys.stdin.buffer.readline()
        debug_log("stdin line bytes:", repr(line))
        if not line:
            debug_log("stdin closed before message")
            return None
        if line in (b"\r\n", b"\n"):
            break
        return json.loads(line.decode("utf-8"))


def write_message(payload: dict[str, Any]) -> None:
    body = json_dumps(payload)
    sys.stdout.buffer.write(body)
    sys.stdout.buffer.write(b"\n")
    sys.stdout.buffer.flush()


class EmacsProxy:
    def __init__(self, emacsclient: str, server_name: str | None) -> None:
        self.emacsclient = emacsclient
        self.server_name = server_name

    def _elisp_string(self, value: str) -> str:
        return json.dumps(value, ensure_ascii=True)

    def _tool_call_expression(self, name: str, params: dict[str, Any]) -> str:
        payload = json.dumps({"name": name, "params": params}, separators=(",", ":"), ensure_ascii=True)
        return (
            "(base64-encode-string "
            f"(encode-coding-string (codex-ide-mcp-bridge--json-tool-call {self._elisp_string(payload)}) 'utf-8) t)"
        )

    def call_tool(self, name: str, params: dict[str, Any] | None = None) -> Any:
        params = params or {}
        command = [self.emacsclient]
        if self.server_name:
            command.extend(["-s", self.server_name])
        command.extend(["--eval", self._tool_call_expression(name, params)])
        debug_log("dispatch command:", command)
        completed = subprocess.run(
            command,
            capture_output=True,
            check=False,
        )
        debug_log("dispatch return code:", completed.returncode)
        debug_log("dispatch stdout:", repr(completed.stdout))
        debug_log("dispatch stderr:", repr(completed.stderr))
        if completed.returncode != 0:
            stderr = completed.stderr.strip() or completed.stdout.strip() or b"emacsclient failed"
            raise RuntimeError(stderr.decode("utf-8", errors="replace"))
        try:
            encoded = json.loads(completed.stdout.decode("utf-8"))
            if not isinstance(encoded, str):
                raise RuntimeError("invalid bridge response: expected base64 string")
            decoded = base64.b64decode(encoded)
            return json.loads(decoded.decode("utf-8"))
        except (ValueError, UnicodeDecodeError, base64.binascii.Error) as exc:
            raise RuntimeError(f"invalid bridge response: {exc}") from exc


def text_result(text: str, *, is_error: bool = False) -> dict[str, Any]:
    result: dict[str, Any] = {"content": [{"type": "text", "text": text}]}
    if is_error:
        result["isError"] = True
    return result


def schema_for_tools() -> list[dict[str, Any]]:
    return [
        {
            "name": command.name,
            "description": command.description,
            "inputSchema": command.inputSchema,
        }
        for command in COMMANDS
    ]


def handle_tool_call(proxy: EmacsProxy, name: str, arguments: dict[str, Any]) -> dict[str, Any]:
    if name not in COMMANDS_BY_NAME:
        return text_result(f"Unknown tool: {name}", is_error=True)
    result = proxy.call_tool(name, arguments)
    return text_result(json.dumps(result, indent=2, sort_keys=True))


def main() -> int:
    debug_log("--- mcp process start ---")
    debug_log("argv:", sys.argv)
    debug_log("cwd:", os.getcwd())
    parser = argparse.ArgumentParser()
    parser.add_argument("--emacsclient", default="emacsclient")
    parser.add_argument("--server-name", default=None)
    args = parser.parse_args()
    debug_log("parsed args:", args)

    proxy = EmacsProxy(args.emacsclient, args.server_name)

    while True:
        message = read_message()
        if message is None:
            debug_log("message loop exiting: no message")
            return 0
        method = message.get("method")
        request_id = message.get("id")
        params = message.get("params") or {}
        debug_log("received method:", method, "id:", request_id)

        try:
            if method == "initialize":
                write_message(
                    {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": {
                            "protocolVersion": PROTOCOL_VERSION,
                            "serverInfo": SERVER_INFO,
                            "capabilities": {"tools": {}},
                        },
                    }
                )
            elif method == "notifications/initialized":
                continue
            elif method == "ping":
                write_message({"jsonrpc": "2.0", "id": request_id, "result": {}})
            elif method == "tools/list":
                write_message(
                    {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": {"tools": schema_for_tools()},
                    }
                )
            elif method == "tools/call":
                write_message(
                    {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "result": handle_tool_call(
                            proxy,
                            params.get("name", ""),
                            params.get("arguments") or {},
                        ),
                    }
                )
            else:
                write_message(
                    {
                        "jsonrpc": "2.0",
                        "id": request_id,
                        "error": {
                            "code": -32601,
                            "message": f"Method not found: {method}",
                        },
                    }
                )
        except Exception as exc:  # pragma: no cover - protocol safety net
            write_message(
                {
                    "jsonrpc": "2.0",
                    "id": request_id,
                    "result": text_result(str(exc), is_error=True),
                }
            )


if __name__ == "__main__":
    raise SystemExit(main())
