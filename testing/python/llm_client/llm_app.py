#!/usr/bin/env python

import asyncio
import json
import logging
import os
import sys
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional

import requests
from PyQt6.QtCore import QThread, pyqtSignal, Qt
from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QVBoxLayout, QHBoxLayout, 
    QWidget, QTextEdit, QPushButton, QListWidget,
    QSplitter, QMessageBox, QInputDialog, QComboBox, QLabel,
    QCheckBox, QPlainTextEdit
)
from PyQt6.QtGui import QKeySequence, QShortcut

from mcp_use import MCPAgent, MCPClient
from langchain_openai import ChatOpenAI

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s")

@dataclass
class Message:
    role: str
    content: str

@dataclass
class Chat:
    id: str
    title: str
    messages: List[Message]
    model: str = "openai/gpt-3.5-turbo"

class OpenRouterClient:
    def __init__(self, api_key: str):
        self.api_key = api_key
        self.base_url = "https://openrouter.ai/api/v1"
    
    def send_message(self, messages: List[Dict[str, str]], model: str) -> str:
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
            "HTTP-Referer": "https://example.com",
            "X-Title": "OpenRouter Chat"
        }
        
        data = {
            "model": model,
            "messages": messages
        }
        
        response = requests.post(
            f"{self.base_url}/chat/completions",
            headers=headers,
            json=data
        )
        response.raise_for_status()
        
        result = response.json()
        return result["choices"][0]["message"]["content"]

class ChatWorker(QThread):
    message_received = pyqtSignal(str)
    error_occurred = pyqtSignal(str)
    
    def __init__(self, client: OpenRouterClient, messages: List[Dict[str, str]], model: str, mcp_config: Optional[Dict]):
        super().__init__()
        self.client = client
        self.messages = messages
        self.model = model
        self.mcp_config = mcp_config
    
    def run(self):
        try:
            if self.mcp_config:
                asyncio.run(self._run_with_mcp())
            else:
                response = self.client.send_message(self.messages, self.model)
                self.message_received.emit(response)
        except Exception as e:
            self.error_occurred.emit(str(e))
    
    async def _run_with_mcp(self):
        mcp_client = MCPClient.from_dict(self.mcp_config)
        
        llm = ChatOpenAI(
            api_key=self.client.api_key,
            base_url=self.client.base_url,
            model=self.model,
            default_headers={
                "HTTP-Referer": "https://example.com",
                "X-Title": "OpenRouter Chat",
            }
        )
        
        agent = MCPAgent(llm=llm, client=mcp_client)
        
        last_message = self.messages[-1]["content"] if self.messages else ""
        result = await agent.run(last_message)
        self.message_received.emit(str(result))

class ChatStorage:
    def __init__(self, storage_path: Path):
        self.storage_path = storage_path
        self.storage_path.mkdir(parents=True, exist_ok=True)
        self.chats_file = self.storage_path / "chats.json"
        self.config_file = self.storage_path / "config.json"
    
    def load_chats(self) -> List[Chat]:
        if not self.chats_file.exists():
            return []
        
        with open(self.chats_file, "r", encoding="utf-8") as f:
            data = json.load(f)
        
        chats = []
        for chat_data in data:
            messages = [Message(**msg) for msg in chat_data["messages"]]
            chats.append(Chat(
                id=chat_data["id"],
                title=chat_data["title"],
                messages=messages,
                model=chat_data.get("model", "openai/gpt-3.5-turbo")
            ))
        return chats
    
    def save_chats(self, chats: List[Chat]):
        data = []
        for chat in chats:
            chat_data = {
                "id": chat.id,
                "title": chat.title,
                "model": chat.model,
                "messages": [{"role": msg.role, "content": msg.content} for msg in chat.messages]
            }
            data.append(chat_data)
        
        with open(self.chats_file, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2, ensure_ascii=False)
    
    def load_mcp_config(self) -> Optional[Dict]:
        if not self.config_file.exists():
            return None
        
        with open(self.config_file, "r", encoding="utf-8") as f:
            return json.load(f)
    
    def save_mcp_config(self, config: Dict):
        with open(self.config_file, "w", encoding="utf-8") as f:
            json.dump(config, f, indent=2, ensure_ascii=False)

class MultiLineTextEdit(QPlainTextEdit):
    send_message = pyqtSignal()
    
    def __init__(self):
        super().__init__()
        self.setMaximumHeight(100)
    
    def keyPressEvent(self, event):
        if event.key() == Qt.Key.Key_Return and event.modifiers() == Qt.KeyboardModifier.ControlModifier:
            self.send_message.emit()
        else:
            super().keyPressEvent(event)

class ChatApplication(QMainWindow):
    def __init__(self):
        super().__init__()
        self.api_key = os.getenv("OPENROUTER_API_KEY")
        if not self.api_key:
            QMessageBox.critical(self, "Error", "OPENROUTER_API_KEY environment variable not set")
            sys.exit(1)
        
        self.client = OpenRouterClient(self.api_key)
        self.storage = ChatStorage(Path.home() / ".openrouter_chat")
        self.chats: List[Chat] = self.storage.load_chats()
        self.mcp_config = self.storage.load_mcp_config()
        self.current_chat: Optional[Chat] = None
        self.worker: Optional[ChatWorker] = None
        
        self.models = [
            "anthropic/claude-haiku-4.5",
        ]
        
        self.setup_ui()
        self.load_chat_list()
    
    def setup_ui(self):
        self.setWindowTitle("OpenRouter Chat")
        self.setGeometry(100, 100, 1200, 800)
        
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        
        main_layout = QHBoxLayout(central_widget)
        
        splitter = QSplitter()
        main_layout.addWidget(splitter)
        
        # Chat list panel
        chat_panel = QWidget()
        chat_layout = QVBoxLayout(chat_panel)
        
        self.new_chat_button = QPushButton("New Chat")
        self.new_chat_button.clicked.connect(self.new_chat)
        chat_layout.addWidget(self.new_chat_button)
        
        self.chat_list = QListWidget()
        self.chat_list.itemClicked.connect(self.select_chat)
        chat_layout.addWidget(self.chat_list)
        
        splitter.addWidget(chat_panel)
        
        # Chat panel
        chat_widget = QWidget()
        chat_widget_layout = QVBoxLayout(chat_widget)
        
        # Model selection and MCP controls
        controls_layout = QHBoxLayout()
        
        controls_layout.addWidget(QLabel("Model:"))
        self.model_combo = QComboBox()
        self.model_combo.addItems(self.models)
        self.model_combo.currentTextChanged.connect(self.model_changed)
        controls_layout.addWidget(self.model_combo)
        
        self.mcp_checkbox = QCheckBox("Use MCP")
        controls_layout.addWidget(self.mcp_checkbox)
        
        self.mcp_config_button = QPushButton("Configure MCP")
        self.mcp_config_button.clicked.connect(self.configure_mcp)
        controls_layout.addWidget(self.mcp_config_button)
        
        controls_layout.addStretch()
        chat_widget_layout.addLayout(controls_layout)
        
        self.chat_display = QTextEdit()
        self.chat_display.setReadOnly(True)
        chat_widget_layout.addWidget(self.chat_display)
        
        input_layout = QVBoxLayout()
        
        self.message_input = MultiLineTextEdit()
        self.message_input.setPlaceholderText("Type your message... (Ctrl+Enter to send)")
        self.message_input.send_message.connect(self.send_message)
        input_layout.addWidget(self.message_input)
        
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        self.send_button = QPushButton("Send (Ctrl+Enter)")
        self.send_button.clicked.connect(self.send_message)
        button_layout.addWidget(self.send_button)
        
        input_layout.addLayout(button_layout)
        chat_widget_layout.addLayout(input_layout)
        
        splitter.addWidget(chat_widget)
        splitter.setSizes([250, 950])
    
    def configure_mcp(self):
        current_config = json.dumps(self.mcp_config or {}, indent=2)
        config_text, ok = QInputDialog.getMultiLineText(
            self, "MCP Configuration", "Enter MCP configuration (JSON):", current_config
        )
        
        if ok:
            try:
                self.mcp_config = json.loads(config_text)
                self.storage.save_mcp_config(self.mcp_config)
            except json.JSONDecodeError as e:
                QMessageBox.critical(self, "Invalid JSON", f"Failed to parse JSON: {e}")
    
    def model_changed(self):
        if self.current_chat:
            self.current_chat.model = self.model_combo.currentText()
            self.storage.save_chats(self.chats)
    
    def load_chat_list(self):
        self.chat_list.clear()
        for chat in self.chats:
            self.chat_list.addItem(chat.title)
    
    def new_chat(self):
        model = self.model_combo.currentText()
        default_title = f"{datetime.now().strftime('%Y-%m-%d %H:%M')} - {model.split('/')[-1]}"
        
        title, ok = QInputDialog.getText(self, "New Chat", "Chat name:", text=default_title)
        if not ok:
            return
        
        import uuid
        chat_id = str(uuid.uuid4())
        chat = Chat(id=chat_id, title=title, messages=[], model=model)
        self.chats.append(chat)
        self.storage.save_chats(self.chats)
        self.load_chat_list()
        self.current_chat = chat
        self.chat_list.setCurrentRow(len(self.chats) - 1)
        self.update_chat_display()
        self.model_combo.setCurrentText(chat.model)
    
    def select_chat(self):
        current_row = self.chat_list.currentRow()
        if 0 <= current_row < len(self.chats):
            self.current_chat = self.chats[current_row]
            self.update_chat_display()
            self.model_combo.setCurrentText(self.current_chat.model)
    
    def update_chat_display(self):
        if not self.current_chat:
            self.chat_display.clear()
            return
        
        display_text = ""
        for message in self.current_chat.messages:
            display_text += f"{message.role.upper()}: {message.content}\n\n"
        
        self.chat_display.setPlainText(display_text)
        cursor = self.chat_display.textCursor()
        cursor.movePosition(cursor.MoveOperation.End)
        self.chat_display.setTextCursor(cursor)
    
    def send_message(self):
        if not self.current_chat:
            return
        
        message_text = self.message_input.toPlainText().strip()
        if not message_text:
            return
        
        user_message = Message(role="user", content=message_text)
        self.current_chat.messages.append(user_message)
        
        self.message_input.clear()
        self.update_chat_display()
        
        self.send_button.setEnabled(False)
        self.message_input.setEnabled(False)
        
        messages_for_api = [{"role": msg.role, "content": msg.content} for msg in self.current_chat.messages]
        
        self.worker = ChatWorker(
            self.client, 
            messages_for_api, 
            self.current_chat.model,
            self.mcp_config
        )
        self.worker.message_received.connect(self.on_message_received)
        self.worker.error_occurred.connect(self.on_error)
        self.worker.start()
    
    def on_message_received(self, response: str):
        if self.current_chat:
            assistant_message = Message(role="assistant", content=response)
            self.current_chat.messages.append(assistant_message)
            self.update_chat_display()
            self.storage.save_chats(self.chats)
        
        self.send_button.setEnabled(True)
        self.message_input.setEnabled(True)
        self.message_input.setFocus()
    
    def on_error(self, error_message: str):
        QMessageBox.critical(self, "Error", f"Failed to send message: {error_message}")
        self.send_button.setEnabled(True)
        self.message_input.setEnabled(True)
        self.message_input.setFocus()
    
    def closeEvent(self, event):
        self.storage.save_chats(self.chats)
        event.accept()

if __name__ == "__main__":
    app = QApplication(sys.argv)
    window = ChatApplication()
    window.show()
    sys.exit(app.exec())
