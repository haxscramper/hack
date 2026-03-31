import json
import logging
import requests
from typing import List, Optional, Dict, Any

from models import PageData, DocTag

logger = logging.getLogger(__name__)

import os

	
LLAMA_SERVER_URL = "http://localhost:8080/v1/chat/completions"

def query_llama_cpp(messages: List[Dict[str, str]], json_format: bool = False) -> Optional[str]:
    payload = {
        "messages": messages,
        "temperature": 0.2,
    }
    if json_format:
        payload["response_format"] = {"type": "json_object"}

    resp = None
    try:
        resp = requests.post(LLAMA_SERVER_URL, json=payload, timeout=60)
        resp.raise_for_status()
        data = resp.json()
        return data["choices"][0]["message"]["content"]
    except Exception as e:
        logger.error(f"Error querying llama.cpp: {e}")
        if resp is not None and hasattr(resp, 'text'):
            logger.error(f"Response: {resp.text}")
        return None

def merge_blocks_llm(block1: str, block2: str) -> Optional[str]:
    prompt_system = (
        "You are an expert document editor. Your task is to seamlessly merge text blocks "
        "extracted from PDF pages into a clean HTML format.\n"
        "Rules:\n"
        "1. Fix broken paragraphs that span across multiple blocks.\n"
        "2. Remove hyphenations at the end of lines.\n"
        "3. Keep the original meaning and wording perfectly intact.\n"
        "4. Output ONLY the corrected text/HTML without any conversational filler."
    )
    prompt_user = f"[Block 1]: {block1}\n[Block 2]: {block2}\nPlease merge and correct these blocks."
    
    messages = [
        {"role": "system", "content": prompt_system},
        {"role": "user", "content": prompt_user}
    ]
    
    return query_llama_cpp(messages)

def flag_issues_llm(text: str) -> List[Dict[str, Any]]:
    prompt_system = (
        "You are a proofreader. Review the following OCR-extracted text and identify:\n"
        "1. Spelling mistakes or garbled text.\n"
        "2. Likely OCR artifacts (e.g., 'l' instead of '1', random symbols).\n"
        "3. Formatting inconsistencies.\n"
        "Output your findings as a strict JSON list of objects: "
        '[{"issue_type": "spelling", "original": "teh", "suggestion": "the", "context": "...teh dog..."}]'
    )
    
    messages = [
        {"role": "system", "content": prompt_system},
        {"role": "user", "content": text}
    ]
    
    response = query_llama_cpp(messages, json_format=False)
    if not response:
        return []
    
    try:
        # Extract json part if it's wrapped in markdown
        import re
        match = re.search(r'\[.*\]', response, re.DOTALL)
        if match:
            response = match.group(0)
        data = json.loads(response)
        if isinstance(data, list):
            return data
        return []
    except Exception as e:
        logger.error(f"Failed to parse LLM issues: {e}")
        return []
