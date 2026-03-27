#!/usr/bin/env python

import requests
import base64

model = "user-v4/joycaption-beta"
image_path = "/home/haxscramper/defaultdirs/input/Generated Image March 21, 2026 - 7_01PM.jpg"
prompt = "Write a long descriptive caption for this image in a formal tone."

with open(image_path, "rb") as f:
    image_b64 = base64.b64encode(f.read()).decode("utf-8")

response = requests.post(
    "http://localhost:11434/api/chat",
    json={
        "model": model,
        "messages": [
            {
                "role": "system",
                "content": "You are a helpful image captioner.",
            },
            {
                "role": "user",
                "content": prompt,
                "images": [image_b64],
            },
        ],
        "stream": False,
    },
)
response.raise_for_status()

data = response.json()
print(data["message"]["content"].strip())
