#!/usr/bin/env python 

import base64
import requests
import difflib
from pathlib import Path
from termcolor import colored
import re

from openai import OpenAI
from openai.types import ImagesResponse

client = OpenAI()

def download_image(result: ImagesResponse, output_path="/tmp/result.png"):
    if result.data[0].b64_json:
        # Download from base64 data
        image_base64 = result.data[0].b64_json
        image_bytes = base64.b64decode(image_base64)
        with open(output_path, "wb") as f:
            f.write(image_bytes)
        return output_path
    elif result.data[0].url:
        # Download from URL
        response = requests.get(result.data[0].url)
        response.raise_for_status()
        with open(output_path, "wb") as f:
            f.write(response.content)
        return output_path
    else:
        raise ValueError("No image data found in the response")

def preprocess_prompt(prompt):
    # Remove newlines and extra whitespace to match DALL-E's preprocessing
    return re.sub(r'\s+', ' ', prompt).strip()

def show_prompt_diff(original_prompt, revised_prompt):
    # Preprocess original prompt to match DALL-E's formatting
    processed_original = preprocess_prompt(original_prompt)
    
    # Split into words for word-level diff
    original_words = processed_original.split()
    revised_words = revised_prompt.split()
    
    # Get diff
    diff = difflib.ndiff(original_words, revised_words)
    
    result = []
    for word in diff:
        if word.startswith('+ '):
            result.append(colored(word[2:], 'green'))
        elif word.startswith('- '):
            result.append(colored(word[2:], 'red'))
        elif word.startswith('  '):
            result.append(word[2:])
    
    return ' '.join(result)

# Read the original prompt
original_prompt = Path("/tmp/prompt.txt").read_text()

# Generate the image
result = client.images.generate(model="gpt-image-1", prompt=original_prompt)

# Print the result
print(result)

# Download the image
image_path = download_image(result)
print(f"Image saved to {image_path}")

# Show the prompt diff
if result.data[0].revised_prompt:
    diff_output = show_prompt_diff(original_prompt, result.data[0].revised_prompt)
    print("\nPrompt differences:")
    print(diff_output)
