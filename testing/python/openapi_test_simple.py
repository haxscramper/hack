#!/usr/bin/env python

import argparse
from pprint import pprint

parser = argparse.ArgumentParser(description="Openapi test")
parser.add_argument("key", type=str, help="OpenAI key")

args = parser.parse_args()

import os
import openai
openai.api_key = args.key
result = openai.Completion.create(
  model="text-davinci-003",
  prompt="""
Brainstorm some ideas combining VR and fitness:
  """,
  max_tokens=200,
  temperature=0.5
)

pprint(result)
