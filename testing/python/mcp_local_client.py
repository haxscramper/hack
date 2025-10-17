#!/usr/bin/env python

import asyncio
from langchain_openai import ChatOpenAI
from mcp_use import MCPAgent, MCPClient
from os import getenv
from dotenv import load_dotenv

load_dotenv()

async def main() -> None:
    config = {
        "mcpServers": {
            "weather": {
                "command": "poetry",
                "type": "stdio",
                "args": [
                    "--directory",
                    "/home/haxscramper/workspace/repos/hack/testing/python/",
                    "run", "./mcp_local_server.py"
                ]
            }
        }
    }

    client = MCPClient.from_dict(config)
    
    llm = ChatOpenAI(
        api_key=getenv("OPENROUTER_API_KEY"),
        base_url="https://openrouter.ai/api/v1",
        model="anthropic/claude-haiku-4.5",
        default_headers={
            "HTTP-Referer": "https://example.com",
            "X-Title": "example",
        }
    )
    
    agent = MCPAgent(llm=llm, client=client)
    result = await agent.run("Get current weather in New york.")
    print(result)

asyncio.run(main())
