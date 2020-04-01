#!/usr/bin/env python
import json
import argparse

parser = argparse.ArgumentParser(
    description="Pretty-print data from smartctl"
)

parser.add_argument("file", metavar="file", help="Json file with data")

args = parser.parse_args()
data = json.load(open(args.file))

descr = []

def register(name, val):
    descr.append([name, val])

register("model", data["model_name"])
register("type", data["device"]["type"])
register("logical block size", data["logical_block_size"])
register("capacity (GB)", int(data["user_capacity"]["bytes"] / 1024 ** 3))


for line in descr:
    print(f"\033[31m{line[0]}:\033[0m {line[1]}")

