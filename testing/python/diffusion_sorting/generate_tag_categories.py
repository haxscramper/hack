#!/usr/bin/env python
import csv
import requests
from pathlib import Path
import logging

root = Path("reference_tag_categories")
root.mkdir(parents=True, exist_ok=True)

def download_file(name: str, url: str):
    if "pastebin.com" in url:
        raw_url = url.replace("/pastebin.com/", "/pastebin.com/raw/")
    elif "rentry.org" in url:
        raw_url = url + "/raw"
    else:
        return

    response = requests.get(raw_url)
    if response.status_code == 200:
        with open(root.joinpath(f"{name}.txt"), "w") as file:
            file.write(f"# {url}\n")
            file.write(response.text)

    logging.info(f"{name} {url} ok")

def process_csv(file_path: str):
    with open(file_path, newline="") as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            download_file(row[0], row[1])

# Example usage:
process_csv("links.csv")
