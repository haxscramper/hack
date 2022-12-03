#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov 19 11:45:24 2022

@author: haxscramper
"""

import os
from datetime import datetime
import requests
import json

from sqlalchemy import Column, Integer, Text
import sqlalchemy as sqa
from sqlalchemy.orm import declarative_base

SQLBase = declarative_base()




import logging
from rich.logging import RichHandler


logging.basicConfig(
    level="NOTSET",
    format="%(message)s",
    datefmt="[%X]",
    handlers=[RichHandler(rich_tracebacks=True, markup=True)],
)

for name in logging.root.manager.loggerDict:
    logger = logging.getLogger(name)
    logger.setLevel(logging.WARNING)

log = logging.getLogger("rich")
log.setLevel(logging.DEBUG)


def IntColumn(nullable: bool = False):
    return Column(Integer, nullable=nullable)


def StrColumn(nullable: bool = False):
    return Column(Text, nullable=nullable)


def IdColumn():
    return Column(Integer, primary_key=True, autoincrement=True)


# Single data table with all posts
class Post(SQLBase):
    __tablename__ = "post"
    id = IdColumn()
    post_id = IntColumn()
    thread = IntColumn()
    text = StrColumn()
    author = StrColumn()
    created = IntColumn()
    url = StrColumn()
    modified = IntColumn(nullable=True)


# To save on requests and networking, cache each page and each post content
# here. This speeds up processing by several orders of magnitude.
cache_dir = os.path.expandvars("$HOME/.cache/nim_forum_fetch")

if not os.path.exists(cache_dir):
    os.mkdir(cache_dir)

# Selenium-based operations should complete in 20 seconds or less,
# otherwise threat it ias a timeout.
timeout = 20


def get_post(post_id):
    post_file = os.path.join(cache_dir, f"posts/post-{post_id}.rst")
    if os.path.exists(post_file):
        content = ""
        with open(post_file) as file:
            content = file.read()

        log.debug(f"Cached content of post {post_id}")
        return content

    else:
        url = f"https://forum.nim-lang.org/post.rst?id={post_id}"
        try:
            response = requests.get(url, timeout=timeout)

            if response.status_code == 200:
                with open(post_file, "w") as file:
                    file.write(response.content.decode("utf-8"))

                log.info(f"Wrote content of post {post_id}")

            else:
                log.warning(
                    f"Fetch of post {post_id} failed with code {response.status_code}"
                )

            return response.content

        except requests.exceptions.TimeoutError:
            return "READ TIMEOUT"

def get_request_json(url, key):
    response = None
    key_file = os.path.join(cache_dir, key)
    if os.path.exists(key_file):
        log.debug(f"cache request for {key}")
        with open(key_file, "r") as file:
            response = file.read()

    else:
        log.debug(url)
        response = requests.get(url, timeout=timeout).content.decode("utf-8")
        with open(key_file, "w") as file:
            file.write(response)

        log.info(f"wrote request to {key}")


    return response

def get_posts(thread_id, session):
    response = get_request_json(
        f"https://forum.nim-lang.org/posts.json?id={thread_id}",
        f"thread-posts-json-{thread_id}"
    )

    try:
        content = json.loads(response)
        for post in content["posts"]:
            post_id = post["id"]
            session.add(
                Post(
                    post_id=post_id,
                    thread=thread_id,
                    created=post["info"]["creation"],
                    url=f"https://forum.nim-lang.org/t/{thread_id}#{post_id}",
                    text=get_post(post_id),
                    author=str(post["author"]["name"]),
                )
            )
    except Exception as e:
        log.critical(str(e))



engine = sqa.create_engine(f"sqlite:////tmp/outdb.sqlite")
Session = sqa.orm.sessionmaker(bind=engine)
session = Session()
con = engine.connect()

SQLBase.metadata.drop_all(engine)
SQLBase.metadata.create_all(engine)
session.commit()

start = 0
count = 100
more_count = 9000000



try:
    while 0 < more_count:
        try:
            content = get_request_json(
                "https://forum.nim-lang.org/"
                + f"threads.json?start={start}&count={count}",
                f"thread-{start}-{count}.json",
            )
            content = json.loads(content)
            more_count = content["moreCount"]
            start += count
            for thread in content["threads"]:
                try:
                    get_posts(thread["id"], session)
                except requests.exceptions.ReadTimeout as e:
                    log.critical(f"Timeout on thread {thread['id']}")

        except requests.exceptions.ReadTimeout as e:
            log.critical(f"Timeout on range {start} + {count}")

except Exception as e:
    log.critical(f"Processing ended due to error {e}")
    pass

log.debug("Processing done")
session.commit()
session.close()
