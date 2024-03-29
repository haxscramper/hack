#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov 19 11:45:24 2022

@author: haxscramper
"""

from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.firefox.options import Options
import os
from datetime import datetime
import requests

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import (
    TimeoutException,
    NoSuchElementException,
)
import pickle
from sqlalchemy import Column, Integer, Text
import sqlalchemy as sqa
from sqlalchemy.orm import declarative_base

SQLBase = declarative_base()


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


def get_content(driver, thread_id):
    # Get content of the thread specified by the thread id and return the
    # HTML of the page. Content is cached in the separate file
    thread_file = os.path.join(cache_dir, f"thread-{thread_id}")
    if os.path.exists(thread_file):
        content = ""
        with open(thread_file) as file:
            content = file.read()

        print(f"Cached content of thread {thread_id} from {thread_file}")
        return content

    else:
        print(f"Getting content for thread {thread_id} using driver")
        driver.get(f"https://forum.nim-lang.org/t/{thread_id}")

        element = WebDriverWait(driver, timeout).until(
            EC.presence_of_element_located((By.ID, "thread-title"))
        )

        if element:

            def get_moreload(driver):
                try:
                    return driver.find_element(
                        By.CSS_SELECTOR, 'div[class="information-title"]'
                    )

                except NoSuchElementException as e:
                    return None

            class no_load_more(object):
                # Custom selenium wait predicate -- used to wait for "load
                # more posts" to complete. The button itself disappears
                # immediately, so we need to wait
                def __init__(self, starting_posts):
                    self.posts = starting_posts

                # Custom predicate is repeatedly called for validation
                def __call__(self, driver):
                    soup = BeautifulSoup(driver.page_source, "html.parser")
                    now = len(soup.select('div[class="post"]'))
                    # Wait until there are more
                    return self.posts < now

            load_more = get_moreload(driver)

            if load_more:
                soup = BeautifulSoup(driver.page_source, "html.parser")
                start_number_of_posts = len(soup.select('div[class="post"]'))
                load_more.click()
                # Wait until remaining parts of the thread are loaded and
                # button disappears.
                WebDriverWait(driver, timeout).until(
                    no_load_more(start_number_of_posts)
                )

            soup = BeautifulSoup(driver.page_source, "html.parser")

            with open(thread_file, "w") as file:
                file.write(driver.page_source)

            print(f"Wrote content of thread {thread_id} to {thread_file}")

        else:
            print("Failed to load the page")


def get_post(post_id):
    post_file = os.path.join(cache_dir, f"post-{post_id}.rst")
    if os.path.exists(post_file):
        content = ""
        with open(post_file) as file:
            content = file.read()

        print(f"Cached content of post {post_id} from {post_file}")
        return content

    else:
        url = f"https://forum.nim-lang.org/post.rst?id={post_id}"
        try:
            response = requests.get(url, timeout=timeout)

            if response.status_code == 200:
                with open(post_file, "w") as file:
                    file.write(response.content.decode("utf-8"))

                print(f"Wrote content of post {post_id} to {post_file}")

            else:
                print(
                    f"Fetch of post {post_id} failed with code {response.status_code}"
                )

            return response.content

        except requests.exceptions.TimeoutError:
            return "READ TIMEOUT"


def get_posts(content, thread_id, session):
    soup = BeautifulSoup(content, "html.parser")
    for post in soup.select('div[class="post"]'):
        meta = post.select('div[class="post-metadata"]')[0]
        modified = meta.select('div[class="post-history"]')
        date_format = "%b %d, %Y %H:%M"

        modified = (
            datetime.strptime(
                modified[0]["title"].replace("Last modified ", ""),
                date_format,
            )
            if 0 < len(modified)
            else None
        )
        created = datetime.strptime(meta.select("a")[-1]["title"], date_format)
        url = meta.select("a")[-1]["href"]
        post_id = int(url.split("#")[1])
        content = get_post(post_id)
        author = post.select('div[class="post-username"]')[0].text
        session.add(
            Post(
                post_id=post_id,
                thread=thread_id,
                created=created.timestamp(),
                modified=modified.timestamp() if modified else None,
                url=url,
                text=content,
                author=str(author),
            )
        )


engine = sqa.create_engine(f"sqlite:////tmp/outdb.sqlite")
Session = sqa.orm.sessionmaker(bind=engine)
session = Session()
con = engine.connect()

SQLBase.metadata.drop_all(engine)

SQLBase.metadata.create_all(engine)
session.commit()


options = Options()
options.headless = True
driver = webdriver.Firefox(options=options)

bad_threads = set()
bad_threads_file = os.path.join(cache_dir, "bad_threads")
if os.path.exists(bad_threads_file):
    with open(bad_threads_file, "rb") as file:
        bad_threads = pickle.load(file)


def fetch_thread(thread):
    if thread in bad_threads:
        print(f"Skipping bad thread {thread}")
        return

    try:
        content = get_content(driver, thread)
        fail_count = 0
        while not content and fail_count < 5:
            content = get_content(driver, thread)
            print(f"No content for thread {thread}, retrying")
            fail_count += 1

        if not content and 5 <= fail_count:
            print(
                f"No content for thread {thread} after {fail_count} attempts, adding to bad list)"
            )
            bad_threads.add(thread)
            return

        get_posts(content, thread, session)
        session.flush()
        session.commit()

    except TimeoutException as e:
        bad_threads.add(thread)
        print(f"Thread {thread} failed to load in time, adding to bad list")

    # Dump list of bad threads to make quick interruption
    # to the looping less painful.
    with open(bad_threads_file, "wb+") as file:
        print("Updated bad thread list")
        pickle.dump(bad_threads, file)

    return


fetch_thread(9572)

for thread in range(9617, 1, -1):
    fetch_thread(thread)

session.close()


driver.quit()
