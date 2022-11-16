#!/usr/bin/env python

from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.firefox.options import Options

options = Options()
options.headless = True
browser = webdriver.Firefox(options=options)
browser.get("https://www.onelook.com/thesaurus/?s=yellow%20metal")

soup = BeautifulSoup(browser.page_source, "html.parser")

with open("/tmp/test.html", "w") as file:
    file.write(soup.prettify())

print(soup.select('span[class*="res"]'))
