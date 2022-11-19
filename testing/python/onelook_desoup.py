#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Nov 19 11:45:24 2022

@author: haxscramper
"""

from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.firefox.options import Options

from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException


options = Options()
options.headless = True
driver = webdriver.Firefox(options=options)
driver.get("https://www.onelook.com/thesaurus/?s=yellow%20metal&res=res_1")

element = WebDriverWait(driver, 20).until(
    EC.presence_of_element_located((By.ID, "res_1"))
)

if element:
    soup = BeautifulSoup(driver.page_source, "html.parser")

    with open("/tmp/test.html", "w") as file:
        file.write(driver.page_source)

    items = [it for it in soup.select('span[class*="relres"]')]
    for item in items:
        print(item.select('b[class="thes_ql_title"]')[0])
        for description in item.select('li[class*="thes-defline"]'):
            print(f"    {description}")


else:
    print("Failed to load the page")

driver.quit()
