#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  9 17:36:18 2022

@author: haxscramper
"""

import re

s = r"Clearly, she has no idea who she\u2019s dealing with. 1"

re.split(r"\\u[0-9abcdefABCDEF]{4}", s)
