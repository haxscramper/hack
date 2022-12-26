#!/usr/bin/env python

"""
Pandoc filter to convert all level 2+ headings to paragraphs with
emphasized text.
"""

from pandocfilters import toJSONFilter, Emph, Para
import sys
from urllib.parse import urlparse
from urllib.request import urlretrieve
import os

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)


def behead(key, value, format, meta):
    if key == "Image":
        rawlink = value[-1][0]
        link = urlparse(rawlink)
        split = link.path.split("/")
        ifile = split[-1] + ".png"
        if not os.path.exists(ifile):
            eprint("download", rawlink)


if __name__ == "__main__":
    toJSONFilter(behead)
