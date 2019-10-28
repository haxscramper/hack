#!/usr/bin/env python
# coding: utf-8

# Create wordcloud images for each message*.html file in current directory and
# one big image for all

from bs4 import BeautifulSoup
import itertools
from glob import glob
from wordcloud import *

from nltk.tokenize import word_tokenize

banned_words = None

try:
    banned_words = set([line.strip() for line in open("banned.txt").readlines()])
except:
    banned_words = set([])

def filter_msg(lines):
    tokens = itertools.chain(*[word_tokenize(line) for line in lines])
    return list(filter(lambda word: not (word in banned_words), tokens))

def get_text(pattern):
    text = ""

    raw_text = ""

    for file in glob(pattern):
        soup = BeautifulSoup(open(file).read(), 'html.parser')

        msgs = [node.findAll(text=True) for node in soup.findAll("div", {'class': 'text'})]
        text += "".join(["\n".join(filter_msg(msg)) for msg in msgs])
        raw_text += "".join(["\n".join(msg) for msg in msgs])

    with open("text", "w") as f:
        f.write(text)

    with open("raw_text", "w") as f:
        f.write(raw_text)

    return text



def generate_image(text, name = "image", width = 1000, height = 1000, max_words = 200):
    import matplotlib.pyplot as plt

    wordcloud = WordCloud(width = width, height = height, max_words = max_words).generate(text)
    plt.figure(figsize=(30,30))
    plt.imshow(wordcloud)
    plt.axis("off")
    plt.savefig(name + ".png", bbox_inches='tight')
    plt.close()


for file in glob("messages*.html"):
    generate_image(get_text(file), name = file + "wordcloud")


generate_image(get_text("messages*.html"), width = 3000, height = 3000, max_words = 2000)
