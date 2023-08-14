#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 13 12:41:15 2023

@author: haxscramper
"""

import spacy
from collections import Counter

# Load the spaCy model
nlp = spacy.load("en_core_web_sm")


def count_words_in_text(filename):
    # Read the file
    with open(filename, "r") as file:
        text = file.read()

    # Process the text using spaCy
    doc = nlp(text)

    # Use a Counter to count word occurrences
    word_freq = Counter()
    pos_counts = {"ADJ": 0, "VERB": 0, "NOUN": 0}

    for token in doc:
        if not token.is_punct and not token.is_stop:
            word_freq[token.lemma_.lower()] += 1

        # Count POS
        if token.pos_ in pos_counts:
            pos_counts[token.pos_] += 1

    return word_freq, pos_counts


if __name__ == "__main__":
    filename = "/tmp/text.txt"
    word_freq, pos_counts = count_words_in_text(filename)

    with open("/tmp/result.txt", "w") as file:
        # Print words sorted by frequency
        for word, freq in word_freq.most_common():
            print(f"{word}: {freq}", file=file)

        # Print POS counts
        print("\nPart-of-speech counts:", file=file)
        for pos, count in pos_counts.items():
            print(f"{pos}: {count}", file=file)
