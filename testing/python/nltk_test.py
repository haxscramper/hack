#!/usr/bin/env python

import nltk

sentence = "cat, dog and mouse"
tokens = nltk.word_tokenize(sentence)
tagged = nltk.pos_tag(tokens)
entities = nltk.chunk.ne_chunk(tagged)

print(entities)
