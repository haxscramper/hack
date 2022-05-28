.PHONY: wordnet

WN=WordNet-3.0
WS=$(WN)/src
WL=$(WN)/lib

wordnet: export WNHOME = /usr/share/wordnet

wordnet: *.c main.nim
	rm -rf nimcache
	nim c -r --nimcache=nimcache main.nim
