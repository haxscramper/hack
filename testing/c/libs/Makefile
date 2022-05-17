.PHONY: wordnet

WN=WordNet-3.0
WS=$(WN)/src
WL=$(WN)/lib

wordnet: export WNHOME = /usr/share/wordnet

wordnet:
	gcc -o wordnet *.c
	echo "compiled"
	./wordnet test -synsn
