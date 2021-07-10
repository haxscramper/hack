main: file.nim
	rm -rf cache
	rm -rf html
	nim c \
		--nimcache:cache							\
		--debugger:native							\
		--passc:--coverage						\
		--passl:--coverage						\
		--out:file.bin file.nim

	./file.bin
	cd cache && gcov --json-format *.o > /dev/null
	cd cache && gunzip *.gz
