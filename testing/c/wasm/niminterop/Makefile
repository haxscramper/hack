autoreload:
	echo -e "Makefile\nbuild.sh\nmain.nim\nmain.c\nbase.html" | entr -rc make run

compile:
	./build.sh

run: compile
	python3 -m http.server
