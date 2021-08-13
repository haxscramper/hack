.PHONY: main
main: main.c shared instrumented
	gcc -ldl -pg main.c -o main
	strace --trace="/openat" ./main 2>&1 | grep -E "(libc|ld)"
	./main
	gprof --brief main

instrumented:


shared:
	g++ shared.cpp -fpic -shared -oshared.so
	g++ -ldl shared_user.cpp -o shared_main
	readelf -s shared.so
	./shared_main
