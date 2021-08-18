.PHONY: main
main: main.c instrumented
	gcc -static-libgcc -Wl,--version-script=libc.version -pg main.c -o main
	# strace --trace="/openat" ./main 2>&1 | grep -E "(libc|ld)"
# ./main
	gprof --brief main

instrumented:
	clang -fprofile-instr-generate -fcoverage-mapping -ldl -pg main.c -o main_clang
	./main_clang
	gprof --brief main_clang


shared:
	g++ shared.cpp -fpic -shared -oshared.so
	g++ -ldl shared_user.cpp -o shared_main
	./shared_main
