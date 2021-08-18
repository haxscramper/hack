testall:
	clang++ -O3 pure_cxx.cpp -opure_cxx
	./pure_cxx
	nim cpp --cc:clang -o:nim_and_cxx switch_vs_dispatch.nim
	./nim_and_cxx
