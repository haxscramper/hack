main: compile_lib compile_test compile_cxx

compile_lib:
	@nim c genny_main.nim
	@cp libgenny_main.so generated

compile_cxx:
	pwd
	clang++ \
		-I$(HOME)/.choosenim/toolchains/nim-1.4.8/lib \
		-I$(CURDIR) \
		-L$(CURDIR) \
		-lgenny_main -ogenny_user_cpp genny_user.cpp
	export LD_LIBRARY_PATH=$(CURDIR) ; ./genny_user_cpp

compile_test:
	nim c --linetrace:off --stacktrace:off -d:release --nimcache:cache input_test.nim
	clang-format -i cache/*.c
