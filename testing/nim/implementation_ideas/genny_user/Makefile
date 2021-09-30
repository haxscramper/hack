main: compile_lib compile_cxx

compile_lib:
	@nim c genny_main.nim
	@cp libgenny_main.so generated

compile_cxx:
	pwd
	clang++ -L$(CURDIR) -lgenny_main -ogenny_user_cpp genny_user.cpp
	export LD_LIBRARY_PATH=$(CURDIR) ; ./genny_user_cpp
