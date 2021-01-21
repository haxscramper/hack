main:
	antlr4 -o resdir -Dlanguage=Cpp typespec.g4
	clang++ \
		-I/usr/include/antlr4-runtime/ \
		-lantlr4-runtime \
		resdir/*.cpp \
		main.cpp

	./a.out
