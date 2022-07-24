.ONESHELL:
target=libtooling_extractor

run:
	make $(target)

class_defintions: class_defintions.cpp
	clang++ class_definitions.cpp

variant_object: variant-object.cpp
	clang++ variant-object.cpp -o variant_object
	./variant_object

ogdf_test: ogdf_test.cpp
	clang++ -DLIBCXX_ENABLE_INCOMPLETE_FEATURES=ON -std=c++20 \
		ogdf_test.cpp -Iogdf/include -o ogdf_test -Logdf -lOGDF -lCOIN
	./ogdf_test

libtooling_matcher:
	cd use_clang
	clang++ libtooling_1.cpp -o libtooling_1 -lclang-cpp -lLLVM
	./libtooling_1 test.cpp --
	echo "done"

libtooling_extractor:
	cd use_clang
	clang++ libtooling_2.cpp -o libtooling_2 -lclang-cpp -lLLVM
	./libtooling_2 test.cpp --
	echo "done"
