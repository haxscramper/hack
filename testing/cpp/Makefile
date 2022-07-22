target=ogdf_test
run:
	make $(target)
	@echo "running target"
	./$(target)
	@echo "ok"

class_defintions: class_defintions.cpp
	clang++ class_definitions.cpp

variant_object: variant-object.cpp
	clang++ variant-object.cpp -o variant_object

ogdf_test: ogdf_test.cpp
	clang++ -DLIBCXX_ENABLE_INCOMPLETE_FEATURES=ON -std=c++20 \
		ogdf_test.cpp -Iogdf/include -o ogdf_test -Logdf -lOGDF -lCOIN
