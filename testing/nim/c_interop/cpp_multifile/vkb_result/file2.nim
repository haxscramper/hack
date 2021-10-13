{.emit: """
/*INCLUDESECTION*/
#include <vector>

/*TYPESECTION*/
template <typename T> struct Result { Result(const T& value) {} };
Result<std::vector<int>> get_image_views() {}
""".}

type
  StdVector*[T] {.importcpp: "std::vector", header: "<vector>".} = object
  Result*[T] {.importcpp: "Result".} = object

proc getImageViews*(): Result[StdVector[cint]]
  {.importcpp: "get_image_views()".}

proc main() =
  let bar = getImageViews()
