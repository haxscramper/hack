const h = "header.hpp"

type
  StdVector*[T] {.importcpp: "std::vector", header: "<vector>".} = object


  VkImageView* {.importcpp, header: h.} = pointer
  VkImage* {.importcpp, header: h.} = pointer

  Result*[T] {.importcpp: "Result", header: h.} = object

  Swapchain* {.importcpp: "Swapchain", header: h.} = object

proc getImages*(s: Swapchain): Result[StdVector[VkImage]] {.
  importcpp: "#.get_images()", header: h.}

proc getImageViews*(s: Swapchain): Result[StdVector[VkImageView]] {.
  importcpp: "#.get_image_views()", header: h.}

var s: Swapchain
discard s.getImages()
discard s.getImageViews()
echo "done!"
