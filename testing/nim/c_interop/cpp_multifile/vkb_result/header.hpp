#include <system_error>
#include <vector>

#define VK_DEFINE_NON_DISPATCHABLE_HANDLE(object)                         \
    typedef struct object##_T* object;

VK_DEFINE_NON_DISPATCHABLE_HANDLE(VkImage)
VK_DEFINE_NON_DISPATCHABLE_HANDLE(VkImageView)

template <typename T>
class Result
{
};

struct Swapchain {
    Result<std::vector<VkImage>> get_images() {
    }
    std::vector<VkImageView> get_image_views() {
    }
};
