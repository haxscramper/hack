template <typename C>
struct Generic {
    void impl() {
    }
};

using Explicit = Generic<int>;
