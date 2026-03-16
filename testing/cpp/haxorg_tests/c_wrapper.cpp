#include <iostream>
#include <vector>
#include <string>
#include <optional>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <cstdint>
#include <cstdio>
#include <cstring>

// ============================================================================
// 1. C API DECLARATIONS (Normally in a .h file)
// ============================================================================
extern "C" {

struct CContext {
    int  has_error;
    char error_msg[256];
};

struct CStdVector;
struct CStdVectorVTable {
    size_t (*size)(const struct CStdVector* self, struct CContext* ctx);
    void* (*back)(const struct CStdVector* self, struct CContext* ctx);
    void* (*at)(
        const struct CStdVector* self,
        size_t                   index,
        struct CContext*         ctx);
    void (*pop_back)(struct CStdVector* self, struct CContext* ctx);
    void (*destroy)(struct CStdVector* self, struct CContext* ctx);
};
struct CStdVector {
    void*                          instance;
    const struct CStdVectorVTable* vtable;
};

struct CStdString;
struct CStdStringVTable {
    size_t (*size)(const struct CStdString* self, struct CContext* ctx);
    const char* (
        *c_str)(const struct CStdString* self, struct CContext* ctx);
    void (*destroy)(struct CStdString* self, struct CContext* ctx);
};
struct CStdString {
    void*                          instance;
    const struct CStdStringVTable* vtable;
};

struct CStdOptional;
struct CStdOptionalVTable {
    int (
        *has_value)(const struct CStdOptional* self, struct CContext* ctx);
    void* (*value)(const struct CStdOptional* self, struct CContext* ctx);
    void (*destroy)(struct CStdOptional* self, struct CContext* ctx);
};
struct CStdOptional {
    void*                            instance;
    const struct CStdOptionalVTable* vtable;
};

// --- Public C API ---
struct CStdString api_get_string_unique(struct CContext* ctx);
void api_process_string(struct CContext* ctx, struct CStdString str);

struct CStdOptional api_get_optional_shared(struct CContext* ctx);
void api_process_optional(struct CContext* ctx, struct CStdOptional opt);

struct CStdVector api_get_vector_ref(struct CContext* ctx);
void api_process_vector(struct CContext* ctx, struct CStdVector vec);
}

// ============================================================================
// 2. C++ WRAPPER INFRASTRUCTURE (Normally in a .hpp / .cpp file)
// ============================================================================

enum class MemoryPolicy : uintptr_t
{
    Reference = 0,
    Unique    = 1,
    Shared    = 2
};

inline void* tag_pointer(void* raw_ptr, MemoryPolicy policy) {
    return (void*)((uintptr_t)raw_ptr | (uintptr_t)policy);
}
inline MemoryPolicy get_pointer_policy(void* tagged_ptr) {
    return static_cast<MemoryPolicy>((uintptr_t)tagged_ptr & 3ULL);
}
inline void* untag_pointer(void* tagged_ptr) {
    return (void*)((uintptr_t)tagged_ptr & ~3ULL);
}

inline void clear_context(CContext* ctx) {
    if (ctx) {
        ctx->has_error    = 0;
        ctx->error_msg[0] = '\0';
    }
}
inline void set_context_error(CContext* ctx, const char* msg) {
    if (ctx) {
        ctx->has_error = 1;
        std::snprintf(ctx->error_msg, sizeof(ctx->error_msg), "%s", msg);
    }
}

// --- Generic Unwrap & Destroy Free Functions ---
template <typename CoreT>
CoreT* unwrap_instance(void* tagged_instance, CContext* ctx = nullptr) {
    if (!tagged_instance) {
        set_context_error(ctx, "Instance pointer is null");
        return nullptr;
    }
    MemoryPolicy policy  = get_pointer_policy(tagged_instance);
    void*        raw_ptr = untag_pointer(tagged_instance);

    if (policy == MemoryPolicy::Shared) {
        return static_cast<std::shared_ptr<CoreT>*>(raw_ptr)->get();
    }
    return static_cast<CoreT*>(raw_ptr);
}

template <typename CoreT>
void destroy_instance(void** tagged_instance_ptr, CContext* ctx) {
    clear_context(ctx);
    try {
        if (!tagged_instance_ptr || !*tagged_instance_ptr) { return; }
        void*        tagged_instance = *tagged_instance_ptr;
        MemoryPolicy policy          = get_pointer_policy(tagged_instance);
        void*        raw_ptr         = untag_pointer(tagged_instance);

        if (policy == MemoryPolicy::Shared) {
            delete static_cast<std::shared_ptr<CoreT>*>(raw_ptr);
        } else if (policy == MemoryPolicy::Unique) {
            delete static_cast<CoreT*>(raw_ptr);
        }

        *tagged_instance_ptr = nullptr;
    } catch (const std::exception& e) {
        set_context_error(ctx, e.what());
    } catch (...) {
        set_context_error(ctx, "Unknown exception during destroy");
    }
}

// --- VTable Implementations ---
template <typename T>
struct VectorVTableImpl {
    static size_t size(const CStdVector* self, CContext* ctx) {
        clear_context(ctx);
        auto* vec = unwrap_instance<std::vector<T>>(self->instance, ctx);
        return vec ? vec->size() : 0;
    }
    static void* back(const CStdVector* self, CContext* ctx) {
        clear_context(ctx);
        auto* vec = unwrap_instance<std::vector<T>>(self->instance, ctx);
        if (!vec || vec->empty()) {
            set_context_error(ctx, "Vector is empty or null");
            return nullptr;
        }
        return static_cast<void*>(&vec->back());
    }
    static void* at(const CStdVector* self, size_t index, CContext* ctx) {
        clear_context(ctx);
        try {
            auto* vec = unwrap_instance<std::vector<T>>(
                self->instance, ctx);
            return vec ? static_cast<void*>(&vec->at(index)) : nullptr;
        } catch (const std::exception& e) {
            set_context_error(ctx, e.what());
            return nullptr;
        }
    }
    static void pop_back(CStdVector* self, CContext* ctx) {
        clear_context(ctx);
        auto* vec = unwrap_instance<std::vector<T>>(self->instance, ctx);
        if (vec && !vec->empty()) { vec->pop_back(); }
    }
    static void destroy(CStdVector* self, CContext* ctx) {
        destroy_instance<std::vector<T>>(&self->instance, ctx);
    }
    static const CStdVectorVTable* get_vtable() {
        static const CStdVectorVTable vtable = {
            &size, &back, &at, &pop_back, &destroy};
        return &vtable;
    }
};

struct StringVTableImpl {
    static size_t size(const CStdString* self, CContext* ctx) {
        clear_context(ctx);
        auto* str = unwrap_instance<std::string>(self->instance, ctx);
        return str ? str->size() : 0;
    }
    static const char* c_str(const CStdString* self, CContext* ctx) {
        clear_context(ctx);
        auto* str = unwrap_instance<std::string>(self->instance, ctx);
        return str ? str->c_str() : nullptr;
    }
    static void destroy(CStdString* self, CContext* ctx) {
        destroy_instance<std::string>(&self->instance, ctx);
    }
    static const CStdStringVTable* get_vtable() {
        static const CStdStringVTable vtable = {&size, &c_str, &destroy};
        return &vtable;
    }
};

template <typename T>
struct OptionalVTableImpl {
    static int has_value(const CStdOptional* self, CContext* ctx) {
        clear_context(ctx);
        auto* opt = unwrap_instance<std::optional<T>>(self->instance, ctx);
        return (opt && opt->has_value()) ? 1 : 0;
    }
    static void* value(const CStdOptional* self, CContext* ctx) {
        clear_context(ctx);
        try {
            auto* opt = unwrap_instance<std::optional<T>>(
                self->instance, ctx);
            return opt ? static_cast<void*>(&opt->value()) : nullptr;
        } catch (const std::exception& e) {
            set_context_error(ctx, e.what());
            return nullptr;
        }
    }
    static void destroy(CStdOptional* self, CContext* ctx) {
        destroy_instance<std::optional<T>>(&self->instance, ctx);
    }
    static const CStdOptionalVTable* get_vtable() {
        static const CStdOptionalVTable vtable = {
            &has_value, &value, &destroy};
        return &vtable;
    }
};


// --- Magic Auto-Unwrapper System ---

// Helper macro/struct to extract C++ ptr or safely throw on ref binding
template <typename CoreT, typename CHandleT>
struct CoreUnwrapper {
    static CoreT* unwrap_ptr(CHandleT handle, CContext* ctx) {
        return unwrap_instance<CoreT>(handle.instance, ctx);
    }
    static CoreT& unwrap_ref(CHandleT handle, CContext* ctx) {
        CoreT* ptr = unwrap_ptr(handle, ctx);
        if (!ptr) {
            throw std::runtime_error(
                "Attempted to bind null handle to C++ reference/value");
        }
        return *ptr;
    }
};

template <typename CppType, typename CType>
struct ArgUnwrapper;

// Fallback for primitives passed as-is (e.g., int, double)
template <typename CppType, typename CType>
struct ArgUnwrapper {
    static CppType unwrap(CType val, CContext*) {
        return static_cast<CppType>(val);
    }
};

// std::string mappings
template <>
struct ArgUnwrapper<std::string*, CStdString> {
    static std::string* unwrap(CStdString h, CContext* c) {
        return CoreUnwrapper<std::string, CStdString>::unwrap_ptr(h, c);
    }
};
template <>
struct ArgUnwrapper<std::string&, CStdString> {
    static std::string& unwrap(CStdString h, CContext* c) {
        return CoreUnwrapper<std::string, CStdString>::unwrap_ref(h, c);
    }
};
template <>
struct ArgUnwrapper<const std::string&, CStdString> {
    static const std::string& unwrap(CStdString h, CContext* c) {
        return CoreUnwrapper<std::string, CStdString>::unwrap_ref(h, c);
    }
};
template <>
struct ArgUnwrapper<std::string, CStdString> {
    static std::string unwrap(CStdString h, CContext* c) {
        return CoreUnwrapper<std::string, CStdString>::unwrap_ref(h, c);
    }
};

// std::vector mappings
template <typename T>
struct ArgUnwrapper<std::vector<T>*, CStdVector> {
    static std::vector<T>* unwrap(CStdVector h, CContext* c) {
        return CoreUnwrapper<std::vector<T>, CStdVector>::unwrap_ptr(h, c);
    }
};
template <typename T>
struct ArgUnwrapper<std::vector<T>&, CStdVector> {
    static std::vector<T>& unwrap(CStdVector h, CContext* c) {
        return CoreUnwrapper<std::vector<T>, CStdVector>::unwrap_ref(h, c);
    }
};
template <typename T>
struct ArgUnwrapper<const std::vector<T>&, CStdVector> {
    static const std::vector<T>& unwrap(CStdVector h, CContext* c) {
        return CoreUnwrapper<std::vector<T>, CStdVector>::unwrap_ref(h, c);
    }
};
template <typename T>
struct ArgUnwrapper<std::vector<T>, CStdVector> {
    static std::vector<T> unwrap(CStdVector h, CContext* c) {
        return CoreUnwrapper<std::vector<T>, CStdVector>::unwrap_ref(h, c);
    }
};

// std::optional mappings
template <typename T>
struct ArgUnwrapper<std::optional<T>*, CStdOptional> {
    static std::optional<T>* unwrap(CStdOptional h, CContext* c) {
        return CoreUnwrapper<std::optional<T>, CStdOptional>::unwrap_ptr(
            h, c);
    }
};
template <typename T>
struct ArgUnwrapper<std::optional<T>&, CStdOptional> {
    static std::optional<T>& unwrap(CStdOptional h, CContext* c) {
        return CoreUnwrapper<std::optional<T>, CStdOptional>::unwrap_ref(
            h, c);
    }
};
template <typename T>
struct ArgUnwrapper<const std::optional<T>&, CStdOptional> {
    static const std::optional<T>& unwrap(CStdOptional h, CContext* c) {
        return CoreUnwrapper<std::optional<T>, CStdOptional>::unwrap_ref(
            h, c);
    }
};
template <typename T>
struct ArgUnwrapper<std::optional<T>, CStdOptional> {
    static std::optional<T> unwrap(CStdOptional h, CContext* c) {
        return CoreUnwrapper<std::optional<T>, CStdOptional>::unwrap_ref(
            h, c);
    }
};


// --- Result Builder ---
template <typename T>
struct ExtractCoreType {
    using Type = T;
};
template <typename T>
struct ExtractCoreType<std::shared_ptr<T>> {
    using Type = T;
};

template <typename ResultCType, typename CppRetT>
struct ResultTypeBuilder;

template <typename CppRetT>
struct ResultTypeBuilder<CStdVector, CppRetT> {
    using CoreT = typename ExtractCoreType<std::decay_t<CppRetT>>::Type;
    static CStdVector build(void* t) {
        return {
            t, VectorVTableImpl<typename CoreT::value_type>::get_vtable()};
    }
    static CStdVector build_null() {
        return {
            nullptr,
            VectorVTableImpl<typename CoreT::value_type>::get_vtable()};
    }
};
template <typename CppRetT>
struct ResultTypeBuilder<CStdString, CppRetT> {
    static CStdString build(void* t) {
        return {t, StringVTableImpl::get_vtable()};
    }
    static CStdString build_null() {
        return {nullptr, StringVTableImpl::get_vtable()};
    }
};
template <typename CppRetT>
struct ResultTypeBuilder<CStdOptional, CppRetT> {
    using CoreT = typename ExtractCoreType<std::decay_t<CppRetT>>::Type;
    static CStdOptional build(void* t) {
        return {
            t,
            OptionalVTableImpl<typename CoreT::value_type>::get_vtable()};
    }
    static CStdOptional build_null() {
        return {
            nullptr,
            OptionalVTableImpl<typename CoreT::value_type>::get_vtable()};
    }
};

// --- Universal Execution Wrapper ---
// Note: We deduce `Ret` and `CppArgs...` directly from the provided
// function pointer.
template <
    typename ResultCType,
    MemoryPolicy Policy,
    typename Ret,
    typename... CppArgs,
    typename... CArgs>
ResultCType execute_cpp_code(
    Ret (*func)(CppArgs...),
    CContext* ctx,
    CArgs... c_args) {
    clear_context(ctx);
    try {
        if constexpr (std::is_same_v<ResultCType, void>) {
            // Automatically unwrap each argument via ArgUnwrapper
            func(ArgUnwrapper<CppArgs, CArgs>::unwrap(c_args, ctx)...);
            return;
        } else {
            decltype(auto) result = func(
                ArgUnwrapper<CppArgs, CArgs>::unwrap(c_args, ctx)...);
            void* raw_ptr = nullptr;

            if constexpr (
                Policy == MemoryPolicy::Unique
                || Policy == MemoryPolicy::Shared) {
                using DecayedT = std::decay_t<decltype(result)>;
                raw_ptr        = new DecayedT(
                    std::forward<decltype(result)>(result));
            } else if constexpr (Policy == MemoryPolicy::Reference) {
                raw_ptr = (void*)(&result);
            }

            void* tagged_ptr = tag_pointer(raw_ptr, Policy);
            return ResultTypeBuilder<ResultCType, Ret>::build(tagged_ptr);
        }
    } catch (const std::exception& e) {
        set_context_error(ctx, e.what());
        if constexpr (!std::is_same_v<ResultCType, void>) {
            return ResultTypeBuilder<ResultCType, Ret>::build_null();
        }
    } catch (...) {
        set_context_error(ctx, "Unknown C++ exception");
        if constexpr (!std::is_same_v<ResultCType, void>) {
            return ResultTypeBuilder<ResultCType, Ret>::build_null();
        }
    }
}


// ============================================================================
// 3. MOCK C++ LIBRARY
// ============================================================================

std::string internal_get_string() {
    return "Hello from type-erased C++ string!";
}
void internal_process_string(const std::string& s) {
    std::cout << "  [C++ says] Received string size: " << s.size() << "\n";
}

std::shared_ptr<std::optional<double>> internal_get_optional() {
    return std::make_shared<std::optional<double>>(3.14159);
}
void internal_process_optional(std::optional<double>* opt) {
    if (opt && opt->has_value()) {
        std::cout << "  [C++ says] Received optional value: "
                  << opt->value() << "\n";
    } else {
        std::cout << "[C++ says] Optional has no value.\n";
    }
}

std::vector<int>& internal_get_vector() {
    static std::vector<int> static_vec = {100, 200, 300};
    return static_vec;
}
void internal_process_vector(std::vector<int>* v) {
    std::cout << "  [C++ says] Processing vector of size: "
              << (v ? v->size() : 0) << "\n";
}


// ============================================================================
// 4. EXPORTED C API (ZERO BOILERPLATE)
// ============================================================================
extern "C" {

struct CStdString api_get_string_unique(struct CContext* ctx) {
    return execute_cpp_code<CStdString, MemoryPolicy::Unique>(
        internal_get_string, ctx);
}

// Automatically maps struct CStdString to const std::string&
void api_process_string(struct CContext* ctx, struct CStdString str) {
    execute_cpp_code<void, MemoryPolicy::Reference>(
        internal_process_string, ctx, str);
}

struct CStdOptional api_get_optional_shared(struct CContext* ctx) {
    return execute_cpp_code<CStdOptional, MemoryPolicy::Shared>(
        internal_get_optional, ctx);
}

// Automatically maps struct CStdOptional to std::optional<double>*
void api_process_optional(struct CContext* ctx, struct CStdOptional opt) {
    execute_cpp_code<void, MemoryPolicy::Reference>(
        internal_process_optional, ctx, opt);
}

struct CStdVector api_get_vector_ref(struct CContext* ctx) {
    return execute_cpp_code<CStdVector, MemoryPolicy::Reference>(
        internal_get_vector, ctx);
}

// Automatically maps struct CStdVector to std::vector<int>*
void api_process_vector(struct CContext* ctx, struct CStdVector vec) {
    execute_cpp_code<void, MemoryPolicy::Reference>(
        internal_process_vector, ctx, vec);
}
}


// ============================================================================
// 5. C CLIENT VERIFICATION (Normally main.c)
// ============================================================================

void check_error(struct CContext* ctx) {
    if (ctx->has_error) {
        printf("  -> [ERROR CAUGHT]: %s\n", ctx->error_msg);
        ctx->has_error = 0;
    }
}

int main() {
    struct CContext ctx = {0, ""};

    // --- 1. String Test ---
    printf("--- Testing std::string ---\n");
    struct CStdString str = api_get_string_unique(&ctx);
    printf("String content: %s\n", str.vtable->c_str(&str, &ctx));

    // Boilerplate-free execution!
    api_process_string(&ctx, str);
    str.vtable->destroy(&str, &ctx);

    // Test automatic throwing on null mapping to references
      struct CStdString null_str = {0};
    api_process_string(&ctx, null_str);
    check_error(&ctx); // Catches "Attempted to bind null handle to C++
                       // reference/value"


    // --- 2. Optional Test ---
    printf("\n--- Testing std::optional<double> ---\n");
    struct CStdOptional opt = api_get_optional_shared(&ctx);

    if (opt.vtable->has_value(&opt, &ctx)) {
        double* val = (double*)opt.vtable->value(&opt, &ctx);
        printf("Optional value: %f\n", *val);
    }

    api_process_optional(&ctx, opt);
    opt.vtable->destroy(&opt, &ctx);


    // --- 3. Vector Test ---
    printf("\n--- Testing std::vector<int> ---\n");
    struct CStdVector vec = api_get_vector_ref(&ctx);

    printf("Vector size: %zu\n", vec.vtable->size(&vec, &ctx));

    api_process_vector(&ctx, vec);

    // Intentional out of bounds
    printf("Attempting Out-of-bounds access at index 100...\n");
    vec.vtable->at(&vec, 100, &ctx);
    check_error(&ctx);

    vec.vtable->destroy(&vec, &ctx);

    return 0;
}
