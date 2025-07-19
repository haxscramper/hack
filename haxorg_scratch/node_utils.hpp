// CRTP base class for exceptions with backtrace in what()
template <typename DerivedError, typename BaseError>
class OrgJsErrorBase
    : public cpptrace::lazy_exception
    , public BaseError {
  public:
    // Constructor that forwards to base
    cpptrace::stacktrace eager;
    mutable std::string  error_text;
    mutable std::string  what_text;
    int                  line;
    char const*          function;
    char const*          file;

    OrgJsErrorBase(BaseError const& base) : BaseError{base} {
        eager = cpptrace::generate_trace();
    }

    static DerivedError New(
        const std::string& message,
        int                line     = __builtin_LINE(),
        char const*        function = __builtin_FUNCTION(),
        char const*        file     = __builtin_FILE()) {

        BaseError baseError = BaseError::New(
            env,
            std::format(
                "{} at {}:{} in {}\n\n{}",
                message,
                file,
                line,
                function,
                cpptrace::generate_trace().to_string()));
        DerivedError result{baseError};
        result.line     = line;
        result.file     = file;
        result.function = function;
        return result;
    }

    virtual const char* message() const noexcept override {
        error_text = BaseError::Message();
        return error_text.c_str();
    }

    const char* what() const noexcept override {
        what_text = std::format(
            "{}\n\n{}", message(), trace().to_string());
        return what_text.c_str();
    }

    virtual cpptrace::stacktrace const& trace() const noexcept override {
        return eager;
    }
};



template <typename Derived, typename Data>
struct SharedPtrWrapBase : public Napi::ObjectWrap<Derived> {
  static Napi::FunctionReference *constructor;
  std::shared_ptr<Data> _stored;
  Data *getPtr() {
    LOGIC_ASSERTION_CHECK(_stored.get() != nullptr,
                          "Stored object data is not initialized");
    return _stored.get();
  }

  SharedPtrWrapBase(const Napi::CallbackInfo &info)
      : Napi::ObjectWrap<Derived>(info) {
    Napi::Env env = info.Env();
    Napi::HandleScope scope(env);
    if constexpr (std::is_default_constructible_v<Data>) {
      _stored = std::make_shared<Data>();
    }
  }

  SharedPtrWrapBase(Napi::CallbackInfo const &info,
                    std::shared_ptr<Data> const &ptr)
      : Napi::ObjectWrap<Derived>{info} {
    Napi::Env env = info.Env();
    Napi::HandleScope scope(env);
    _stored = ptr;
  }

  static Napi::Object InitSharedWrap(
      Napi::Env env, Napi::Object exports, const char *utf8name,
      const std::initializer_list<
          typename Napi::ObjectWrap<Derived>::PropertyDescriptor> &properties) {
    Napi::Function func =
        Napi::ObjectWrap<Derived>::DefineClass(env, utf8name, properties);

    constructor = new Napi::FunctionReference();
    *constructor = Napi::Persistent(func);
    env.SetInstanceData(constructor);
    exports.Set(utf8name, func);
    return exports;
  }
};

template <typename Derived, typename Data>
Napi::FunctionReference *SharedPtrWrapBase<Derived, Data>::constructor =
    nullptr;

template <typename JsType, typename Func>
Napi::Object CreateCxx(Func const &config) {
  Napi::Object obj = JsType::constructor->New({});
  JsType *instance = JsType::Unwrap(obj);
  config(instance);
  return obj;
}

template <typename E>
struct JsEnumWrapper : public Napi::ObjectWrap<JsEnumWrapper<E>> {
  E value;
  static Napi::FunctionReference *constructor;
  using Base = Napi::ObjectWrap<JsEnumWrapper<E>>;
  using This = JsEnumWrapper<E>;

  static Napi::Object Init(Napi::Env env, Napi::Object exports,
                           const char *className) {
    using Prop = Base::PropertyDescriptor;
    std::vector<Prop> props;
    props.push_back(Base::InstanceMethod("toString", &This::ToString));
    props.push_back(Base::InstanceMethod("toInt", &This::ToInt));

    // for (hstd::EnumFieldDesc<E> const& it :
    //      hstd::describe_enumerators<E>()) {
    //     props.push_back(Base::StaticValue(
    //         std::string(it.name + "Int").c_str(),
    //         Napi::Number::New(env, static_cast<double>(it.value))));
    // }

    Napi::Function func = Base::DefineClass(env, className, props);

    constructor = new Napi::FunctionReference();
    *constructor = Napi::Persistent(func);
    env.SetInstanceData(constructor);
    exports.Set(className, func);
    return exports;
  }

  JsEnumWrapper(Napi::CallbackInfo const &info)
      : Napi::ObjectWrap<JsEnumWrapper<E>>{info} {
    Napi::Env env = info.Env();
    Napi::HandleScope scope(env);
    if (info[0].IsNumber()) {
      value = static_cast<E>(info[0].As<Napi::Number>().Int64Value());
    }
  }

  static Napi::Value FromString(Napi::CallbackInfo const &info,
                                std::string const &value) {
    return CreateCxx<This>([&](This *instance) {
      auto opt = hstd::enum_serde<E>::from_string(value);
      if (opt) {
        instance->value = opt.value();
      } else {
        throw OrgJsError::New(
            info.Env(), std::format("'{}' cannot be converted to {}", value,
                                    hstd::value_metadata<E>::typeName()));
      }
    });
  }

  static Napi::Value FromValue(Napi::CallbackInfo const &info, E value) {
    return CreateCxx<This>([&](This *instance) { instance->value = value; });
  }

  static Napi::Value FromInt(Napi::CallbackInfo const &info, int value) {
    return CreateCxx<This>(
        [&](This *instance) { instance->value = static_cast<E>(value); });
  }

  Napi::Value ToString(Napi::CallbackInfo const &info) {
    return Napi::String::New(info.Env(), hstd::enum_serde<E>::to_string(value));
  }

  Napi::Value ToInt(Napi::CallbackInfo const &info) {
    return Napi::Number::New(info.Env(), static_cast<double>(value));
  }
};

template <typename E>
Napi::FunctionReference *JsEnumWrapper<E>::constructor = nullptr;

template <typename T>
struct hstdVec_bind : public SharedPtrWrapBase<hstdVec_bind<T>, hstd::Vec<T>> {
  using ThisDerived = hstdVec_bind<T>;
  using ThisData = hstd::Vec<T>;
  using ThisBase = SharedPtrWrapBase<ThisDerived, ThisData>;
  using ThisBase::getPtr;
  using ThisBase::InitSharedWrap;
  using ThisBase::InstanceMethod;
  using SharedPtrWrapBase<ThisDerived, ThisData>::SharedPtrWrapBase;

  static Napi::Object Init(Napi::Env env, Napi::Object exports,
                           const char *className) {
    return InitSharedWrap(env, exports, className,
                          {
                              InstanceMethod("push", &hstdVec_bind::Push),
                              InstanceMethod("get", &hstdVec_bind::Get),
                              InstanceMethod("size", &hstdVec_bind::Size),
                              InstanceMethod("clear", &hstdVec_bind::Clear),
                          });
  }

  hstdVec_bind(const Napi::CallbackInfo &info) : ThisBase{info} {
    if (info.Length() > 0 && info[0].IsArray()) {
      Napi::Array jsArray = info[0].As<Napi::Array>();
      for (uint32_t i = 0; i < jsArray.Length(); ++i) {
        getPtr()->push_back(
            JsConverter<T>::from_js_value(info, jsArray.Get(i)));
      }
    }
  }

  Napi::Value Push(const Napi::CallbackInfo &info) {
    if (info.Length() < 1) {
      throw Napi::Error::New(info.Env(), "Missing argument");
    }
    getPtr()->push_back(JsConverter<T>::from_js_value(info, info[0]));
    return info.Env().Undefined();
  }

  Napi::Value Get(const Napi::CallbackInfo &info) {
    if (info.Length() < 1) {
      throw Napi::Error::New(info.Env(), "Missing index");
    }
    uint32_t index = info[0].ToNumber().Uint32Value();
    if (index >= getPtr()->size()) {
      throw Napi::Error::New(info.Env(), "Index out of bounds");
    }
    return JsConverter<T>::to_js_value(info, getPtr()->at(index));
  }

  Napi::Value Size(const Napi::CallbackInfo &info) {
    return Napi::Number::New(info.Env(), getPtr()->size());
  }

  Napi::Value Clear(const Napi::CallbackInfo &info) {
    getPtr()->clear();
    return info.Env().Undefined();
  }
};

template <typename T> struct js_to_org_type<hstdVec_bind<T>> {
  using type = hstd::Vec<T>;
};

template <typename T> struct org_to_js_type<hstd::Vec<T>> {
  using type = hstdVec_bind<T>;
};

template <typename T>
struct immerflex_vector_bind
    : public SharedPtrWrapBase<immerflex_vector_bind<T>,
                               immer::flex_vector<T>> {
  using ThisDerived = immerflex_vector_bind<T>;
  using ThisData = immer::flex_vector<T>;
  using SharedPtrWrapBase<ThisDerived, ThisData>::SharedPtrWrapBase;
  using SharedPtrWrapBase<ThisDerived, ThisData>::getPtr;
  using SharedPtrWrapBase<ThisDerived, ThisData>::InitSharedWrap;

  static Napi::Object Init(Napi::Env env, Napi::Object exports,
                           const char *className) {
    return InitSharedWrap(env, exports, className, {});
  }
};

template <typename T> struct js_to_org_type<immerflex_vector_bind<T>> {
  using type = immer::flex_vector<T>;
};

template <typename T> struct org_to_js_type<immer::flex_vector<T>> {
  using type = immerflex_vector_bind<T>;
};

template <typename T>
struct immerbox_bind
    : public SharedPtrWrapBase<immerbox_bind<T>, immer::box<T>> {
  using ThisDerived = immerbox_bind<T>;
  using ThisData = immer::box<T>;
  using SharedPtrWrapBase<ThisDerived, ThisData>::SharedPtrWrapBase;
  using SharedPtrWrapBase<ThisDerived, ThisData>::getPtr;
  using SharedPtrWrapBase<ThisDerived, ThisData>::InitSharedWrap;

  static Napi::Object Init(Napi::Env env, Napi::Object exports,
                           const char *className) {
    return InitSharedWrap(env, exports, className, {});
  }
};

template <typename T> struct js_to_org_type<immerbox_bind<T>> {
  using type = immer::box<T>;
};

template <typename T> struct org_to_js_type<immer::box<T>> {
  using type = immerbox_bind<T>;
};

template <typename T>
struct hstdIntSet_bind
    : public SharedPtrWrapBase<hstdIntSet_bind<T>, hstd::IntSet<T>> {
  using ThisDerived = hstdIntSet_bind<T>;
  using ThisData = hstd::IntSet<T>;
  using SharedPtrWrapBase<ThisDerived, ThisData>::SharedPtrWrapBase;
  using SharedPtrWrapBase<ThisDerived, ThisData>::getPtr;
  using SharedPtrWrapBase<ThisDerived, ThisData>::InitSharedWrap;

  static Napi::Object Init(Napi::Env env, Napi::Object exports,
                           const char *className) {
    return InitSharedWrap(env, exports, className, {});
  }
};

template <typename T> struct js_to_org_type<hstdIntSet_bind<T>> {
  using type = hstd::IntSet<T>;
};

template <typename T> struct org_to_js_type<hstd::IntSet<T>> {
  using type = hstdIntSet_bind<T>;
};

template <typename K, typename V>
struct hstdUnorderedMap_bind
    : public SharedPtrWrapBase<hstdUnorderedMap_bind<K, V>,
                               hstd::UnorderedMap<K, V>> {
  using ThisDerived = hstdUnorderedMap_bind<K, V>;
  using ThisData = hstd::UnorderedMap<K, V>;
  using SharedPtrWrapBase<ThisDerived, ThisData>::SharedPtrWrapBase;
  using SharedPtrWrapBase<ThisDerived, ThisData>::getPtr;
  using SharedPtrWrapBase<ThisDerived, ThisData>::InitSharedWrap;

  static Napi::Object Init(Napi::Env env, Napi::Object exports,
                           const char *className) {
    return InitSharedWrap(env, exports, className, {});
  }
};

template <typename K, typename V>
struct js_to_org_type<hstdUnorderedMap_bind<K, V>> {
  using type = hstd::UnorderedMap<K, V>;
};

template <typename K, typename V>
struct org_to_js_type<hstd::UnorderedMap<K, V>> {
  using type = hstdUnorderedMap_bind<K, V>;
};

template <typename T> struct JsConverter<std::optional<T>> {
  static std::optional<T> from_js_value(Napi::CallbackInfo const &info,
                                        Napi::Value const &value) {
    if (value.IsNull()) {
      return std::nullopt;
    } else {
      return JsConverter<T>::from_js_value(info, value);
    }
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 const std::optional<T> &value) {
    if (value.has_value()) {
      return JsConverter<T>::to_js_value(info, value.value());
    } else {
      return info.Env().Null();
    }
  }
};

// Specialization for string
template <> struct JsConverter<std::string> {
  static std::string from_js_value(Napi::CallbackInfo const &info,
                                   Napi::Value const &value) {
    if (value.IsString()) {
      return value.As<Napi::String>().Utf8Value();
    } else {
      throw OrgJsTypeError::New(value.Env(), "String expected");
    }
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 const std::string &value) {
    return Napi::String::New(info.Env(), value);
  }
};

template <> struct JsConverter<hstd::Str> {
  static hstd::Str from_js_value(Napi::CallbackInfo const &info,
                                 Napi::Value const &value) {
    if (value.IsString()) {
      return value.As<Napi::String>().Utf8Value();
    } else {
      throw OrgJsTypeError::New(value.Env(), "String expected");
    }
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 const hstd::Str &value) {
    return Napi::String::New(info.Env(), value.toBase());
  }
};

// Specialization for boolean
template <> struct JsConverter<bool> {
  static bool from_js_value(Napi::CallbackInfo const &info,
                            Napi::Value const &value) {
    if (value.IsBoolean()) {
      return value.As<Napi::Boolean>().Value();
    } else {
      throw OrgJsTypeError::New(value.Env(), "Boolean expected");
    }
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 const bool &value) {
    return Napi::Boolean::New(info.Env(), value);
  }
};

// Specialization for number (double)
template <> struct JsConverter<double> {
  static double from_js_value(Napi::CallbackInfo const &info,
                              Napi::Value const &value) {
    if (value.IsNumber()) {
      return value.As<Napi::Number>().DoubleValue();
    } else {
      throw OrgJsTypeError::New(value.Env(), "Number expected");
    }
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 const double &value) {
    return Napi::Number::New(info.Env(), value);
  }
};

// Specialization for integer
template <> struct JsConverter<int> {
  static int from_js_value(Napi::CallbackInfo const &info,
                           Napi::Value const &value) {
    if (value.IsNumber()) {
      return value.As<Napi::Number>().Int32Value();
    } else {
      throw OrgJsTypeError::New(value.Env(), "Integer expected");
    }
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 const int &value) {
    return Napi::Number::New(info.Env(), value);
  }
};

template <> struct JsConverter<unsigned int> {
  static unsigned int from_js_value(Napi::CallbackInfo const &info,
                                    Napi::Value const &value) {
    if (value.IsNumber()) {
      return value.As<Napi::Number>().Uint32Value();
    } else {
      throw OrgJsTypeError::New(value.Env(), "Integer expected");
    }
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 const unsigned int &value) {
    return Napi::Number::New(info.Env(), value);
  }
};

template <typename... Args> struct JsConverter<std::variant<Args...>> {
  static std::variant<Args...> from_js_value(Napi::CallbackInfo const &info,
                                             Napi::Value const &value) {
    logic_todo_impl();
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 const std::variant<Args...> &var) {
    Napi::Value result;
    std::visit(
        [&]<typename T>(T const &value) -> int {
          result = JsConverter<T>::to_js_value(info, value);
          return int{};
        },
        var);
    return result;
  }
};

template <typename JsType,
          typename OrgType = typename js_to_org_type<JsType>::type>
Napi::Value CreateWrappedObjectFromPtr(Napi::CallbackInfo const &info,
                                       std::shared_ptr<OrgType> const &ptr) {
  // Check if the constructor reference has been initialized
  if (JsType::constructor->IsEmpty()) {
    throw Napi::Error::New(
        info.Env(), "Constructor not initialized. Make sure to call T::Init() "
                    "first.");
  } else {
    return CreateCxx<JsType>(
        [&](JsType *instance) { instance->_stored = ptr; });
  }
}

template <typename JsType, typename... Args>
Napi::Value CreateWrappedObject(Napi::CallbackInfo const &info,
                                Args &&...args) {
  using OrgType = typename js_to_org_type<JsType>::type;
  return CreateWrappedObjectFromPtr<JsType>(
      std::make_shared<OrgType>(std::forward<Args>(args)...));
}

template <typename T> T *ExtractWrappedObject(Napi::Value const &value) {
  if (!value.IsObject()) {
    throw OrgJsTypeError::New(
        value.Env(), std::format("Object expected for extraction of {}",
                                 hstd::value_metadata<T>::typeName()));
  }

  Napi::Object obj = value.As<Napi::Object>();

  // Method 1: For objects created with ObjectWrap
  if (obj.InstanceOf(T::constructor->Value())) {
    return Napi::ObjectWrap<T>::Unwrap(obj);
  }

  // Method 2: For objects with an external field
  if (obj.Has("__external")) {
    Napi::External<T> external = obj.Get("__external").As<Napi::External<T>>();
    return external.Data();
  }

  // Method 3: For objects with an external field holding a shared_ptr
  if (obj.Has("__ptr")) {
    Napi::External<std::shared_ptr<T>> external =
        obj.Get("__ptr").As<Napi::External<std::shared_ptr<T>>>();
    return external.Data()->get();
  }

  throw Napi::Error::New(value.Env(), "Cannot extract wrapped C++ object");
}

template <org::sem::IsOrg T> struct JsConverter<org::sem::SemId<T>> {
  static org::sem::SemId<T> from_js_value(Napi::CallbackInfo const &info,
                                          Napi::Value const &value) {
    auto *wrappedPtr =
        ExtractWrappedObject<typename org_to_js_type<T>::type>(value);
    return org::sem::SemId<T>(wrappedPtr->_stored);
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 org::sem::SemId<T> const &value) {
    return CreateWrappedObjectFromPtr<typename org_to_js_type<T>::type>(
        info, value.value);
  }
};

template <> struct JsConverter<org::sem::SemId<org::sem::Org>> {
  static org::sem::SemId<org::sem::Org>
  from_js_value(Napi::CallbackInfo const &info, Napi::Value const &value) {
    throw Napi::Error::New(info.Env(), "TODO implement conversion");
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 org::sem::SemId<org::sem::Org> const &value) {
    Napi::Value result;
    org::switch_node_id(
        value, [&]<typename T>(org::sem::SemId<T> const &convId) {
          result = JsConverter<org::sem::SemId<T>>::to_js_value(info, convId);
        });
    return result;
  }
};

template <hstd::DescribedEnum E> struct JsConverter<E> {
  using JsType = JsEnumWrapper<E>;
  static E from_js_value(Napi::CallbackInfo const &info,
                         Napi::Value const &value) {
    JsType *ptr = ExtractWrappedObject<JsType>(value);
    return ptr->value;
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info, E value) {
    return JsType::FromValue(info, value);
  }
};

template <HasJsMapping OrgType> struct JsConverter<OrgType *> {
  using JsType = typename org_to_js_type<OrgType>::type;
  static OrgType *from_js_value(Napi::CallbackInfo const &info,
                                Napi::Value const &value) {
    JsType *ptr = ExtractWrappedObject<JsType>(value);
    OrgType *argPtr = ptr->getPtr();
    return argPtr;
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 OrgType *value) {
    return CreateWrappedObjectFromPtr<JsType>(
        info, std::shared_ptr<OrgType>(value, [](OrgType *) {}));
  }
};

template <org::imm::IsImmOrg T> struct JsConverter<org::imm::ImmIdT<T>> {
  static org::imm::ImmIdT<T> from_js_value(Napi::CallbackInfo const &info,
                                           Napi::Value const &value) {
    bool lossless = false;
    return org::imm::ImmIdT<T>::FromValue(
        value.As<Napi::BigInt>().Uint64Value(&lossless));
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 org::imm::ImmIdT<T> const &value) {
    return Napi::BigInt::New(info.Env(),
                             static_cast<uint64_t>(value.getValue()));
  }
};

template <HasJsMapping OrgType> struct JsConverter<OrgType const &> {
  using JsType = typename org_to_js_type<OrgType>::type;
  static OrgType const &from_js_value(Napi::CallbackInfo const &info,
                                      Napi::Value const &value) {
    return *JsConverter<OrgType *>::from_js_value(info, value);
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 OrgType const &value) {
    return JsConverter<OrgType *>::to_js_value(info,
                                               const_cast<OrgType *>(&value));
  }
};

template <HasJsMapping OrgType> struct JsConverter<OrgType &> {
  using JsType = typename org_to_js_type<OrgType>::type;
  static OrgType const &from_js_value(Napi::CallbackInfo const &info,
                                      Napi::Value &value) {
    return *JsConverter<OrgType *>::from_js_value(info, value);
  }

  static Napi::Value to_js_value(Napi::CallbackInfo &info, OrgType &value) {
    return JsConverter<OrgType *>::to_js_value(info, &value);
  }
};

template <HasJsMapping OrgType> struct JsConverter<OrgType> {
  using JsType = typename org_to_js_type<OrgType>::type;
  static OrgType from_js_value(Napi::CallbackInfo const &info,
                               Napi::Value const &value) {
    JsType *ptr = ExtractWrappedObject<JsType>(value);
    OrgType *argPtr = ptr->getPtr();
    return *argPtr;
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 OrgType const &value) {
    return CreateWrappedObjectFromPtr<JsType>(info,
                                              std::make_shared<OrgType>(value));
  }
};

template <HasJsMapping T> struct JsConverter<std::shared_ptr<T>> {
  static std::shared_ptr<T> from_js_value(Napi::CallbackInfo const &info,
                                          Napi::Value const &value) {
    auto *wrappedPtr =
        ExtractWrappedObject<typename org_to_js_type<T>::type>(value);
    return wrappedPtr->_stored;
  }

  static Napi::Value to_js_value(Napi::CallbackInfo const &info,
                                 std::shared_ptr<T> const &value) {
    return CreateWrappedObjectFromPtr<typename org_to_js_type<T>::type>(info,
                                                                        value);
  }
};

// Generic wrapper function for any Callable
template <typename CallableType, typename ClassInstance, size_t... Indices>
Napi::Value
WrapCallableImpl(Napi::CallbackInfo const &info, const CallableType &callable,
                 ClassInstance instance, std::index_sequence<Indices...>,
                 int line = __builtin_LINE(),
                 char const *function = __builtin_FUNCTION(),
                 char const *file = __builtin_FILE()) {
  Napi::Env env = info.Env();

  LOG(INFO) << std::format("Triggering callable {} arguments at {}:{}",
                           info.Length(), function, line);
  for (int i = 0; i < info.Length(); ++i) {
    LOG(INFO) << std::format("  @[{}] = {}", i, FormatNapiValue(info[i]));
  }

  if (info.Length() < callable.getRequiredArgsCount()) {
    throw OrgJsError::New(
        info.Env(),
        std::format("Argument count mismatch, expected at least {}, but got "
                    "{}",
                    callable.getRequiredArgsCount(), info.Length()));
  }

  try {
    // Get the argument specifications
    const auto &argSpecs = callable.args();

    // Convert arguments with proper type checking - now using pack
    // expansion instead of indexing into the tuple at runtime
    auto convertArg =
        [&info,
         &argSpecs]<size_t Index,
                    typename CallResult = std::decay_t<std::tuple_element_t<
                        Index, typename CallableType::ArgsBaseTypes>>>()
        -> CallResult {
      // Get the type of the argument at compile-time index
      using ArgSpecType =
          std::tuple_element_t<Index, std::decay_t<decltype(argSpecs)>>;
      using ArgType = typename ArgSpecType::value_type;

      const auto &argSpec = std::get<Index>(argSpecs);

      try {
        if (Index < info.Length()) {
          CallResult &&tmp =
              JsConverter<ArgType>::from_js_value(info, info[Index]);
          return tmp;
        } else if (argSpec.defaultValue) {
          return *argSpec.defaultValue;
        } else {
          throw Napi::TypeError::New(info.Env(), "Missing required argument: " +
                                                     argSpec.name);
        }
      } catch (OrgJsTypeError &t) {
        throw OrgJsTypeError::New(
            info.Env(),
            std::format("Type error when processing argument #{}, {}: {}",
                        Index, argSpec.name, t.message()));
      }
    };

    // Invoke the callable with appropriate arguments
    if constexpr (std::is_same_v<ClassInstance, std::nullptr_t>) {
      // Function call (no instance)
      using ReturnType =
          decltype(callable(convertArg.template operator()<Indices>()...));

      if constexpr (std::is_void_v<ReturnType>) {
        callable(convertArg.template operator()<Indices>()...);
        return env.Undefined();
      } else {
        ReturnType result =
            callable(convertArg.template operator()<Indices>()...);
        return JsConverter<ReturnType>::to_js_value(info, result);
      }
    } else {
      // Method call (with instance)
      using ReturnType = decltype(callable(
          instance, convertArg.template operator()<Indices>()...));

      if constexpr (std::is_void_v<ReturnType>) {
        callable(instance, convertArg.template operator()<Indices>()...);
        return env.Undefined();
      } else {
        ReturnType result =
            callable(instance, convertArg.template operator()<Indices>()...);
        // Returning `T const&` is not directly handled by the
        // converter
        return JsConverter<std::remove_cvref_t<ReturnType>>::to_js_value(
            info, result);
      }
    }
  } catch (const Napi::Error &e) {
    // Re-throw Napi errors
    throw e;
  } catch (const std::exception &e) {
    // Convert C++ exceptions to JavaScript errors
    throw Napi::Error::New(env, e.what());
  } catch (...) {
    // Handle unknown exceptions
    throw Napi::Error::New(env, "Unknown C++ exception occurred");
  }
}

// Wrapper for standalone functions
template <typename ReturnType, typename... Args>
Napi::Value WrapFunction(Napi::CallbackInfo const &info,
                         const Callable<CallableClass<std::monostate>,
                                        ReturnType, Args...> &callable,
                         int line = __builtin_LINE(),
                         char const *function = __builtin_FUNCTION(),
                         char const *file = __builtin_FILE()) {
  return WrapCallableImpl(info, callable, nullptr,
                          std::index_sequence_for<Args...>{}, line, function,
                          file);
}

// Wrapper for non-const methods
template <typename ClassType, typename ReturnType, typename... Args>
Napi::Value WrapMethod(
    Napi::CallbackInfo const &info, ClassType *instance,
    const Callable<CallableClass<ClassType>, ReturnType, Args...> &callable,
    int line = __builtin_LINE(), char const *function = __builtin_FUNCTION(),
    char const *file = __builtin_FILE()) {
  return WrapCallableImpl(info, callable, instance,
                          std::index_sequence_for<Args...>{}, line, function,
                          file);
}

// Wrapper for const methods
template <typename ClassType, typename ReturnType, typename... Args>
Napi::Value WrapConstMethod(Napi::CallbackInfo const &info,
                            const ClassType *instance,
                            const Callable<CallableClass<const ClassType>,
                                           ReturnType, Args...> &callable,
                            int line = __builtin_LINE(),
                            char const *function = __builtin_FUNCTION(),
                            char const *file = __builtin_FILE()) {
  return WrapCallableImpl(info, callable, instance,
                          std::index_sequence_for<Args...>{}, line, function,
                          file);
}
