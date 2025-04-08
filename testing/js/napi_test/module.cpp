#include <format>
#include <memory>
#include <napi.h>
#include <string>

class CustomData {
public:
  int value;
  CustomData(int v) : value(v) {}
  std::string to_string() const { return std::format("CustomData({})", value); }
};

class Wrapper : public Napi::ObjectWrap<Wrapper> {
public:
  static Napi::Object Init(Napi::Env env, Napi::Object exports) {
    Napi::Function func =
        DefineClass(env, "Wrapper",
                    {InstanceMethod("getValue", &Wrapper::GetValue),
                     InstanceMethod("toString", &Wrapper::ToString)});

    constructor = Napi::Persistent(func);
    constructor.SuppressDestruct();
    exports.Set("Wrapper", func);
    return exports;
  }

  Wrapper(const Napi::CallbackInfo &info) : Napi::ObjectWrap<Wrapper>(info) {
    if (info.Length() != 1 || !info[0].IsNumber())
      throw Napi::TypeError::New(info.Env(), "Expected one number");
    int val = info[0].As<Napi::Number>().Int32Value();
    data = std::make_shared<CustomData>(val);
  }

  Napi::Value GetValue(const Napi::CallbackInfo &info) {
    return Napi::Number::New(info.Env(), data->value);
  }

  Napi::Value ToString(const Napi::CallbackInfo &info) {
    return Napi::String::New(info.Env(), data->to_string());
  }

  static inline Napi::FunctionReference constructor;

private:
  std::shared_ptr<CustomData> data;
};

Napi::Value CreateShared(const Napi::CallbackInfo &info) {
  try {
    auto env = info.Env();
    if (!info[0].IsNumber())
      throw std::invalid_argument("Expected a number");
    int val = info[0].As<Napi::Number>().Int32Value();
    auto obj = Wrapper::constructor.New({Napi::Number::New(env, val)});
    return obj;
  } catch (const std::exception &ex) {
    throw Napi::Error::New(info.Env(), ex.what());
  }
}

Napi::Object InitAll(Napi::Env env, Napi::Object exports) {
  Wrapper::Init(env, exports);
  exports.Set("createShared", Napi::Function::New(env, CreateShared));
  return exports;
}

NODE_API_MODULE(addon, InitAll)
