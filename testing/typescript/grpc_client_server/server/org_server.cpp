#include "org_server.hpp"

#include <cpptrace/cpptrace.hpp>
#include <format>
#include <quill/Backend.h>
#include <quill/Frontend.h>
#include <quill/LogMacros.h>
#include <quill/Logger.h>
#include <quill/sinks/ConsoleSink.h>
#include <quill/sinks/FileSink.h>
#include <quill/std/Array.h>

struct hexception : cpptrace::lazy_exception {
  std::string msg;
  int line;
  char const *function;
  char const *file;
  cpptrace::stacktrace eager;

  static hexception init(std::string const &msg, int line = __builtin_LINE(),
                         char const *function = __builtin_FUNCTION(),
                         char const *file = __builtin_FILE()) {
    auto result = hexception{};
    result.eager = cpptrace::generate_trace();
    result.msg = msg;
    result.line = line;
    result.file = file;
    result.function = function;
    return result;
  }

public:
  virtual cpptrace::stacktrace const &trace() const noexcept override {
    return eager;
  }

  virtual const char *message() const noexcept override {
    return strdup(
        std::format("{} at {}:{} in {}", msg, file, line, function).c_str());
  }
};

OrgServiceImpl::OrgServiceImpl(std::shared_ptr<Org> root) : root{root} {
  LOG_INFO(ol_log(), "Create service implementation instance");
}

OrgServiceImpl::~OrgServiceImpl() = default;

int64_t OrgServiceImpl::registerObject(std::shared_ptr<Org> obj) {
  if (obj.get() == nullptr) {
    throw hexception::init(
        "Cannot register shared PTR object targeting nullptr");
  }
  LOG_INFO(ol_log(), "Register object");
  std::lock_guard<std::mutex> lock{mutex_};
  int64_t handle = handles.size();
  for (auto const &[key, obj] : handles) {
    LOG_INFO(ol_log(), "[]-- {}", key);
  }
  handles.insert_or_assign(handle, obj);
  LOG_INFO(ol_log(), "Handle is {}", handle);
  return handle;
}

std::shared_ptr<Org> OrgServiceImpl::getObject(int64_t handle) {
  LOG_INFO(ol_log(), "get object for handle {}", handle);
  std::lock_guard<std::mutex> lock{mutex_};
  auto it = handles.find(handle);
  if (it == handles.end()) {
    return nullptr;
  } else {
    return it->second;
  }
}

template <typename Func> grpc::Status execute_service(Func const &f) {
  try {
    return f();
  } catch (std::exception &ex) {
    LOG_ERROR(ol_log(), "{}", ex.what());
    return grpc::Status(grpc::StatusCode::INTERNAL, ex.what());
  }
}

grpc::Status OrgServiceImpl::GetKind(grpc::ServerContext *context,
                                     const org::HandleRequest *request,
                                     org::KindResponse *response) {
  return execute_service([&]() {
    LOG_INFO(ol_log(), "get kind");
    auto obj = getObject(request->handle());
    if (!obj) {
      return grpc::Status(
          grpc::StatusCode::NOT_FOUND,
          std::format("Handle '{}' not found", request->handle()));
    }

    switch (obj->getKind()) {
    case OrgSemKind::Text:
      response->set_kind(org::KindResponse::TEXT);
      break;
    case OrgSemKind::Paragraph:
      response->set_kind(org::KindResponse::PARAGRAPH);
      break;
    case OrgSemKind::Subtree:
      response->set_kind(org::KindResponse::SUBTREE);
      break;
    default:
      response->set_kind(org::KindResponse::NONE);
    }
    return grpc::Status::OK;
  });
}

grpc::Status OrgServiceImpl::GetSubnodeCount(grpc::ServerContext *context,
                                             const org::HandleRequest *request,
                                             org::CountResponse *response) {
  return execute_service([&]() {
    LOG_INFO(ol_log(), "get subnode count");
    auto obj = getObject(request->handle());
    if (!obj) {
      return grpc::Status(grpc::StatusCode::NOT_FOUND, "Handle not found");
    }
    response->set_count(obj->subnodes.size());
    return grpc::Status::OK;
  });
}

grpc::Status OrgServiceImpl::GetSubnode(grpc::ServerContext *context,
                                        const org::SubnodeRequest *request,
                                        org::HandleResponse *response) {
  return execute_service([&]() {
    LOG_INFO(ol_log(), "get subnode");
    auto obj = getObject(request->handle());
    if (!obj) {
      return grpc::Status(grpc::StatusCode::NOT_FOUND, "Handle not found");
    }

    if (request->index() < 0 || request->index() >= obj->subnodes.size()) {
      return grpc::Status(grpc::StatusCode::OUT_OF_RANGE, "Index out of range");
    }

    response->set_handle(registerObject(obj->subnodes[request->index()]));
    return grpc::Status::OK;
  });
}

grpc::Status OrgServiceImpl::GetText(grpc::ServerContext *context,
                                     const org::HandleRequest *request,
                                     org::TextResponse *response) {
  return execute_service([&]() {
    LOG_INFO(ol_log(), "get text");
    auto obj = getObject(request->handle());
    if (!obj) {
      return grpc::Status(grpc::StatusCode::NOT_FOUND, "Handle not found");
    }

    auto text = std::dynamic_pointer_cast<Text>(obj);
    if (!text) {
      return grpc::Status(grpc::StatusCode::FAILED_PRECONDITION,
                          "Not a text node");
    }

    response->set_text(text->text);
    return grpc::Status::OK;
  });
}

grpc::Status OrgServiceImpl::GetSubtreeLevel(grpc::ServerContext *context,
                                             const org::HandleRequest *request,
                                             org::LevelResponse *response) {
  return execute_service([&]() {
    LOG_INFO(ol_log(), "get subtree level");
    auto obj = getObject(request->handle());
    if (!obj) {
      return grpc::Status(grpc::StatusCode::NOT_FOUND, "Handle not found");
    }

    auto subtree = std::dynamic_pointer_cast<Subtree>(obj);
    if (!subtree) {
      return grpc::Status(grpc::StatusCode::FAILED_PRECONDITION,
                          "Not a subtree node");
    }

    response->set_level(subtree->level);
    return grpc::Status::OK;
  });
}

grpc::Status OrgServiceImpl::GetRoot(grpc::ServerContext *context,
                                     const org::Empty *request,
                                     org::HandleResponse *response) {
  return execute_service([&]() {
    LOG_INFO(ol_log(), "get root");
    response->set_handle(registerObject(root));
    return grpc::Status::OK;
  });
}

namespace {
quill::Logger *l;
}

void init_logger() {
  quill::BackendOptions backend_options;
  quill::Backend::start(backend_options);

  auto console_sink = quill::Frontend::create_or_get_sink<quill::FileSink>(
      "/tmp/org_grpc_server.log");
  l = quill::Frontend::create_or_get_logger("root", std::move(console_sink));

  // Change the LogLevel to print everything
  l->set_log_level(quill::LogLevel::TraceL3);
}

quill::Logger *ol_log() { return l; }
