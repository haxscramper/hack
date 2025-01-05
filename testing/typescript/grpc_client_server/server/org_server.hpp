#pragma once

#include "org.grpc.pb.h"
#include "org.hpp"
#include <grpcpp/grpcpp.h>
#include <mutex>
#include <quill/LogMacros.h>
#include <quill/Logger.h>
#include <unordered_map>

void init_logger();
quill::Logger *ol_log();

class OrgServiceImpl final : public org::OrgService::Service {
public:
  std::shared_ptr<Org> root;
  explicit OrgServiceImpl(std::shared_ptr<Org> root);
  ~OrgServiceImpl();

  grpc::Status GetKind(grpc::ServerContext *context,
                       const org::HandleRequest *request,
                       org::KindResponse *response) override;

  grpc::Status GetSubnodeCount(grpc::ServerContext *context,
                               const org::HandleRequest *request,
                               org::CountResponse *response) override;

  grpc::Status GetSubnode(grpc::ServerContext *context,
                          const org::SubnodeRequest *request,
                          org::HandleResponse *response) override;

  grpc::Status GetText(grpc::ServerContext *context,
                       const org::HandleRequest *request,
                       org::TextResponse *response) override;

  grpc::Status GetSubtreeLevel(grpc::ServerContext *context,
                               const org::HandleRequest *request,
                               org::LevelResponse *response) override;

  grpc::Status GetRoot(grpc::ServerContext *context, const org::Empty *request,
                       org::HandleResponse *response) override;

private:
  std::shared_ptr<Org> root_;
  std::mutex mutex_;
  std::unordered_map<int64_t, std::shared_ptr<Org>> handles_;
  int64_t next_handle_ = 1;

  int64_t registerObject(std::shared_ptr<Org> obj);
  std::shared_ptr<Org> getObject(int64_t handle);
};
