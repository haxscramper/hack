#include "org_server.hpp"
#include <grpcpp/ext/channelz_service_plugin.h>
#include <grpcpp/ext/proto_server_reflection_plugin.h>
#include <grpcpp/grpcpp.h>
#include <iostream>
#include <memory>
#include <string>

void RunServer(std::shared_ptr<Org> root) {
  std::string server_address("0.0.0.0:50051");
  OrgServiceImpl service{root};
  grpc::channelz::experimental::InitChannelzService();
  grpc::reflection::InitProtoReflectionServerBuilderPlugin();

  grpc::ServerBuilder builder;
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);

  std::unique_ptr<grpc::Server> server{builder.BuildAndStart()};
  LOG_INFO(ol_log(), "Server listening on '{}'", server_address);
  server->Wait();
}

std::shared_ptr<Text> text(std::string const &t) {
  std::shared_ptr<Text> res = std::make_shared<Text>();
  res->text = t;
  return res;
}

int main() {
  init_logger();
  // Create your root org structure here
  auto root = std::make_shared<Subtree>();
  auto s1 = std::make_shared<Subtree>();
  root->add(s1);
  root->level = 1;
  s1->level = 2;
  auto par1 = std::make_shared<Paragraph>();
  s1->add(par1);
  par1->add(text("s1"));
  par1->add(text(" "));
  par1->add(text("body"));
  par1->add(text(" "));
  par1->add(text("paragraph"));
  auto title = std::make_shared<Paragraph>();
  s1->title = title;
  s1->title->add(text("s1"));
  s1->title->add(text(" "));
  s1->title->add(text("title"));
  s1->title->add(text(" "));
  s1->title->add(text("paragraph"));

  RunServer(root);
  return 0;
}
