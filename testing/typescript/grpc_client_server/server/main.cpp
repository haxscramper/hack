#include "org_server.hpp"
#include <grpcpp/grpcpp.h>
#include <iostream>
#include <memory>
#include <string>

void RunServer(std::shared_ptr<Org> root) {
  std::string server_address("0.0.0.0:50051");
  OrgServiceImpl service(root);

  grpc::ServerBuilder builder;
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);

  std::unique_ptr<grpc::Server> server(builder.BuildAndStart());
  std::cout << "Server listening on " << server_address << std::endl;
  server->Wait();
}

int main() {
  init_logger();
  // Create your root org structure here
  auto root = std::make_shared<Subtree>();
  // ... populate the tree ...

  RunServer(root);
  return 0;
}
