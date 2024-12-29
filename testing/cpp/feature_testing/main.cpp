#include <gtest/gtest.h>

#include "util.hpp"
#include <quill/Backend.h>
#include <quill/Frontend.h>
#include <quill/LogMacros.h>
#include <quill/Logger.h>
#include <quill/sinks/ConsoleSink.h>
#include <quill/sinks/FileSink.h>
#include <quill/std/Array.h>

int main(int argc, char **argv) {
  quill::BackendOptions backend_options;
  quill::Backend::start(backend_options);

  auto console_sink = quill::Frontend::create_or_get_sink<quill::FileSink>(
      "/tmp/hack_feature_testing.log");
  quill::Logger *logger =
      quill::Frontend::create_or_get_logger("root", std::move(console_sink));

  // Change the LogLevel to print everything
  logger->set_log_level(quill::LogLevel::TraceL3);

  set_ol_logger(logger);

  LOG_INFO(ol_log(), "First message");

  ::testing::InitGoogleTest(&argc, argv);
  auto result = RUN_ALL_TESTS();
  return result;
}
