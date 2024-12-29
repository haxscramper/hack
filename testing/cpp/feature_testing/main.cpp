#include <gtest/gtest.h>

#include "util.hpp"
#include <memory>
#include <quill/Backend.h>
#include <quill/Frontend.h>
#include <quill/LogMacros.h>
#include <quill/Logger.h>
#include <quill/sinks/ConsoleSink.h>
#include <quill/sinks/FileSink.h>
#include <quill/std/Array.h>

class CustomLoggingHandler : public ::testing::TestEventListener {
public:
  // clang-format off
    void OnTestProgramStart(const ::testing::UnitTest& unit_test) override {}
    void OnTestIterationStart(const ::testing::UnitTest& unit_test, int iteration) override {}
    void OnEnvironmentsSetUpStart(const ::testing::UnitTest& unit_test) override {}
    void OnEnvironmentsSetUpEnd(const ::testing::UnitTest& unit_test) override {}
    void OnTestCaseStart(const ::testing::TestCase& test_case) override {}

    void OnTestCaseEnd(const ::testing::TestCase& test_case) override {}
    void OnEnvironmentsTearDownStart(const ::testing::UnitTest& unit_test) override {}
    void OnEnvironmentsTearDownEnd(const ::testing::UnitTest& unit_test) override {}
    void OnTestIterationEnd(const ::testing::UnitTest& unit_test, int iteration) override {}
    void OnTestProgramEnd(const ::testing::UnitTest& unit_test) override {}
  // clang-format on

  void OnTestStart(const ::testing::TestInfo &test_info) override {
    LOG_INFO(ol_log(), "Test Start: {}.{}", test_info.test_case_name(),
             test_info.name());
  }

  void
  OnTestPartResult(const ::testing::TestPartResult &test_part_result) override {
    if (test_part_result.failed()) {
      LOG_INFO(ol_log(), "Test Failed: {}", test_part_result.summary());
    } else {
      LOG_INFO(ol_log(), "Test Passed: {}", test_part_result.summary());
    }
  }

  void OnTestEnd(const ::testing::TestInfo &test_info) override {
    if (test_info.result()->Passed()) {
      LOG_INFO(ol_log(), "Test End: {}.{} Passed", test_info.test_case_name(),
               test_info.name());
    } else {
      LOG_INFO(ol_log(), "Test End: {}.{} Failed", test_info.test_case_name(),
               test_info.name());
    }
  }
};

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

  ::testing::UnitTest::GetInstance()->listeners().Append(
      new CustomLoggingHandler{});

  auto result = RUN_ALL_TESTS();
  return result;
}
