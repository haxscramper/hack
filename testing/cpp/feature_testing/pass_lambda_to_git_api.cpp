#include <exception>
#include <functional>
#include <git2/config.h>
#include <iostream>
#include <string.h>
#define __GIT_THROW_EXCEPTION throw "git failed with code???";

#include "util.hpp"
#include <gtest/gtest.h>

void config_backend_foreach_match(
    git_config_backend *backend, const char *regexp,
    std::function<int(const git_config_entry *)> callback) {
  auto payload = &callback;

  auto code = git_config_backend_foreach_match(
      backend, regexp,
      [](const git_config_entry *entry, void *payload) -> int {
        auto impl = static_cast<decltype(callback) *>(payload);
        return (*impl)(entry);
      },
      payload);

  if (code < 0) {
    __GIT_THROW_EXCEPTION(code);
  }
}

int exec_c_callback(int (*callback)(const char *, void *), void *payload) {
  return callback("test", payload);
}

int exec_c_callback(std::function<int(const char *)> callback) {
  return exec_c_callback(
      [](const char *in, void *payload) -> int {
        auto impl = static_cast<decltype(callback) *>(payload);
        return (*impl)(in);
      },
      &callback);
}

TEST(Lambda, PassToCAPi) {
  // comment
  LOG_INFO(ol_log(), "Callback result {}",
           exec_c_callback([](const char *in) -> int { return strlen(in); }));
}
