#pragma once

#include <quill/LogMacros.h>
#include <quill/Logger.h>
#include <thread>

quill::Logger *ol_log();
void set_ol_logger(quill::Logger *logger);

inline void set_thread_name(std::thread &t, std::string const &name) {
  pthread_setname_np(t.native_handle(), name.c_str());
}
