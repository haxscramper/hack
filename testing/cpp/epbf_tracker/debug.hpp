#ifndef _DEBUG_H
#define _DEBUG_H

#include <format>

#define __MSG_IMPL(level, fmt, ...)                                            \
  std::cout << std::format(level " {}:{} " fmt, __FILE__,                      \
                           __LINE__ __VA_OPT__(, ) __VA_ARGS__)                \
            << std::endl;

#define INFO(fmt, ...) __MSG_IMPL("INFO", fmt __VA_OPT__(, ) __VA_ARGS__)
#define WARN(fmt, ...) __MSG_IMPL("WARN", fmt __VA_OPT__(, ) __VA_ARGS__)
#define ERROR(fmt, ...) __MSG_IMPL("ERRR", fmt __VA_OPT__(, ) __VA_ARGS__)

#endif
