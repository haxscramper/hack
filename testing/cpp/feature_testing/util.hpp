#pragma once

#include <quill/Logger.h>
#include <quill/LogMacros.h>

quill::Logger *ol_log();
void set_ol_logger(quill::Logger *logger);
