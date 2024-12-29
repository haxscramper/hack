#include "util.hpp"

static quill::Logger *l = nullptr;

void set_ol_logger(quill::Logger *logger) { l = logger; }
quill::Logger *ol_log() { return l; }
