#define for_i(var_name, max_range)                                        \
    for (int var_name = 0; var_name < max_range; ++var_name)

#define METHOD_DBG puts(__PRETTY_FUNCTION__);
// TODO pretty-print function name
#define LINE_DBG printf("file: %s line: %d\n", __FILE__, __LINE__);
