void config_entry_free(git_config_entry *entry) {
    git_config_entry_free(entry);
}



void config_find_global(git_buf *out) {
    auto code = git_config_find_global(out);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_find_global");
    }
}



void config_find_xdg(git_buf *out) {
    auto code = git_config_find_xdg(out);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_find_xdg");
    }
}



void config_find_system(git_buf *out) {
    auto code = git_config_find_system(out);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_find_system");
    }
}



void config_find_programdata(git_buf *out) {
    auto code = git_config_find_programdata(out);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_find_programdata");
    }
}



void config_open_default(git_config **out) {
    auto code = git_config_open_default(out);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_open_default");
    }
}



void config_new(git_config **out) {
    auto code = git_config_new(out);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_new");
    }
}



void config_add_file_ondisk(git_config *cfg, const char *path, git_config_level_t level, const git_repository *repo, int force) {
    auto code = git_config_add_file_ondisk(cfg, path, level, repo, force);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_add_file_ondisk");
    }
}



void config_open_ondisk(git_config **out, const char *path) {
    auto code = git_config_open_ondisk(out, path);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_open_ondisk");
    }
}



void config_open_level(git_config **out, const git_config *parent, git_config_level_t level) {
    auto code = git_config_open_level(out, parent, level);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_open_level");
    }
}



void config_open_global(git_config **out, git_config *config) {
    auto code = git_config_open_global(out, config);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_open_global");
    }
}



void config_snapshot(git_config **out, git_config *config) {
    auto code = git_config_snapshot(out, config);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_snapshot");
    }
}



void config_free(git_config *cfg) {
    git_config_free(cfg);
}



void config_get_entry(git_config_entry **out, const git_config *cfg, const char *name) {
    auto code = git_config_get_entry(out, cfg, name);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_entry");
    }
}



void config_get_int32(int32_t *out, const git_config *cfg, const char *name) {
    auto code = git_config_get_int32(out, cfg, name);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_int32");
    }
}



void config_get_int64(int64_t *out, const git_config *cfg, const char *name) {
    auto code = git_config_get_int64(out, cfg, name);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_int64");
    }
}



void config_get_bool(int *out, const git_config *cfg, const char *name) {
    auto code = git_config_get_bool(out, cfg, name);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_bool");
    }
}



void config_get_path(git_buf *out, const git_config *cfg, const char *name) {
    auto code = git_config_get_path(out, cfg, name);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_path");
    }
}



void config_get_string(const char **out, const git_config *cfg, const char *name) {
    auto code = git_config_get_string(out, cfg, name);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_string");
    }
}



void config_get_string_buf(git_buf *out, const git_config *cfg, const char *name) {
    auto code = git_config_get_string_buf(out, cfg, name);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_string_buf");
    }
}



void config_get_multivar_foreach(const git_config *cfg, const char *name, const char *regexp, git_config_foreach_cb callback, void *payload) {
    auto code = git_config_get_multivar_foreach(cfg, name, regexp, callback, payload);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_multivar_foreach");
    }
}



void config_multivar_iterator_new(git_config_iterator **out, const git_config *cfg, const char *name, const char *regexp) {
    auto code = git_config_multivar_iterator_new(out, cfg, name, regexp);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_multivar_iterator_new");
    }
}



git_config_entry *config_next(git_config_iterator *iter) {
    git_config_entry *entry;
    auto code = git_config_next(&entry, iter);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_next");
    } else {
        return entry;
    }
}



void config_iterator_free(git_config_iterator *iter) {
    git_config_iterator_free(iter);
}



void config_set_int32(git_config *cfg, const char *name, int32_t value) {
    auto code = git_config_set_int32(cfg, name, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_set_int32");
    }
}



void config_set_int64(git_config *cfg, const char *name, int64_t value) {
    auto code = git_config_set_int64(cfg, name, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_set_int64");
    }
}



void config_set_bool(git_config *cfg, const char *name, int value) {
    auto code = git_config_set_bool(cfg, name, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_set_bool");
    }
}



void config_set_string(git_config *cfg, const char *name, const char *value) {
    auto code = git_config_set_string(cfg, name, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_set_string");
    }
}



void config_set_multivar(git_config *cfg, const char *name, const char *regexp, const char *value) {
    auto code = git_config_set_multivar(cfg, name, regexp, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_set_multivar");
    }
}



void config_delete_entry(git_config *cfg, const char *name) {
    auto code = git_config_delete_entry(cfg, name);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_delete_entry");
    }
}



void config_delete_multivar(git_config *cfg, const char *name, const char *regexp) {
    auto code = git_config_delete_multivar(cfg, name, regexp);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_delete_multivar");
    }
}



void config_foreach(const git_config *cfg, git_config_foreach_cb callback, void *payload) {
    auto code = git_config_foreach(cfg, callback, payload);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_foreach");
    }
}



void config_iterator_new(git_config_iterator **out, const git_config *cfg) {
    auto code = git_config_iterator_new(out, cfg);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_iterator_new");
    }
}



void config_iterator_glob_new(git_config_iterator **out, const git_config *cfg, const char *regexp) {
    auto code = git_config_iterator_glob_new(out, cfg, regexp);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_iterator_glob_new");
    }
}



void config_foreach_match(const git_config *cfg, const char *regexp, git_config_foreach_cb callback, void *payload) {
    auto code = git_config_foreach_match(cfg, regexp, callback, payload);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_foreach_match");
    }
}



void config_get_mapped(int *out, const git_config *cfg, const char *name, const git_configmap *maps, size_t map_n) {
    auto code = git_config_get_mapped(out, cfg, name, maps, map_n);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_get_mapped");
    }
}



void config_lookup_map_value(int *out, const git_configmap *maps, size_t map_n, const char *value) {
    auto code = git_config_lookup_map_value(out, maps, map_n, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_lookup_map_value");
    }
}



void config_parse_bool(int *out, const char *value) {
    auto code = git_config_parse_bool(out, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_parse_bool");
    }
}



void config_parse_int32(int32_t *out, const char *value) {
    auto code = git_config_parse_int32(out, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_parse_int32");
    }
}



void config_parse_int64(int64_t *out, const char *value) {
    auto code = git_config_parse_int64(out, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_parse_int64");
    }
}



void config_parse_path(git_buf *out, const char *value) {
    auto code = git_config_parse_path(out, value);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_parse_path");
    }
}



void config_backend_foreach_match(git_config_backend *backend, const char *regexp, git_config_foreach_cb callback, void *payload) {
    auto code = git_config_backend_foreach_match(backend, regexp, callback, payload);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_backend_foreach_match");
    }
}



void config_lock(git_transaction **tx, git_config *cfg) {
    auto code = git_config_lock(tx, cfg);
    if (code < 0) {
        __GIT_THROW_EXCEPTION(code, "git_config_lock");
    }
}



