void config_entry_free(git_config_entry *entry) {
    git_config_entry_free(entry);
}



void config_find_global(git_buf *out) {
    auto __result = git_config_find_global(out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_find_global");
    }
}



void config_find_xdg(git_buf *out) {
    auto __result = git_config_find_xdg(out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_find_xdg");
    }
}



void config_find_system(git_buf *out) {
    auto __result = git_config_find_system(out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_find_system");
    }
}



void config_find_programdata(git_buf *out) {
    auto __result = git_config_find_programdata(out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_find_programdata");
    }
}



void config_open_default(git_config **out) {
    auto __result = git_config_open_default(out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_open_default");
    }
}



void config_new(git_config **out) {
    auto __result = git_config_new(out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_new");
    }
}



void config_add_file_ondisk(git_config *cfg, const char *path, git_config_level_t level, const git_repository *repo, int force) {
    auto __result = git_config_add_file_ondisk(cfg, path, level, repo, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_add_file_ondisk");
    }
}



void config_open_ondisk(git_config **out, const char *path) {
    auto __result = git_config_open_ondisk(out, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_open_ondisk");
    }
}



void config_open_level(git_config **out, const git_config *parent, git_config_level_t level) {
    auto __result = git_config_open_level(out, parent, level);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_open_level");
    }
}



void config_open_global(git_config **out, git_config *config) {
    auto __result = git_config_open_global(out, config);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_open_global");
    }
}



void config_snapshot(git_config **out, git_config *config) {
    auto __result = git_config_snapshot(out, config);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_snapshot");
    }
}



void config_free(git_config *cfg) {
    git_config_free(cfg);
}



void config_get_entry(git_config_entry **out, const git_config *cfg, const char *name) {
    auto __result = git_config_get_entry(out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_entry");
    }
}



void config_get_int32(int32_t *out, const git_config *cfg, const char *name) {
    auto __result = git_config_get_int32(out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_int32");
    }
}



void config_get_int64(int64_t *out, const git_config *cfg, const char *name) {
    auto __result = git_config_get_int64(out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_int64");
    }
}



void config_get_bool(int *out, const git_config *cfg, const char *name) {
    auto __result = git_config_get_bool(out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_bool");
    }
}



void config_get_path(git_buf *out, const git_config *cfg, const char *name) {
    auto __result = git_config_get_path(out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_path");
    }
}



void config_get_string(const char **out, const git_config *cfg, const char *name) {
    auto __result = git_config_get_string(out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_string");
    }
}



void config_get_string_buf(git_buf *out, const git_config *cfg, const char *name) {
    auto __result = git_config_get_string_buf(out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_string_buf");
    }
}



void config_get_multivar_foreach(const git_config *cfg, const char *name, const char *regexp, git_config_foreach_cb callback, void *payload) {
    auto __result = git_config_get_multivar_foreach(cfg, name, regexp, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_multivar_foreach");
    }
}



git_config_iterator *config_multivar_iterator_new(const git_config *cfg, const char *name, const char *regexp) {
    git_config_iterator *out;
    auto __result = git_config_multivar_iterator_new(&out, cfg, name, regexp);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_multivar_iterator_new");
    } else {
        return out;
    }
}



git_config_entry *config_next(git_config_iterator *iter) {
    git_config_entry *entry;
    auto __result = git_config_next(&entry, iter);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_next");
    } else {
        return entry;
    }
}



void config_iterator_free(git_config_iterator *iter) {
    git_config_iterator_free(iter);
}



void config_set_int32(git_config *cfg, const char *name, int32_t value) {
    auto __result = git_config_set_int32(cfg, name, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_set_int32");
    }
}



void config_set_int64(git_config *cfg, const char *name, int64_t value) {
    auto __result = git_config_set_int64(cfg, name, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_set_int64");
    }
}



void config_set_bool(git_config *cfg, const char *name, int value) {
    auto __result = git_config_set_bool(cfg, name, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_set_bool");
    }
}



void config_set_string(git_config *cfg, const char *name, const char *value) {
    auto __result = git_config_set_string(cfg, name, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_set_string");
    }
}



void config_set_multivar(git_config *cfg, const char *name, const char *regexp, const char *value) {
    auto __result = git_config_set_multivar(cfg, name, regexp, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_set_multivar");
    }
}



void config_delete_entry(git_config *cfg, const char *name) {
    auto __result = git_config_delete_entry(cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_delete_entry");
    }
}



void config_delete_multivar(git_config *cfg, const char *name, const char *regexp) {
    auto __result = git_config_delete_multivar(cfg, name, regexp);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_delete_multivar");
    }
}



void config_foreach(const git_config *cfg, git_config_foreach_cb callback, void *payload) {
    auto __result = git_config_foreach(cfg, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_foreach");
    }
}



void config_iterator_new(git_config_iterator **out, const git_config *cfg) {
    auto __result = git_config_iterator_new(out, cfg);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_iterator_new");
    }
}



void config_iterator_glob_new(git_config_iterator **out, const git_config *cfg, const char *regexp) {
    auto __result = git_config_iterator_glob_new(out, cfg, regexp);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_iterator_glob_new");
    }
}



void config_foreach_match(const git_config *cfg, const char *regexp, git_config_foreach_cb callback, void *payload) {
    auto __result = git_config_foreach_match(cfg, regexp, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_foreach_match");
    }
}



void config_get_mapped(int *out, const git_config *cfg, const char *name, const git_configmap *maps, size_t map_n) {
    auto __result = git_config_get_mapped(out, cfg, name, maps, map_n);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_mapped");
    }
}



void config_lookup_map_value(int *out, const git_configmap *maps, size_t map_n, const char *value) {
    auto __result = git_config_lookup_map_value(out, maps, map_n, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_lookup_map_value");
    }
}



void config_parse_bool(int *out, const char *value) {
    auto __result = git_config_parse_bool(out, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_parse_bool");
    }
}



void config_parse_int32(int32_t *out, const char *value) {
    auto __result = git_config_parse_int32(out, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_parse_int32");
    }
}



void config_parse_int64(int64_t *out, const char *value) {
    auto __result = git_config_parse_int64(out, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_parse_int64");
    }
}



void config_parse_path(git_buf *out, const char *value) {
    auto __result = git_config_parse_path(out, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_parse_path");
    }
}



void config_backend_foreach_match(git_config_backend *backend, const char *regexp, git_config_foreach_cb callback, void *payload) {
    auto __result = git_config_backend_foreach_match(backend, regexp, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_backend_foreach_match");
    }
}



void config_lock(git_transaction **tx, git_config *cfg) {
    auto __result = git_config_lock(tx, cfg);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_lock");
    }
}



void libgit2_init() {
    auto __result = git_libgit2_init();
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_libgit2_init");
    }
}



void libgit2_shutdown() {
    auto __result = git_libgit2_shutdown();
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_libgit2_shutdown");
    }
}



git_repository *repository_open(const char *path) {
    git_repository *out;
    auto __result = git_repository_open(&out, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_open");
    } else {
        return out;
    }
}



void repository_open_from_worktree(git_repository **out, git_worktree *wt) {
    auto __result = git_repository_open_from_worktree(out, wt);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_open_from_worktree");
    }
}



void repository_wrap_odb(git_repository **out, git_odb *odb) {
    auto __result = git_repository_wrap_odb(out, odb);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_wrap_odb");
    }
}



void repository_discover(git_buf *out, const char *start_path, int across_fs, const char *ceiling_dirs) {
    auto __result = git_repository_discover(out, start_path, across_fs, ceiling_dirs);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_discover");
    }
}



void repository_open_ext(git_repository **out, const char *path, unsigned int flags, const char *ceiling_dirs) {
    auto __result = git_repository_open_ext(out, path, flags, ceiling_dirs);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_open_ext");
    }
}



void repository_open_bare(git_repository **out, const char *bare_path) {
    auto __result = git_repository_open_bare(out, bare_path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_open_bare");
    }
}



void repository_free(git_repository *repo) {
    git_repository_free(repo);
}



void repository_init(git_repository **out, const char *path, unsigned int is_bare) {
    auto __result = git_repository_init(out, path, is_bare);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_init");
    }
}



void repository_init_options_init(git_repository_init_options *opts, unsigned int version) {
    auto __result = git_repository_init_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_init_options_init");
    }
}



void repository_init_ext(git_repository **out, const char *repo_path, git_repository_init_options *opts) {
    auto __result = git_repository_init_ext(out, repo_path, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_init_ext");
    }
}



void repository_head(git_reference **out, git_repository *repo) {
    auto __result = git_repository_head(out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_head");
    }
}



void repository_head_for_worktree(git_reference **out, git_repository *repo, const char *name) {
    auto __result = git_repository_head_for_worktree(out, repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_head_for_worktree");
    }
}



void repository_head_detached(git_repository *repo) {
    auto __result = git_repository_head_detached(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_head_detached");
    }
}



void repository_head_detached_for_worktree(git_repository *repo, const char *name) {
    auto __result = git_repository_head_detached_for_worktree(repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_head_detached_for_worktree");
    }
}



void repository_head_unborn(git_repository *repo) {
    auto __result = git_repository_head_unborn(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_head_unborn");
    }
}



void repository_is_empty(git_repository *repo) {
    auto __result = git_repository_is_empty(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_is_empty");
    }
}



void repository_item_path(git_buf *out, const git_repository *repo, git_repository_item_t item) {
    auto __result = git_repository_item_path(out, repo, item);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_item_path");
    }
}



const char *repository_path(const git_repository *repo) {
    auto __result = git_repository_path(repo);
    return __result;
}



const char *repository_workdir(const git_repository *repo) {
    auto __result = git_repository_workdir(repo);
    return __result;
}



const char *repository_commondir(const git_repository *repo) {
    auto __result = git_repository_commondir(repo);
    return __result;
}



void repository_set_workdir(git_repository *repo, const char *workdir, int update_gitlink) {
    auto __result = git_repository_set_workdir(repo, workdir, update_gitlink);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_set_workdir");
    }
}



void repository_is_bare(const git_repository *repo) {
    auto __result = git_repository_is_bare(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_is_bare");
    }
}



void repository_is_worktree(const git_repository *repo) {
    auto __result = git_repository_is_worktree(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_is_worktree");
    }
}



void repository_config(git_config **out, git_repository *repo) {
    auto __result = git_repository_config(out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_config");
    }
}



void repository_config_snapshot(git_config **out, git_repository *repo) {
    auto __result = git_repository_config_snapshot(out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_config_snapshot");
    }
}



void repository_odb(git_odb **out, git_repository *repo) {
    auto __result = git_repository_odb(out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_odb");
    }
}



void repository_refdb(git_refdb **out, git_repository *repo) {
    auto __result = git_repository_refdb(out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_refdb");
    }
}



void repository_index(git_index **out, git_repository *repo) {
    auto __result = git_repository_index(out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_index");
    }
}



void repository_message(git_buf *out, git_repository *repo) {
    auto __result = git_repository_message(out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_message");
    }
}



void repository_message_remove(git_repository *repo) {
    auto __result = git_repository_message_remove(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_message_remove");
    }
}



void repository_state_cleanup(git_repository *repo) {
    auto __result = git_repository_state_cleanup(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_state_cleanup");
    }
}



void repository_fetchhead_foreach(git_repository *repo, git_repository_fetchhead_foreach_cb callback, void *payload) {
    auto __result = git_repository_fetchhead_foreach(repo, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_fetchhead_foreach");
    }
}



void repository_mergehead_foreach(git_repository *repo, git_repository_mergehead_foreach_cb callback, void *payload) {
    auto __result = git_repository_mergehead_foreach(repo, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_mergehead_foreach");
    }
}



void repository_hashfile(git_oid *out, git_repository *repo, const char *path, git_object_t type, const char *as_path) {
    auto __result = git_repository_hashfile(out, repo, path, type, as_path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_hashfile");
    }
}



void repository_set_head(git_repository *repo, const char *refname) {
    auto __result = git_repository_set_head(repo, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_set_head");
    }
}



void repository_set_head_detached(git_repository *repo, const git_oid *committish) {
    auto __result = git_repository_set_head_detached(repo, committish);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_set_head_detached");
    }
}



void repository_set_head_detached_from_annotated(git_repository *repo, const git_annotated_commit *committish) {
    auto __result = git_repository_set_head_detached_from_annotated(repo, committish);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_set_head_detached_from_annotated");
    }
}



void repository_detach_head(git_repository *repo) {
    auto __result = git_repository_detach_head(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_detach_head");
    }
}



void repository_state(git_repository *repo) {
    auto __result = git_repository_state(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_state");
    }
}



void repository_set_namespace(git_repository *repo, const char *nmspace) {
    auto __result = git_repository_set_namespace(repo, nmspace);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_set_namespace");
    }
}



const char *repository_get_namespace(git_repository *repo) {
    auto __result = git_repository_get_namespace(repo);
    return __result;
}



void repository_is_shallow(git_repository *repo) {
    auto __result = git_repository_is_shallow(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_is_shallow");
    }
}



void repository_ident(const char **name, const char **email, const git_repository *repo) {
    auto __result = git_repository_ident(name, email, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_ident");
    }
}



void repository_set_ident(git_repository *repo, const char *name, const char *email) {
    auto __result = git_repository_set_ident(repo, name, email);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_set_ident");
    }
}



