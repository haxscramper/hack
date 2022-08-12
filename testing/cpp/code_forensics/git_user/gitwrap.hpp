void libgit2_version(int *major, int *minor, int *rev) {
    auto __result = git_libgit2_version(major, minor, rev);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_libgit2_version");
    }
}



const char *libgit2_prerelease() {
    auto __result = git_libgit2_prerelease();
    return __result;
}



int libgit2_features() {
    auto __result = git_libgit2_features();
    return __result;
}



void libgit2_opts(int option) {
    auto __result = git_libgit2_opts(option);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_libgit2_opts");
    }
}



void buf_dispose(git_buf *buffer) {
    git_buf_dispose(buffer);
}



git_oid oid_fromstr(const char *str) {
    git_oid out;
    auto __result = git_oid_fromstr(&out, str);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_fromstr");
    } else {
        return out;
    }
}



git_oid oid_fromstrp(const char *str) {
    git_oid out;
    auto __result = git_oid_fromstrp(&out, str);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_fromstrp");
    } else {
        return out;
    }
}



git_oid oid_fromstrn(const char *str, size_t length) {
    git_oid out;
    auto __result = git_oid_fromstrn(&out, str, length);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_fromstrn");
    } else {
        return out;
    }
}



git_oid oid_fromraw(const unsigned char *raw) {
    git_oid out;
    auto __result = git_oid_fromraw(&out, raw);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_fromraw");
    } else {
        return out;
    }
}



char oid_fmt(const git_oid *id) {
    char out;
    auto __result = git_oid_fmt(&out, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_fmt");
    } else {
        return out;
    }
}



char oid_nfmt(size_t n, const git_oid *id) {
    char out;
    auto __result = git_oid_nfmt(&out, n, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_nfmt");
    } else {
        return out;
    }
}



char oid_pathfmt(const git_oid *id) {
    char out;
    auto __result = git_oid_pathfmt(&out, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_pathfmt");
    } else {
        return out;
    }
}



char *oid_tostr_s(const git_oid *oid) {
    auto __result = git_oid_tostr_s(oid);
    return __result;
}



char *oid_tostr(size_t n, const git_oid *id) {
    char out;
    auto __result = git_oid_tostr(&out, n, id);
    return __result;
}



git_oid oid_cpy(const git_oid *src) {
    git_oid out;
    auto __result = git_oid_cpy(&out, src);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_cpy");
    } else {
        return out;
    }
}



int oid_cmp(const git_oid *a, const git_oid *b) {
    auto __result = git_oid_cmp(a, b);
    return __result;
}



void oid_equal(const git_oid *a, const git_oid *b) {
    auto __result = git_oid_equal(a, b);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_equal");
    }
}



void oid_ncmp(const git_oid *a, const git_oid *b, size_t len) {
    auto __result = git_oid_ncmp(a, b, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_ncmp");
    }
}



void oid_streq(const git_oid *id, const char *str) {
    auto __result = git_oid_streq(id, str);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_streq");
    }
}



void oid_strcmp(const git_oid *id, const char *str) {
    auto __result = git_oid_strcmp(id, str);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_strcmp");
    }
}



void oid_is_zero(const git_oid *id) {
    auto __result = git_oid_is_zero(id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_is_zero");
    }
}



git_oid_shorten *oid_shorten_new(size_t min_length) {
    auto __result = git_oid_shorten_new(min_length);
    return __result;
}



void oid_shorten_add(git_oid_shorten *os, const char *text_id) {
    auto __result = git_oid_shorten_add(os, text_id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_oid_shorten_add");
    }
}



void oid_shorten_free(git_oid_shorten *os) {
    git_oid_shorten_free(os);
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



git_repository *repository_open_from_worktree(git_worktree *wt) {
    git_repository *out;
    auto __result = git_repository_open_from_worktree(&out, wt);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_open_from_worktree");
    } else {
        return out;
    }
}



git_repository *repository_wrap_odb(git_odb *odb) {
    git_repository *out;
    auto __result = git_repository_wrap_odb(&out, odb);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_wrap_odb");
    } else {
        return out;
    }
}



git_buf repository_discover(const char *start_path, int across_fs, const char *ceiling_dirs) {
    git_buf out;
    auto __result = git_repository_discover(&out, start_path, across_fs, ceiling_dirs);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_discover");
    } else {
        return out;
    }
}



git_repository *repository_open_ext(const char *path, unsigned int flags, const char *ceiling_dirs) {
    git_repository *out;
    auto __result = git_repository_open_ext(&out, path, flags, ceiling_dirs);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_open_ext");
    } else {
        return out;
    }
}



git_repository *repository_open_bare(const char *bare_path) {
    git_repository *out;
    auto __result = git_repository_open_bare(&out, bare_path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_open_bare");
    } else {
        return out;
    }
}



void repository_free(git_repository *repo) {
    git_repository_free(repo);
}



git_repository *repository_init(const char *path, unsigned int is_bare) {
    git_repository *out;
    auto __result = git_repository_init(&out, path, is_bare);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_init");
    } else {
        return out;
    }
}



void repository_init_options_init(git_repository_init_options *opts, unsigned int version) {
    auto __result = git_repository_init_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_init_options_init");
    }
}



git_repository *repository_init_ext(const char *repo_path, git_repository_init_options *opts) {
    git_repository *out;
    auto __result = git_repository_init_ext(&out, repo_path, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_init_ext");
    } else {
        return out;
    }
}



git_reference *repository_head(git_repository *repo) {
    git_reference *out;
    auto __result = git_repository_head(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_head");
    } else {
        return out;
    }
}



git_reference *repository_head_for_worktree(git_repository *repo, const char *name) {
    git_reference *out;
    auto __result = git_repository_head_for_worktree(&out, repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_head_for_worktree");
    } else {
        return out;
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



git_buf repository_item_path(const git_repository *repo, git_repository_item_t item) {
    git_buf out;
    auto __result = git_repository_item_path(&out, repo, item);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_item_path");
    } else {
        return out;
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



git_config *repository_config(git_repository *repo) {
    git_config *out;
    auto __result = git_repository_config(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_config");
    } else {
        return out;
    }
}



git_config *repository_config_snapshot(git_repository *repo) {
    git_config *out;
    auto __result = git_repository_config_snapshot(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_config_snapshot");
    } else {
        return out;
    }
}



git_odb *repository_odb(git_repository *repo) {
    git_odb *out;
    auto __result = git_repository_odb(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_odb");
    } else {
        return out;
    }
}



git_refdb *repository_refdb(git_repository *repo) {
    git_refdb *out;
    auto __result = git_repository_refdb(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_refdb");
    } else {
        return out;
    }
}



git_index *repository_index(git_repository *repo) {
    git_index *out;
    auto __result = git_repository_index(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_index");
    } else {
        return out;
    }
}



git_buf repository_message(git_repository *repo) {
    git_buf out;
    auto __result = git_repository_message(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_message");
    } else {
        return out;
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



git_oid repository_hashfile(git_repository *repo, const char *path, git_object_t type, const char *as_path) {
    git_oid out;
    auto __result = git_repository_hashfile(&out, repo, path, type, as_path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_repository_hashfile");
    } else {
        return out;
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



git_annotated_commit *annotated_commit_from_ref(git_repository *repo, const git_reference *ref) {
    git_annotated_commit *out;
    auto __result = git_annotated_commit_from_ref(&out, repo, ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_annotated_commit_from_ref");
    } else {
        return out;
    }
}



git_annotated_commit *annotated_commit_from_fetchhead(git_repository *repo, const char *branch_name, const char *remote_url, const git_oid *id) {
    git_annotated_commit *out;
    auto __result = git_annotated_commit_from_fetchhead(&out, repo, branch_name, remote_url, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_annotated_commit_from_fetchhead");
    } else {
        return out;
    }
}



git_annotated_commit *annotated_commit_lookup(git_repository *repo, const git_oid *id) {
    git_annotated_commit *out;
    auto __result = git_annotated_commit_lookup(&out, repo, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_annotated_commit_lookup");
    } else {
        return out;
    }
}



git_annotated_commit *annotated_commit_from_revspec(git_repository *repo, const char *revspec) {
    git_annotated_commit *out;
    auto __result = git_annotated_commit_from_revspec(&out, repo, revspec);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_annotated_commit_from_revspec");
    } else {
        return out;
    }
}



const git_oid *annotated_commit_id(const git_annotated_commit *commit) {
    auto __result = git_annotated_commit_id(commit);
    return __result;
}



const char *annotated_commit_ref(const git_annotated_commit *commit) {
    auto __result = git_annotated_commit_ref(commit);
    return __result;
}



void annotated_commit_free(git_annotated_commit *commit) {
    git_annotated_commit_free(commit);
}



void object_lookup(git_object **object, git_repository *repo, const git_oid *id, git_object_t type) {
    auto __result = git_object_lookup(object, repo, id, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_object_lookup");
    }
}



void object_lookup_prefix(git_object **object_out, git_repository *repo, const git_oid *id, size_t len, git_object_t type) {
    auto __result = git_object_lookup_prefix(object_out, repo, id, len, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_object_lookup_prefix");
    }
}



git_object *object_lookup_bypath(const git_object *treeish, const char *path, git_object_t type) {
    git_object *out;
    auto __result = git_object_lookup_bypath(&out, treeish, path, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_object_lookup_bypath");
    } else {
        return out;
    }
}



const git_oid *object_id(const git_object *obj) {
    auto __result = git_object_id(obj);
    return __result;
}



git_buf object_short_id(const git_object *obj) {
    git_buf out;
    auto __result = git_object_short_id(&out, obj);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_object_short_id");
    } else {
        return out;
    }
}



git_object_t object_type(const git_object *obj) {
    auto __result = git_object_type(obj);
    return __result;
}



git_repository *object_owner(const git_object *obj) {
    auto __result = git_object_owner(obj);
    return __result;
}



void object_free(git_object *object) {
    git_object_free(object);
}



const char *object_type2string(git_object_t type) {
    auto __result = git_object_type2string(type);
    return __result;
}



git_object_t object_string2type(const char *str) {
    auto __result = git_object_string2type(str);
    return __result;
}



void object_typeisloose(git_object_t type) {
    auto __result = git_object_typeisloose(type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_object_typeisloose");
    }
}



void object_peel(git_object **peeled, const git_object *object, git_object_t target_type) {
    auto __result = git_object_peel(peeled, object, target_type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_object_peel");
    }
}



void object_dup(git_object **dest, git_object *source) {
    auto __result = git_object_dup(dest, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_object_dup");
    }
}



void object_rawcontent_is_valid(int *valid, const char *buf, size_t len, git_object_t type) {
    auto __result = git_object_rawcontent_is_valid(valid, buf, len, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_object_rawcontent_is_valid");
    }
}



git_tree *tree_lookup(git_repository *repo, const git_oid *id) {
    git_tree *out;
    auto __result = git_tree_lookup(&out, repo, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_lookup");
    } else {
        return out;
    }
}



git_tree *tree_lookup_prefix(git_repository *repo, const git_oid *id, size_t len) {
    git_tree *out;
    auto __result = git_tree_lookup_prefix(&out, repo, id, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_lookup_prefix");
    } else {
        return out;
    }
}



void tree_free(git_tree *tree) {
    git_tree_free(tree);
}



const git_oid *tree_id(const git_tree *tree) {
    auto __result = git_tree_id(tree);
    return __result;
}



git_repository *tree_owner(const git_tree *tree) {
    auto __result = git_tree_owner(tree);
    return __result;
}



size_t tree_entrycount(const git_tree *tree) {
    auto __result = git_tree_entrycount(tree);
    return __result;
}



const git_tree_entry *tree_entry_byname(const git_tree *tree, const char *filename) {
    auto __result = git_tree_entry_byname(tree, filename);
    return __result;
}



const git_tree_entry *tree_entry_byindex(const git_tree *tree, size_t idx) {
    auto __result = git_tree_entry_byindex(tree, idx);
    return __result;
}



const git_tree_entry *tree_entry_byid(const git_tree *tree, const git_oid *id) {
    auto __result = git_tree_entry_byid(tree, id);
    return __result;
}



git_tree_entry *tree_entry_bypath(const git_tree *root, const char *path) {
    git_tree_entry *out;
    auto __result = git_tree_entry_bypath(&out, root, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_entry_bypath");
    } else {
        return out;
    }
}



git_tree_entry *tree_entry_dup(const git_tree_entry *source) {
    git_tree_entry *dest;
    auto __result = git_tree_entry_dup(&dest, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_entry_dup");
    } else {
        return dest;
    }
}



void tree_entry_free(git_tree_entry *entry) {
    git_tree_entry_free(entry);
}



const char *tree_entry_name(const git_tree_entry *entry) {
    auto __result = git_tree_entry_name(entry);
    return __result;
}



const git_oid *tree_entry_id(const git_tree_entry *entry) {
    auto __result = git_tree_entry_id(entry);
    return __result;
}



git_object_t tree_entry_type(const git_tree_entry *entry) {
    auto __result = git_tree_entry_type(entry);
    return __result;
}



git_filemode_t tree_entry_filemode(const git_tree_entry *entry) {
    auto __result = git_tree_entry_filemode(entry);
    return __result;
}



git_filemode_t tree_entry_filemode_raw(const git_tree_entry *entry) {
    auto __result = git_tree_entry_filemode_raw(entry);
    return __result;
}



void tree_entry_cmp(const git_tree_entry *e1, const git_tree_entry *e2) {
    auto __result = git_tree_entry_cmp(e1, e2);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_entry_cmp");
    }
}



git_object *tree_entry_to_object(git_repository *repo, const git_tree_entry *entry) {
    git_object *object_out;
    auto __result = git_tree_entry_to_object(&object_out, repo, entry);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_entry_to_object");
    } else {
        return object_out;
    }
}



git_treebuilder *treebuilder_new(git_repository *repo, const git_tree *source) {
    git_treebuilder *out;
    auto __result = git_treebuilder_new(&out, repo, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_treebuilder_new");
    } else {
        return out;
    }
}



void treebuilder_clear(git_treebuilder *bld) {
    auto __result = git_treebuilder_clear(bld);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_treebuilder_clear");
    }
}



size_t treebuilder_entrycount(git_treebuilder *bld) {
    auto __result = git_treebuilder_entrycount(bld);
    return __result;
}



void treebuilder_free(git_treebuilder *bld) {
    git_treebuilder_free(bld);
}



const git_tree_entry *treebuilder_get(git_treebuilder *bld, const char *filename) {
    auto __result = git_treebuilder_get(bld, filename);
    return __result;
}



const git_tree_entry *treebuilder_insert(git_treebuilder *bld, const char *filename, const git_oid *id, git_filemode_t filemode) {
    const git_tree_entry *out;
    auto __result = git_treebuilder_insert(&out, bld, filename, id, filemode);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_treebuilder_insert");
    } else {
        return out;
    }
}



void treebuilder_remove(git_treebuilder *bld, const char *filename) {
    auto __result = git_treebuilder_remove(bld, filename);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_treebuilder_remove");
    }
}



void treebuilder_filter(git_treebuilder *bld, git_treebuilder_filter_cb filter, void *payload) {
    auto __result = git_treebuilder_filter(bld, filter, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_treebuilder_filter");
    }
}



void treebuilder_write(git_oid *id, git_treebuilder *bld) {
    auto __result = git_treebuilder_write(id, bld);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_treebuilder_write");
    }
}



void tree_walk(const git_tree *tree, git_treewalk_mode mode, git_treewalk_cb callback, void *payload) {
    auto __result = git_tree_walk(tree, mode, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_walk");
    }
}



git_tree *tree_dup(git_tree *source) {
    git_tree *out;
    auto __result = git_tree_dup(&out, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_dup");
    } else {
        return out;
    }
}



git_oid tree_create_updated(git_repository *repo, git_tree *baseline, size_t nupdates, const git_tree_update *updates) {
    git_oid out;
    auto __result = git_tree_create_updated(&out, repo, baseline, nupdates, updates);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tree_create_updated");
    } else {
        return out;
    }
}



void strarray_dispose(git_strarray *array) {
    git_strarray_dispose(array);
}



void strarray_copy(git_strarray *tgt, const git_strarray *src) {
    auto __result = git_strarray_copy(tgt, src);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_strarray_copy");
    }
}



git_reference *reference_lookup(git_repository *repo, const char *name) {
    git_reference *out;
    auto __result = git_reference_lookup(&out, repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_lookup");
    } else {
        return out;
    }
}



git_oid reference_name_to_id(git_repository *repo, const char *name) {
    git_oid out;
    auto __result = git_reference_name_to_id(&out, repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_name_to_id");
    } else {
        return out;
    }
}



git_reference *reference_dwim(git_repository *repo, const char *shorthand) {
    git_reference *out;
    auto __result = git_reference_dwim(&out, repo, shorthand);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_dwim");
    } else {
        return out;
    }
}



git_reference *reference_symbolic_create_matching(git_repository *repo, const char *name, const char *target, int force, const char *current_value, const char *log_message) {
    git_reference *out;
    auto __result = git_reference_symbolic_create_matching(&out, repo, name, target, force, current_value, log_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_symbolic_create_matching");
    } else {
        return out;
    }
}



git_reference *reference_symbolic_create(git_repository *repo, const char *name, const char *target, int force, const char *log_message) {
    git_reference *out;
    auto __result = git_reference_symbolic_create(&out, repo, name, target, force, log_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_symbolic_create");
    } else {
        return out;
    }
}



git_reference *reference_create(git_repository *repo, const char *name, const git_oid *id, int force, const char *log_message) {
    git_reference *out;
    auto __result = git_reference_create(&out, repo, name, id, force, log_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_create");
    } else {
        return out;
    }
}



git_reference *reference_create_matching(git_repository *repo, const char *name, const git_oid *id, int force, const git_oid *current_id, const char *log_message) {
    git_reference *out;
    auto __result = git_reference_create_matching(&out, repo, name, id, force, current_id, log_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_create_matching");
    } else {
        return out;
    }
}



const git_oid *reference_target(const git_reference *ref) {
    auto __result = git_reference_target(ref);
    return __result;
}



const git_oid *reference_target_peel(const git_reference *ref) {
    auto __result = git_reference_target_peel(ref);
    return __result;
}



const char *reference_symbolic_target(const git_reference *ref) {
    auto __result = git_reference_symbolic_target(ref);
    return __result;
}



git_reference_t reference_type(const git_reference *ref) {
    auto __result = git_reference_type(ref);
    return __result;
}



const char *reference_name(const git_reference *ref) {
    auto __result = git_reference_name(ref);
    return __result;
}



git_reference *reference_resolve(const git_reference *ref) {
    git_reference *out;
    auto __result = git_reference_resolve(&out, ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_resolve");
    } else {
        return out;
    }
}



git_repository *reference_owner(const git_reference *ref) {
    auto __result = git_reference_owner(ref);
    return __result;
}



git_reference *reference_symbolic_set_target(git_reference *ref, const char *target, const char *log_message) {
    git_reference *out;
    auto __result = git_reference_symbolic_set_target(&out, ref, target, log_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_symbolic_set_target");
    } else {
        return out;
    }
}



git_reference *reference_set_target(git_reference *ref, const git_oid *id, const char *log_message) {
    git_reference *out;
    auto __result = git_reference_set_target(&out, ref, id, log_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_set_target");
    } else {
        return out;
    }
}



void reference_rename(git_reference **new_ref, git_reference *ref, const char *new_name, int force, const char *log_message) {
    auto __result = git_reference_rename(new_ref, ref, new_name, force, log_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_rename");
    }
}



void reference_delete(git_reference *ref) {
    auto __result = git_reference_delete(ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_delete");
    }
}



void reference_remove(git_repository *repo, const char *name) {
    auto __result = git_reference_remove(repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_remove");
    }
}



void reference_list(git_strarray *array, git_repository *repo) {
    auto __result = git_reference_list(array, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_list");
    }
}



void reference_foreach(git_repository *repo, git_reference_foreach_cb callback, void *payload) {
    auto __result = git_reference_foreach(repo, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_foreach");
    }
}



void reference_foreach_name(git_repository *repo, git_reference_foreach_name_cb callback, void *payload) {
    auto __result = git_reference_foreach_name(repo, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_foreach_name");
    }
}



void reference_dup(git_reference **dest, git_reference *source) {
    auto __result = git_reference_dup(dest, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_dup");
    }
}



void reference_free(git_reference *ref) {
    git_reference_free(ref);
}



void reference_cmp(const git_reference *ref1, const git_reference *ref2) {
    auto __result = git_reference_cmp(ref1, ref2);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_cmp");
    }
}



git_reference_iterator *reference_iterator_new(git_repository *repo) {
    git_reference_iterator *out;
    auto __result = git_reference_iterator_new(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_iterator_new");
    } else {
        return out;
    }
}



git_reference_iterator *reference_iterator_glob_new(git_repository *repo, const char *glob) {
    git_reference_iterator *out;
    auto __result = git_reference_iterator_glob_new(&out, repo, glob);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_iterator_glob_new");
    } else {
        return out;
    }
}



git_reference *reference_next(git_reference_iterator *iter) {
    git_reference *out;
    auto __result = git_reference_next(&out, iter);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_next");
    } else {
        return out;
    }
}



const char *reference_next_name(git_reference_iterator *iter) {
    const char *out;
    auto __result = git_reference_next_name(&out, iter);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_next_name");
    } else {
        return out;
    }
}



void reference_iterator_free(git_reference_iterator *iter) {
    git_reference_iterator_free(iter);
}



void reference_foreach_glob(git_repository *repo, const char *glob, git_reference_foreach_name_cb callback, void *payload) {
    auto __result = git_reference_foreach_glob(repo, glob, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_foreach_glob");
    }
}



void reference_has_log(git_repository *repo, const char *refname) {
    auto __result = git_reference_has_log(repo, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_has_log");
    }
}



void reference_ensure_log(git_repository *repo, const char *refname) {
    auto __result = git_reference_ensure_log(repo, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_ensure_log");
    }
}



void reference_is_branch(const git_reference *ref) {
    auto __result = git_reference_is_branch(ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_is_branch");
    }
}



void reference_is_remote(const git_reference *ref) {
    auto __result = git_reference_is_remote(ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_is_remote");
    }
}



void reference_is_tag(const git_reference *ref) {
    auto __result = git_reference_is_tag(ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_is_tag");
    }
}



void reference_is_note(const git_reference *ref) {
    auto __result = git_reference_is_note(ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_is_note");
    }
}



void reference_normalize_name(char *buffer_out, size_t buffer_size, const char *name, unsigned int flags) {
    auto __result = git_reference_normalize_name(buffer_out, buffer_size, name, flags);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_normalize_name");
    }
}



git_object *reference_peel(const git_reference *ref, git_object_t type) {
    git_object *out;
    auto __result = git_reference_peel(&out, ref, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_peel");
    } else {
        return out;
    }
}



void reference_name_is_valid(int *valid, const char *refname) {
    auto __result = git_reference_name_is_valid(valid, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reference_name_is_valid");
    }
}



const char *reference_shorthand(const git_reference *ref) {
    auto __result = git_reference_shorthand(ref);
    return __result;
}



void diff_options_init(git_diff_options *opts, unsigned int version) {
    auto __result = git_diff_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_options_init");
    }
}



void diff_find_options_init(git_diff_find_options *opts, unsigned int version) {
    auto __result = git_diff_find_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_find_options_init");
    }
}



void diff_free(git_diff *diff) {
    git_diff_free(diff);
}



void diff_tree_to_tree(git_diff **diff, git_repository *repo, git_tree *old_tree, git_tree *new_tree, const git_diff_options *opts) {
    auto __result = git_diff_tree_to_tree(diff, repo, old_tree, new_tree, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_tree_to_tree");
    }
}



void diff_tree_to_index(git_diff **diff, git_repository *repo, git_tree *old_tree, git_index *index, const git_diff_options *opts) {
    auto __result = git_diff_tree_to_index(diff, repo, old_tree, index, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_tree_to_index");
    }
}



void diff_index_to_workdir(git_diff **diff, git_repository *repo, git_index *index, const git_diff_options *opts) {
    auto __result = git_diff_index_to_workdir(diff, repo, index, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_index_to_workdir");
    }
}



void diff_tree_to_workdir(git_diff **diff, git_repository *repo, git_tree *old_tree, const git_diff_options *opts) {
    auto __result = git_diff_tree_to_workdir(diff, repo, old_tree, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_tree_to_workdir");
    }
}



void diff_tree_to_workdir_with_index(git_diff **diff, git_repository *repo, git_tree *old_tree, const git_diff_options *opts) {
    auto __result = git_diff_tree_to_workdir_with_index(diff, repo, old_tree, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_tree_to_workdir_with_index");
    }
}



void diff_index_to_index(git_diff **diff, git_repository *repo, git_index *old_index, git_index *new_index, const git_diff_options *opts) {
    auto __result = git_diff_index_to_index(diff, repo, old_index, new_index, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_index_to_index");
    }
}



void diff_merge(git_diff *onto, const git_diff *from) {
    auto __result = git_diff_merge(onto, from);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_merge");
    }
}



void diff_find_similar(git_diff *diff, const git_diff_find_options *options) {
    auto __result = git_diff_find_similar(diff, options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_find_similar");
    }
}



size_t diff_num_deltas(const git_diff *diff) {
    auto __result = git_diff_num_deltas(diff);
    return __result;
}



size_t diff_num_deltas_of_type(const git_diff *diff, git_delta_t type) {
    auto __result = git_diff_num_deltas_of_type(diff, type);
    return __result;
}



const git_diff_delta *diff_get_delta(const git_diff *diff, size_t idx) {
    auto __result = git_diff_get_delta(diff, idx);
    return __result;
}



void diff_is_sorted_icase(const git_diff *diff) {
    auto __result = git_diff_is_sorted_icase(diff);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_is_sorted_icase");
    }
}



void diff_foreach(git_diff *diff, git_diff_file_cb file_cb, git_diff_binary_cb binary_cb, git_diff_hunk_cb hunk_cb, git_diff_line_cb line_cb, void *payload) {
    auto __result = git_diff_foreach(diff, file_cb, binary_cb, hunk_cb, line_cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_foreach");
    }
}



char diff_status_char(git_delta_t status) {
    auto __result = git_diff_status_char(status);
    return __result;
}



void diff_print(git_diff *diff, git_diff_format_t format, git_diff_line_cb print_cb, void *payload) {
    auto __result = git_diff_print(diff, format, print_cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_print");
    }
}



git_buf diff_to_buf(git_diff *diff, git_diff_format_t format) {
    git_buf out;
    auto __result = git_diff_to_buf(&out, diff, format);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_to_buf");
    } else {
        return out;
    }
}



void diff_blobs(const git_blob *old_blob, const char *old_as_path, const git_blob *new_blob, const char *new_as_path, const git_diff_options *options, git_diff_file_cb file_cb, git_diff_binary_cb binary_cb, git_diff_hunk_cb hunk_cb, git_diff_line_cb line_cb, void *payload) {
    auto __result = git_diff_blobs(old_blob, old_as_path, new_blob, new_as_path, options, file_cb, binary_cb, hunk_cb, line_cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_blobs");
    }
}



void diff_blob_to_buffer(const git_blob *old_blob, const char *old_as_path, const char *buffer, size_t buffer_len, const char *buffer_as_path, const git_diff_options *options, git_diff_file_cb file_cb, git_diff_binary_cb binary_cb, git_diff_hunk_cb hunk_cb, git_diff_line_cb line_cb, void *payload) {
    auto __result = git_diff_blob_to_buffer(old_blob, old_as_path, buffer, buffer_len, buffer_as_path, options, file_cb, binary_cb, hunk_cb, line_cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_blob_to_buffer");
    }
}



void diff_buffers(const void *old_buffer, size_t old_len, const char *old_as_path, const void *new_buffer, size_t new_len, const char *new_as_path, const git_diff_options *options, git_diff_file_cb file_cb, git_diff_binary_cb binary_cb, git_diff_hunk_cb hunk_cb, git_diff_line_cb line_cb, void *payload) {
    auto __result = git_diff_buffers(old_buffer, old_len, old_as_path, new_buffer, new_len, new_as_path, options, file_cb, binary_cb, hunk_cb, line_cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_buffers");
    }
}



git_diff *diff_from_buffer(const char *content, size_t content_len) {
    git_diff *out;
    auto __result = git_diff_from_buffer(&out, content, content_len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_from_buffer");
    } else {
        return out;
    }
}



git_diff_stats *diff_get_stats(git_diff *diff) {
    git_diff_stats *out;
    auto __result = git_diff_get_stats(&out, diff);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_get_stats");
    } else {
        return out;
    }
}



size_t diff_stats_files_changed(const git_diff_stats *stats) {
    auto __result = git_diff_stats_files_changed(stats);
    return __result;
}



size_t diff_stats_insertions(const git_diff_stats *stats) {
    auto __result = git_diff_stats_insertions(stats);
    return __result;
}



size_t diff_stats_deletions(const git_diff_stats *stats) {
    auto __result = git_diff_stats_deletions(stats);
    return __result;
}



git_buf diff_stats_to_buf(const git_diff_stats *stats, git_diff_stats_format_t format, size_t width) {
    git_buf out;
    auto __result = git_diff_stats_to_buf(&out, stats, format, width);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_stats_to_buf");
    } else {
        return out;
    }
}



void diff_stats_free(git_diff_stats *stats) {
    git_diff_stats_free(stats);
}



void diff_patchid_options_init(git_diff_patchid_options *opts, unsigned int version) {
    auto __result = git_diff_patchid_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_patchid_options_init");
    }
}



git_oid diff_patchid(git_diff *diff, git_diff_patchid_options *opts) {
    git_oid out;
    auto __result = git_diff_patchid(&out, diff, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_diff_patchid");
    } else {
        return out;
    }
}



void apply_options_init(git_apply_options *opts, unsigned int version) {
    auto __result = git_apply_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_apply_options_init");
    }
}



git_index *apply_to_tree(git_repository *repo, git_tree *preimage, git_diff *diff, const git_apply_options *options) {
    git_index *out;
    auto __result = git_apply_to_tree(&out, repo, preimage, diff, options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_apply_to_tree");
    } else {
        return out;
    }
}



void apply(git_repository *repo, git_diff *diff, git_apply_location_t location, const git_apply_options *options) {
    auto __result = git_apply(repo, diff, location, options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_apply");
    }
}



git_attr_value_t attr_value(const char *attr) {
    auto __result = git_attr_value(attr);
    return __result;
}



void attr_get(const char **value_out, git_repository *repo, uint32_t flags, const char *path, const char *name) {
    auto __result = git_attr_get(value_out, repo, flags, path, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_attr_get");
    }
}



void attr_get_ext(const char **value_out, git_repository *repo, git_attr_options *opts, const char *path, const char *name) {
    auto __result = git_attr_get_ext(value_out, repo, opts, path, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_attr_get_ext");
    }
}



void attr_get_many(const char **values_out, git_repository *repo, uint32_t flags, const char *path, size_t num_attr, const char **names) {
    auto __result = git_attr_get_many(values_out, repo, flags, path, num_attr, names);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_attr_get_many");
    }
}



void attr_get_many_ext(const char **values_out, git_repository *repo, git_attr_options *opts, const char *path, size_t num_attr, const char **names) {
    auto __result = git_attr_get_many_ext(values_out, repo, opts, path, num_attr, names);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_attr_get_many_ext");
    }
}



void attr_foreach(git_repository *repo, uint32_t flags, const char *path, git_attr_foreach_cb callback, void *payload) {
    auto __result = git_attr_foreach(repo, flags, path, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_attr_foreach");
    }
}



void attr_foreach_ext(git_repository *repo, git_attr_options *opts, const char *path, git_attr_foreach_cb callback, void *payload) {
    auto __result = git_attr_foreach_ext(repo, opts, path, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_attr_foreach_ext");
    }
}



void attr_cache_flush(git_repository *repo) {
    auto __result = git_attr_cache_flush(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_attr_cache_flush");
    }
}



void attr_add_macro(git_repository *repo, const char *name, const char *values) {
    auto __result = git_attr_add_macro(repo, name, values);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_attr_add_macro");
    }
}



void blob_lookup(git_blob **blob, git_repository *repo, const git_oid *id) {
    auto __result = git_blob_lookup(blob, repo, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_lookup");
    }
}



void blob_lookup_prefix(git_blob **blob, git_repository *repo, const git_oid *id, size_t len) {
    auto __result = git_blob_lookup_prefix(blob, repo, id, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_lookup_prefix");
    }
}



void blob_free(git_blob *blob) {
    git_blob_free(blob);
}



const git_oid *blob_id(const git_blob *blob) {
    auto __result = git_blob_id(blob);
    return __result;
}



git_repository *blob_owner(const git_blob *blob) {
    auto __result = git_blob_owner(blob);
    return __result;
}



const void *blob_rawcontent(const git_blob *blob) {
    auto __result = git_blob_rawcontent(blob);
    return __result;
}



git_object_size_t blob_rawsize(const git_blob *blob) {
    auto __result = git_blob_rawsize(blob);
    return __result;
}



void blob_filter_options_init(git_blob_filter_options *opts, unsigned int version) {
    auto __result = git_blob_filter_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_filter_options_init");
    }
}



git_buf blob_filter(git_blob *blob, const char *as_path, git_blob_filter_options *opts) {
    git_buf out;
    auto __result = git_blob_filter(&out, blob, as_path, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_filter");
    } else {
        return out;
    }
}



void blob_create_from_workdir(git_oid *id, git_repository *repo, const char *relative_path) {
    auto __result = git_blob_create_from_workdir(id, repo, relative_path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_create_from_workdir");
    }
}



void blob_create_from_disk(git_oid *id, git_repository *repo, const char *path) {
    auto __result = git_blob_create_from_disk(id, repo, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_create_from_disk");
    }
}



git_writestream *blob_create_from_stream(git_repository *repo, const char *hintpath) {
    git_writestream *out;
    auto __result = git_blob_create_from_stream(&out, repo, hintpath);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_create_from_stream");
    } else {
        return out;
    }
}



git_oid blob_create_from_stream_commit(git_writestream *stream) {
    git_oid out;
    auto __result = git_blob_create_from_stream_commit(&out, stream);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_create_from_stream_commit");
    } else {
        return out;
    }
}



void blob_create_from_buffer(git_oid *id, git_repository *repo, const void *buffer, size_t len) {
    auto __result = git_blob_create_from_buffer(id, repo, buffer, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_create_from_buffer");
    }
}



void blob_is_binary(const git_blob *blob) {
    auto __result = git_blob_is_binary(blob);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_is_binary");
    }
}



void blob_data_is_binary(const char *data, size_t len) {
    auto __result = git_blob_data_is_binary(data, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_data_is_binary");
    }
}



git_blob *blob_dup(git_blob *source) {
    git_blob *out;
    auto __result = git_blob_dup(&out, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blob_dup");
    } else {
        return out;
    }
}



void blame_options_init(git_blame_options *opts, unsigned int version) {
    auto __result = git_blame_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blame_options_init");
    }
}



uint32_t blame_get_hunk_count(git_blame *blame) {
    auto __result = git_blame_get_hunk_count(blame);
    return __result;
}



const git_blame_hunk *blame_get_hunk_byindex(git_blame *blame, uint32_t index) {
    auto __result = git_blame_get_hunk_byindex(blame, index);
    return __result;
}



const git_blame_hunk *blame_get_hunk_byline(git_blame *blame, size_t lineno) {
    auto __result = git_blame_get_hunk_byline(blame, lineno);
    return __result;
}



git_blame *blame_file(git_repository *repo, const char *path, git_blame_options *options) {
    git_blame *out;
    auto __result = git_blame_file(&out, repo, path, options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blame_file");
    } else {
        return out;
    }
}



git_blame *blame_buffer(git_blame *reference, const char *buffer, size_t buffer_len) {
    git_blame *out;
    auto __result = git_blame_buffer(&out, reference, buffer, buffer_len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_blame_buffer");
    } else {
        return out;
    }
}



void blame_free(git_blame *blame) {
    git_blame_free(blame);
}



git_reference *branch_create(git_repository *repo, const char *branch_name, const git_commit *target, int force) {
    git_reference *out;
    auto __result = git_branch_create(&out, repo, branch_name, target, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_create");
    } else {
        return out;
    }
}



void branch_create_from_annotated(git_reference **ref_out, git_repository *repository, const char *branch_name, const git_annotated_commit *commit, int force) {
    auto __result = git_branch_create_from_annotated(ref_out, repository, branch_name, commit, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_create_from_annotated");
    }
}



void branch_delete(git_reference *branch) {
    auto __result = git_branch_delete(branch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_delete");
    }
}



git_branch_iterator *branch_iterator_new(git_repository *repo, git_branch_t list_flags) {
    git_branch_iterator *out;
    auto __result = git_branch_iterator_new(&out, repo, list_flags);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_iterator_new");
    } else {
        return out;
    }
}



git_reference *branch_next(git_branch_t *out_type, git_branch_iterator *iter) {
    git_reference *out;
    auto __result = git_branch_next(&out, out_type, iter);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_next");
    } else {
        return out;
    }
}



void branch_iterator_free(git_branch_iterator *iter) {
    git_branch_iterator_free(iter);
}



git_reference *branch_move(git_reference *branch, const char *new_branch_name, int force) {
    git_reference *out;
    auto __result = git_branch_move(&out, branch, new_branch_name, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_move");
    } else {
        return out;
    }
}



git_reference *branch_lookup(git_repository *repo, const char *branch_name, git_branch_t branch_type) {
    git_reference *out;
    auto __result = git_branch_lookup(&out, repo, branch_name, branch_type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_lookup");
    } else {
        return out;
    }
}



const char *branch_name(const git_reference *ref) {
    const char *out;
    auto __result = git_branch_name(&out, ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_name");
    } else {
        return out;
    }
}



git_reference *branch_upstream(const git_reference *branch) {
    git_reference *out;
    auto __result = git_branch_upstream(&out, branch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_upstream");
    } else {
        return out;
    }
}



void branch_set_upstream(git_reference *branch, const char *branch_name) {
    auto __result = git_branch_set_upstream(branch, branch_name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_set_upstream");
    }
}



git_buf branch_upstream_name(git_repository *repo, const char *refname) {
    git_buf out;
    auto __result = git_branch_upstream_name(&out, repo, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_upstream_name");
    } else {
        return out;
    }
}



void branch_is_head(const git_reference *branch) {
    auto __result = git_branch_is_head(branch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_is_head");
    }
}



void branch_is_checked_out(const git_reference *branch) {
    auto __result = git_branch_is_checked_out(branch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_is_checked_out");
    }
}



git_buf branch_remote_name(git_repository *repo, const char *refname) {
    git_buf out;
    auto __result = git_branch_remote_name(&out, repo, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_remote_name");
    } else {
        return out;
    }
}



void branch_upstream_remote(git_buf *buf, git_repository *repo, const char *refname) {
    auto __result = git_branch_upstream_remote(buf, repo, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_upstream_remote");
    }
}



void branch_upstream_merge(git_buf *buf, git_repository *repo, const char *refname) {
    auto __result = git_branch_upstream_merge(buf, repo, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_upstream_merge");
    }
}



void branch_name_is_valid(int *valid, const char *name) {
    auto __result = git_branch_name_is_valid(valid, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_branch_name_is_valid");
    }
}



void checkout_options_init(git_checkout_options *opts, unsigned int version) {
    auto __result = git_checkout_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_checkout_options_init");
    }
}



void checkout_head(git_repository *repo, const git_checkout_options *opts) {
    auto __result = git_checkout_head(repo, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_checkout_head");
    }
}



void checkout_index(git_repository *repo, git_index *index, const git_checkout_options *opts) {
    auto __result = git_checkout_index(repo, index, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_checkout_index");
    }
}



void checkout_tree(git_repository *repo, const git_object *treeish, const git_checkout_options *opts) {
    auto __result = git_checkout_tree(repo, treeish, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_checkout_tree");
    }
}



void oidarray_dispose(git_oidarray *array) {
    git_oidarray_dispose(array);
}



void indexer_options_init(git_indexer_options *opts, unsigned int version) {
    auto __result = git_indexer_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_indexer_options_init");
    }
}



git_indexer *indexer_new(const char *path, unsigned int mode, git_odb *odb, git_indexer_options *opts) {
    git_indexer *out;
    auto __result = git_indexer_new(&out, path, mode, odb, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_indexer_new");
    } else {
        return out;
    }
}



void indexer_append(git_indexer *idx, const void *data, size_t size, git_indexer_progress *stats) {
    auto __result = git_indexer_append(idx, data, size, stats);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_indexer_append");
    }
}



void indexer_commit(git_indexer *idx, git_indexer_progress *stats) {
    auto __result = git_indexer_commit(idx, stats);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_indexer_commit");
    }
}



const git_oid *indexer_hash(const git_indexer *idx) {
    auto __result = git_indexer_hash(idx);
    return __result;
}



const char *indexer_name(const git_indexer *idx) {
    auto __result = git_indexer_name(idx);
    return __result;
}



void indexer_free(git_indexer *idx) {
    git_indexer_free(idx);
}



git_index *index_open(const char *index_path) {
    git_index *out;
    auto __result = git_index_open(&out, index_path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_open");
    } else {
        return out;
    }
}



git_index *index_new() {
    git_index *out;
    auto __result = git_index_new(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_new");
    } else {
        return out;
    }
}



void index_free(git_index *index) {
    git_index_free(index);
}



git_repository *index_owner(const git_index *index) {
    auto __result = git_index_owner(index);
    return __result;
}



void index_caps(const git_index *index) {
    auto __result = git_index_caps(index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_caps");
    }
}



void index_set_caps(git_index *index, int caps) {
    auto __result = git_index_set_caps(index, caps);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_set_caps");
    }
}



unsigned int index_version(git_index *index) {
    auto __result = git_index_version(index);
    return __result;
}



void index_set_version(git_index *index, unsigned int version) {
    auto __result = git_index_set_version(index, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_set_version");
    }
}



void index_read(git_index *index, int force) {
    auto __result = git_index_read(index, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_read");
    }
}



void index_write(git_index *index) {
    auto __result = git_index_write(index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_write");
    }
}



const char *index_path(const git_index *index) {
    auto __result = git_index_path(index);
    return __result;
}



const git_oid *index_checksum(git_index *index) {
    auto __result = git_index_checksum(index);
    return __result;
}



void index_read_tree(git_index *index, const git_tree *tree) {
    auto __result = git_index_read_tree(index, tree);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_read_tree");
    }
}



git_oid index_write_tree(git_index *index) {
    git_oid out;
    auto __result = git_index_write_tree(&out, index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_write_tree");
    } else {
        return out;
    }
}



git_oid index_write_tree_to(git_index *index, git_repository *repo) {
    git_oid out;
    auto __result = git_index_write_tree_to(&out, index, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_write_tree_to");
    } else {
        return out;
    }
}



size_t index_entrycount(const git_index *index) {
    auto __result = git_index_entrycount(index);
    return __result;
}



void index_clear(git_index *index) {
    auto __result = git_index_clear(index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_clear");
    }
}



const git_index_entry *index_get_byindex(git_index *index, size_t n) {
    auto __result = git_index_get_byindex(index, n);
    return __result;
}



const git_index_entry *index_get_bypath(git_index *index, const char *path, int stage) {
    auto __result = git_index_get_bypath(index, path, stage);
    return __result;
}



void index_remove(git_index *index, const char *path, int stage) {
    auto __result = git_index_remove(index, path, stage);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_remove");
    }
}



void index_remove_directory(git_index *index, const char *dir, int stage) {
    auto __result = git_index_remove_directory(index, dir, stage);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_remove_directory");
    }
}



void index_add(git_index *index, const git_index_entry *source_entry) {
    auto __result = git_index_add(index, source_entry);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_add");
    }
}



void index_entry_stage(const git_index_entry *entry) {
    auto __result = git_index_entry_stage(entry);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_entry_stage");
    }
}



void index_entry_is_conflict(const git_index_entry *entry) {
    auto __result = git_index_entry_is_conflict(entry);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_entry_is_conflict");
    }
}



void index_iterator_new(git_index_iterator **iterator_out, git_index *index) {
    auto __result = git_index_iterator_new(iterator_out, index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_iterator_new");
    }
}



const git_index_entry *index_iterator_next(git_index_iterator *iterator) {
    const git_index_entry *out;
    auto __result = git_index_iterator_next(&out, iterator);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_iterator_next");
    } else {
        return out;
    }
}



void index_iterator_free(git_index_iterator *iterator) {
    git_index_iterator_free(iterator);
}



void index_add_bypath(git_index *index, const char *path) {
    auto __result = git_index_add_bypath(index, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_add_bypath");
    }
}



void index_add_from_buffer(git_index *index, const git_index_entry *entry, const void *buffer, size_t len) {
    auto __result = git_index_add_from_buffer(index, entry, buffer, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_add_from_buffer");
    }
}



void index_remove_bypath(git_index *index, const char *path) {
    auto __result = git_index_remove_bypath(index, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_remove_bypath");
    }
}



void index_add_all(git_index *index, const git_strarray *pathspec, unsigned int flags, git_index_matched_path_cb callback, void *payload) {
    auto __result = git_index_add_all(index, pathspec, flags, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_add_all");
    }
}



void index_remove_all(git_index *index, const git_strarray *pathspec, git_index_matched_path_cb callback, void *payload) {
    auto __result = git_index_remove_all(index, pathspec, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_remove_all");
    }
}



void index_update_all(git_index *index, const git_strarray *pathspec, git_index_matched_path_cb callback, void *payload) {
    auto __result = git_index_update_all(index, pathspec, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_update_all");
    }
}



void index_find(size_t *at_pos, git_index *index, const char *path) {
    auto __result = git_index_find(at_pos, index, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_find");
    }
}



void index_find_prefix(size_t *at_pos, git_index *index, const char *prefix) {
    auto __result = git_index_find_prefix(at_pos, index, prefix);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_find_prefix");
    }
}



void index_conflict_add(git_index *index, const git_index_entry *ancestor_entry, const git_index_entry *our_entry, const git_index_entry *their_entry) {
    auto __result = git_index_conflict_add(index, ancestor_entry, our_entry, their_entry);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_conflict_add");
    }
}



void index_conflict_get(const git_index_entry **ancestor_out, const git_index_entry **our_out, const git_index_entry **their_out, git_index *index, const char *path) {
    auto __result = git_index_conflict_get(ancestor_out, our_out, their_out, index, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_conflict_get");
    }
}



void index_conflict_remove(git_index *index, const char *path) {
    auto __result = git_index_conflict_remove(index, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_conflict_remove");
    }
}



void index_conflict_cleanup(git_index *index) {
    auto __result = git_index_conflict_cleanup(index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_conflict_cleanup");
    }
}



void index_has_conflicts(const git_index *index) {
    auto __result = git_index_has_conflicts(index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_has_conflicts");
    }
}



void index_conflict_iterator_new(git_index_conflict_iterator **iterator_out, git_index *index) {
    auto __result = git_index_conflict_iterator_new(iterator_out, index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_conflict_iterator_new");
    }
}



void index_conflict_next(const git_index_entry **ancestor_out, const git_index_entry **our_out, const git_index_entry **their_out, git_index_conflict_iterator *iterator) {
    auto __result = git_index_conflict_next(ancestor_out, our_out, their_out, iterator);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_index_conflict_next");
    }
}



void index_conflict_iterator_free(git_index_conflict_iterator *iterator) {
    git_index_conflict_iterator_free(iterator);
}



void merge_file_input_init(git_merge_file_input *opts, unsigned int version) {
    auto __result = git_merge_file_input_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_file_input_init");
    }
}



void merge_file_options_init(git_merge_file_options *opts, unsigned int version) {
    auto __result = git_merge_file_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_file_options_init");
    }
}



void merge_options_init(git_merge_options *opts, unsigned int version) {
    auto __result = git_merge_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_options_init");
    }
}



void merge_analysis(git_merge_analysis_t *analysis_out, git_merge_preference_t *preference_out, git_repository *repo, const git_annotated_commit **their_heads, size_t their_heads_len) {
    auto __result = git_merge_analysis(analysis_out, preference_out, repo, their_heads, their_heads_len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_analysis");
    }
}



void merge_analysis_for_ref(git_merge_analysis_t *analysis_out, git_merge_preference_t *preference_out, git_repository *repo, git_reference *our_ref, const git_annotated_commit **their_heads, size_t their_heads_len) {
    auto __result = git_merge_analysis_for_ref(analysis_out, preference_out, repo, our_ref, their_heads, their_heads_len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_analysis_for_ref");
    }
}



git_oid merge_base(git_repository *repo, const git_oid *one, const git_oid *two) {
    git_oid out;
    auto __result = git_merge_base(&out, repo, one, two);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_base");
    } else {
        return out;
    }
}



git_oidarray merge_bases(git_repository *repo, const git_oid *one, const git_oid *two) {
    git_oidarray out;
    auto __result = git_merge_bases(&out, repo, one, two);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_bases");
    } else {
        return out;
    }
}



git_oid merge_base_many(git_repository *repo, size_t length, const git_oid input_array[]) {
    git_oid out;
    auto __result = git_merge_base_many(&out, repo, length, input_array);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_base_many");
    } else {
        return out;
    }
}



git_oidarray merge_bases_many(git_repository *repo, size_t length, const git_oid input_array[]) {
    git_oidarray out;
    auto __result = git_merge_bases_many(&out, repo, length, input_array);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_bases_many");
    } else {
        return out;
    }
}



git_oid merge_base_octopus(git_repository *repo, size_t length, const git_oid input_array[]) {
    git_oid out;
    auto __result = git_merge_base_octopus(&out, repo, length, input_array);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_base_octopus");
    } else {
        return out;
    }
}



git_merge_file_result merge_file(const git_merge_file_input *ancestor, const git_merge_file_input *ours, const git_merge_file_input *theirs, const git_merge_file_options *opts) {
    git_merge_file_result out;
    auto __result = git_merge_file(&out, ancestor, ours, theirs, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_file");
    } else {
        return out;
    }
}



git_merge_file_result merge_file_from_index(git_repository *repo, const git_index_entry *ancestor, const git_index_entry *ours, const git_index_entry *theirs, const git_merge_file_options *opts) {
    git_merge_file_result out;
    auto __result = git_merge_file_from_index(&out, repo, ancestor, ours, theirs, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_file_from_index");
    } else {
        return out;
    }
}



void merge_file_result_free(git_merge_file_result *result) {
    git_merge_file_result_free(result);
}



git_index *merge_trees(git_repository *repo, const git_tree *ancestor_tree, const git_tree *our_tree, const git_tree *their_tree, const git_merge_options *opts) {
    git_index *out;
    auto __result = git_merge_trees(&out, repo, ancestor_tree, our_tree, their_tree, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_trees");
    } else {
        return out;
    }
}



git_index *merge_commits(git_repository *repo, const git_commit *our_commit, const git_commit *their_commit, const git_merge_options *opts) {
    git_index *out;
    auto __result = git_merge_commits(&out, repo, our_commit, their_commit, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge_commits");
    } else {
        return out;
    }
}



void merge(git_repository *repo, const git_annotated_commit **their_heads, size_t their_heads_len, const git_merge_options *merge_opts, const git_checkout_options *checkout_opts) {
    auto __result = git_merge(repo, their_heads, their_heads_len, merge_opts, checkout_opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_merge");
    }
}



void cherrypick_options_init(git_cherrypick_options *opts, unsigned int version) {
    auto __result = git_cherrypick_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_cherrypick_options_init");
    }
}



git_index *cherrypick_commit(git_repository *repo, git_commit *cherrypick_commit, git_commit *our_commit, unsigned int mainline, const git_merge_options *merge_options) {
    git_index *out;
    auto __result = git_cherrypick_commit(&out, repo, cherrypick_commit, our_commit, mainline, merge_options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_cherrypick_commit");
    } else {
        return out;
    }
}



void cherrypick(git_repository *repo, git_commit *commit, const git_cherrypick_options *cherrypick_options) {
    auto __result = git_cherrypick(repo, commit, cherrypick_options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_cherrypick");
    }
}



void refspec_parse(git_refspec **refspec, const char *input, int is_fetch) {
    auto __result = git_refspec_parse(refspec, input, is_fetch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refspec_parse");
    }
}



void refspec_free(git_refspec *refspec) {
    git_refspec_free(refspec);
}



const char *refspec_src(const git_refspec *refspec) {
    auto __result = git_refspec_src(refspec);
    return __result;
}



const char *refspec_dst(const git_refspec *refspec) {
    auto __result = git_refspec_dst(refspec);
    return __result;
}



const char *refspec_string(const git_refspec *refspec) {
    auto __result = git_refspec_string(refspec);
    return __result;
}



void refspec_force(const git_refspec *refspec) {
    auto __result = git_refspec_force(refspec);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refspec_force");
    }
}



git_direction refspec_direction(const git_refspec *spec) {
    auto __result = git_refspec_direction(spec);
    return __result;
}



void refspec_src_matches(const git_refspec *refspec, const char *refname) {
    auto __result = git_refspec_src_matches(refspec, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refspec_src_matches");
    }
}



void refspec_dst_matches(const git_refspec *refspec, const char *refname) {
    auto __result = git_refspec_dst_matches(refspec, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refspec_dst_matches");
    }
}



git_buf refspec_transform(const git_refspec *spec, const char *name) {
    git_buf out;
    auto __result = git_refspec_transform(&out, spec, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refspec_transform");
    } else {
        return out;
    }
}



git_buf refspec_rtransform(const git_refspec *spec, const char *name) {
    git_buf out;
    auto __result = git_refspec_rtransform(&out, spec, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refspec_rtransform");
    } else {
        return out;
    }
}



void credential_free(git_credential *cred) {
    git_credential_free(cred);
}



void credential_has_username(git_credential *cred) {
    auto __result = git_credential_has_username(cred);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_has_username");
    }
}



const char *credential_get_username(git_credential *cred) {
    auto __result = git_credential_get_username(cred);
    return __result;
}



git_credential *credential_userpass_plaintext_new(const char *username, const char *password) {
    git_credential *out;
    auto __result = git_credential_userpass_plaintext_new(&out, username, password);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_userpass_plaintext_new");
    } else {
        return out;
    }
}



git_credential *credential_default_new() {
    git_credential *out;
    auto __result = git_credential_default_new(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_default_new");
    } else {
        return out;
    }
}



git_credential *credential_username_new(const char *username) {
    git_credential *out;
    auto __result = git_credential_username_new(&out, username);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_username_new");
    } else {
        return out;
    }
}



git_credential *credential_ssh_key_new(const char *username, const char *publickey, const char *privatekey, const char *passphrase) {
    git_credential *out;
    auto __result = git_credential_ssh_key_new(&out, username, publickey, privatekey, passphrase);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_ssh_key_new");
    } else {
        return out;
    }
}



git_credential *credential_ssh_key_memory_new(const char *username, const char *publickey, const char *privatekey, const char *passphrase) {
    git_credential *out;
    auto __result = git_credential_ssh_key_memory_new(&out, username, publickey, privatekey, passphrase);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_ssh_key_memory_new");
    } else {
        return out;
    }
}



git_credential *credential_ssh_interactive_new(const char *username, git_credential_ssh_interactive_cb prompt_callback, void *payload) {
    git_credential *out;
    auto __result = git_credential_ssh_interactive_new(&out, username, prompt_callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_ssh_interactive_new");
    } else {
        return out;
    }
}



git_credential *credential_ssh_key_from_agent(const char *username) {
    git_credential *out;
    auto __result = git_credential_ssh_key_from_agent(&out, username);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_ssh_key_from_agent");
    } else {
        return out;
    }
}



git_credential *credential_ssh_custom_new(const char *username, const char *publickey, size_t publickey_len, git_credential_sign_cb sign_callback, void *payload) {
    git_credential *out;
    auto __result = git_credential_ssh_custom_new(&out, username, publickey, publickey_len, sign_callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_ssh_custom_new");
    } else {
        return out;
    }
}



git_packbuilder *packbuilder_new(git_repository *repo) {
    git_packbuilder *out;
    auto __result = git_packbuilder_new(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_new");
    } else {
        return out;
    }
}



unsigned int packbuilder_set_threads(git_packbuilder *pb, unsigned int n) {
    auto __result = git_packbuilder_set_threads(pb, n);
    return __result;
}



void packbuilder_insert(git_packbuilder *pb, const git_oid *id, const char *name) {
    auto __result = git_packbuilder_insert(pb, id, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_insert");
    }
}



void packbuilder_insert_tree(git_packbuilder *pb, const git_oid *id) {
    auto __result = git_packbuilder_insert_tree(pb, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_insert_tree");
    }
}



void packbuilder_insert_commit(git_packbuilder *pb, const git_oid *id) {
    auto __result = git_packbuilder_insert_commit(pb, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_insert_commit");
    }
}



void packbuilder_insert_walk(git_packbuilder *pb, git_revwalk *walk) {
    auto __result = git_packbuilder_insert_walk(pb, walk);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_insert_walk");
    }
}



void packbuilder_insert_recur(git_packbuilder *pb, const git_oid *id, const char *name) {
    auto __result = git_packbuilder_insert_recur(pb, id, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_insert_recur");
    }
}



void packbuilder_write_buf(git_buf *buf, git_packbuilder *pb) {
    auto __result = git_packbuilder_write_buf(buf, pb);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_write_buf");
    }
}



void packbuilder_write(git_packbuilder *pb, const char *path, unsigned int mode, git_indexer_progress_cb progress_cb, void *progress_cb_payload) {
    auto __result = git_packbuilder_write(pb, path, mode, progress_cb, progress_cb_payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_write");
    }
}



const git_oid *packbuilder_hash(git_packbuilder *pb) {
    auto __result = git_packbuilder_hash(pb);
    return __result;
}



const char *packbuilder_name(git_packbuilder *pb) {
    auto __result = git_packbuilder_name(pb);
    return __result;
}



void packbuilder_foreach(git_packbuilder *pb, git_packbuilder_foreach_cb cb, void *payload) {
    auto __result = git_packbuilder_foreach(pb, cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_foreach");
    }
}



size_t packbuilder_object_count(git_packbuilder *pb) {
    auto __result = git_packbuilder_object_count(pb);
    return __result;
}



size_t packbuilder_written(git_packbuilder *pb) {
    auto __result = git_packbuilder_written(pb);
    return __result;
}



void packbuilder_set_callbacks(git_packbuilder *pb, git_packbuilder_progress progress_cb, void *progress_cb_payload) {
    auto __result = git_packbuilder_set_callbacks(pb, progress_cb, progress_cb_payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_packbuilder_set_callbacks");
    }
}



void packbuilder_free(git_packbuilder *pb) {
    git_packbuilder_free(pb);
}



void proxy_options_init(git_proxy_options *opts, unsigned int version) {
    auto __result = git_proxy_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_proxy_options_init");
    }
}



git_remote *remote_create(git_repository *repo, const char *name, const char *url) {
    git_remote *out;
    auto __result = git_remote_create(&out, repo, name, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_create");
    } else {
        return out;
    }
}



void remote_create_options_init(git_remote_create_options *opts, unsigned int version) {
    auto __result = git_remote_create_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_create_options_init");
    }
}



git_remote *remote_create_with_opts(const char *url, const git_remote_create_options *opts) {
    git_remote *out;
    auto __result = git_remote_create_with_opts(&out, url, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_create_with_opts");
    } else {
        return out;
    }
}



git_remote *remote_create_with_fetchspec(git_repository *repo, const char *name, const char *url, const char *fetch) {
    git_remote *out;
    auto __result = git_remote_create_with_fetchspec(&out, repo, name, url, fetch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_create_with_fetchspec");
    } else {
        return out;
    }
}



git_remote *remote_create_anonymous(git_repository *repo, const char *url) {
    git_remote *out;
    auto __result = git_remote_create_anonymous(&out, repo, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_create_anonymous");
    } else {
        return out;
    }
}



git_remote *remote_create_detached(const char *url) {
    git_remote *out;
    auto __result = git_remote_create_detached(&out, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_create_detached");
    } else {
        return out;
    }
}



git_remote *remote_lookup(git_repository *repo, const char *name) {
    git_remote *out;
    auto __result = git_remote_lookup(&out, repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_lookup");
    } else {
        return out;
    }
}



void remote_dup(git_remote **dest, git_remote *source) {
    auto __result = git_remote_dup(dest, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_dup");
    }
}



git_repository *remote_owner(const git_remote *remote) {
    auto __result = git_remote_owner(remote);
    return __result;
}



const char *remote_name(const git_remote *remote) {
    auto __result = git_remote_name(remote);
    return __result;
}



const char *remote_url(const git_remote *remote) {
    auto __result = git_remote_url(remote);
    return __result;
}



const char *remote_pushurl(const git_remote *remote) {
    auto __result = git_remote_pushurl(remote);
    return __result;
}



void remote_set_url(git_repository *repo, const char *remote, const char *url) {
    auto __result = git_remote_set_url(repo, remote, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_set_url");
    }
}



void remote_set_pushurl(git_repository *repo, const char *remote, const char *url) {
    auto __result = git_remote_set_pushurl(repo, remote, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_set_pushurl");
    }
}



void remote_set_instance_url(git_remote *remote, const char *url) {
    auto __result = git_remote_set_instance_url(remote, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_set_instance_url");
    }
}



void remote_set_instance_pushurl(git_remote *remote, const char *url) {
    auto __result = git_remote_set_instance_pushurl(remote, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_set_instance_pushurl");
    }
}



void remote_add_fetch(git_repository *repo, const char *remote, const char *refspec) {
    auto __result = git_remote_add_fetch(repo, remote, refspec);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_add_fetch");
    }
}



void remote_get_fetch_refspecs(git_strarray *array, const git_remote *remote) {
    auto __result = git_remote_get_fetch_refspecs(array, remote);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_get_fetch_refspecs");
    }
}



void remote_add_push(git_repository *repo, const char *remote, const char *refspec) {
    auto __result = git_remote_add_push(repo, remote, refspec);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_add_push");
    }
}



void remote_get_push_refspecs(git_strarray *array, const git_remote *remote) {
    auto __result = git_remote_get_push_refspecs(array, remote);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_get_push_refspecs");
    }
}



size_t remote_refspec_count(const git_remote *remote) {
    auto __result = git_remote_refspec_count(remote);
    return __result;
}



const git_refspec *remote_get_refspec(const git_remote *remote, size_t n) {
    auto __result = git_remote_get_refspec(remote, n);
    return __result;
}



const git_remote_head **remote_ls(size_t *size, git_remote *remote) {
    const git_remote_head **out;
    auto __result = git_remote_ls(&out, size, remote);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_ls");
    } else {
        return out;
    }
}



void remote_connected(const git_remote *remote) {
    auto __result = git_remote_connected(remote);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_connected");
    }
}



void remote_stop(git_remote *remote) {
    auto __result = git_remote_stop(remote);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_stop");
    }
}



void remote_disconnect(git_remote *remote) {
    auto __result = git_remote_disconnect(remote);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_disconnect");
    }
}



void remote_free(git_remote *remote) {
    git_remote_free(remote);
}



git_strarray remote_list(git_repository *repo) {
    git_strarray out;
    auto __result = git_remote_list(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_list");
    } else {
        return out;
    }
}



void remote_init_callbacks(git_remote_callbacks *opts, unsigned int version) {
    auto __result = git_remote_init_callbacks(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_init_callbacks");
    }
}



void fetch_options_init(git_fetch_options *opts, unsigned int version) {
    auto __result = git_fetch_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_fetch_options_init");
    }
}



void push_options_init(git_push_options *opts, unsigned int version) {
    auto __result = git_push_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_push_options_init");
    }
}



void remote_connect_options_init(git_remote_connect_options *opts, unsigned int version) {
    auto __result = git_remote_connect_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_connect_options_init");
    }
}



void remote_connect(git_remote *remote, git_direction direction, const git_remote_callbacks *callbacks, const git_proxy_options *proxy_opts, const git_strarray *custom_headers) {
    auto __result = git_remote_connect(remote, direction, callbacks, proxy_opts, custom_headers);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_connect");
    }
}



void remote_connect_ext(git_remote *remote, git_direction direction, const git_remote_connect_options *opts) {
    auto __result = git_remote_connect_ext(remote, direction, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_connect_ext");
    }
}



void remote_download(git_remote *remote, const git_strarray *refspecs, const git_fetch_options *opts) {
    auto __result = git_remote_download(remote, refspecs, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_download");
    }
}



void remote_upload(git_remote *remote, const git_strarray *refspecs, const git_push_options *opts) {
    auto __result = git_remote_upload(remote, refspecs, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_upload");
    }
}



void remote_update_tips(git_remote *remote, const git_remote_callbacks *callbacks, int update_fetchhead, git_remote_autotag_option_t download_tags, const char *reflog_message) {
    auto __result = git_remote_update_tips(remote, callbacks, update_fetchhead, download_tags, reflog_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_update_tips");
    }
}



void remote_fetch(git_remote *remote, const git_strarray *refspecs, const git_fetch_options *opts, const char *reflog_message) {
    auto __result = git_remote_fetch(remote, refspecs, opts, reflog_message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_fetch");
    }
}



void remote_prune(git_remote *remote, const git_remote_callbacks *callbacks) {
    auto __result = git_remote_prune(remote, callbacks);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_prune");
    }
}



void remote_push(git_remote *remote, const git_strarray *refspecs, const git_push_options *opts) {
    auto __result = git_remote_push(remote, refspecs, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_push");
    }
}



const git_indexer_progress *remote_stats(git_remote *remote) {
    auto __result = git_remote_stats(remote);
    return __result;
}



git_remote_autotag_option_t remote_autotag(const git_remote *remote) {
    auto __result = git_remote_autotag(remote);
    return __result;
}



void remote_set_autotag(git_repository *repo, const char *remote, git_remote_autotag_option_t value) {
    auto __result = git_remote_set_autotag(repo, remote, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_set_autotag");
    }
}



void remote_prune_refs(const git_remote *remote) {
    auto __result = git_remote_prune_refs(remote);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_prune_refs");
    }
}



void remote_rename(git_strarray *problems, git_repository *repo, const char *name, const char *new_name) {
    auto __result = git_remote_rename(problems, repo, name, new_name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_rename");
    }
}



void remote_name_is_valid(int *valid, const char *remote_name) {
    auto __result = git_remote_name_is_valid(valid, remote_name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_name_is_valid");
    }
}



void remote_delete(git_repository *repo, const char *name) {
    auto __result = git_remote_delete(repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_delete");
    }
}



git_buf remote_default_branch(git_remote *remote) {
    git_buf out;
    auto __result = git_remote_default_branch(&out, remote);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_remote_default_branch");
    } else {
        return out;
    }
}



void clone_options_init(git_clone_options *opts, unsigned int version) {
    auto __result = git_clone_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_clone_options_init");
    }
}



git_repository *clone(const char *url, const char *local_path, const git_clone_options *options) {
    git_repository *out;
    auto __result = git_clone(&out, url, local_path, options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_clone");
    } else {
        return out;
    }
}



git_commit *commit_lookup(git_repository *repo, const git_oid *id) {
    git_commit *commit;
    auto __result = git_commit_lookup(&commit, repo, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_lookup");
    } else {
        return commit;
    }
}



git_commit *commit_lookup_prefix(git_repository *repo, const git_oid *id, size_t len) {
    git_commit *commit;
    auto __result = git_commit_lookup_prefix(&commit, repo, id, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_lookup_prefix");
    } else {
        return commit;
    }
}



void commit_free(git_commit *commit) {
    git_commit_free(commit);
}



const git_oid *commit_id(const git_commit *commit) {
    auto __result = git_commit_id(commit);
    return __result;
}



git_repository *commit_owner(const git_commit *commit) {
    auto __result = git_commit_owner(commit);
    return __result;
}



const char *commit_message_encoding(const git_commit *commit) {
    auto __result = git_commit_message_encoding(commit);
    return __result;
}



const char *commit_message(const git_commit *commit) {
    auto __result = git_commit_message(commit);
    return __result;
}



const char *commit_message_raw(const git_commit *commit) {
    auto __result = git_commit_message_raw(commit);
    return __result;
}



const char *commit_summary(git_commit *commit) {
    auto __result = git_commit_summary(commit);
    return __result;
}



const char *commit_body(git_commit *commit) {
    auto __result = git_commit_body(commit);
    return __result;
}



git_time_t commit_time(const git_commit *commit) {
    auto __result = git_commit_time(commit);
    return __result;
}



int commit_time_offset(const git_commit *commit) {
    auto __result = git_commit_time_offset(commit);
    return __result;
}



const git_signature *commit_committer(const git_commit *commit) {
    auto __result = git_commit_committer(commit);
    return __result;
}



const git_signature *commit_author(const git_commit *commit) {
    auto __result = git_commit_author(commit);
    return __result;
}



git_signature *commit_committer_with_mailmap(const git_commit *commit, const git_mailmap *mailmap) {
    git_signature *out;
    auto __result = git_commit_committer_with_mailmap(&out, commit, mailmap);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_committer_with_mailmap");
    } else {
        return out;
    }
}



git_signature *commit_author_with_mailmap(const git_commit *commit, const git_mailmap *mailmap) {
    git_signature *out;
    auto __result = git_commit_author_with_mailmap(&out, commit, mailmap);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_author_with_mailmap");
    } else {
        return out;
    }
}



const char *commit_raw_header(const git_commit *commit) {
    auto __result = git_commit_raw_header(commit);
    return __result;
}



git_tree *commit_tree(const git_commit *commit) {
    git_tree *tree_out;
    auto __result = git_commit_tree(&tree_out, commit);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_tree");
    } else {
        return tree_out;
    }
}



const git_oid *commit_tree_id(const git_commit *commit) {
    auto __result = git_commit_tree_id(commit);
    return __result;
}



unsigned int commit_parentcount(const git_commit *commit) {
    auto __result = git_commit_parentcount(commit);
    return __result;
}



git_commit *commit_parent(const git_commit *commit, unsigned int n) {
    git_commit *out;
    auto __result = git_commit_parent(&out, commit, n);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_parent");
    } else {
        return out;
    }
}



const git_oid *commit_parent_id(const git_commit *commit, unsigned int n) {
    auto __result = git_commit_parent_id(commit, n);
    return __result;
}



void commit_nth_gen_ancestor(git_commit **ancestor, const git_commit *commit, unsigned int n) {
    auto __result = git_commit_nth_gen_ancestor(ancestor, commit, n);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_nth_gen_ancestor");
    }
}



git_buf commit_header_field(const git_commit *commit, const char *field) {
    git_buf out;
    auto __result = git_commit_header_field(&out, commit, field);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_header_field");
    } else {
        return out;
    }
}



void commit_extract_signature(git_buf *signature, git_buf *signed_data, git_repository *repo, git_oid *commit_id, const char *field) {
    auto __result = git_commit_extract_signature(signature, signed_data, repo, commit_id, field);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_extract_signature");
    }
}



void commit_create(git_oid *id, git_repository *repo, const char *update_ref, const git_signature *author, const git_signature *committer, const char *message_encoding, const char *message, const git_tree *tree, size_t parent_count, const git_commit *parents[]) {
    auto __result = git_commit_create(id, repo, update_ref, author, committer, message_encoding, message, tree, parent_count, parents);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_create");
    }
}



void commit_create_v(git_oid *id, git_repository *repo, const char *update_ref, const git_signature *author, const git_signature *committer, const char *message_encoding, const char *message, const git_tree *tree, size_t parent_count) {
    auto __result = git_commit_create_v(id, repo, update_ref, author, committer, message_encoding, message, tree, parent_count);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_create_v");
    }
}



void commit_amend(git_oid *id, const git_commit *commit_to_amend, const char *update_ref, const git_signature *author, const git_signature *committer, const char *message_encoding, const char *message, const git_tree *tree) {
    auto __result = git_commit_amend(id, commit_to_amend, update_ref, author, committer, message_encoding, message, tree);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_amend");
    }
}



git_buf commit_create_buffer(git_repository *repo, const git_signature *author, const git_signature *committer, const char *message_encoding, const char *message, const git_tree *tree, size_t parent_count, const git_commit *parents[]) {
    git_buf out;
    auto __result = git_commit_create_buffer(&out, repo, author, committer, message_encoding, message, tree, parent_count, parents);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_create_buffer");
    } else {
        return out;
    }
}



git_oid commit_create_with_signature(git_repository *repo, const char *commit_content, const char *signature, const char *signature_field) {
    git_oid out;
    auto __result = git_commit_create_with_signature(&out, repo, commit_content, signature, signature_field);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_create_with_signature");
    } else {
        return out;
    }
}



git_commit *commit_dup(git_commit *source) {
    git_commit *out;
    auto __result = git_commit_dup(&out, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_commit_dup");
    } else {
        return out;
    }
}



void config_entry_free(git_config_entry *entry) {
    git_config_entry_free(entry);
}



git_buf config_find_global() {
    git_buf out;
    auto __result = git_config_find_global(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_find_global");
    } else {
        return out;
    }
}



git_buf config_find_xdg() {
    git_buf out;
    auto __result = git_config_find_xdg(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_find_xdg");
    } else {
        return out;
    }
}



git_buf config_find_system() {
    git_buf out;
    auto __result = git_config_find_system(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_find_system");
    } else {
        return out;
    }
}



git_buf config_find_programdata() {
    git_buf out;
    auto __result = git_config_find_programdata(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_find_programdata");
    } else {
        return out;
    }
}



git_config *config_open_default() {
    git_config *out;
    auto __result = git_config_open_default(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_open_default");
    } else {
        return out;
    }
}



git_config *config_new() {
    git_config *out;
    auto __result = git_config_new(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_new");
    } else {
        return out;
    }
}



void config_add_file_ondisk(git_config *cfg, const char *path, git_config_level_t level, const git_repository *repo, int force) {
    auto __result = git_config_add_file_ondisk(cfg, path, level, repo, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_add_file_ondisk");
    }
}



git_config *config_open_ondisk(const char *path) {
    git_config *out;
    auto __result = git_config_open_ondisk(&out, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_open_ondisk");
    } else {
        return out;
    }
}



git_config *config_open_level(const git_config *parent, git_config_level_t level) {
    git_config *out;
    auto __result = git_config_open_level(&out, parent, level);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_open_level");
    } else {
        return out;
    }
}



git_config *config_open_global(git_config *config) {
    git_config *out;
    auto __result = git_config_open_global(&out, config);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_open_global");
    } else {
        return out;
    }
}



git_config *config_snapshot(git_config *config) {
    git_config *out;
    auto __result = git_config_snapshot(&out, config);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_snapshot");
    } else {
        return out;
    }
}



void config_free(git_config *cfg) {
    git_config_free(cfg);
}



git_config_entry *config_get_entry(const git_config *cfg, const char *name) {
    git_config_entry *out;
    auto __result = git_config_get_entry(&out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_entry");
    } else {
        return out;
    }
}



int32_t config_get_int32(const git_config *cfg, const char *name) {
    int32_t out;
    auto __result = git_config_get_int32(&out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_int32");
    } else {
        return out;
    }
}



int64_t config_get_int64(const git_config *cfg, const char *name) {
    int64_t out;
    auto __result = git_config_get_int64(&out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_int64");
    } else {
        return out;
    }
}



int config_get_bool(const git_config *cfg, const char *name) {
    int out;
    auto __result = git_config_get_bool(&out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_bool");
    } else {
        return out;
    }
}



git_buf config_get_path(const git_config *cfg, const char *name) {
    git_buf out;
    auto __result = git_config_get_path(&out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_path");
    } else {
        return out;
    }
}



const char *config_get_string(const git_config *cfg, const char *name) {
    const char *out;
    auto __result = git_config_get_string(&out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_string");
    } else {
        return out;
    }
}



git_buf config_get_string_buf(const git_config *cfg, const char *name) {
    git_buf out;
    auto __result = git_config_get_string_buf(&out, cfg, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_string_buf");
    } else {
        return out;
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



git_config_iterator *config_iterator_new(const git_config *cfg) {
    git_config_iterator *out;
    auto __result = git_config_iterator_new(&out, cfg);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_iterator_new");
    } else {
        return out;
    }
}



git_config_iterator *config_iterator_glob_new(const git_config *cfg, const char *regexp) {
    git_config_iterator *out;
    auto __result = git_config_iterator_glob_new(&out, cfg, regexp);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_iterator_glob_new");
    } else {
        return out;
    }
}



void config_foreach_match(const git_config *cfg, const char *regexp, git_config_foreach_cb callback, void *payload) {
    auto __result = git_config_foreach_match(cfg, regexp, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_foreach_match");
    }
}



int config_get_mapped(const git_config *cfg, const char *name, const git_configmap *maps, size_t map_n) {
    int out;
    auto __result = git_config_get_mapped(&out, cfg, name, maps, map_n);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_get_mapped");
    } else {
        return out;
    }
}



int config_lookup_map_value(const git_configmap *maps, size_t map_n, const char *value) {
    int out;
    auto __result = git_config_lookup_map_value(&out, maps, map_n, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_lookup_map_value");
    } else {
        return out;
    }
}



int config_parse_bool(const char *value) {
    int out;
    auto __result = git_config_parse_bool(&out, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_parse_bool");
    } else {
        return out;
    }
}



int32_t config_parse_int32(const char *value) {
    int32_t out;
    auto __result = git_config_parse_int32(&out, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_parse_int32");
    } else {
        return out;
    }
}



int64_t config_parse_int64(const char *value) {
    int64_t out;
    auto __result = git_config_parse_int64(&out, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_parse_int64");
    } else {
        return out;
    }
}



git_buf config_parse_path(const char *value) {
    git_buf out;
    auto __result = git_config_parse_path(&out, value);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_config_parse_path");
    } else {
        return out;
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



void describe_options_init(git_describe_options *opts, unsigned int version) {
    auto __result = git_describe_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_describe_options_init");
    }
}



void describe_format_options_init(git_describe_format_options *opts, unsigned int version) {
    auto __result = git_describe_format_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_describe_format_options_init");
    }
}



void describe_commit(git_describe_result **result, git_object *committish, git_describe_options *opts) {
    auto __result = git_describe_commit(result, committish, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_describe_commit");
    }
}



git_describe_result *describe_workdir(git_repository *repo, git_describe_options *opts) {
    git_describe_result *out;
    auto __result = git_describe_workdir(&out, repo, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_describe_workdir");
    } else {
        return out;
    }
}



git_buf describe_format(const git_describe_result *result, const git_describe_format_options *opts) {
    git_buf out;
    auto __result = git_describe_format(&out, result, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_describe_format");
    } else {
        return out;
    }
}



void describe_result_free(git_describe_result *result) {
    git_describe_result_free(result);
}



const git_error *error_last() {
    auto __result = git_error_last();
    return __result;
}



void error_clear() {
    git_error_clear();
}



void error_set(int error_class, const char *fmt) {
    git_error_set(error_class, fmt);
}



void error_set_str(int error_class, const char *string) {
    auto __result = git_error_set_str(error_class, string);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_error_set_str");
    }
}



void error_set_oom() {
    git_error_set_oom();
}



void filter_list_load(git_filter_list **filters, git_repository *repo, git_blob *blob, const char *path, git_filter_mode_t mode, uint32_t flags) {
    auto __result = git_filter_list_load(filters, repo, blob, path, mode, flags);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_load");
    }
}



void filter_list_load_ext(git_filter_list **filters, git_repository *repo, git_blob *blob, const char *path, git_filter_mode_t mode, git_filter_options *opts) {
    auto __result = git_filter_list_load_ext(filters, repo, blob, path, mode, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_load_ext");
    }
}



void filter_list_contains(git_filter_list *filters, const char *name) {
    auto __result = git_filter_list_contains(filters, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_contains");
    }
}



git_buf filter_list_apply_to_buffer(git_filter_list *filters, const char *in, size_t in_len) {
    git_buf out;
    auto __result = git_filter_list_apply_to_buffer(&out, filters, in, in_len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_apply_to_buffer");
    } else {
        return out;
    }
}



git_buf filter_list_apply_to_file(git_filter_list *filters, git_repository *repo, const char *path) {
    git_buf out;
    auto __result = git_filter_list_apply_to_file(&out, filters, repo, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_apply_to_file");
    } else {
        return out;
    }
}



git_buf filter_list_apply_to_blob(git_filter_list *filters, git_blob *blob) {
    git_buf out;
    auto __result = git_filter_list_apply_to_blob(&out, filters, blob);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_apply_to_blob");
    } else {
        return out;
    }
}



void filter_list_stream_buffer(git_filter_list *filters, const char *buffer, size_t len, git_writestream *target) {
    auto __result = git_filter_list_stream_buffer(filters, buffer, len, target);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_stream_buffer");
    }
}



void filter_list_stream_file(git_filter_list *filters, git_repository *repo, const char *path, git_writestream *target) {
    auto __result = git_filter_list_stream_file(filters, repo, path, target);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_stream_file");
    }
}



void filter_list_stream_blob(git_filter_list *filters, git_blob *blob, git_writestream *target) {
    auto __result = git_filter_list_stream_blob(filters, blob, target);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_filter_list_stream_blob");
    }
}



void filter_list_free(git_filter_list *filters) {
    git_filter_list_free(filters);
}



void rebase_options_init(git_rebase_options *opts, unsigned int version) {
    auto __result = git_rebase_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_rebase_options_init");
    }
}



git_rebase *rebase_init(git_repository *repo, const git_annotated_commit *branch, const git_annotated_commit *upstream, const git_annotated_commit *onto, const git_rebase_options *opts) {
    git_rebase *out;
    auto __result = git_rebase_init(&out, repo, branch, upstream, onto, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_rebase_init");
    } else {
        return out;
    }
}



git_rebase *rebase_open(git_repository *repo, const git_rebase_options *opts) {
    git_rebase *out;
    auto __result = git_rebase_open(&out, repo, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_rebase_open");
    } else {
        return out;
    }
}



const char *rebase_orig_head_name(git_rebase *rebase) {
    auto __result = git_rebase_orig_head_name(rebase);
    return __result;
}



const git_oid *rebase_orig_head_id(git_rebase *rebase) {
    auto __result = git_rebase_orig_head_id(rebase);
    return __result;
}



const char *rebase_onto_name(git_rebase *rebase) {
    auto __result = git_rebase_onto_name(rebase);
    return __result;
}



const git_oid *rebase_onto_id(git_rebase *rebase) {
    auto __result = git_rebase_onto_id(rebase);
    return __result;
}



size_t rebase_operation_entrycount(git_rebase *rebase) {
    auto __result = git_rebase_operation_entrycount(rebase);
    return __result;
}



size_t rebase_operation_current(git_rebase *rebase) {
    auto __result = git_rebase_operation_current(rebase);
    return __result;
}



git_rebase_operation *rebase_operation_byindex(git_rebase *rebase, size_t idx) {
    auto __result = git_rebase_operation_byindex(rebase, idx);
    return __result;
}



void rebase_next(git_rebase_operation **operation, git_rebase *rebase) {
    auto __result = git_rebase_next(operation, rebase);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_rebase_next");
    }
}



void rebase_inmemory_index(git_index **index, git_rebase *rebase) {
    auto __result = git_rebase_inmemory_index(index, rebase);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_rebase_inmemory_index");
    }
}



void rebase_commit(git_oid *id, git_rebase *rebase, const git_signature *author, const git_signature *committer, const char *message_encoding, const char *message) {
    auto __result = git_rebase_commit(id, rebase, author, committer, message_encoding, message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_rebase_commit");
    }
}



void rebase_abort(git_rebase *rebase) {
    auto __result = git_rebase_abort(rebase);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_rebase_abort");
    }
}



void rebase_finish(git_rebase *rebase, const git_signature *signature) {
    auto __result = git_rebase_finish(rebase, signature);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_rebase_finish");
    }
}



void rebase_free(git_rebase *rebase) {
    git_rebase_free(rebase);
}



void trace_set(git_trace_level_t level, git_trace_cb cb) {
    auto __result = git_trace_set(level, cb);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_trace_set");
    }
}



void revert_options_init(git_revert_options *opts, unsigned int version) {
    auto __result = git_revert_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revert_options_init");
    }
}



git_index *revert_commit(git_repository *repo, git_commit *revert_commit, git_commit *our_commit, unsigned int mainline, const git_merge_options *merge_options) {
    git_index *out;
    auto __result = git_revert_commit(&out, repo, revert_commit, our_commit, mainline, merge_options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revert_commit");
    } else {
        return out;
    }
}



void revert(git_repository *repo, git_commit *commit, const git_revert_options *given_opts) {
    auto __result = git_revert(repo, commit, given_opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revert");
    }
}



git_object *revparse_single(git_repository *repo, const char *spec) {
    git_object *out;
    auto __result = git_revparse_single(&out, repo, spec);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revparse_single");
    } else {
        return out;
    }
}



void revparse_ext(git_object **object_out, git_reference **reference_out, git_repository *repo, const char *spec) {
    auto __result = git_revparse_ext(object_out, reference_out, repo, spec);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revparse_ext");
    }
}



git_revspec revparse(git_repository *repo, const char *spec) {
    git_revspec revspec;
    auto __result = git_revparse(&revspec, repo, spec);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revparse");
    } else {
        return revspec;
    }
}



git_oid stash_save(git_repository *repo, const git_signature *stasher, const char *message, uint32_t flags) {
    git_oid out;
    auto __result = git_stash_save(&out, repo, stasher, message, flags);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_stash_save");
    } else {
        return out;
    }
}



void stash_apply_options_init(git_stash_apply_options *opts, unsigned int version) {
    auto __result = git_stash_apply_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_stash_apply_options_init");
    }
}



void stash_apply(git_repository *repo, size_t index, const git_stash_apply_options *options) {
    auto __result = git_stash_apply(repo, index, options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_stash_apply");
    }
}



void stash_foreach(git_repository *repo, git_stash_cb callback, void *payload) {
    auto __result = git_stash_foreach(repo, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_stash_foreach");
    }
}



void stash_drop(git_repository *repo, size_t index) {
    auto __result = git_stash_drop(repo, index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_stash_drop");
    }
}



void stash_pop(git_repository *repo, size_t index, const git_stash_apply_options *options) {
    auto __result = git_stash_pop(repo, index, options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_stash_pop");
    }
}



void status_options_init(git_status_options *opts, unsigned int version) {
    auto __result = git_status_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_status_options_init");
    }
}



void status_foreach(git_repository *repo, git_status_cb callback, void *payload) {
    auto __result = git_status_foreach(repo, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_status_foreach");
    }
}



void status_foreach_ext(git_repository *repo, const git_status_options *opts, git_status_cb callback, void *payload) {
    auto __result = git_status_foreach_ext(repo, opts, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_status_foreach_ext");
    }
}



void status_file(unsigned int *status_flags, git_repository *repo, const char *path) {
    auto __result = git_status_file(status_flags, repo, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_status_file");
    }
}



git_status_list *status_list_new(git_repository *repo, const git_status_options *opts) {
    git_status_list *out;
    auto __result = git_status_list_new(&out, repo, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_status_list_new");
    } else {
        return out;
    }
}



size_t status_list_entrycount(git_status_list *statuslist) {
    auto __result = git_status_list_entrycount(statuslist);
    return __result;
}



const git_status_entry *status_byindex(git_status_list *statuslist, size_t idx) {
    auto __result = git_status_byindex(statuslist, idx);
    return __result;
}



void status_list_free(git_status_list *statuslist) {
    git_status_list_free(statuslist);
}



void status_should_ignore(int *ignored, git_repository *repo, const char *path) {
    auto __result = git_status_should_ignore(ignored, repo, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_status_should_ignore");
    }
}



void submodule_update_options_init(git_submodule_update_options *opts, unsigned int version) {
    auto __result = git_submodule_update_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_update_options_init");
    }
}



void submodule_update(git_submodule *submodule, int init, git_submodule_update_options *options) {
    auto __result = git_submodule_update(submodule, init, options);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_update");
    }
}



git_submodule *submodule_lookup(git_repository *repo, const char *name) {
    git_submodule *out;
    auto __result = git_submodule_lookup(&out, repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_lookup");
    } else {
        return out;
    }
}



git_submodule *submodule_dup(git_submodule *source) {
    git_submodule *out;
    auto __result = git_submodule_dup(&out, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_dup");
    } else {
        return out;
    }
}



void submodule_free(git_submodule *submodule) {
    git_submodule_free(submodule);
}



void submodule_foreach(git_repository *repo, git_submodule_cb callback, void *payload) {
    auto __result = git_submodule_foreach(repo, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_foreach");
    }
}



git_submodule *submodule_add_setup(git_repository *repo, const char *url, const char *path, int use_gitlink) {
    git_submodule *out;
    auto __result = git_submodule_add_setup(&out, repo, url, path, use_gitlink);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_add_setup");
    } else {
        return out;
    }
}



git_repository *submodule_clone(git_submodule *submodule, const git_submodule_update_options *opts) {
    git_repository *out;
    auto __result = git_submodule_clone(&out, submodule, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_clone");
    } else {
        return out;
    }
}



void submodule_add_finalize(git_submodule *submodule) {
    auto __result = git_submodule_add_finalize(submodule);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_add_finalize");
    }
}



void submodule_add_to_index(git_submodule *submodule, int write_index) {
    auto __result = git_submodule_add_to_index(submodule, write_index);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_add_to_index");
    }
}



git_repository *submodule_owner(git_submodule *submodule) {
    auto __result = git_submodule_owner(submodule);
    return __result;
}



const char *submodule_name(git_submodule *submodule) {
    auto __result = git_submodule_name(submodule);
    return __result;
}



const char *submodule_path(git_submodule *submodule) {
    auto __result = git_submodule_path(submodule);
    return __result;
}



const char *submodule_url(git_submodule *submodule) {
    auto __result = git_submodule_url(submodule);
    return __result;
}



git_buf submodule_resolve_url(git_repository *repo, const char *url) {
    git_buf out;
    auto __result = git_submodule_resolve_url(&out, repo, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_resolve_url");
    } else {
        return out;
    }
}



const char *submodule_branch(git_submodule *submodule) {
    auto __result = git_submodule_branch(submodule);
    return __result;
}



void submodule_set_branch(git_repository *repo, const char *name, const char *branch) {
    auto __result = git_submodule_set_branch(repo, name, branch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_set_branch");
    }
}



void submodule_set_url(git_repository *repo, const char *name, const char *url) {
    auto __result = git_submodule_set_url(repo, name, url);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_set_url");
    }
}



const git_oid *submodule_index_id(git_submodule *submodule) {
    auto __result = git_submodule_index_id(submodule);
    return __result;
}



const git_oid *submodule_head_id(git_submodule *submodule) {
    auto __result = git_submodule_head_id(submodule);
    return __result;
}



const git_oid *submodule_wd_id(git_submodule *submodule) {
    auto __result = git_submodule_wd_id(submodule);
    return __result;
}



git_submodule_ignore_t submodule_ignore(git_submodule *submodule) {
    auto __result = git_submodule_ignore(submodule);
    return __result;
}



void submodule_set_ignore(git_repository *repo, const char *name, git_submodule_ignore_t ignore) {
    auto __result = git_submodule_set_ignore(repo, name, ignore);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_set_ignore");
    }
}



git_submodule_update_t submodule_update_strategy(git_submodule *submodule) {
    auto __result = git_submodule_update_strategy(submodule);
    return __result;
}



void submodule_set_update(git_repository *repo, const char *name, git_submodule_update_t update) {
    auto __result = git_submodule_set_update(repo, name, update);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_set_update");
    }
}



git_submodule_recurse_t submodule_fetch_recurse_submodules(git_submodule *submodule) {
    auto __result = git_submodule_fetch_recurse_submodules(submodule);
    return __result;
}



void submodule_set_fetch_recurse_submodules(git_repository *repo, const char *name, git_submodule_recurse_t fetch_recurse_submodules) {
    auto __result = git_submodule_set_fetch_recurse_submodules(repo, name, fetch_recurse_submodules);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_set_fetch_recurse_submodules");
    }
}



void submodule_init(git_submodule *submodule, int overwrite) {
    auto __result = git_submodule_init(submodule, overwrite);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_init");
    }
}



git_repository *submodule_repo_init(const git_submodule *sm, int use_gitlink) {
    git_repository *out;
    auto __result = git_submodule_repo_init(&out, sm, use_gitlink);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_repo_init");
    } else {
        return out;
    }
}



void submodule_sync(git_submodule *submodule) {
    auto __result = git_submodule_sync(submodule);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_sync");
    }
}



void submodule_open(git_repository **repo, git_submodule *submodule) {
    auto __result = git_submodule_open(repo, submodule);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_open");
    }
}



void submodule_reload(git_submodule *submodule, int force) {
    auto __result = git_submodule_reload(submodule, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_reload");
    }
}



void submodule_status(unsigned int *status, git_repository *repo, const char *name, git_submodule_ignore_t ignore) {
    auto __result = git_submodule_status(status, repo, name, ignore);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_status");
    }
}



void submodule_location(unsigned int *location_status, git_submodule *submodule) {
    auto __result = git_submodule_location(location_status, submodule);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_submodule_location");
    }
}



git_strarray worktree_list(git_repository *repo) {
    git_strarray out;
    auto __result = git_worktree_list(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_list");
    } else {
        return out;
    }
}



git_worktree *worktree_lookup(git_repository *repo, const char *name) {
    git_worktree *out;
    auto __result = git_worktree_lookup(&out, repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_lookup");
    } else {
        return out;
    }
}



git_worktree *worktree_open_from_repository(git_repository *repo) {
    git_worktree *out;
    auto __result = git_worktree_open_from_repository(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_open_from_repository");
    } else {
        return out;
    }
}



void worktree_free(git_worktree *wt) {
    git_worktree_free(wt);
}



void worktree_validate(const git_worktree *wt) {
    auto __result = git_worktree_validate(wt);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_validate");
    }
}



void worktree_add_options_init(git_worktree_add_options *opts, unsigned int version) {
    auto __result = git_worktree_add_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_add_options_init");
    }
}



git_worktree *worktree_add(git_repository *repo, const char *name, const char *path, const git_worktree_add_options *opts) {
    git_worktree *out;
    auto __result = git_worktree_add(&out, repo, name, path, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_add");
    } else {
        return out;
    }
}



void worktree_lock(git_worktree *wt, const char *reason) {
    auto __result = git_worktree_lock(wt, reason);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_lock");
    }
}



void worktree_unlock(git_worktree *wt) {
    auto __result = git_worktree_unlock(wt);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_unlock");
    }
}



void worktree_is_locked(git_buf *reason, const git_worktree *wt) {
    auto __result = git_worktree_is_locked(reason, wt);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_is_locked");
    }
}



const char *worktree_name(const git_worktree *wt) {
    auto __result = git_worktree_name(wt);
    return __result;
}



const char *worktree_path(const git_worktree *wt) {
    auto __result = git_worktree_path(wt);
    return __result;
}



void worktree_prune_options_init(git_worktree_prune_options *opts, unsigned int version) {
    auto __result = git_worktree_prune_options_init(opts, version);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_prune_options_init");
    }
}



void worktree_is_prunable(git_worktree *wt, git_worktree_prune_options *opts) {
    auto __result = git_worktree_is_prunable(wt, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_is_prunable");
    }
}



void worktree_prune(git_worktree *wt, git_worktree_prune_options *opts) {
    auto __result = git_worktree_prune(wt, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_worktree_prune");
    }
}



git_credential *credential_userpass(const char *url, const char *user_from_url, unsigned int allowed_types, void *payload) {
    git_credential *out;
    auto __result = git_credential_userpass(&out, url, user_from_url, allowed_types, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_credential_userpass");
    } else {
        return out;
    }
}



git_buf email_create_from_diff(git_diff *diff, size_t patch_idx, size_t patch_count, const git_oid *commit_id, const char *summary, const char *body, const git_signature *author, const git_email_create_options *opts) {
    git_buf out;
    auto __result = git_email_create_from_diff(&out, diff, patch_idx, patch_count, commit_id, summary, body, author, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_email_create_from_diff");
    } else {
        return out;
    }
}



git_buf email_create_from_commit(git_commit *commit, const git_email_create_options *opts) {
    git_buf out;
    auto __result = git_email_create_from_commit(&out, commit, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_email_create_from_commit");
    } else {
        return out;
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



void graph_ahead_behind(size_t *ahead, size_t *behind, git_repository *repo, const git_oid *local, const git_oid *upstream) {
    auto __result = git_graph_ahead_behind(ahead, behind, repo, local, upstream);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_graph_ahead_behind");
    }
}



void graph_descendant_of(git_repository *repo, const git_oid *commit, const git_oid *ancestor) {
    auto __result = git_graph_descendant_of(repo, commit, ancestor);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_graph_descendant_of");
    }
}



void graph_reachable_from_any(git_repository *repo, const git_oid *commit, const git_oid descendant_array[], size_t length) {
    auto __result = git_graph_reachable_from_any(repo, commit, descendant_array, length);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_graph_reachable_from_any");
    }
}



void ignore_add_rule(git_repository *repo, const char *rules) {
    auto __result = git_ignore_add_rule(repo, rules);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_ignore_add_rule");
    }
}



void ignore_clear_internal_rules(git_repository *repo) {
    auto __result = git_ignore_clear_internal_rules(repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_ignore_clear_internal_rules");
    }
}



void ignore_path_is_ignored(int *ignored, git_repository *repo, const char *path) {
    auto __result = git_ignore_path_is_ignored(ignored, repo, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_ignore_path_is_ignored");
    }
}



git_mailmap *mailmap_new() {
    git_mailmap *out;
    auto __result = git_mailmap_new(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_mailmap_new");
    } else {
        return out;
    }
}



void mailmap_free(git_mailmap *mm) {
    git_mailmap_free(mm);
}



void mailmap_add_entry(git_mailmap *mm, const char *real_name, const char *real_email, const char *replace_name, const char *replace_email) {
    auto __result = git_mailmap_add_entry(mm, real_name, real_email, replace_name, replace_email);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_mailmap_add_entry");
    }
}



git_mailmap *mailmap_from_buffer(const char *buf, size_t len) {
    git_mailmap *out;
    auto __result = git_mailmap_from_buffer(&out, buf, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_mailmap_from_buffer");
    } else {
        return out;
    }
}



git_mailmap *mailmap_from_repository(git_repository *repo) {
    git_mailmap *out;
    auto __result = git_mailmap_from_repository(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_mailmap_from_repository");
    } else {
        return out;
    }
}



void mailmap_resolve(const char **real_name, const char **real_email, const git_mailmap *mm, const char *name, const char *email) {
    auto __result = git_mailmap_resolve(real_name, real_email, mm, name, email);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_mailmap_resolve");
    }
}



git_signature *mailmap_resolve_signature(const git_mailmap *mm, const git_signature *sig) {
    git_signature *out;
    auto __result = git_mailmap_resolve_signature(&out, mm, sig);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_mailmap_resolve_signature");
    } else {
        return out;
    }
}



git_buf message_prettify(const char *message, int strip_comments, char comment_char) {
    git_buf out;
    auto __result = git_message_prettify(&out, message, strip_comments, comment_char);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_message_prettify");
    } else {
        return out;
    }
}



void message_trailers(git_message_trailer_array *arr, const char *message) {
    auto __result = git_message_trailers(arr, message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_message_trailers");
    }
}



void message_trailer_array_free(git_message_trailer_array *arr) {
    git_message_trailer_array_free(arr);
}



git_note_iterator *note_iterator_new(git_repository *repo, const char *notes_ref) {
    git_note_iterator *out;
    auto __result = git_note_iterator_new(&out, repo, notes_ref);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_iterator_new");
    } else {
        return out;
    }
}



git_note_iterator *note_commit_iterator_new(git_commit *notes_commit) {
    git_note_iterator *out;
    auto __result = git_note_commit_iterator_new(&out, notes_commit);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_commit_iterator_new");
    } else {
        return out;
    }
}



void note_iterator_free(git_note_iterator *it) {
    git_note_iterator_free(it);
}



void note_next(git_oid *note_id, git_oid *annotated_id, git_note_iterator *it) {
    auto __result = git_note_next(note_id, annotated_id, it);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_next");
    }
}



git_note *note_read(git_repository *repo, const char *notes_ref, const git_oid *oid) {
    git_note *out;
    auto __result = git_note_read(&out, repo, notes_ref, oid);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_read");
    } else {
        return out;
    }
}



git_note *note_commit_read(git_repository *repo, git_commit *notes_commit, const git_oid *oid) {
    git_note *out;
    auto __result = git_note_commit_read(&out, repo, notes_commit, oid);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_commit_read");
    } else {
        return out;
    }
}



const git_signature *note_author(const git_note *note) {
    auto __result = git_note_author(note);
    return __result;
}



const git_signature *note_committer(const git_note *note) {
    auto __result = git_note_committer(note);
    return __result;
}



const char *note_message(const git_note *note) {
    auto __result = git_note_message(note);
    return __result;
}



const git_oid *note_id(const git_note *note) {
    auto __result = git_note_id(note);
    return __result;
}



git_oid note_create(git_repository *repo, const char *notes_ref, const git_signature *author, const git_signature *committer, const git_oid *oid, const char *note, int force) {
    git_oid out;
    auto __result = git_note_create(&out, repo, notes_ref, author, committer, oid, note, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_create");
    } else {
        return out;
    }
}



void note_commit_create(git_oid *notes_commit_out, git_oid *notes_blob_out, git_repository *repo, git_commit *parent, const git_signature *author, const git_signature *committer, const git_oid *oid, const char *note, int allow_note_overwrite) {
    auto __result = git_note_commit_create(notes_commit_out, notes_blob_out, repo, parent, author, committer, oid, note, allow_note_overwrite);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_commit_create");
    }
}



void note_remove(git_repository *repo, const char *notes_ref, const git_signature *author, const git_signature *committer, const git_oid *oid) {
    auto __result = git_note_remove(repo, notes_ref, author, committer, oid);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_remove");
    }
}



void note_commit_remove(git_oid *notes_commit_out, git_repository *repo, git_commit *notes_commit, const git_signature *author, const git_signature *committer, const git_oid *oid) {
    auto __result = git_note_commit_remove(notes_commit_out, repo, notes_commit, author, committer, oid);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_commit_remove");
    }
}



void note_free(git_note *note) {
    git_note_free(note);
}



git_buf note_default_ref(git_repository *repo) {
    git_buf out;
    auto __result = git_note_default_ref(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_default_ref");
    } else {
        return out;
    }
}



void note_foreach(git_repository *repo, const char *notes_ref, git_note_foreach_cb note_cb, void *payload) {
    auto __result = git_note_foreach(repo, notes_ref, note_cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_note_foreach");
    }
}



git_odb *odb_new() {
    git_odb *out;
    auto __result = git_odb_new(&out);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_new");
    } else {
        return out;
    }
}



git_odb *odb_open(const char *objects_dir) {
    git_odb *out;
    auto __result = git_odb_open(&out, objects_dir);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_open");
    } else {
        return out;
    }
}



void odb_add_disk_alternate(git_odb *odb, const char *path) {
    auto __result = git_odb_add_disk_alternate(odb, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_add_disk_alternate");
    }
}



void odb_free(git_odb *db) {
    git_odb_free(db);
}



git_odb_object *odb_read(git_odb *db, const git_oid *id) {
    git_odb_object *out;
    auto __result = git_odb_read(&out, db, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_read");
    } else {
        return out;
    }
}



git_odb_object *odb_read_prefix(git_odb *db, const git_oid *short_id, size_t len) {
    git_odb_object *out;
    auto __result = git_odb_read_prefix(&out, db, short_id, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_read_prefix");
    } else {
        return out;
    }
}



void odb_read_header(size_t *len_out, git_object_t *type_out, git_odb *db, const git_oid *id) {
    auto __result = git_odb_read_header(len_out, type_out, db, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_read_header");
    }
}



void odb_exists(git_odb *db, const git_oid *id) {
    auto __result = git_odb_exists(db, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_exists");
    }
}



void odb_exists_ext(git_odb *db, const git_oid *id, unsigned int flags) {
    auto __result = git_odb_exists_ext(db, id, flags);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_exists_ext");
    }
}



git_oid odb_exists_prefix(git_odb *db, const git_oid *short_id, size_t len) {
    git_oid out;
    auto __result = git_odb_exists_prefix(&out, db, short_id, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_exists_prefix");
    } else {
        return out;
    }
}



void odb_expand_ids(git_odb *db, git_odb_expand_id *ids, size_t count) {
    auto __result = git_odb_expand_ids(db, ids, count);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_expand_ids");
    }
}



void odb_refresh(struct git_odb *db) {
    auto __result = git_odb_refresh(db);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_refresh");
    }
}



void odb_foreach(git_odb *db, git_odb_foreach_cb cb, void *payload) {
    auto __result = git_odb_foreach(db, cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_foreach");
    }
}



git_oid odb_write(git_odb *odb, const void *data, size_t len, git_object_t type) {
    git_oid out;
    auto __result = git_odb_write(&out, odb, data, len, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_write");
    } else {
        return out;
    }
}



git_odb_stream *odb_open_wstream(git_odb *db, git_object_size_t size, git_object_t type) {
    git_odb_stream *out;
    auto __result = git_odb_open_wstream(&out, db, size, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_open_wstream");
    } else {
        return out;
    }
}



void odb_stream_write(git_odb_stream *stream, const char *buffer, size_t len) {
    auto __result = git_odb_stream_write(stream, buffer, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_stream_write");
    }
}



git_oid odb_stream_finalize_write(git_odb_stream *stream) {
    git_oid out;
    auto __result = git_odb_stream_finalize_write(&out, stream);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_stream_finalize_write");
    } else {
        return out;
    }
}



void odb_stream_read(git_odb_stream *stream, char *buffer, size_t len) {
    auto __result = git_odb_stream_read(stream, buffer, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_stream_read");
    }
}



void odb_stream_free(git_odb_stream *stream) {
    git_odb_stream_free(stream);
}



git_odb_stream *odb_open_rstream(size_t *len, git_object_t *type, git_odb *db, const git_oid *oid) {
    git_odb_stream *out;
    auto __result = git_odb_open_rstream(&out, len, type, db, oid);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_open_rstream");
    } else {
        return out;
    }
}



git_odb_writepack *odb_write_pack(git_odb *db, git_indexer_progress_cb progress_cb, void *progress_payload) {
    git_odb_writepack *out;
    auto __result = git_odb_write_pack(&out, db, progress_cb, progress_payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_write_pack");
    } else {
        return out;
    }
}



void odb_write_multi_pack_index(git_odb *db) {
    auto __result = git_odb_write_multi_pack_index(db);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_write_multi_pack_index");
    }
}



git_oid odb_hash(const void *data, size_t len, git_object_t type) {
    git_oid out;
    auto __result = git_odb_hash(&out, data, len, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_hash");
    } else {
        return out;
    }
}



git_oid odb_hashfile(const char *path, git_object_t type) {
    git_oid out;
    auto __result = git_odb_hashfile(&out, path, type);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_hashfile");
    } else {
        return out;
    }
}



void odb_object_dup(git_odb_object **dest, git_odb_object *source) {
    auto __result = git_odb_object_dup(dest, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_object_dup");
    }
}



void odb_object_free(git_odb_object *object) {
    git_odb_object_free(object);
}



const git_oid *odb_object_id(git_odb_object *object) {
    auto __result = git_odb_object_id(object);
    return __result;
}



const void *odb_object_data(git_odb_object *object) {
    auto __result = git_odb_object_data(object);
    return __result;
}



size_t odb_object_size(git_odb_object *object) {
    auto __result = git_odb_object_size(object);
    return __result;
}



git_object_t odb_object_type(git_odb_object *object) {
    auto __result = git_odb_object_type(object);
    return __result;
}



void odb_add_backend(git_odb *odb, git_odb_backend *backend, int priority) {
    auto __result = git_odb_add_backend(odb, backend, priority);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_add_backend");
    }
}



void odb_add_alternate(git_odb *odb, git_odb_backend *backend, int priority) {
    auto __result = git_odb_add_alternate(odb, backend, priority);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_add_alternate");
    }
}



size_t odb_num_backends(git_odb *odb) {
    auto __result = git_odb_num_backends(odb);
    return __result;
}



git_odb_backend *odb_get_backend(git_odb *odb, size_t pos) {
    git_odb_backend *out;
    auto __result = git_odb_get_backend(&out, odb, pos);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_get_backend");
    } else {
        return out;
    }
}



void odb_set_commit_graph(git_odb *odb, git_commit_graph *cgraph) {
    auto __result = git_odb_set_commit_graph(odb, cgraph);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_set_commit_graph");
    }
}



git_odb_backend *odb_backend_pack(const char *objects_dir) {
    git_odb_backend *out;
    auto __result = git_odb_backend_pack(&out, objects_dir);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_backend_pack");
    } else {
        return out;
    }
}



git_odb_backend *odb_backend_loose(const char *objects_dir, int compression_level, int do_fsync, unsigned int dir_mode, unsigned int file_mode) {
    git_odb_backend *out;
    auto __result = git_odb_backend_loose(&out, objects_dir, compression_level, do_fsync, dir_mode, file_mode);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_backend_loose");
    } else {
        return out;
    }
}



git_odb_backend *odb_backend_one_pack(const char *index_file) {
    git_odb_backend *out;
    auto __result = git_odb_backend_one_pack(&out, index_file);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_odb_backend_one_pack");
    } else {
        return out;
    }
}



git_repository *patch_owner(const git_patch *patch) {
    auto __result = git_patch_owner(patch);
    return __result;
}



git_patch *patch_from_diff(git_diff *diff, size_t idx) {
    git_patch *out;
    auto __result = git_patch_from_diff(&out, diff, idx);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_from_diff");
    } else {
        return out;
    }
}



git_patch *patch_from_blobs(const git_blob *old_blob, const char *old_as_path, const git_blob *new_blob, const char *new_as_path, const git_diff_options *opts) {
    git_patch *out;
    auto __result = git_patch_from_blobs(&out, old_blob, old_as_path, new_blob, new_as_path, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_from_blobs");
    } else {
        return out;
    }
}



git_patch *patch_from_blob_and_buffer(const git_blob *old_blob, const char *old_as_path, const void *buffer, size_t buffer_len, const char *buffer_as_path, const git_diff_options *opts) {
    git_patch *out;
    auto __result = git_patch_from_blob_and_buffer(&out, old_blob, old_as_path, buffer, buffer_len, buffer_as_path, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_from_blob_and_buffer");
    } else {
        return out;
    }
}



git_patch *patch_from_buffers(const void *old_buffer, size_t old_len, const char *old_as_path, const void *new_buffer, size_t new_len, const char *new_as_path, const git_diff_options *opts) {
    git_patch *out;
    auto __result = git_patch_from_buffers(&out, old_buffer, old_len, old_as_path, new_buffer, new_len, new_as_path, opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_from_buffers");
    } else {
        return out;
    }
}



void patch_free(git_patch *patch) {
    git_patch_free(patch);
}



const git_diff_delta *patch_get_delta(const git_patch *patch) {
    auto __result = git_patch_get_delta(patch);
    return __result;
}



size_t patch_num_hunks(const git_patch *patch) {
    auto __result = git_patch_num_hunks(patch);
    return __result;
}



void patch_line_stats(size_t *total_context, size_t *total_additions, size_t *total_deletions, const git_patch *patch) {
    auto __result = git_patch_line_stats(total_context, total_additions, total_deletions, patch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_line_stats");
    }
}



const git_diff_hunk *patch_get_hunk(size_t *lines_in_hunk, git_patch *patch, size_t hunk_idx) {
    const git_diff_hunk *out;
    auto __result = git_patch_get_hunk(&out, lines_in_hunk, patch, hunk_idx);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_get_hunk");
    } else {
        return out;
    }
}



void patch_num_lines_in_hunk(const git_patch *patch, size_t hunk_idx) {
    auto __result = git_patch_num_lines_in_hunk(patch, hunk_idx);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_num_lines_in_hunk");
    }
}



const git_diff_line *patch_get_line_in_hunk(git_patch *patch, size_t hunk_idx, size_t line_of_hunk) {
    const git_diff_line *out;
    auto __result = git_patch_get_line_in_hunk(&out, patch, hunk_idx, line_of_hunk);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_get_line_in_hunk");
    } else {
        return out;
    }
}



size_t patch_size(git_patch *patch, int include_context, int include_hunk_headers, int include_file_headers) {
    auto __result = git_patch_size(patch, include_context, include_hunk_headers, include_file_headers);
    return __result;
}



void patch_print(git_patch *patch, git_diff_line_cb print_cb, void *payload) {
    auto __result = git_patch_print(patch, print_cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_print");
    }
}



git_buf patch_to_buf(git_patch *patch) {
    git_buf out;
    auto __result = git_patch_to_buf(&out, patch);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_patch_to_buf");
    } else {
        return out;
    }
}



git_pathspec *pathspec_new(const git_strarray *pathspec) {
    git_pathspec *out;
    auto __result = git_pathspec_new(&out, pathspec);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_pathspec_new");
    } else {
        return out;
    }
}



void pathspec_free(git_pathspec *ps) {
    git_pathspec_free(ps);
}



void pathspec_matches_path(const git_pathspec *ps, uint32_t flags, const char *path) {
    auto __result = git_pathspec_matches_path(ps, flags, path);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_pathspec_matches_path");
    }
}



git_pathspec_match_list *pathspec_match_workdir(git_repository *repo, uint32_t flags, git_pathspec *ps) {
    git_pathspec_match_list *out;
    auto __result = git_pathspec_match_workdir(&out, repo, flags, ps);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_pathspec_match_workdir");
    } else {
        return out;
    }
}



git_pathspec_match_list *pathspec_match_index(git_index *index, uint32_t flags, git_pathspec *ps) {
    git_pathspec_match_list *out;
    auto __result = git_pathspec_match_index(&out, index, flags, ps);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_pathspec_match_index");
    } else {
        return out;
    }
}



git_pathspec_match_list *pathspec_match_tree(git_tree *tree, uint32_t flags, git_pathspec *ps) {
    git_pathspec_match_list *out;
    auto __result = git_pathspec_match_tree(&out, tree, flags, ps);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_pathspec_match_tree");
    } else {
        return out;
    }
}



git_pathspec_match_list *pathspec_match_diff(git_diff *diff, uint32_t flags, git_pathspec *ps) {
    git_pathspec_match_list *out;
    auto __result = git_pathspec_match_diff(&out, diff, flags, ps);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_pathspec_match_diff");
    } else {
        return out;
    }
}



void pathspec_match_list_free(git_pathspec_match_list *m) {
    git_pathspec_match_list_free(m);
}



size_t pathspec_match_list_entrycount(const git_pathspec_match_list *m) {
    auto __result = git_pathspec_match_list_entrycount(m);
    return __result;
}



const char *pathspec_match_list_entry(const git_pathspec_match_list *m, size_t pos) {
    auto __result = git_pathspec_match_list_entry(m, pos);
    return __result;
}



const git_diff_delta *pathspec_match_list_diff_entry(const git_pathspec_match_list *m, size_t pos) {
    auto __result = git_pathspec_match_list_diff_entry(m, pos);
    return __result;
}



size_t pathspec_match_list_failed_entrycount(const git_pathspec_match_list *m) {
    auto __result = git_pathspec_match_list_failed_entrycount(m);
    return __result;
}



const char *pathspec_match_list_failed_entry(const git_pathspec_match_list *m, size_t pos) {
    auto __result = git_pathspec_match_list_failed_entry(m, pos);
    return __result;
}



git_refdb *refdb_new(git_repository *repo) {
    git_refdb *out;
    auto __result = git_refdb_new(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refdb_new");
    } else {
        return out;
    }
}



git_refdb *refdb_open(git_repository *repo) {
    git_refdb *out;
    auto __result = git_refdb_open(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refdb_open");
    } else {
        return out;
    }
}



void refdb_compress(git_refdb *refdb) {
    auto __result = git_refdb_compress(refdb);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_refdb_compress");
    }
}



void refdb_free(git_refdb *refdb) {
    git_refdb_free(refdb);
}



git_reflog *reflog_read(git_repository *repo, const char *name) {
    git_reflog *out;
    auto __result = git_reflog_read(&out, repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reflog_read");
    } else {
        return out;
    }
}



void reflog_write(git_reflog *reflog) {
    auto __result = git_reflog_write(reflog);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reflog_write");
    }
}



void reflog_append(git_reflog *reflog, const git_oid *id, const git_signature *committer, const char *msg) {
    auto __result = git_reflog_append(reflog, id, committer, msg);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reflog_append");
    }
}



void reflog_rename(git_repository *repo, const char *old_name, const char *name) {
    auto __result = git_reflog_rename(repo, old_name, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reflog_rename");
    }
}



void reflog_delete(git_repository *repo, const char *name) {
    auto __result = git_reflog_delete(repo, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reflog_delete");
    }
}



size_t reflog_entrycount(git_reflog *reflog) {
    auto __result = git_reflog_entrycount(reflog);
    return __result;
}



const git_reflog_entry *reflog_entry_byindex(const git_reflog *reflog, size_t idx) {
    auto __result = git_reflog_entry_byindex(reflog, idx);
    return __result;
}



void reflog_drop(git_reflog *reflog, size_t idx, int rewrite_previous_entry) {
    auto __result = git_reflog_drop(reflog, idx, rewrite_previous_entry);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reflog_drop");
    }
}



const git_oid *reflog_entry_id_old(const git_reflog_entry *entry) {
    auto __result = git_reflog_entry_id_old(entry);
    return __result;
}



const git_oid *reflog_entry_id_new(const git_reflog_entry *entry) {
    auto __result = git_reflog_entry_id_new(entry);
    return __result;
}



const git_signature *reflog_entry_committer(const git_reflog_entry *entry) {
    auto __result = git_reflog_entry_committer(entry);
    return __result;
}



const char *reflog_entry_message(const git_reflog_entry *entry) {
    auto __result = git_reflog_entry_message(entry);
    return __result;
}



void reflog_free(git_reflog *reflog) {
    git_reflog_free(reflog);
}



void reset(git_repository *repo, const git_object *target, git_reset_t reset_type, const git_checkout_options *checkout_opts) {
    auto __result = git_reset(repo, target, reset_type, checkout_opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reset");
    }
}



void reset_from_annotated(git_repository *repo, const git_annotated_commit *commit, git_reset_t reset_type, const git_checkout_options *checkout_opts) {
    auto __result = git_reset_from_annotated(repo, commit, reset_type, checkout_opts);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reset_from_annotated");
    }
}



void reset_default(git_repository *repo, const git_object *target, const git_strarray *pathspecs) {
    auto __result = git_reset_default(repo, target, pathspecs);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_reset_default");
    }
}



git_revwalk *revwalk_new(git_repository *repo) {
    git_revwalk *out;
    auto __result = git_revwalk_new(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_new");
    } else {
        return out;
    }
}



void revwalk_reset(git_revwalk *walker) {
    auto __result = git_revwalk_reset(walker);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_reset");
    }
}



void revwalk_push(git_revwalk *walk, const git_oid *id) {
    auto __result = git_revwalk_push(walk, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_push");
    }
}



void revwalk_push_glob(git_revwalk *walk, const char *glob) {
    auto __result = git_revwalk_push_glob(walk, glob);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_push_glob");
    }
}



void revwalk_push_head(git_revwalk *walk) {
    auto __result = git_revwalk_push_head(walk);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_push_head");
    }
}



void revwalk_hide(git_revwalk *walk, const git_oid *commit_id) {
    auto __result = git_revwalk_hide(walk, commit_id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_hide");
    }
}



void revwalk_hide_glob(git_revwalk *walk, const char *glob) {
    auto __result = git_revwalk_hide_glob(walk, glob);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_hide_glob");
    }
}



void revwalk_hide_head(git_revwalk *walk) {
    auto __result = git_revwalk_hide_head(walk);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_hide_head");
    }
}



void revwalk_push_ref(git_revwalk *walk, const char *refname) {
    auto __result = git_revwalk_push_ref(walk, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_push_ref");
    }
}



void revwalk_hide_ref(git_revwalk *walk, const char *refname) {
    auto __result = git_revwalk_hide_ref(walk, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_hide_ref");
    }
}



int revwalk_next(git_oid *out, git_revwalk *walk) {
    auto __result = git_revwalk_next(out, walk);
    return __result;
}



void revwalk_sorting(git_revwalk *walk, unsigned int sort_mode) {
    auto __result = git_revwalk_sorting(walk, sort_mode);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_sorting");
    }
}



void revwalk_push_range(git_revwalk *walk, const char *range) {
    auto __result = git_revwalk_push_range(walk, range);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_push_range");
    }
}



void revwalk_simplify_first_parent(git_revwalk *walk) {
    auto __result = git_revwalk_simplify_first_parent(walk);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_simplify_first_parent");
    }
}



void revwalk_free(git_revwalk *walk) {
    git_revwalk_free(walk);
}



git_repository *revwalk_repository(git_revwalk *walk) {
    auto __result = git_revwalk_repository(walk);
    return __result;
}



void revwalk_add_hide_cb(git_revwalk *walk, git_revwalk_hide_cb hide_cb, void *payload) {
    auto __result = git_revwalk_add_hide_cb(walk, hide_cb, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_revwalk_add_hide_cb");
    }
}



git_signature *signature_new(const char *name, const char *email, git_time_t time, int offset) {
    git_signature *out;
    auto __result = git_signature_new(&out, name, email, time, offset);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_signature_new");
    } else {
        return out;
    }
}



git_signature *signature_now(const char *name, const char *email) {
    git_signature *out;
    auto __result = git_signature_now(&out, name, email);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_signature_now");
    } else {
        return out;
    }
}



git_signature *signature_default(git_repository *repo) {
    git_signature *out;
    auto __result = git_signature_default(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_signature_default");
    } else {
        return out;
    }
}



git_signature *signature_from_buffer(const char *buf) {
    git_signature *out;
    auto __result = git_signature_from_buffer(&out, buf);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_signature_from_buffer");
    } else {
        return out;
    }
}



void signature_dup(git_signature **dest, const git_signature *sig) {
    auto __result = git_signature_dup(dest, sig);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_signature_dup");
    }
}



void signature_free(git_signature *sig) {
    git_signature_free(sig);
}



git_tag *tag_lookup(git_repository *repo, const git_oid *id) {
    git_tag *out;
    auto __result = git_tag_lookup(&out, repo, id);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_lookup");
    } else {
        return out;
    }
}



git_tag *tag_lookup_prefix(git_repository *repo, const git_oid *id, size_t len) {
    git_tag *out;
    auto __result = git_tag_lookup_prefix(&out, repo, id, len);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_lookup_prefix");
    } else {
        return out;
    }
}



void tag_free(git_tag *tag) {
    git_tag_free(tag);
}



const git_oid *tag_id(const git_tag *tag) {
    auto __result = git_tag_id(tag);
    return __result;
}



git_repository *tag_owner(const git_tag *tag) {
    auto __result = git_tag_owner(tag);
    return __result;
}



void tag_target(git_object **target_out, const git_tag *tag) {
    auto __result = git_tag_target(target_out, tag);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_target");
    }
}



const git_oid *tag_target_id(const git_tag *tag) {
    auto __result = git_tag_target_id(tag);
    return __result;
}



git_object_t tag_target_type(const git_tag *tag) {
    auto __result = git_tag_target_type(tag);
    return __result;
}



const char *tag_name(const git_tag *tag) {
    auto __result = git_tag_name(tag);
    return __result;
}



const git_signature *tag_tagger(const git_tag *tag) {
    auto __result = git_tag_tagger(tag);
    return __result;
}



const char *tag_message(const git_tag *tag) {
    auto __result = git_tag_message(tag);
    return __result;
}



void tag_create(git_oid *oid, git_repository *repo, const char *tag_name, const git_object *target, const git_signature *tagger, const char *message, int force) {
    auto __result = git_tag_create(oid, repo, tag_name, target, tagger, message, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_create");
    }
}



void tag_annotation_create(git_oid *oid, git_repository *repo, const char *tag_name, const git_object *target, const git_signature *tagger, const char *message) {
    auto __result = git_tag_annotation_create(oid, repo, tag_name, target, tagger, message);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_annotation_create");
    }
}



void tag_create_from_buffer(git_oid *oid, git_repository *repo, const char *buffer, int force) {
    auto __result = git_tag_create_from_buffer(oid, repo, buffer, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_create_from_buffer");
    }
}



void tag_create_lightweight(git_oid *oid, git_repository *repo, const char *tag_name, const git_object *target, int force) {
    auto __result = git_tag_create_lightweight(oid, repo, tag_name, target, force);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_create_lightweight");
    }
}



void tag_delete(git_repository *repo, const char *tag_name) {
    auto __result = git_tag_delete(repo, tag_name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_delete");
    }
}



void tag_list(git_strarray *tag_names, git_repository *repo) {
    auto __result = git_tag_list(tag_names, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_list");
    }
}



void tag_list_match(git_strarray *tag_names, const char *pattern, git_repository *repo) {
    auto __result = git_tag_list_match(tag_names, pattern, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_list_match");
    }
}



void tag_foreach(git_repository *repo, git_tag_foreach_cb callback, void *payload) {
    auto __result = git_tag_foreach(repo, callback, payload);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_foreach");
    }
}



void tag_peel(git_object **tag_target_out, const git_tag *tag) {
    auto __result = git_tag_peel(tag_target_out, tag);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_peel");
    }
}



git_tag *tag_dup(git_tag *source) {
    git_tag *out;
    auto __result = git_tag_dup(&out, source);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_dup");
    } else {
        return out;
    }
}



void tag_name_is_valid(int *valid, const char *name) {
    auto __result = git_tag_name_is_valid(valid, name);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_tag_name_is_valid");
    }
}



git_transaction *transaction_new(git_repository *repo) {
    git_transaction *out;
    auto __result = git_transaction_new(&out, repo);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_transaction_new");
    } else {
        return out;
    }
}



void transaction_lock_ref(git_transaction *tx, const char *refname) {
    auto __result = git_transaction_lock_ref(tx, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_transaction_lock_ref");
    }
}



void transaction_set_target(git_transaction *tx, const char *refname, const git_oid *target, const git_signature *sig, const char *msg) {
    auto __result = git_transaction_set_target(tx, refname, target, sig, msg);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_transaction_set_target");
    }
}



void transaction_set_symbolic_target(git_transaction *tx, const char *refname, const char *target, const git_signature *sig, const char *msg) {
    auto __result = git_transaction_set_symbolic_target(tx, refname, target, sig, msg);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_transaction_set_symbolic_target");
    }
}



void transaction_set_reflog(git_transaction *tx, const char *refname, const git_reflog *reflog) {
    auto __result = git_transaction_set_reflog(tx, refname, reflog);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_transaction_set_reflog");
    }
}



void transaction_remove(git_transaction *tx, const char *refname) {
    auto __result = git_transaction_remove(tx, refname);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_transaction_remove");
    }
}



void transaction_commit(git_transaction *tx) {
    auto __result = git_transaction_commit(tx);
    if (__result < 0) {
        __GIT_THROW_EXCEPTION(__result, "git_transaction_commit");
    }
}



void transaction_free(git_transaction *tx) {
    git_transaction_free(tx);
}



