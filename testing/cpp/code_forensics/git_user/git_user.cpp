#include "../common.hpp"
#include "git_ir.hpp"

#include <exception>
#include <string>
#include <git2.h>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <fmt/color.h>
#include <map>
#include <fstream>
#include <thread>
#include <mutex>
#include <algorithm>
#include <tuple>
#include <future>
#include <sstream>
#include <optional>
#include <semaphore>
#include <set>

#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/process.hpp>
#include <boost/log/trivial.hpp>

using namespace boost;

using Date = gregorian::date;

template <>
struct fmt::formatter<Date> : fmt::formatter<Str> {
    auto format(CR<Date> date, fmt::format_context& ctx) const {
        return fmt::formatter<Str>::format(
            gregorian::to_iso_extended_string(date), ctx);
    }
};

namespace git {
struct exception : public std::exception {
    Str message;
    inline exception(int error, const char* funcname) {
        const git_error* e = git_error_last();
        message            = fmt::format(
            "Error {}/{} while calling {}: {}",
            error,
            funcname,
            e->klass,
            e->message);
    }

    virtual const char* what() const noexcept override {
        return message.c_str();
    }
};
} // namespace git

#include <iostream>
#define __GIT_THROW_EXCEPTION(code, function)                             \
    throw git::exception(code, function);

namespace git {
#include "gitwrap.hpp"
}

using namespace git;
namespace stime = std::chrono;

void tree_walk(
    const git_tree*                               tree,
    git_treewalk_mode                             mode,
    Func<int(const char*, const git_tree_entry*)> callback) {
    using CB      = decltype(callback);
    CB* allocated = new CB;
    *allocated    = std::move(callback);
    git::tree_walk(
        tree,
        mode,
        [](const char*           root,
           const git_tree_entry* entry,
           void*                 payload) -> int {
            CB* impl = static_cast<CB*>(payload);
            try {
                auto __result = (*impl)(root, entry);
                return __result;
            } catch (...) { throw; }
        },
        allocated);
}


struct walker_config {
    /// Analyse commits via subprocess launches or via libgit blame
    /// execution
    bool use_subprocess = true;
    enum threading_mode { async, defer, sequential };
    threading_mode use_threading = threading_mode::async;
    /// Current project root path (absolute path)
    Str repo;
    Str heads;

    /// Allow processing of a specific path in the repository
    Func<bool(CR<Str>)> allow_path;
    /// Get integer index of the period for Date
    Func<int(const Date&)> get_period;
    /// Check whether commits at the specified date should be analysed
    Func<bool(const Date&)> allow_sample_at_date;
};

template <typename T>
std::future<T> async_task(
    walker_config::threading_mode mode,
    Func<T()>                     task) {
    switch (mode) {
        case walker_config::async: {
            return std::async(std::launch::async, task);
        }
        case walker_config::defer: {
            return std::async(std::launch::deferred, task);
        }
        case walker_config::sequential: {
            auto tmp = task();
            return std::async(
                std::launch::deferred, [tmp]() { return tmp; });
        }
    }
}

using TimePoint = stime::time_point<stime::system_clock>;

#define MAX_PARALLEL 16

/// Mutable state passed around walker configurations
struct walker_state {
    CP<walker_config> config;

    git_revwalk* walker;
    /// Current git repository
    git_repository* repo;
    /// Semaphore to cap maximum number of parallel subprocesses/threads
    /// (in case of libgit-based analysis)
    std::counting_semaphore<MAX_PARALLEL> semaphore{16};

    std::map<Str, TimePoint> bench_points;

    /// Create new benchmark point with provided \arg name
    void push_bench_point(CR<Str> name) {
        bench_points[name] = stime::system_clock::now();
    }

    /// Return time elapsed since the start of the benchmark point \arg
    /// name
    stime::milliseconds pop_bench_point(CR<Str> name) {
        return stime::duration_cast<stime::milliseconds>(
            stime::system_clock::now() - bench_points[name]);
    }

    std::mutex           m;
    ir::content_manager* content;
};

using SLock = std::scoped_lock<std::mutex>;

/// Collection of different commit sampling strategies
struct allow_state {
    // Equally spaced commit samples
    const int days_period = 10;
    /// Analyse commits starting from this date (for equally spaced
    /// samples)
    const Date start = {2020, 1, 1};

    /// For yearly sampling - visited years
    std::set<int> visited_years;
    /// Index of the previous period
    int prev_period = -1;

    /// Only analyze single commit - whichever one will be tried first
    bool allow_once() {
        if (prev_period == -1) {
            prev_period = 0;
            return true;
        } else {
            return false;
        }
    }

    bool allow_once_per_year(CR<Date> date) {
        if (!visited_years.contains(date.year())) {
            visited_years.insert(date.year());
            return true;
        } else {
            return false;
        }
    }

    bool allow_once_per_period(CR<Date> date) {
        int period = (date - start).days() / days_period;
        if (period != prev_period) {
            // HACK to ignore multiple commits that happened on the same
            // day
            prev_period = period;
            return true;
        } else {
            return false;
        }
    }

    int year_to_period(CR<Date> date) { return date.year(); }

    int range_to_period(CR<Date> date) {
        return (date - start).days() / days_period;
    }
};

Str oid_tostr(git_oid oid) {
    char result[GIT_OID_HEXSZ + 1];
    git_oid_tostr(result, sizeof(result), &oid);
    return result;
}

ir::FileId stats_via_subprocess(
    git_oid       oid,
    walker_state* walker,
    ir::file      file,
    CR<Str>       relpath) {

    Str str_oid{oid_tostr(oid)};

    // Getting file id immediately at the start in order to use it for the
    // line construction.
    auto result = ir::FileId::Nil();
    {
        SLock lock{walker->m};
        result = walker->content->add(file);
    }
    /// Start git blame subprocess
    Vec<Str> args{
        process::search_path("git").string(),
        "blame",
        "--line-porcelain",
        str_oid,
        "--",
        relpath};

    /// Read it's standard output
    process::ipstream out;
    /// Proces is started in the specified project directory
    process::child blame{
        args,
        process::std_out > out,
        process::start_dir(walker->config->repo)};

    Str line;
    // --line-porcelain generates chunks with twelve consecutive elements -
    // I'm only interested in the AuthorTime, everything else can be
    // skipped for now. Code below implements a simple state machine with
    // states encoded in the `LK` enum
    enum LK {
        Commit        = 0,
        Author        = 1,
        AuthorMail    = 2,
        AuthorTime    = 3,
        AuthorTz      = 4,
        Committer     = 5,
        CommitterMail = 6,
        CommitterTime = 7,
        CommitterTz   = 8,
        Summary       = 9,
        Previous      = 10,
        Boundary      = 11,
        Filename      = 12,
        Content       = 13
    };

    LK         state = LK::Commit;
    Str        time;
    ir::author author;
    Str        text;

    while (blame.running() && std::getline(out, line) && !line.empty()) {
        // even for 'machine reading' output is not consistent - some parts
        // are optional and can be missing in the output, requiring extra
        // hacks for processing.
        switch (state) {
            case LK::Previous: {
                if (line.starts_with("filename")) { state = LK::Filename; }
                break;
            }

            case LK::Boundary: {
                if (line.starts_with("filename")) { state = LK::Filename; }
                break;
            }

            default: break;
        }

        std::stringstream is{line};

        switch (state) {
            /// For now we are only looking into the authoring time
            case LK::AuthorTime: {
                is >> time;
                assert(time == "author-time");
                is >> time;
                break;
            }

            case LK::Author: {
                is >> author.name;
                is >> author.name;
                break;
            }

            case LK::Content: {
                SLock lock{walker->m};
                is >> text;
                // Constructin a new line data using already parsed
                // elements and the file ID. Adding new line into the store
                // and immediately appending the content to the file.
                walker->content->at(result).lines.push_back(
                    walker->content->add(ir::line{
                        .author  = walker->content->add(author),
                        .file    = result,
                        .time    = std::stol(time),
                        .content = walker->content->add(text)}));
                break;
            }

            default:
                // Ignore everything else
                break;
        }

        // (ab)use decaying of the enum to integer
        state = static_cast<LK>((state + 1) % (LK::Content + 1));
    }

    // Wait until the whole process is finished
    blame.wait();

    return result;
}

ir::FileId stats_via_libgit(
    walker_state*         state,
    git_oid               oid,
    const git_tree_entry* entry,
    CR<Str>               relpath,
    ir::file              file) {

    auto result = ir::FileId::Nil();
    {
        SLock lock{state->m};
        result = state->content->add(file);
    }
    // Init default blame creation options
    git_blame_options blameopts = GIT_BLAME_OPTIONS_INIT;
    // We are only interested in blame information up until target commit
    blameopts.newest_commit = oid;
    // Extract git blob object
    git_object* object = tree_entry_to_object(state->repo, entry);
    // get blame information
    git_blame* blame = blame_file(
        state->repo, relpath.c_str(), &blameopts);
    assert(object_type(object) == GIT_OBJECT_BLOB);
    // `git_object` can be freely cast to the blob, provided we checked the
    // type first
    git_blob* blob = (git_blob*)object;
    // Byte position in the blob content
    int i = 0;
    // Counter for file line iteration
    int line = 1;
    // When null hunk is encountered - complete execution
    bool break_on_null_hunk = false;
    // Get full size (in bytes) of the target blob
    git_object_size_t rawsize = blob_rawsize(blob);
    // Get raw content of the git blob
    const char* rawdata = (const char*)git_blob_rawcontent(blob);

    // Process blob bytes - this is the only explicit delimiter we get when
    // working with blobs
    while (i < rawsize) {
        // Search for the next end of line
        const char* eol = (const char*)memchr(
            rawdata + i, '\n', (size_t)(rawsize - i));
        // get information for the current line
        const git_blame_hunk* hunk = blame_get_hunk_byline(blame, line);

        // if hunk is empty stop processing
        if (break_on_null_hunk && !hunk) { break; }

        if (hunk != nullptr && hunk->final_signature != nullptr) {
            break_on_null_hunk = true;
            // get date when hunk had been altered
            SLock lock{state->m};
            state->content->multi.at(result).lines.push_back(
                state->content->add(ir::line{
                    .author  = state->content->multi.add(ir::author{}),
                    .file    = result,
                    .time    = hunk->final_signature->when.time,
                    .content = state->content->add(Str{})}));
        }

        // Advance over raw data
        i = (int)(eol - rawdata + 1);
        // Increment line
        line++;
    }

    // Blame information is no longer needed
    blame_free(blame);

    return result;
}

ir::FileId exec_walker(
    git_oid               oid,
    walker_state*         state,
    ir::CommitId          commit,
    const char*           root,
    const git_tree_entry* entry) {

    // We are looking for blobs
    if (tree_entry_type(entry) != GIT_OBJECT_BLOB) {
        return ir::FileId::Nil();
    }
    // get entry name relative to `root`
    Str path{tree_entry_name(entry)};
    // Create full relative path for the target object
    auto relpath = Str{root + path};
    // Check for provided predicate
    if (state->config->allow_path && !state->config->allow_path(relpath)) {
        return ir::FileId::Nil();
    }


    // IR has several fields that must be initialized at the start, so
    // using an optional for the file and calling init in the
    // RAII-lock-guarded section.
    Opt<ir::file> init;

    {
        SLock lock{state->m};
        init = ir::file{
            .commit_id = commit,
            .parent    = state->content->multi.add(ir::dir{
                   .parent = ir::DirectoryId::Nil(),
                   .name   = state->content->add(Str{root})}),
            .name      = state->content->add(path)};
    }

    ir::FileId result = state->config->use_subprocess
                            ? stats_via_subprocess(
                                  oid, state, init.value(), relpath)
                            : stats_via_libgit(
                                  state,
                                  oid,
                                  entry,
                                  relpath,
                                  init.value());

    static int total_done;
    {
        SLock lock{state->m};
        fmt::print(
            "DONE ({:<5}) {} {}\n", total_done++, oid_tostr(oid), relpath);
    }

    return result;
}

void add_sub_task(
    git_oid                       oid,
    walker_state*                 state,
    ir::CommitId                  out_commit,
    const char*                   root,
    const git_tree_entry*         entry,
    Vec<std::future<ir::FileId>>& sub_futures) {
    // Duplicate all data passed to the callback - it is not owned by the
    // user code and might disappear by the time we get to the actual
    // walker execution
    auto sub_task = [oid,
                     out_commit,
                     state,
                     user_str = Str(root),
                     user_dup = tree_entry_dup(entry)]() -> ir::FileId {
        // cap maximum number of actively executed walkers - their
        // implementation does not have any overlapping critical sections,
        // but performance limitations are present (large projects might
        // have thousands of calls)

        state->semaphore.acquire();
        finally lock{[state] { state->semaphore.release(); }};
        // Walker returns optional analysis result
        auto result = exec_walker(
            oid, state, out_commit, user_str.c_str(), user_dup);

        // tree entry must be freed manually, FIXME work around
        // exceptions in the walker executor
        tree_entry_free(user_dup);

        return result;
    };

    sub_futures.push_back(
        async_task<ir::FileId>(state->config->use_threading, sub_task));
}

std::atomic<int> file_count;

ir::CommitId process_commit_impl(git_oid oid, walker_state* state) {
    git_commit* commit = commit_lookup(state->repo, &oid);
    // Get tree for a commit
    auto tree = commit_tree(commit);
    // commit information should be cleaned up when we exit the scope
    finally close{[&commit]() { commit_free(commit); }};

    auto out_commit = ir::CommitId::Nil();
    {
        SLock lock{state->m};
        out_commit = state->content->add(ir::commit{});
    }
    Vec<std::future<ir::FileId>> sub_futures;
    // walk all entries in the tree
    tree_walk(
        tree,
        // order is not particularly important, doing preorder
        // traversal here
        GIT_TREEWALK_PRE,
        [oid, state, out_commit, &sub_futures](
            const char* root, const git_tree_entry* entry) {
            add_sub_task(oid, state, out_commit, root, entry, sub_futures);

            return GIT_OK;
        });


    // For all futures provided in the subtask analysis - get result,
    // if it is non-empty, merge it with input data.
    for (auto& future : sub_futures) {
        auto  result = future.get();
        SLock lock{state->m};
        state->content->at(out_commit).files.push_back(result);
    }

    return out_commit;
}

std::future<ir::CommitId> process_commit(
    git_oid       oid,
    walker_state* state) {
    return async_task<ir::CommitId>(
        state->config->use_threading, [oid, state]() -> ir::CommitId {
            return process_commit_impl(oid, state);
        });
}


#define GIT_SUCCESS 0

Vec<std::future<ir::CommitId>> launch_analysis(
    git_oid&      oid,
    walker_state* state) {
    // All constructed information
    Vec<std::future<ir::CommitId>> processed;

    // Walk over every commit in the history
    while (revwalk_next(&oid, state->walker) == GIT_SUCCESS) {
        // Get commit from the provided oid
        git_commit* commit = commit_lookup(state->repo, &oid);
        // Convert from unix timestamp used by git to humane format
        Date date = posix_time::from_time_t(commit_time(commit)).date();

        // commit is no longer needed in this scope
        commit_free(commit);
        // check if we can process it
        if (state->config->allow_sample_at_date(date)) {
            // and store analysis results if we can
            processed.push_back(process_commit(oid, state));
        }
    }

    return processed;
}

void open_walker(git_oid& oid, walker_state& state) {
    // Read HEAD on master
    Str head_filepath{state.config->repo + state.config->heads};
    // REFACTOR this part was copied from the SO example and I'm pretty
    // sure it can be implemented in a cleaner manner, but I haven't
    // touched this part yet.
    FILE* head_fileptr;
    char  head_rev[41];

    if ((head_fileptr = fopen(head_filepath.c_str(), "r")) == NULL) {
        throw std::system_error{
            std::error_code{},
            fmt::format("Error opening {}", head_filepath)};
    }

    if (fread(head_rev, 40, 1, head_fileptr) != 1) {
        throw std::system_error{
            std::error_code{},
            fmt::format("Error reading from {}", head_filepath)};
        fclose(head_fileptr);
    }

    fclose(head_fileptr);

    oid = oid_fromstr(head_rev);
    // Initialize revision walker
    state.walker = revwalk_new(state.repo);
    // Iterate all commits in the topological order
    revwalk_sorting(state.walker, GIT_SORT_TOPOLOGICAL);
    revwalk_push(state.walker, &oid);
}

int main() {
    // Configure state of the sampling strategies
    allow_state allow{.days_period = 90, .start = {2020, 1, 1}};

    // Provide implementation callback strategies
    auto config = UPtr<walker_config>(new walker_config{
        .use_subprocess = true,
        .use_threading  = walker_config::async,
        .repo           = "/tmp/fusion",
        .heads          = "/.git/refs/heads/master",
        .allow_path     = [](CR<Str> path) -> bool {
            if (path.ends_with(".nim")) {
                return true;
                // return path.find("compiler/") != Str::npos ||
                //        path.find("rod/") != Str::npos;
            } else if (path.ends_with(".pas")) {
                return path.find("nim/") != Str::npos;
            } else {
                return false;
            }
        },
        .get_period = [&](const Date& date) -> int {
            auto result = allow.year_to_period(date);
            return result;
        },
        .allow_sample_at_date = [&](const Date& date) -> bool {
            bool result = allow.allow_once_per_year(date);
            return result;
        }});


    libgit2_init();
    // Check whether threads can be enabled
    assert(libgit2_features() & GIT_FEATURE_THREADS);

    ir::content_manager content;

    auto state = UPtr<walker_state>(new walker_state{
        .config  = config.get(),
        .repo    = repository_open_ext(config->repo.c_str(), 0, nullptr),
        .content = &content});

    git_oid oid;
    open_walker(oid, *state);
    auto processed = launch_analysis(oid, state.get());

    Vec<ir::CommitId> commits;
    for (auto& future : processed) {
        commits.push_back(future.get());
    }

    auto storage = ir::create_db();
    storage.sync_schema();

    storage.remove_all<ir::orm_line>();
    storage.remove_all<ir::orm_string>();
    storage.remove_all<ir::orm_commit>();
    storage.remove_all<ir::orm_dir>();
    storage.remove_all<ir::orm_author>();
    storage.remove_all<ir::orm_file>();

    storage.begin_transaction();

    for (const auto& [id, string] : content.multi.store<Str>().pairs()) {
        storage.insert(ir::orm_string{*string, id});
    }

    for (const auto& [id, line] :
         content.multi.store<ir::line>().pairs()) {
        storage.insert(ir::orm_line{*line, id});
    }

    for (const auto& [id, author] :
         content.multi.store<ir::author>().pairs()) {
        storage.insert(ir::orm_author{*author, id});
    }

    for (const auto& [id, commit] :
         content.multi.store<ir::commit>().pairs()) {
        storage.insert(ir::orm_commit{*commit, id});
    }

    for (const auto& [id, dir] : content.multi.store<ir::dir>().pairs()) {
        storage.insert(ir::orm_dir{*dir, id});
    }

    for (const auto& [id, file] :
         content.multi.store<ir::file>().pairs()) {
        storage.insert(ir::orm_file{*file, id});
    }

    storage.commit();

    return 0;
}
