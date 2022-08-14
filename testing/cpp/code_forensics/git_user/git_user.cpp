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
#include <shared_mutex>
#include <algorithm>
#include <tuple>
#include <future>
#include <sstream>
#include <optional>
#include <semaphore>
#include <unordered_map>
#include <unordered_set>
#include <set>

#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/process.hpp>

#include <boost/thread/mutex.hpp>
#include <boost/thread/lock_guard.hpp>

#include <boost/log/trivial.hpp>
#include <boost/log/common.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/attributes.hpp>
#include <boost/log/sinks.hpp>
#include <boost/log/sources/logger.hpp>
#include <boost/log/utility/record_ordering.hpp>


using namespace boost;

namespace boost::log {
namespace expr  = boost::log::expressions;
namespace attrs = boost::log::attributes;
}; // namespace boost::log

using Date = gregorian::date;

template <>
struct fmt::formatter<Date> : fmt::formatter<Str> {
    auto format(CR<Date> date, fmt::format_context& ctx) const {
        return fmt::formatter<Str>::format(
            gregorian::to_iso_extended_string(date), ctx);
    }
};

template <dod::IsIdType Id>
struct fmt::formatter<Id> : fmt::formatter<Str> {
    auto format(CR<Id> date, fmt::format_context& ctx) const {
        return fmt::formatter<Str>::format(date.getStr(), ctx);
    }
};

template <typename T>
struct fmt::formatter<Opt<T>> : fmt::formatter<Str> {
    auto format(CR<Opt<T>> date, fmt::format_context& ctx) const {
        if (date) {
            return fmt::formatter<T>().format(date.value(), ctx);
        } else {
            return fmt::formatter<Str>().format("none()", ctx);
        }
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

    const char* what() const noexcept override { return message.c_str(); }
};
} // namespace git


#include <iostream>
// NOLINTNEXTLINE
#define __GIT_THROW_EXCEPTION(code, function)                             \
    throw git::exception(code, function);

namespace git {
#include "gitwrap.hpp"
}

auto oid_tostr(git_oid oid) -> Str {
    std::array<char, GIT_OID_HEXSZ + 1> result;
    git_oid_tostr(result.data(), sizeof(result), &oid);
    return Str{result.data(), result.size() - 1};
}

template <>
struct fmt::formatter<git_oid> : fmt::formatter<Str> {
    auto format(CR<git_oid> date, fmt::format_context& ctx) const {
        return fmt::formatter<Str>::format(oid_tostr(date), ctx);
    }
};


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

    Str  db_path;
    bool try_incremental;

    /// Allow processing of a specific path in the repository
    Func<bool(CR<Str>)> allow_path;
    /// Get integer index of the period for Date
    Func<int(const Date&)> get_period;
    /// Check whether commits at the specified date should be analysed
    Func<bool(const Date&)> allow_sample_at_date;
};

template <typename T>
auto async_task(walker_config::threading_mode mode, Func<T()> task)
    -> std::future<T> {
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


namespace std {
template <>
struct hash<git_oid> {
    auto operator()(const git_oid& it) const -> std::size_t {
        return std::hash<Str>()(
            Str(reinterpret_cast<const char*>(&it.id[0]), sizeof(it.id)));
    }
};
} // namespace std

auto operator==(CR<git_oid> lhs, CR<git_oid> rhs) -> bool {
    return oid_cmp(&lhs, &rhs) == 0;
}

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

    /// Ordered list of commits that were considered for the processing run
    Vec<git_oid> full_commits;
    /// Mapping from the commit id to it's position in the whole list of
    /// considered commits
    std::unordered_map<git_oid, int> rev_index;
    /// Mapping from the commits to the analysis periods they are in
    std::unordered_map<git_oid, int> rev_periods;
    std::atomic<int>                 completed_commits;

    void add_full_commit(CR<git_oid> oid, int period) {
        rev_index.insert({oid, full_commits.size()});
        rev_periods.insert({oid, period});
        full_commits.push_back(oid);
    }

    /// Get period that commit is attributed to. May return 'none' option
    /// for commits that were not registered in the revese period index -
    /// ones that come from a different branch that we didn't iterate over.
    auto get_period(CR<git_oid> commit) const noexcept -> Opt<int> {
        // NOTE dynamically patching table of missing commits each time an
        // unknown is encountered is possible, but undesirable.
        auto found = rev_periods.find(commit);
        if (found != rev_periods.end()) {
            return Opt<int>{found->second};
        } else {
            return Opt<int>{};
        }
    }

    auto get_period(CR<git_oid> commit, CR<git_oid> line) const noexcept
        -> int {
        auto lp = get_period(line);
        auto cp = get_period(commit);
        return lp.value_or(cp.value());
    }

    /// Whether to consider commit referred to by \arg commit_id has
    /// changed in the same period as the line (\arg line_changed_id).
    ///
    /// If line comes from an unknow commit (different branch for example)
    /// it is considered changed.
    auto consider_changed(
        CR<git_oid> commit_id,
        CR<git_oid> line_change_id) const -> bool {
        auto commit = get_period(commit_id);
        auto line   = get_period(line_change_id);
        if (line) {
            return line.value() == commit.value();
        } else {
            return true;
        }
    }

    /// List of commits that were selected for the processing run
    std::unordered_set<git_oid> sampled_commits;

    /// Create new benchmark point with provided \arg name
    void push_bench_point(CR<Str> name) {
        bench_points[name] = stime::system_clock::now();
    }

    /// Return time elapsed since the start of the benchmark point \arg
    /// name
    auto pop_bench_point(CR<Str> name) -> stime::milliseconds {
        return stime::duration_cast<stime::milliseconds>(
            stime::system_clock::now() - bench_points[name]);
    }

    std::mutex           m;
    ir::content_manager* content;
    SPtr<log::sources::severity_logger<log::trivial::severity_level>>
        logger;
};

/// \defgroup all_logging logging macros
/// Shorthand macros to write an output to the logger
/// @{
using severity = log::trivial::severity_level;

/// Wrapper around the logger call for setting the 'File', 'Line', and
/// 'Func' attributes.
#define CUSTOM_LOG(logger, sev)                                           \
    set_get_attrib("File", Str{__FILE__});                                \
    set_get_attrib("Line", __LINE__);                                     \
    set_get_attrib("Func", Str{__PRETTY_FUNCTION__});                     \
    BOOST_LOG_SEV(logger, sev)


/// Type alias for mutable constant template used in the logging. The
/// application *might* run in the multithreaded mode, so shared mutex is
/// used for guarding access to the data.
template <typename T>
using MutLog = log::attrs::mutable_constant<T, std::shared_mutex>;

/// Set value of the attribute and return a reference to it, for further
/// modifications.
///
/// \note The type of the value must match *exactly* with the original
/// attribute declaration - using `const char*` instead of the
/// `std::string` will result in the exception
template <typename ValueType>
auto set_get_attrib(const char* name, ValueType value) -> ValueType {
    auto attr = log::attribute_cast<MutLog<ValueType>>(
        log::core::get()->get_global_attributes()[name]);
    attr.set(value);
    return attr.get();
}

#define LOG_T(state) CUSTOM_LOG((*((state)->logger)), severity::trace)
#define LOG_D(state) CUSTOM_LOG((*((state)->logger)), severity::debug)
#define LOG_I(state) CUSTOM_LOG((*((state)->logger)), severity::info)
#define LOG_W(state) CUSTOM_LOG((*((state)->logger)), severity::warning)
#define LOG_E(state) CUSTOM_LOG((*((state)->logger)), severity::error)
#define LOG_F(state) CUSTOM_LOG((*((state)->logger)), severity::fatal)
/// @}

using SLock = std::scoped_lock<std::mutex>;

/// Collection of different commit sampling strategies
struct allow_state {
    // Equally spaced commit samples
    const int days_period = 10;
    /// Analyse commits starting from this date (for equally spaced
    /// samples)
    const Date start = {2020, 1, 1};

    /// For yearly sampling - visited years
    std::set<int> visited_periods;
    /// Index of the previous period
    int prev_period = -1;

    /// Only analyze single commit - whichever one will be tried first
    auto allow_once() -> bool {
        bool is_first = prev_period == -1;
        if (is_first) { prev_period = 0; }
        return is_first;
    }

    auto can_visit_period(int period) -> bool {
        auto missing = !visited_periods.contains(period);
        if (missing) { visited_periods.insert(period); }
        return missing;
    }

    auto allow_once_per_month(CR<Date> date) -> bool {
        return can_visit_period(month_to_period(date));
    }

    auto allow_once_per_year(CR<Date> date) -> bool {
        return can_visit_period(year_to_period(date));
    }

    auto allow_once_per_period(CR<Date> date) -> bool {
        int  period = (date - start).days() / days_period;
        bool is_new = period != prev_period;
        if (is_new) { prev_period = period; }
        return is_new;
    }

    auto year_to_period(CR<Date> date) -> int { return date.year(); }

    auto month_to_period(CR<Date> date) -> int {
        return date.year() * 1000 + date.month();
    }

    auto range_to_period(CR<Date> date) -> int {
        return (date - start).days() / days_period;
    }
};

auto get_nesting(CR<Str> line) -> int {
    int result = 0;
    while (result < line.size()) {
        char c = line[result];
        if (c != ' ' && c != '\n') { break; }

        ++result;
    }

    return result;
}

/// Append new line to the file and update related counteres (total
/// complexity, line count and so on)
void push_line(
    ir::FileId       id,
    walker_state*    walker,
    CR<ir::LineData> line,
    bool             changed,
    int              period) {
    auto& file      = walker->content->at(id);
    int   new_index = file.lines.size();
    auto& ranges    = file.changed_ranges;

    if (changed) { file.had_changes = true; }


    if (!ranges.empty() && (ranges.back().end + 1 == new_index) &&
        (ranges.back().period == period)) {
        ranges.back().end = new_index;
    } else {
        ranges.push_back(
            {.begin   = new_index,
             .end     = new_index,
             .period  = period,
             .changed = changed});
    }

    file.lines.push_back(walker->content->add(line));
    file.total_complexity += line.nesting;
    file.line_count += 1;
}

auto stats_via_subprocess(
    git_oid       commit_oid,
    walker_state* walker,
    ir::File      file,
    CR<Str>       relpath) -> ir::FileId {

    Str str_oid{oid_tostr(commit_oid)};

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
    ir::Author author;
    Str        text;
    Str        changed_at;

    while (blame.running() && std::getline(out, line) && !line.empty()) {
        // even for 'machine reading' output is not consistent - some parts
        // are optional and can be missing in the output, requiring extra
        // hacks for processing.
        switch (state) {
            case LK::Previous:
            case LK::Boundary: {
                if (line.starts_with("filename")) { state = LK::Filename; }
                break;
            }

            default: break;
        }

        std::stringstream is{line};

        switch (state) {
            case LK::Commit: {
                is >> changed_at;
                break;
            }

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
                is >> text;
                // Constructin a new line data using already parsed
                // elements and the file ID. Adding new line into the store
                // and immediately appending the content to the file.
                SLock   lock{walker->m};
                git_oid line_changed = oid_fromstr(changed_at.c_str());
                push_line(
                    result,
                    walker,
                    ir::LineData{
                        .author  = walker->content->add(author),
                        .time    = std::stol(time),
                        .content = walker->content->add(ir::String{text}),
                        .nesting = get_nesting(text)},
                    walker->consider_changed(commit_oid, line_changed),
                    walker->get_period(commit_oid, line_changed));
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

auto stats_via_libgit(
    walker_state*         state,
    git_oid               commit_oid,
    const git_tree_entry* entry,
    CR<Str>               relpath,
    ir::File              file) -> ir::FileId {

    auto result = ir::FileId::Nil();
    {
        SLock lock{state->m};
        result = state->content->add(file);
    }
    // Init default blame creation options
    git_blame_options blameopts = GIT_BLAME_OPTIONS_INIT;
    // We are only interested in blame information up until target commit
    blameopts.newest_commit = commit_oid;
    // Extract git blob object
    git_object* object = tree_entry_to_object(state->repo, entry);
    // get blame information
    git_blame* blame = blame_file(
        state->repo, relpath.c_str(), &blameopts);
    assert(object_type(object) == GIT_OBJECT_BLOB);
    // `git_object` can be freely cast to the blob, provided we checked the
    // type first.
    auto blob = reinterpret_cast<git_blob*>(object);
    // Byte position in the blob content
    int i = 0;
    // Counter for file line iteration
    int line = 1;
    // When null hunk is encountered - complete execution
    bool break_on_null_hunk = false;
    // Get full size (in bytes) of the target blob
    git_object_size_t rawsize = blob_rawsize(blob);
    // Get raw content of the git blob
    const char* rawdata = static_cast<const char*>(
        git_blob_rawcontent(blob));

    // Process blob bytes - this is the only explicit delimiter we get when
    // working with blobs
    while (i < rawsize) {
        // Search for the next end of line
        const char* eol = static_cast<const char*>(
            memchr(rawdata + i, '\n', static_cast<size_t>(rawsize - i)));
        // Find input end index
        const int endpos = static_cast<int>(eol - rawdata + 1);
        // get information for the current line
        const git_blame_hunk* hunk = blame_get_hunk_byline(blame, line);

        // if hunk is empty stop processing
        if (break_on_null_hunk && hunk == nullptr) { break; }

        if (hunk != nullptr && hunk->final_signature != nullptr) {
            break_on_null_hunk = true;
            // get date when hunk had been altered
            auto ptr = (eol == nullptr) ? (rawdata + rawsize) : (eol);
            const auto size = static_cast<Str::size_type>(
                std::distance(rawdata + i, ptr));

            Str   str{rawdata + i, size};
            SLock lock{state->m};
            push_line(
                result,
                state,
                ir::LineData{
                    .author = state->content->add(ir::Author{}),
                    .time   = hunk->final_signature->when.time,
                    // FIXME get slice of the string for the content
                    .content = state->content->add(ir::String{str}),
                    .nesting = get_nesting(str)},
                state->consider_changed(commit_oid, hunk->final_commit_id),
                state->get_period(commit_oid, hunk->final_commit_id));
        }

        // Advance over raw data
        i = endpos;
        // Increment line
        line++;
    }

    // Blame information is no longer needed
    blame_free(blame);

    return result;
}

auto exec_walker(
    git_oid               commit_oid,
    walker_state*         state,
    ir::CommitId          commit,
    const char*           root,
    const git_tree_entry* entry) -> ir::FileId {

    // We are looking for blobs
    if (tree_entry_type(entry) != GIT_OBJECT_BLOB) {
        return ir::FileId::Nil();
    }
    // get entry name relative to `root`
    Str path{tree_entry_name(entry)};
    // Create full relative path for the target object
    auto relpath = Str{root + path};
    // Check for provided predicate

    // IR has several fields that must be initialized at the start, so
    // using an optional for the file and calling init in the
    // RAII-lock-guarded section.
    Opt<ir::File> init;

    {
        SLock lock{state->m};
        init = ir::File{
            .commit_id = commit,
            .parent    = state->content->getDirectory(Str{root}),
            .name      = state->content->add(ir::String{path})};
    }

    // Choose between different modes of data processing and call into one.
    ir::FileId result = state->config->use_subprocess
                            ? stats_via_subprocess(
                                  commit_oid, state, init.value(), relpath)
                            : stats_via_libgit(
                                  state,
                                  commit_oid,
                                  entry,
                                  relpath,
                                  init.value());


    return result;
}

auto add_sub_task(
    git_oid               commit_oid,
    walker_state*         state,
    ir::CommitId          out_commit,
    CR<Str>               root,
    const git_tree_entry* entry) -> std::future<ir::FileId> {
    // Duplicate all data passed to the callback - it is not owned by the
    // user code and might disappear by the time we get to the actual
    // walker execution
    auto sub_task =
        [commit_oid, out_commit, state, root, entry]() -> ir::FileId {
        // cap maximum number of actively executed walkers - their
        // implementation does not have any overlapping critical sections,
        // but performance limitations are present (large projects might
        // have thousands of calls)

        state->semaphore.acquire(); // cap maximum number of the parallel
                                    // processing calls
        // RAII helper in case file processing causes an exception.
        finally lock{[state] { state->semaphore.release(); }};
        // Walker returns optional analysis result
        auto result = exec_walker(
            commit_oid, state, out_commit, root.c_str(), entry);

        return result;
    };

    return async_task<ir::FileId>(state->config->use_threading, sub_task);
}


/// Implementaiton of the commit processing function. Walks files that were
/// available in the repository at the time and process each file
/// individually, filling data into the content store.
auto process_commit_impl(git_oid commit_oid, walker_state* state)
    -> ir::CommitId {
    git_commit* commit = commit_lookup(state->repo, &commit_oid);
    // Get tree for a commit
    auto tree = commit_tree(commit);
    // commit information should be cleaned up when we exit the scope
    finally close{[commit]() {
        // FIXME freeing the commit causes segmentation fault and I have no
        // idea what is causing this - the issue occurs even in the
        // sequential, non-parallelized mode. The issue was introduces in
        // the commit '4e0bda9'
        //
        // commit_free(commit);
    }};

    auto hash = oid_tostr(*git_commit_id(commit));

    if (state->config->try_incremental) {
        for (auto& [id, commit] :
             state->content->multi.store<ir::Commit>().pairs()) {
            if (commit->hash == hash) { return id; }
        }
    }

    // Work around possible exceptions in the commit addition - content is
    // shared, this piece of code might be executed in parallel.
    auto out_commit = ir::CommitId::Nil();
    {
        auto signature = const_cast<git_signature*>(commit_author(commit));

        finally close{[signature]() { signature_free(signature); }};
        SLock   lock{state->m};
        out_commit = state->content->add(ir::Commit{
            .author   = state->content->add(ir::Author{
                  .name  = Str{signature->name},
                  .email = Str{signature->email}}),
            .time     = commit_time(commit),
            .timezone = commit_time_offset(commit),
            .hash     = hash,
            .period   = state->config->get_period(
                posix_time::from_time_t(commit_time(commit)).date()),
            .message = Str{commit_message(commit)}});
    }

    // List of subtasks that need to be executed for each specific file.
    Vec<std::tuple<Str, git_tree_entry*, std::future<ir::FileId>>>
        treewalk;
    // walk all entries in the tree and collect them for further
    // processing.
    tree_walk(
        tree,
        // order is not particularly important, doing preorder
        // traversal here
        GIT_TREEWALK_PRE,
        // Capture all necessary data for execution and delegate the
        // implementation to the actual function.
        [&treewalk, state](const char* root, const git_tree_entry* entry) {
            auto relpath = Str{Str{root} + Str{tree_entry_name(entry)}};
            if (!state->config->allow_path ||
                state->config->allow_path(relpath)) {
                treewalk.push_back(
                    {Str{root},
                     tree_entry_dup(entry),
                     std::future<ir::FileId>{}});
            }
            return GIT_OK;
        });


    for (auto& [root, entry, task] : treewalk) {
        task = add_sub_task(commit_oid, state, out_commit, root, entry);
    }


    // For all futures provided in the subtask analysis - get result,
    // if it is non-empty, merge it with input data.
    int count = 0;
    for (auto& [root, entry, task] : treewalk) {
        auto result = task.get();
        if (!result.isNil()) {
            ++count;
            LOG_I(state) << fmt::format(
                "Processed file {}{} ({}/{} of {}) as id:{} with {} "
                "ranges",
                root,
                tree_entry_name(entry),
                count,
                treewalk.size(),
                hash,
                result,
                state->content->at(result).changed_ranges.size());

            SLock lock{state->m};
            state->content->at(out_commit).files.push_back(result);
        }
        tree_entry_free(entry);
    }


    ++state->completed_commits;
    LOG_I(state) << fmt::format(
        "Finished commit {} ({}/{})",
        hash,
        state->completed_commits.load(),
        state->sampled_commits.size());

    return out_commit;
}

/// Launch single commit processing task
auto process_commit(git_oid oid, walker_state* state)
    -> std::future<ir::CommitId> {
    return async_task<ir::CommitId>(
        state->config->use_threading, [oid, state]() -> ir::CommitId {
            // `process_commmit` is largely a helper function that is used
            // to create closure with necessary captures and then delegate
            // everything to the reguar implementation
            return process_commit_impl(oid, state);
        });
}


#define GIT_SUCCESS 0

auto launch_analysis(git_oid& oid, walker_state* state)
    -> Vec<std::future<ir::CommitId>> {
    // All constructed information
    Vec<std::future<ir::CommitId>> processed{};
    // Walk over every commit in the history
    Vec<std::pair<git_oid, Date>> full_commits{};
    while (revwalk_next(&oid, state->walker) == GIT_SUCCESS) {
        // Get commit from the provided oid
        git_commit* commit = commit_lookup(state->repo, &oid);
        // Convert from unix timestamp used by git to humane format
        Date date = posix_time::from_time_t(commit_time(commit)).date();

        // commit is no longer needed in this scope
        commit_free(commit);
        full_commits.push_back({oid, date});
        // check if we can process it
        if (state->config->allow_sample_at_date(date)) {
            int period = state->config->get_period(date);
            // Store in the list of commits for sampling
            state->sampled_commits.insert(oid);
            LOG_I(state) << fmt::format(
                "Processing commit {} at {} into period {}",
                oid,
                date,
                period);
        }
    }

    std::reverse(full_commits.begin(), full_commits.end());
    for (const auto& [commit, date] : full_commits) {
        state->add_full_commit(commit, state->config->get_period(date));
    }

    for (const auto& oid : state->sampled_commits) {
        processed.push_back(process_commit(oid, state));
    }


    return processed;
}

void open_walker(git_oid& oid, walker_state& state) {
    // Read HEAD on master
    Str head_filepath{state.config->repo + state.config->heads};
    // REFACTOR this part was copied from the SO example and I'm pretty
    // sure it can be implemented in a cleaner manner, but I haven't
    // touched this part yet.
    FILE*                head_fileptr = nullptr;
    std::array<char, 41> head_rev;

    if ((head_fileptr = fopen(head_filepath.c_str(), "r")) == nullptr) {
        throw std::system_error{
            std::error_code{},
            fmt::format("Error opening {}", head_filepath)};
    }

    if (fread(head_rev.data(), 40, 1, head_fileptr) != 1) {
        throw std::system_error{
            std::error_code{},
            fmt::format("Error reading from {}", head_filepath)};
        fclose(head_fileptr);
    }

    fclose(head_fileptr);

    oid = oid_fromstr(head_rev.data());
    // Initialize revision walker
    state.walker = revwalk_new(state.repo);
    // Iterate all commits in the topological order
    revwalk_sorting(state.walker, GIT_SORT_TOPOLOGICAL);
    revwalk_push(state.walker, &oid);
}

void load_content(walker_config* config, ir::content_manager& content) {
    auto storage = ir::create_db(config->db_path);
    storage.sync_schema();
    for (CR<ir::orm_line> line : storage.iterate<ir::orm_line>()) {
        // Explicitly specifying template parameters to use slicing for the
        // second argument.
        content.multi.insert<ir::LineId, ir::LineData>(line.id, line);
    }

    for (CR<ir::orm_commit> commit : storage.iterate<ir::orm_commit>()) {
        content.multi.insert<ir::CommitId, ir::Commit>(commit.id, commit);
    }

    for (CR<ir::orm_file> file : storage.iterate<ir::orm_file>()) {
        content.multi.insert<ir::FileId, ir::File>(file.id, file);
    }

    for (CR<ir::orm_dir> dir : storage.iterate<ir::orm_dir>()) {
        content.multi.insert<ir::DirectoryId, ir::Directory>(dir.id, dir);
    }

    for (CR<ir::orm_string> str : storage.iterate<ir::orm_string>()) {
        content.multi.insert<ir::StringId, ir::String>(str.id, str);
    }
}

void store_content(
    walker_config*          config,
    CR<ir::content_manager> content) {
    // Create storage connection
    auto storage = ir::create_db(config->db_path);
    // Sync with stored data
    storage.sync_schema();
    // Start the transaction - all data is inserted in bulk
    storage.begin_transaction();

    // Remove all previously stored data
    //
    // NOTE due to foreign key constraints on the database the order is
    // very important, otherwise deletion fails with `FOREIGN KEY
    // constraint failed` error
    //
    // HACK I temporarily removed all the foreign key constraints from the
    // ORM description, because it continued to randomly fail, even though
    // object ordering worked as expected. Maybe in the future I will fix
    // it back, but for now this piece of garbage can be ordered in any
    // way.
    if (!config->try_incremental) {
        storage.remove_all<ir::orm_line>();
        storage.remove_all<ir::orm_file>();
        storage.remove_all<ir::orm_commit>();
        storage.remove_all<ir::orm_lines_table>();
        storage.remove_all<ir::orm_changed_range>();
        storage.remove_all<ir::orm_dir>();
        storage.remove_all<ir::orm_author>();
        storage.remove_all<ir::orm_string>();
    }

    for (const auto& [id, string] :
         content.multi.store<ir::String>().pairs()) {
        storage.insert(ir::orm_string(id, ir::String{*string}));
    }


    for (const auto& [id, author] :
         content.multi.store<ir::Author>().pairs()) {
        storage.insert(ir::orm_author(id, *author));
    }

    for (const auto& [id, line] :
         content.multi.store<ir::LineData>().pairs()) {
        storage.insert(ir::orm_line(id, *line));
    }

    for (const auto& [id, commit] :
         content.multi.store<ir::Commit>().pairs()) {
        storage.insert(ir::orm_commit(id, *commit));
    }

    for (const auto& [id, dir] :
         content.multi.store<ir::Directory>().pairs()) {
        storage.insert(ir::orm_dir(id, *dir));
    }

    for (const auto& [id, file] :
         content.multi.store<ir::File>().pairs()) {
        storage.insert(ir::orm_file(id, *file));
        for (int idx = 0; idx < file->lines.size(); ++idx) {
            storage.insert(ir::orm_lines_table{
                .file = id, .index = idx, .line = file->lines[idx]});
        }

        for (int idx = 0; idx < file->changed_ranges.size(); ++idx) {
            storage.insert(ir::orm_changed_range{
                file->changed_ranges[idx], .file = id, .index = idx});
        }
    }

    storage.commit();
}

using backend_t = log::sinks::text_ostream_backend;
using sink_t    = log::sinks::asynchronous_sink<
    backend_t,
    log::sinks::unbounded_ordering_queue<log::attribute_value_ordering<
        unsigned int,
        std::less<unsigned int>>>>;

void log_formatter(
    log::record_view const&  rec,
    log::formatting_ostream& strm) {

    std::filesystem::path file{log::extract<Str>("File", rec).get()};

    strm << log::extract<boost::posix_time::ptime>("TimeStamp", rec);

    // strm    << " at " << file.filename().native()
    //     << ":" << log::extract<int>("Line", rec);

    strm << std::setw(4) << log::extract<unsigned int>("RecordID", rec) //
         << ": " << std::setw(7) << rec[log::trivial::severity]         //
         << " " << rec[log::expr::smessage];
}

auto create_file_sink(CR<Str> outfile) -> boost::shared_ptr<sink_t> {
    boost::shared_ptr<std::ostream> log_stream{new std::ofstream(outfile)};
    auto backend = boost::make_shared<backend_t>();
    // Flush log file after each record is written - this is done in a
    // separate thread, so won't block the processing for too long
    // (supposedly) and creates a much nicer-looking `trail -f` run
    backend->auto_flush(true);
    boost::shared_ptr<sink_t> sink(new sink_t(
        backend,
        // We'll apply record ordering to ensure that records from
        // different threads go sequentially in the file
        log::keywords::order = log ::make_attr_ordering<unsigned int>(
            "RecordID", std::less<unsigned int>())));

    sink->locked_backend()->add_stream(log_stream);
    sink->set_formatter(&log_formatter);

    return sink;
}

auto main() -> int {
    auto sink = create_file_sink("/tmp/git_user.log");
    log::core::get()->add_sink(sink);

    // Add some attributes too
    log::core::get()->add_global_attribute(
        "TimeStamp", log::attrs::local_clock());
    log::core::get()->add_global_attribute(
        "RecordID", log::attrs::counter<unsigned int>());


    log::core::get()->add_global_attribute("File", MutLog<Str>(""));
    log::core::get()->add_global_attribute("Func", MutLog<Str>(""));
    log::core::get()->add_global_attribute("Line", MutLog<int>(0));

    // Configure state of the sampling strategies
    allow_state allow{.days_period = 90, .start = {2020, 1, 1}};

    const bool use_fusion = false;

    // Provide implementation callback strategies
    auto config = UPtr<walker_config>(new walker_config{
        .use_subprocess = true,
        // Full process parallelization
        .use_threading   = walker_config::async,
        .repo            = use_fusion ? "/tmp/fusion" : "/tmp/Nim",
        .heads           = use_fusion ? "/.git/refs/heads/master"
                                      : "/.git/refs/heads/devel",
        .db_path         = "/tmp/db.sqlite",
        .try_incremental = false,
        .allow_path      = [use_fusion](CR<Str> path) -> bool {
            if (path.ends_with(".nim")) {
                if (use_fusion) {
                    return true;
                } else {
                    return path.find("compiler/") != Str::npos ||
                           path.find("rod/") != Str::npos;
                }
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
    // Create main walker state used in the whole commit analysis state
    auto state = UPtr<walker_state>(new walker_state{
        .config  = config.get(),
        .repo    = repository_open_ext(config->repo.c_str(), 0, nullptr),
        .content = &content,
        .logger  = std::make_shared<log::sources::severity_logger<
            log::trivial::severity_level>>()});

    if (config->try_incremental) {
        if (std::filesystem::exists(config->db_path)) {
            load_content(config.get(), content);
        } else {
            LOG_W(state) << "cannot load incremental from"
                         << config->db_path;
        }
    }

    try {
        git_oid oid;
        // Initialize state of the commit walker
        open_walker(oid, *state);
        // Start pararallel processing of the input data
        auto processed = launch_analysis(oid, state.get());

        // Store finalized commit IDs from executed tasks
        Vec<ir::CommitId> commits{};
        for (auto& future : processed) {
            commits.push_back(future.get());
        }

        LOG_I(state) << "Finished analysis, writing database";
        store_content(config.get(), content);

    } catch (...) {
        // Flush all buffered records
        sink->stop();
        sink->flush();
        throw;
    }

    LOG_I(state) << "Finished execution, DB written successfully";

    // Flush all buffered records
    sink->stop();
    sink->flush();


    return 0;
}
