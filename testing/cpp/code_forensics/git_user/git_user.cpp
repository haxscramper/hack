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

#include <boost/program_options.hpp>
#include <boost/log/trivial.hpp>
#include <boost/log/common.hpp>
#include <boost/log/expressions.hpp>
#include <boost/log/attributes.hpp>
#include <boost/log/sinks.hpp>
#include <boost/log/sources/logger.hpp>
#include <boost/log/utility/record_ordering.hpp>
#include <boost/core/null_deleter.hpp>
#include <boost/python.hpp>

#include <datetime.h>

#include <indicators/progress_bar.hpp>
#include <indicators/block_progress_bar.hpp>
#include <indicators/cursor_control.hpp>

using namespace boost;

namespace boost::log {
namespace expr  = boost::log::expressions;
namespace attrs = boost::log::attributes;
}; // namespace boost::log


namespace fs = std::filesystem;

using Date         = gregorian::date;
using PTime        = posix_time::ptime;
using TimeDuration = posix_time::time_duration;

template <typename A, typename B>
using Pair = std::tuple<A, B>;

template <>
struct fmt::formatter<Date> : fmt::formatter<Str> {
    auto format(CR<Date> date, fmt::format_context& ctx) const {
        return fmt::formatter<Str>::format(
            gregorian::to_iso_extended_string(date), ctx);
    }
};

template <>
struct fmt::formatter<PTime> : fmt::formatter<Str> {
    auto format(CR<PTime> time, fmt::format_context& ctx) const {
        return fmt::formatter<Str>::format(
            posix_time::to_iso_extended_string(time), ctx);
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
    Func<int(CR<PTime>)> get_period;
    /// Check whether commits at the specified date should be analysed
    Func<bool(CR<PTime>, CR<Str>, CR<Str>)> allow_sample;
};


using TimePoint = stime::time_point<stime::system_clock>;

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

using Logger = log::sources::severity_logger<log::trivial::severity_level>;

/// Mutable state passed around walker configurations
struct walker_state {
    CP<walker_config> config;

    git_revwalk* walker;
    /// Current git repository
    git_repository* repo;

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

    std::mutex           m;
    ir::content_manager* content;
    SPtr<Logger>         logger;
};

/// \defgroup all_logging logging macros
/// Shorthand macros to write an output to the logger
/// @{
namespace boost::log {
using severity = boost::log::trivial::severity_level;
}

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

auto get_logger(walker_state* state) -> Logger& {
    return *(state->logger);
}

auto get_logger(UPtr<walker_state>& state) -> Logger& {
    return *(state->logger);
}

auto get_logger(SPtr<Logger>& in) -> Logger& { return *in; }

#define LOG_T(state) CUSTOM_LOG((get_logger(state)), log::severity::trace)
#define LOG_D(state) CUSTOM_LOG((get_logger(state)), log::severity::debug)
#define LOG_I(state) CUSTOM_LOG((get_logger(state)), log::severity::info)
#define LOG_W(state)                                                      \
    CUSTOM_LOG((get_logger(state)), log::severity::warning)
#define LOG_E(state) CUSTOM_LOG((get_logger(state)), log::severity::error)
#define LOG_F(state) CUSTOM_LOG((get_logger(state)), log::severity::fatal)
/// @}

using SLock = std::scoped_lock<std::mutex>;

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
    Str        changed_at;

    int line_counter = 0;
    while (std::getline(out, line) && !line.empty()) {
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
                        .content = walker->content->add(ir::String{line}),
                        .nesting = get_nesting(line)},
                    walker->consider_changed(commit_oid, line_changed),
                    walker->get_period(commit_oid, line_changed));
                ++line_counter;
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

struct SubTaskParams {
    git_oid         commit_oid;
    ir::CommitId    out_commit;
    Str             root;
    git_tree_entry* entry;
    int             index;
    int             max_count;
};

/// Implementaiton of the commit processing function. Walks files that were
/// available in the repository at the time and process each file
/// individually, filling data into the content store.
auto process_commit(git_oid commit_oid, walker_state* state)
    -> ir::CommitId {
    git_commit* commit = commit_lookup(state->repo, &commit_oid);
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

    {
        auto signature = const_cast<git_signature*>(commit_author(commit));
        finally close{[signature]() { signature_free(signature); }};
        return state->content->add(ir::Commit{
            .author   = state->content->add(ir::Author{
                  .name  = Str{signature->name},
                  .email = Str{signature->email}}),
            .time     = commit_time(commit),
            .timezone = commit_time_offset(commit),
            .hash     = hash,
            .period   = state->config->get_period(
                posix_time::from_time_t(commit_time(commit))),
            .message = Str{commit_message(commit)}});
    }
}

auto file_tasks(
    Vec<SubTaskParams>& treewalk, /// List of subtasks that need to be
                                  /// executed for each specific file.
    walker_state* state,
    git_oid       commit_oid,
    ir::CommitId  out_commit) {
    git_commit* commit = commit_lookup(state->repo, &commit_oid);
    // Get tree for a commit
    auto tree = commit_tree(commit);
    commit_free(commit);

    // walk all entries in the tree and collect them for further
    // processing.
    tree_walk(
        tree,
        // order is not particularly important, doing preorder
        // traversal here
        GIT_TREEWALK_PRE,
        // Capture all necessary data for execution and delegate the
        // implementation to the actual function.
        [&treewalk, state, out_commit, commit_oid](
            const char* root, const git_tree_entry* entry) {
            auto relpath = Str{Str{root} + Str{tree_entry_name(entry)}};
            if (!state->config->allow_path ||
                state->config->allow_path(relpath)) {
                treewalk.push_back(SubTaskParams{
                    .commit_oid = commit_oid,
                    .out_commit = out_commit,
                    .root       = Str{root},
                    .entry      = tree_entry_dup(entry)});
            }
            return GIT_OK;
        });
}


#define GIT_SUCCESS 0

auto launch_analysis(git_oid& oid, walker_state* state)
    -> Vec<ir::CommitId> {
    // All constructed information
    Vec<ir::CommitId> processed{};
    // Walk over every commit in the history
    Vec<std::pair<git_oid, PTime>> full_commits{};
    while (revwalk_next(&oid, state->walker) == GIT_SUCCESS) {
        // Get commit from the provided oid
        git_commit* commit = commit_lookup(state->repo, &oid);
        // Convert from unix timestamp used by git to humane format
        PTime date = posix_time::from_time_t(commit_time(commit));

        // commit is no longer needed in this scope
        commit_free(commit);
        full_commits.push_back({oid, date});
        // check if we can process it
        //
        // FIXME `commit_author` returns invalid signature here that causes
        // a segfault during conversion to a string. Otherwise
        // `commit_author(commit)->name` is the correct way (according to
        // the documentation least).
        if (state->config->allow_sample(date, "", oid_tostr(oid))) {
            int period = state->config->get_period(date);
            // Store in the list of commits for sampling
            state->sampled_commits.insert(oid);
            LOG_T(state) << fmt::format(
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

    using namespace indicators;
    Vec<SubTaskParams> params;
    {
        BlockProgressBar get_files{
            option::BarWidth{60},
            option::ForegroundColor{Color::white},
            option::FontStyles{std::vector<FontStyle>{FontStyle::bold}},
            option::MaxProgress{state->sampled_commits.size()}};

        int count = 0;
        LOG_I(state)
            << "Getting the list of files and commits to analyse ...";
        // Avoid verlap of the progress bar and the stdout logging.
        log::core::get()->flush();
        for (const auto& oid : state->sampled_commits) {
            file_tasks(params, state, oid, process_commit(oid, state));
            get_files.set_option(option::PostfixText{fmt::format(
                "{}/{}", ++count, state->sampled_commits.size())});
            get_files.tick();
        }
        get_files.mark_as_completed();
        LOG_I(state) << "Done. Total number of files: " << params.size();
    }

    int index = 0;
    for (auto& param : params) {
        param.index     = index;
        param.max_count = params.size();
        ++index;
    }

    constexpr int                         max_parallel = 32;
    std::counting_semaphore<max_parallel> counting{max_parallel};
    using clock = std::chrono::high_resolution_clock;
    auto                         start = clock::now();
    Vec<std::future<ir::FileId>> walked{};
    {

        BlockProgressBar process_files{
            option::BarWidth{60},
            option::ForegroundColor{Color::white},
            option::FontStyles{std::vector<FontStyle>{FontStyle::bold}},
            option::MaxProgress{params.size()}};

        log::core::get()->flush();
        int count = 0;
        for (const auto& param : params) {
            ++count;
            std::chrono::duration<double> diff = clock::now() - start;
            process_files.set_option(option::PostfixText{fmt::format(
                "{}/{} (avg {:1.4f}s/file)",
                count,
                params.size(),
                (diff / count).count())});

            process_files.tick();
            auto sub_task =
                [state, param, &counting, &start]() -> ir::FileId {
                finally finish{[&counting]() { counting.release(); }};
                // Walker returns optional analysis result
                auto result = exec_walker(
                    param.commit_oid,
                    state,
                    param.out_commit,
                    param.root.c_str(),
                    param.entry);

                if (!result.isNil()) {
                    std::chrono::duration<double> diff = clock::now() -
                                                         start;
                    // FIXME This sink is placed inside of the
                    // `process_filter` tick range, so increasing debugging
                    // level would invariably mess up the stdout. It might
                    // be possible to introduce a HACK via CLI
                    // configuration  - disable progres bar if stdout sink
                    // shows trace records (after all these two items
                    // perform the same task)
                    LOG_T(state) << fmt::format(
                        "FILE {:>5}/{:<5} (avg: {:1.4f}s) {:07.4f}% {} {}",
                        param.index,
                        param.max_count,
                        (diff / param.index).count(),
                        float(param.index) / param.max_count * 100.0,
                        oid_tostr(param.commit_oid),
                        param.root + tree_entry_name(param.entry));
                }

                return result;
            };

            counting.acquire();

            switch (state->config->use_threading) {
                case walker_config::async: {
                    walked.push_back(
                        std::async(std::launch::async, sub_task));
                    break;
                }
                case walker_config::defer: {
                    walked.push_back(
                        std::async(std::launch::deferred, sub_task));
                    break;
                }
                case walker_config::sequential: {
                    auto tmp = sub_task();
                    walked.push_back(std::async(
                        std::launch::deferred, [tmp]() { return tmp; }));
                    break;
                }
            }
        }


        process_files.mark_as_completed();
    }
    for (auto& future : walked) {
        future.get();
    }

    LOG_I(state) << "All commits finished";

    for (auto& param : params) {
        tree_entry_free(param.entry);
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

void store_content(walker_state* state, CR<ir::content_manager> content) {
    // Create storage connection
    auto storage = ir::create_db(state->config->db_path);
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
    if (!state->config->try_incremental) {
        LOG_I(state) << "Non-incremental update, cleaning up the database";
        storage.remove_all<ir::orm_line>();
        storage.remove_all<ir::orm_file>();
        storage.remove_all<ir::orm_commit>();
        storage.remove_all<ir::orm_lines_table>();
        storage.remove_all<ir::orm_changed_range>();
        storage.remove_all<ir::orm_dir>();
        storage.remove_all<ir::orm_author>();
        storage.remove_all<ir::orm_string>();
    } else {
        LOG_I(state) << "Incremental update, reusing the database";
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

BOOST_LOG_ATTRIBUTE_KEYWORD(
    severity,
    "Severity",
    log::trivial::severity_level)

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

Pair<char, fmt::text_style> format_style(log::severity level) {
    switch (level) {
        case log::severity::warning:
            return {'W', fmt::fg(fmt::color::yellow)};
        case log::severity::info: return {'I', fmt::fg(fmt::color::cyan)};
        case log::severity::fatal:
            return {
                'F', fmt::emphasis::bold | fmt::fg(fmt::color::magenta)};
        case log::severity::error:
            return {
                'E',
                fmt::emphasis::bold | fmt::emphasis::blink |
                    fmt::fg(fmt::color::red)};
        case log::severity::trace:
            return {'T', fmt::fg(fmt::color::white)};
        case log::severity::debug:
            return {'D', fmt::fg(fmt::color::white)};
        default: return {'?', fmt::fg(fmt::color::white)};
    }
}

void out_formatter(
    log::record_view const&  rec,
    log::formatting_ostream& strm) {
    auto [color, style] = format_style(rec[log::trivial::severity].get());
    strm << fmt::format("[{}] ", fmt::styled(color, style));
    strm << rec[log::expr::smessage];
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

auto create_std_sink() -> boost::shared_ptr<sink_t> {
    auto backend = boost::make_shared<backend_t>();
    backend->auto_flush(true);
    boost::shared_ptr<std::ostream> log_stream{&std::cout, null_deleter()};
    boost::shared_ptr<sink_t>       sink(new sink_t(
        backend,
        log::keywords::order = log ::make_attr_ordering<unsigned int>(
            "RecordID", std::less<unsigned int>())));
    sink->locked_backend()->add_stream(log_stream);
    sink->set_formatter(&out_formatter);
    return sink;
}

using namespace boost::program_options;
namespace py = boost::python;

class PyForensics {
    py::object   path_predicate;
    py::object   period_mapping;
    py::object   sample_predicate;
    SPtr<Logger> logger;

  public:
    void set_logger(SPtr<Logger> log) { logger = log; }

    void log_info(CR<Str> text) { LOG_I(logger) << text; }
    void log_warning(CR<Str> text) { LOG_W(logger) << text; }
    void log_trace(CR<Str> text) { LOG_T(logger) << text; }
    void log_debug(CR<Str> text) { LOG_D(logger) << text; }
    void log_error(CR<Str> text) { LOG_E(logger) << text; }
    void log_fatal(CR<Str> text) { LOG_F(logger) << text; }

    void set_path_predicate(py::object predicate) {
        path_predicate = predicate;
    }

    void set_period_mapping(py::object mapping) {
        period_mapping = mapping;
    }

    void set_sample_predicate(py::object predicate) {
        sample_predicate = predicate;
    }

    bool allow_path(CR<Str> path) const {
        if (path_predicate) {
            return py::extract<bool>(path_predicate(path));
        } else {
            return true;
        }
    }

    int get_period(CR<PTime> date) const {
        if (period_mapping) {
            return py::extract<int>(period_mapping(date));
        } else {
            return 0;
        }
    }

    bool allow_sample_at_date(CR<PTime> date, CR<Str> author, CR<Str> id)
        const {
        if (sample_predicate) {
            return py::extract<bool>(sample_predicate(date, author, id));
        } else {
            return true;
        }
    }
};

template <typename T>
struct type_into_python {
    static PyObject* convert(T const&);
};

template <typename T>
struct type_from_python {
    type_from_python() {
        py::converter::registry::push_back(
            convertible, construct, py::type_id<T>());
    }

    static void* convertible(PyObject* obj);

    static void construct(
        PyObject*                                      obj,
        py::converter::rvalue_from_python_stage1_data* data);
};

template <>
PyObject* type_into_python<PTime>::convert(CR<PTime> t) {
    auto d    = t.date();
    auto tod  = t.time_of_day();
    auto usec = tod.total_microseconds() % 1000000;
    return PyDateTime_FromDateAndTime(
        d.year(),
        d.month(),
        d.day(),
        tod.hours(),
        tod.minutes(),
        tod.seconds(),
        usec);
}

template <>
void* type_from_python<PTime>::convertible(PyObject* obj) {
    return PyDateTime_Check(obj) ? obj : nullptr;
}

template <>
void type_from_python<PTime>::construct(
    PyObject*                                      obj,
    py::converter::rvalue_from_python_stage1_data* data) {
    auto storage = reinterpret_cast<
                       py::converter::rvalue_from_python_storage<PTime>*>(
                       data)
                       ->storage.bytes;
    Date date_only(
        PyDateTime_GET_YEAR(obj),
        PyDateTime_GET_MONTH(obj),
        PyDateTime_GET_DAY(obj));
    TimeDuration time_of_day(
        PyDateTime_DATE_GET_HOUR(obj),
        PyDateTime_DATE_GET_MINUTE(obj),
        PyDateTime_DATE_GET_SECOND(obj));
    time_of_day += posix_time::microsec(
        PyDateTime_DATE_GET_MICROSECOND(obj));
    new (storage) PTime(date_only, time_of_day);
    data->convertible = storage;
}

template <>
PyObject* type_into_python<Date>::convert(Date const& d) {
    return PyDate_FromDate(d.year(), d.month(), d.day());
}

template <>
void* type_from_python<Date>::convertible(PyObject* obj) {
    return PyDate_Check(obj) ? obj : nullptr;
}

template <>
void type_from_python<Date>::construct(
    PyObject*                                      obj,
    py::converter::rvalue_from_python_stage1_data* data) {
    auto storage = reinterpret_cast<
                       py::converter::rvalue_from_python_storage<Date>*>(
                       data)
                       ->storage.bytes;
    new (storage) Date(
        PyDateTime_GET_YEAR(obj),
        PyDateTime_GET_MONTH(obj),
        PyDateTime_GET_DAY(obj));
    data->convertible = storage;
}

BOOST_PYTHON_MODULE(forensics) {
    PyDateTime_IMPORT;

    py::to_python_converter<PTime, type_into_python<PTime>>();
    type_from_python<PTime>();

    py::to_python_converter<Date, type_into_python<Date>>();
    type_from_python<Date>();

    py::object class_creator =
        //
        py::class_<PyForensics>("Forensics") //
            .def("log_info", &PyForensics::log_info, py::args("text"))
            .def(
                "log_warning", &PyForensics::log_warning, py::args("text"))
            .def("log_trace", &PyForensics::log_trace, py::args("text"))
            .def("log_debug", &PyForensics::log_debug, py::args("text"))
            .def("log_error", &PyForensics::log_error, py::args("text"))
            .def("log_fatal", &PyForensics::log_fatal, py::args("text"))
            .def(
                "set_path_predicate",
                &PyForensics::set_path_predicate,
                py::args("predicate"))
            .def(
                "set_period_mapping",
                &PyForensics::set_period_mapping,
                py::args("mapping"))
            .def(
                "set_sample_predicate",
                &PyForensics::set_sample_predicate,
                py::args("predicate"));

    py::object module_level_object = class_creator();
    py::scope().attr("config")     = module_level_object;
}


auto parse_cmdline(int argc, const char** argv) -> variables_map {
    variables_map                  vm;
    options_description            desc{"Options"};
    positional_options_description pos{};

    desc.add_options()
        //
        ("help,h", "Help screen") //
        ("logfile",
         value<Str>()->default_value("/tmp/git_user.log"),
         "Log file location") //
        ("branch",
         value<Str>()->default_value("master"),
         "Repository branch to analyse") //
        ("incremental",
         "Load previosly created database and only process commits that "
         "were not registered previously") //
        ("outfile",
         value<Str>(),
         "Output file location. If not supplied output will be "
         "generated based on the input repo location")(
            "allowed",
            value<Vec<Str>>(),
            "List of globs for allowed paths") //
        ("config",
         value<Vec<Str>>(),
         "Config file where options may be specified (can be specified "
         "more than once)") //
        ("blame-subprocess",
         bool_switch()->default_value(true),
         "Use blame for subprocess")                    //
        ("repo", value<Str>(), "Input repository path") //
        ("filter-script",
         value<Str>(),
         "User-provided python script that configures code forensics "
         "filter")
        //
        ;

    pos.add("repo", 1);

    try {
        store(
            command_line_parser(argc, argv)
                .options(desc)
                .positional(pos)
                .run(),
            vm);

        if (vm.count("help")) {
            std::cout << desc << "\n";
            exit(0);
        }

        if (vm.count("config") > 0) {
            for (const auto& config : vm["config"].as<Vec<Str>>()) {
                std::ifstream ifs{config};

                if (ifs.fail()) {
                    std::cerr << "Error opening config file: " << config
                              << std::endl;
                    exit(1);
                }

                store(parse_config_file(ifs, desc), vm);
            }
        }

        notify(vm);

    } catch (const error& ex) {
        std::cerr << ex.what() << "\n";
        exit(1);
    }


    return vm;
}

void init_logger_properties() {
    // Add some attributes too
    log::core::get()->add_global_attribute(
        "TimeStamp", log::attrs::local_clock());
    log::core::get()->add_global_attribute(
        "RecordID", log::attrs::counter<unsigned int>());

    log::core::get()->add_global_attribute("File", MutLog<Str>(""));
    log::core::get()->add_global_attribute("Func", MutLog<Str>(""));
    log::core::get()->add_global_attribute("Line", MutLog<int>(0));
}

/// Parses the value of the active python exception
/// NOTE SHOULD NOT BE CALLED IF NO EXCEPTION
std::string parse_python_exception() {
    PyObject *type_ptr = NULL, *value_ptr = NULL, *traceback_ptr = NULL;
    // Fetch the exception info from the Python C API
    PyErr_Fetch(&type_ptr, &value_ptr, &traceback_ptr);

    // Fallback error
    std::string ret("Unfetchable Python error");
    // If the fetch got a type pointer, parse the type into the exception
    // string
    if (type_ptr != NULL) {
        py::handle<> h_type(type_ptr);
        py::str      type_pstr(h_type);
        // Extract the string from the boost::python object
        py::extract<std::string> e_type_pstr(type_pstr);
        // If a valid string extraction is available, use it
        //  otherwise use fallback
        if (e_type_pstr.check()) {
            ret = e_type_pstr();
        } else {
            ret = "Unknown exception type";
        }
    }
    // Do the same for the exception value (the stringification of the
    // exception)
    if (value_ptr != NULL) {
        py::handle<>             h_val(value_ptr);
        py::str                  a(h_val);
        py::extract<std::string> returned(a);
        if (returned.check()) {
            ret += ": " + returned();
        } else {
            ret += std::string(": Unparseable Python error: ");
        }
    }
    // Parse lines from the traceback using the Python traceback module
    if (traceback_ptr != NULL) {
        py::handle<> h_tb(traceback_ptr);
        // Load the traceback module and the format_tb function
        py::object tb(py::import("traceback"));
        py::object fmt_tb(tb.attr("format_tb"));
        // Call format_tb to get a list of traceback strings
        py::object tb_list(fmt_tb(h_tb));
        // Join the traceback strings into a single string
        py::object tb_str(py::str("\n").join(tb_list));
        // Extract the string, check the extraction, and fallback in
        // necessary
        py::extract<std::string> returned(tb_str);
        if (returned.check()) {
            ret += ": " + returned();
        } else {
            ret += std::string(": Unparseable Python traceback");
        }
    }
    return ret;
}

auto main(int argc, const char** argv) -> int {
    auto vm        = parse_cmdline(argc, argv);
    auto file_sink = create_file_sink(vm["logfile"].as<Str>());
    auto out_sink  = create_std_sink();

    out_sink->set_filter(severity >= log::severity::info);

    finally close_out_sink{[&out_sink]() {
        out_sink->stop();
        out_sink->flush();
    }};

    finally close_file_sink{[&file_sink]() {
        file_sink->stop();
        file_sink->flush();
    }};

    auto logger = std::make_shared<Logger>();

    Str in_repo    = vm["repo"].as<Str>();
    Str in_branch  = vm["branch"].as<Str>();
    Str in_outfile = vm.count("outfile") ? vm["outfile"].as<Str>()
                                         : "/tmp/db.sqlite";

    log::core::get()->add_sink(file_sink);
    log::core::get()->add_sink(out_sink);
    init_logger_properties();

    const bool use_fusion = false;

    auto in_blame_subprocess = vm["blame-subprocess"].as<bool>();
    LOG_I(logger) << fmt::format(
        "Use blame subprocess for file analysis: {}", in_blame_subprocess);

    auto in_script = vm.count("filter-script")
                         ? vm["filter-script"].as<Str>()
                         : "";

    PyForensics* forensics;
    // Register user-defined module in python - this would allow importing
    // `forensics` module in the C++ side of the application
    PyImport_AppendInittab("forensics", &PyInit_forensics);
    // Initialize main python library part
    Py_Initialize();
    if (!in_script.empty()) {
        LOG_I(logger) << "User-defined filter configuration was provided, "
                         "evaluating "
                      << in_script;
        Path path{in_script};
        if (fs::exists(path)) {
            py::object main_module = py::import("__main__");
            py::object name_space  = main_module.attr("__dict__");

            try {
                // Get the configuration module object
                py::object forensics_module = py::import("forensics");
                // Retrieve config pointer object from it
                py::object config_object = forensics_module.attr(
                    "__dict__")["config"];
                // Extract everything to a pointer
                forensics = py::extract<PyForensics*>(config_object);
                if (forensics == nullptr) {
                    LOG_F(logger) << "Could not extract pointer for "
                                     "forensics configuration object";
                    return 1;
                }

                forensics->set_logger(logger);

                auto abs = fs::absolute(path);
                LOG_T(logger) << "Python file, executing as expected, "
                                 "absolute path is "
                              << abs.c_str();


                auto result = py::exec_file(
                    py::str(abs.c_str()), name_space, name_space);


                LOG_D(logger) << "Execution of the configuration script "
                                 "was successfull";

            } catch (py::error_already_set& err) {

#define LOG_PY_ERROR(logger)                                              \
    {                                                                     \
        auto exception = parse_python_exception();                        \
        LOG_E(logger) << "Error during python code execution";            \
        LOG_E(logger) << exception;                                       \
    }

                LOG_PY_ERROR(logger);

                return 1;
            }

        } else {
            LOG_E(logger)
                << "User configuration file script does not exist "
                << path.native() << " no such file or directory";
            return 1;
        }
    }

    // Provide implementation callback strategies
    auto config = UPtr<walker_config>(new walker_config{
        .use_subprocess = in_blame_subprocess,
        // Full process parallelization
        .use_threading   = walker_config::async,
        .repo            = in_repo,
        .heads           = fmt::format("/.git/refs/heads/{}", in_branch),
        .db_path         = in_outfile,
        .try_incremental = 0 < vm.count("incremental"),
        .allow_path      = [&logger, forensics](CR<Str> path) -> bool {
            try {
                return forensics->allow_path(path);
            } catch (py::error_already_set& err) {
                LOG_PY_ERROR(logger);
                return false;
            }
        },
        .get_period = [&logger, forensics](CR<PTime> date) -> int {
            try {
                return forensics->get_period(date);
            } catch (py::error_already_set& err) {
                LOG_PY_ERROR(logger);
                return 0;
            }
        },
        .allow_sample =
            [&logger, forensics](
                CR<PTime> date, CR<Str> author, CR<Str> id) -> bool {
            try {
                return forensics->allow_sample_at_date(date, author, id);
            } catch (py::error_already_set& err) {
                LOG_PY_ERROR(logger);
                return false;
            }
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
        .logger  = logger});

    if (config->try_incremental) {
        if (std::filesystem::exists(config->db_path)) {
            load_content(config.get(), content);
        } else {
            LOG_W(state) << "cannot load incremental from"
                         << config->db_path;
        }
    }

    git_oid oid;
    // Initialize state of the commit walker
    open_walker(oid, *state);
    // Store finalized commit IDs from executed tasks
    Vec<ir::CommitId> commits{};
    auto              result = launch_analysis(oid, state.get());
    for (auto& commit : result) {
        commits.push_back(commit);
    }

    LOG_I(state) << "Finished analysis, writing database";
    store_content(state.get(), content);

    LOG_I(state) << "Finished execution, DB written successfully";

    Py_Finalize();
    return 0;
}
