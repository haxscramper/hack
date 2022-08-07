#include "../common.hpp"

#include <exception>
#include <string>
#include <git2.h>
#include <fmt/core.h>
#include <fmt/ranges.h>
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

struct analysis {
    int  filecount;  /// Number of files processed for the commit analysis
    Date check_date; /// Date when commit was made
    /// Per-year line count registered in the blame
    struct period_description {
        Opt<Date> start;
        Opt<Date> end;
        int       line_count;

        period_description operator+(CR<period_description> other) {
            period_description res = *this;
            if (!res.end || res.end < other.end) { res.end = other.end; }

            if (!res.start || other.start < res.start) {
                res.start = other.start;
            }

            res.line_count += other.line_count;
            return res;
        }
    };
    std::map<int, period_description> per_period;


    analysis operator+(CR<analysis> other) {
        analysis res = *this;
        for (auto& [period, stats] : other.per_period) {
            res.per_period[period] = res.per_period[period] + stats;
        }

        return res;
    }
};

struct walker_config {
    Func<bool(CR<Str>)>     allow_path;
    Func<int(const Date&)>  get_period;
    Func<bool(const Date&)> check_date;
    bool                    use_subprocess = true;
    Str                     project_root;
};

using TimePoint = stime::time_point<stime::system_clock>;

/// Mutable state passed around walker configurations
struct walker_state {
    git_repository*             repo;
    std::counting_semaphore<16> semaphore{16};

    std::map<Str, TimePoint> bench_points;

    void push_bench_point(CR<Str> name) {
        bench_points[name] = stime::system_clock::now();
    }

    stime::milliseconds pop_bench_point(CR<Str> name) {
        return stime::duration_cast<stime::milliseconds>(
            stime::system_clock::now() - bench_points[name]);
    }
};

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

void visited_line(
    analysis&         stats,
    CP<walker_config> config,
    CR<Date>          date) {
    // get period index and used it to update the analysis
    // information
    int period = config->get_period(date);

    auto& per = stats.per_period[period];

    ++per.line_count;
    // if this is a first time we've seen this period, store the
    // current 'date' for latter plotting.
    if (!per.end || per.end < date) { per.end = date; }
    if (!per.start || date < per.start) { per.start = date; }
}


void stats_via_subprocess(
    git_oid           oid,
    analysis&         stats,
    CP<walker_config> config,
    CR<Str>           relpath) {

    Str str_oid{oid_tostr(oid)};

    Vec<Str> args{process::search_path("git").string(),
                  "blame",
                  "--line-porcelain",
                  str_oid,
                  "--",
                  relpath};

    process::ipstream out;
    process::child    blame{args,
                         process::std_out > out,
                         process::start_dir(config->project_root)};

    Str line;
    // --line-porcelain generates chunks with twelve consecutive elements -
    // I'm only interested in the AuthorTime, everything else can be
    // skipped for now. Code below implements a simple state machine with
    // states encoded in the `LK` enum
    enum LK
    {
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

    LK state = LK::Commit;
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


        switch (state) {
            case LK::AuthorTime: {
                std::stringstream is{line};
                Str               time;
                is >> time;
                assert(time == "author-time");
                is >> time;
                visited_line(
                    stats,
                    config,
                    posix_time::from_time_t(std::stol(time)).date());
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
}

void stats_via_libgit(
    walker_state*         state,
    git_oid               oid,
    analysis&             stats,
    const git_tree_entry* entry,
    CP<walker_config>     config,
    CR<Str>               relpath) {

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
            visited_line(
                stats,
                config,
                posix_time::from_time_t(hunk->final_signature->when.time)
                    .date());
        }

        // Advance over raw data
        i = (int)(eol - rawdata + 1);
        // Increment line
        line++;
    }

    // Blame information is no longer needed
    blame_free(blame);
}

Opt<analysis> exec_walker(
    walker_state*         state,
    git_oid               oid,
    const char*           root,
    const git_tree_entry* entry,
    CP<walker_config>     config) {
    analysis stats;
    // We are looking for blobs
    if (tree_entry_type(entry) != GIT_OBJECT_BLOB) {
        return Opt<analysis>{};
    }
    // get entry name relative to `root`
    Str path{tree_entry_name(entry)};
    // Create full relative path for the target object
    auto relpath = Str{root + path};
    // Check for provided predicate
    if (config->allow_path && !config->allow_path(relpath)) {
        return Opt<analysis>{};
    }

    if (config->use_subprocess) {
        stats_via_subprocess(oid, stats, config, relpath);
    } else {
        stats_via_libgit(state, oid, stats, entry, config, relpath);
    }

    static std::mutex done;
    static int        total_done;
    {
        std::unique_lock print_lock{done};
        fmt::print(
            "DONE ({:<5}) {} {}\n",
            total_done++,
            stats.check_date,
            oid_tostr(oid),
            relpath);
    }
    // Increase file count (for statistics)
    ++stats.filecount;
    return stats;
}

std::future<analysis> process_commit(
    walker_state*     state,
    git_oid           oid,
    CP<walker_config> config) {
    // Create task closure - NOTE maybe at some point git won't break with
    // threads and I could implement parallel processing.
    auto task = [oid, state, config]() {
        git_commit* commit = commit_lookup(state->repo, &oid);
        // Get tree for a commit
        auto tree = commit_tree(commit);
        // Create object with c

        Vec<std::future<Opt<analysis>>> sub_futures;

        analysis stats{
            .check_date = posix_time::from_time_t(commit_time(commit))
                              .date()};

        // walk all entries in the tree
        tree_walk(
            tree,
            // order is not particularly important, doing preorder
            // traversal here
            GIT_TREEWALK_PRE,
            [state, oid, config, &sub_futures](
                const char* root, const git_tree_entry* entry) {
                git_tree_entry* user_dup = tree_entry_dup(entry);
                SPtr<Str>       user_str = std::make_shared<Str>(root);
                auto            sub_task =
                    [&, user_str, user_dup]() -> Opt<analysis> {
                    state->semaphore.acquire();
                    auto result = exec_walker(
                        state, oid, user_str->c_str(), user_dup, config);
                    state->semaphore.release();

                    tree_entry_free(user_dup);

                    return result;
                };

                sub_futures.push_back(std::async(sub_task));
                return GIT_OK;
            });


        for (auto& future : sub_futures) {
            auto res = future.get();
            if (res) { stats = stats + res.value(); }
        }

        // commit information is no longer needed
        commit_free(commit);
        return stats;
    };

    return std::async(std::launch::async, task);
}


#define GIT_SUCCESS 0

struct analysis_config {
    Str repo  = "/tmp/nimskull";
    Str heads = "/.git/refs/heads/devel";
};

int main() {
    analysis_config main_conf;

    libgit2_init();
    // Check whether threads can be enabled
    assert(libgit2_features() & GIT_FEATURE_THREADS);

    auto state = UPtr<walker_state>(new walker_state{
        // Open directory from the provided path - for now hardcoded
        .repo = repository_open_ext(main_conf.repo.c_str(), 0, nullptr)});

    // Read HEAD on master
    Str head_filepath{main_conf.repo + main_conf.heads};

    // REFACTOR this part was copied from the SO example and I'm pretty
    // sure it can be implemented in a cleaner manner, but I haven't
    // touched this part yet.
    FILE* head_fileptr;
    char  head_rev[41];

    if ((head_fileptr = fopen(head_filepath.c_str(), "r")) == NULL) {
        std::cerr << "Error opening " << head_filepath << "\n";
        return 1;
    }

    if (fread(head_rev, 40, 1, head_fileptr) != 1) {
        std::cerr << "Error reading from " << head_filepath << "\n";
        fclose(head_fileptr);
        return 1;
    }

    fclose(head_fileptr);


    git_oid oid = oid_fromstr(head_rev);
    // Initialize revision walker
    git_revwalk* walker = revwalk_new(state->repo);
    // Iterate all commits in the topological order
    revwalk_sorting(walker, GIT_SORT_TOPOLOGICAL);
    revwalk_push(walker, &oid);

    // All constructed information
    Vec<std::future<analysis>> processed;

    // Configure state of the sampling strategies
    allow_state allow{.days_period = 90, .start = {2020, 1, 1}};

    // Provide implementation callback strategies
    auto config = UPtr<walker_config>(new walker_config{
        //
        .allow_path = [](CR<Str> path) -> bool {
            return path.ends_with(".nim") &&
                   path.find("compiler/") != Str::npos;
        },
        .get_period = [&](const Date& date) -> int {
            return allow.year_to_period(date);
        },
        .check_date = [&](const Date& date) -> bool {
            if (allow.allow_once_per_year(date)) {
                static int max = 0;
                auto       res = ++max < 2;
                if (!res) {
                    std::cout << "SKIP " << max << "\n";
                } else {
                    std::cout << "RUN " << max << "\n";
                };
                return res;
            } else {
                return false;
            }
        },
        .use_subprocess = true,
        .project_root   = main_conf.repo});

    state->push_bench_point("total");

    // Walk over every commit in the history
    int prev_period = -1;
    while (revwalk_next(&oid, walker) == GIT_SUCCESS) {
        // Get commit from the provided oid
        git_commit* commit = commit_lookup(state->repo, &oid);
        // Convert from unix timestamp used by git to humane format
        Date date = posix_time::from_time_t(commit_time(commit)).date();

        // commit is no longer needed in this scope
        commit_free(commit);
        // check if we can process it
        if (config->check_date(date)) {
            // and store analysis results if we can
            processed.push_back(
                process_commit(state.get(), oid, config.get()));
        }
    }

    // Initial data retrival
    struct period_spec {
        Date     start;
        Date     end;
        Vec<int> lines;
    };
    std::map<int, period_spec> period_burndown;
    Vec<Date>                  sample_points;
    for (auto& future : processed) {
        auto res = future.get();
        sample_points.push_back(res.check_date);
        for (auto& [period, stats] : res.per_period) {
            period_burndown[period].lines.push_back(stats.line_count);
            period_burndown[period].start = stats.start.value();
            period_burndown[period].end   = stats.end.value();
        }
    }

    auto total = state->pop_bench_point("total");

    std::cout << "total time was "
              << stime::duration_cast<stime::seconds>(total).count()
              << " seconds\n";

    // Walker and repository are no longer necessary
    revwalk_free(walker);
    repository_free(state->repo);

    // Reverse commits (they were
    // iterated from the last to first, we need from first to last)
    for (auto& [period, spec] : period_burndown) {
        std::reverse(spec.lines.begin(), spec.lines.end());
    }

    std::reverse(sample_points.begin(), sample_points.end());

    fmt::print(
        "{:>32} {}\n", "sample taken", fmt::join(sample_points, " "));
    for (auto& [period, stats] : period_burndown) {
        auto lines = stats.lines;
        // for proper stacked barplot I need to equalize sizes of all data
        // series, padding information about misssing dates with zeroes.
        lines.insert(
            lines.begin(), sample_points.size() - lines.size(), 0);
        fmt::print(
            "({:^4}) [{}]-[{}] {:<10}\n",
            period,
            stats.start,
            stats.end,
            fmt::join(lines, " "));
        // store current period time into legend annotations
    }

    return 0;
}
