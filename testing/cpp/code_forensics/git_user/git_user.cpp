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

#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/process.hpp>

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


void tree_walk(
    const git_tree*                               tree,
    git_treewalk_mode                             mode,
    Func<int(const char*, const git_tree_entry*)> callback) {
    git::tree_walk(
        tree,
        mode,
        [](const char*           root,
           const git_tree_entry* entry,
           void*                 payload) -> int {
            auto impl = static_cast<decltype(callback)*>(payload);
            return (*impl)(root, entry);
        },
        &callback);
}

struct commit_analysis_result {
    int  filecount;  /// Number of files processed for the commit analysis
    Date check_date; /// Date when commit was made
    /// Per-year line count registered in the blame
    std::map<int, std::pair<Opt<Date>, int>> per_period;
};

struct walker_config {
    Func<bool(CR<Str>)>     allow_path;
    Func<int(const Date&)>  get_period;
    Func<bool(const Date&)> check_date;
    bool                    use_subprocess = true;
    Str                     project_root;
};

Str oid_tostr(git_oid oid) {
    char result[GIT_OID_HEXSZ + 1];
    git_oid_tostr(result, sizeof(result), &oid);
    return result;
}

void visited_line(
    commit_analysis_result& stats,
    CR<walker_config>       config,
    CR<Date>                date) {
    // get period index and used it to update the analysis
    // information
    int period = config.get_period(date);

    ++stats.per_period[period].second;
    // if this is a first time we've seen this period, store the
    // current 'date' for latter plotting.
    if (!stats.per_period[period].first) {
        stats.per_period[period].first = date;
    }
}


void stats_via_subprocess(
    git_oid                 oid,
    commit_analysis_result& stats,
    CR<walker_config>       config,
    CR<Str>                 relpath) {

    Vec<Str> args{process::search_path("git").string(),
                  "blame",
                  "--line-porcelain",
                  oid_tostr(oid),
                  "--",
                  relpath};

    process::ipstream out;
    process::child    blame{args,
                         process::std_out > out,
                         process::start_dir(config.project_root)};

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
        Filename      = 11,
        Content       = 12
    };

    LK state = LK::Commit;
    while (blame.running() && std::getline(out, line) && !line.empty()) {
        // even for 'machine reading' output is not consistent - some parts
        // are optional and can be missing in the output, requiring extra
        // hacks for processing.
        if (state == LK::Previous && !line.starts_with("previous")) {
            state = LK::Filename;
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
    git_repository*         repo,
    git_oid                 oid,
    commit_analysis_result& stats,
    const git_tree_entry*   entry,
    CR<walker_config>       config,
    CR<Str>                 relpath) {
    // Init default blame creation options
    git_blame_options blameopts = GIT_BLAME_OPTIONS_INIT;
    // We are only interested in blame information up until target commit
    blameopts.newest_commit = oid;
    // Extract git blob object
    git_object* object = tree_entry_to_object(repo, entry);
    // get blame information
    git_blame* blame = blame_file(repo, relpath.c_str(), &blameopts);
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

int exec_walker(
    git_repository*         repo,
    git_oid                 oid,
    commit_analysis_result& stats,
    const char*             root,
    const git_tree_entry*   entry,
    CR<walker_config>       config) {
    // We are looking for blobs
    if (tree_entry_type(entry) != GIT_OBJECT_BLOB) { return GIT_OK; }
    // get entry name relative to `root`
    Str path{tree_entry_name(entry)};
    // Create full relative path for the target object
    auto relpath = Str{root + path};
    // Check for provided predicate
    if (config.allow_path && !config.allow_path(relpath)) {
        return GIT_OK;
    }

    if (config.use_subprocess) {
        stats_via_subprocess(oid, stats, config, relpath);
    } else {
        stats_via_libgit(repo, oid, stats, entry, config, relpath);
    }

    // Increase file count (for statistics)
    ++stats.filecount;
    return GIT_OK;
}

commit_analysis_result process_commit(
    git_repository*   repo,
    git_oid           oid,
    CR<walker_config> config) {
    // Create task closure - NOTE maybe at some point git won't break with
    // threads and I could implement parallel processing.
    auto task = [oid, repo, &config]() {
        git_commit* commit = commit_lookup(repo, &oid);

        // Get tree for a commit
        auto tree = commit_tree(commit);
        // Create object with c
        commit_analysis_result stats{
            .check_date = posix_time::from_time_t(commit_time(commit))
                              .date()};
        // walk all entries in the tree
        tree_walk(
            tree,
            // order is not particularly important, doing preorder
            // traverrsal here
            GIT_TREEWALK_PRE,
            [repo, oid, &stats, &config](
                const char* root, const git_tree_entry* entry) {
                return exec_walker(repo, oid, stats, root, entry, config);
            });

        // commit information is no longer needed
        commit_free(commit);
        return stats;
    };

    return task();
}


#define REPO "/tmp/fusion"
#define HEADS "/.git/refs/heads/master"
#define GIT_SUCCESS 0

int main() {
    libgit2_init();
    // Check whether threads are enabled
    assert(libgit2_features() & GIT_FEATURE_THREADS);
    // Open directory from the provided path - for now hardcoded
    git_repository* repo = repository_open_ext(REPO, 0, nullptr);

    // Read HEAD on master
    Str head_filepath{REPO HEADS};

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
    git_revwalk* walker = revwalk_new(repo);
    // Iterate all commits in the topological order
    revwalk_sorting(walker, GIT_SORT_TOPOLOGICAL);
    revwalk_push(walker, &oid);

    // All constructed information
    Vec<commit_analysis_result> processed;

    // Analyse commits starting from this date
    Date start = {2020, 1, 1};
    // Configuration for date period analysis
    const int days_period = 120;
    // Main confugration for walker checks
    walker_config config{.allow_path = [](CR<Str> path) -> bool {
                             return path.ends_with(".nim");
                         },
                         .get_period = [&](const Date& date) -> int {
                             // Get period number using simple integer
                             // division - 20 days from now will be a
                             // period 1, 40 days will be period 2 and so
                             // on.
                             return (date - start).days() / days_period;
                         },
                         .check_date = [&](const Date& date) -> bool {
                             // If date is exactly divisible by target
                             // period, process it
                             return (date - start).days() % days_period ==
                                    0;
                         },
                         .use_subprocess = true,
                         .project_root   = REPO};


    // Walk over every commit in the history
    int prev_period = -1;
    while (revwalk_next(&oid, walker) == GIT_SUCCESS) {
        // Get commit from the provided oid
        git_commit* commit = commit_lookup(repo, &oid);
        // Convert from unix timestamp used by git to humane format
        Date date = posix_time::from_time_t(commit_time(commit)).date();

        // commit is no longer needed in this scope
        commit_free(commit);
        // check if we can process it
        if (config.check_date(date) &&
            config.get_period(date) != prev_period) {
            // and store analysis results if we can
            processed.push_back(process_commit(repo, oid, config));
            // HACK ignore multiple commits that happened on the same day
            prev_period = config.get_period(date);
        }
    }


    // Walker and repository are no longer necessary
    revwalk_free(walker);
    repository_free(repo);

    // Initial data retrival
    std::map<int, std::pair<Date, Vec<int>>> period_burndown;
    Vec<Date>                                sample_points;
    for (auto& res : processed) {
        sample_points.push_back(res.check_date);
        for (auto& [period, stats] : res.per_period) {
            period_burndown[period].second.push_back(stats.second);
            period_burndown[period].first = stats.first.value();
        }
    }

    // Reverse commits (they were
    // iterated from the last to first, we need from first to last)
    for (auto& [period, lines] : period_burndown) {
        std::reverse(lines.second.begin(), lines.second.end());
    }
    std::reverse(sample_points.begin(), sample_points.end());

    // matplot data
    Vec<Vec<double>> Y;
    // legend annotation information
    Vec<Str> legend;
    fmt::print(
        "{:<28}{}\n", "sample taken", fmt::join(sample_points, " "));
    for (auto& [period, stats] : period_burndown) {
        const auto& date  = stats.first;
        auto        lines = stats.second;
        // for proper stacked barplot I need to equalize sizes of all data
        // series, padding information about misssing dates with zeroes.
        lines.insert(
            lines.begin(), sample_points.size() - lines.size(), 0);
        fmt::print(
            "written in {} lines {:<10}\n", date, fmt::join(lines, " "));
        // store current period time into legend annotations
        legend.push_back(gregorian::to_iso_extended_string(date));
        Y.push_back(Vec<double>(lines.size()));

        for (auto count : lines) {
            Y.back().push_back(count);
        }
    }

    return 0;
}
