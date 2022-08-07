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

#include <matplot/matplot.h>

#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

using namespace boost;

namespace git {
struct exception : public std::exception {
    std::string message;
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
    const git_tree*                                        tree,
    git_treewalk_mode                                      mode,
    std::function<int(const char*, const git_tree_entry*)> callback) {
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
    int filecount; /// Number of files processed for the commit analysis
    gregorian::date check_date; /// Date when commit was made
    /// Per-year line count registered in the blame
    std::map<int, std::pair<std::optional<gregorian::date>, int>>
        per_period;
};

template <typename T>
using Func = std::function<T>;

struct walker_config {
    Func<bool(const std::string&)>     allow_path;
    Func<int(const gregorian::date&)>  get_period;
    Func<bool(const gregorian::date&)> check_date;
};

int exec_walker(
    git_repository*         repo,
    git_oid                 oid,
    commit_analysis_result& stats,
    const char*             root,
    const git_tree_entry*   entry,
    const walker_config&    config) {
    // We are looking for blobs
    if (tree_entry_type(entry) != GIT_OBJECT_BLOB) { return GIT_OK; }
    // get entry name relative to `root`
    std::string path{tree_entry_name(entry)};
    // Create full relative path for the target object
    auto relpath = std::string{root + path};
    // Check for provided predicate
    if (config.allow_path && !config.allow_path(relpath)) {
        std::cout << "    SKIP " << relpath << "\n";
        return GIT_OK;
    } else {
        std::cout << "    EXEC " << relpath << "\n";
    }

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
            gregorian::date date = posix_time::from_time_t(
                                       hunk->final_signature->when.time)
                                       .date();
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

        // Advance over raw data
        i = (int)(eol - rawdata + 1);
        // Increment line
        line++;
    }

    // Blame information is no longer needed
    blame_free(blame);
    // Increase file count (for statistics)
    ++stats.filecount;
    return GIT_OK;
}

commit_analysis_result process_commit(
    git_repository*      repo,
    git_oid              oid,
    const walker_config& config) {
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
#define GIT_SUCCESS 0

int main() {
    libgit2_init();
    // Check whether threads are enabled
    assert(libgit2_features() & GIT_FEATURE_THREADS);
    // Open directory from the provided path - for now hardcoded
    git_repository* repo = repository_open_ext(REPO, 0, nullptr);

    // Read HEAD on master
    char head_filepath[512];

    // REFACTOR this part was copied from the SO example and I'm pretty
    // sure it can be implemented in a cleaner manner, but I haven't
    // touched this part yet.
    FILE* head_fileptr;
    char  head_rev[41];

    strcpy(head_filepath, REPO);
    strcat(head_filepath, "/.git/refs/heads/master");

    if ((head_fileptr = fopen(head_filepath, "r")) == NULL) {
        fprintf(stderr, "Error opening '%s'\n", head_filepath);
        return 1;
    }

    if (fread(head_rev, 40, 1, head_fileptr) != 1) {
        fprintf(stderr, "Error reading from '%s'\n", head_filepath);
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
    std::vector<commit_analysis_result> processed;


    // Analyse commits starting from this date
    gregorian::date start = {2007, 1, 1};
    // Configuration for date period analysis
    const int days_period = 360;
    // Main confugration for walker checks
    walker_config config{
        .allow_path = [](const std::string& path) -> bool {
            return path.ends_with(".nim") /*&&
                   path.find("compiler/") != std::string::npos*/
                ;
        },
        .get_period = [&](const gregorian::date& date) -> int {
            // Get period number using simple integer division - 20 days
            // from now will be a period 1, 40 days will be period 2 and
            // so on.
            return (date - start).days() / days_period;
        },
        .check_date = [&](const gregorian::date& date) -> bool {
            // If date is exactly divisible by target period, process it
            return (date - start).days() % days_period == 0;
        }};


    // Walk over every commit in the history
    int prev_period = -1;
    while (revwalk_next(&oid, walker) == GIT_SUCCESS) {
        // Get commit from the provided oid
        git_commit* commit = commit_lookup(repo, &oid);
        // Convert from unix timestamp used by git to humane format
        gregorian::date date = posix_time::from_time_t(commit_time(commit))
                                   .date();

        // commit is no longer needed in this scope
        commit_free(commit);
        // check if we can process it
        if (config.check_date(date) &&
            config.get_period(date) != prev_period) {
            std::cout << "EXEC " << date << "\n";
            // and store analysis results if we can
            processed.push_back(process_commit(repo, oid, config));
            std::cout << "DONE " << date << "\n";

            // HACK ignore multiple commits that happened on the same day
            prev_period = config.get_period(date);
        }
    }


    // Walker and repository are no longer necessary
    revwalk_free(walker);
    repository_free(repo);


    // Initial data retrival
    std::map<int, std::pair<gregorian::date, std::vector<int>>>
        period_burndown;
    for (auto& res : processed) {
        for (auto& [period, stats] : res.per_period) {
            period_burndown[period].second.push_back(stats.second);
            period_burndown[period].first = stats.first.value();
        }
    }


    // Find out how many data points we had, reverse commits (they were
    // iterated from the last to first, we need from first to last)
    int sample_points = 0;
    for (auto& [period, lines] : period_burndown) {
        sample_points = std::max<int>(sample_points, lines.second.size());
        std::reverse(lines.second.begin(), lines.second.end());
        fmt::print("period {} lines {}\n", period, lines.second);
    }

    // matplot data
    std::vector<std::vector<double>> Y;
    // legend annotation information
    std::vector<std::string> legend;
    for (auto& [period, stats] : period_burndown) {
        const auto& date  = stats.first;
        auto        lines = stats.second;
        // for proper stacked barplot I need to equalize sizes of all data
        // series, padding information about misssing dates with zeroes.
        lines.insert(lines.begin(), sample_points - lines.size(), 0);
        // store current period time into legend annotations
        legend.push_back(gregorian::to_iso_extended_string(date));
        Y.push_back(std::vector<double>(lines.size()));

        for (auto count : lines) {
            Y.back().push_back(count);
        }
    }

    if (!legend.empty()) {
        matplot::barstacked(Y);
        auto plot_legend = matplot::legend(legend);
        plot_legend->location(matplot::legend::general_alignment::topleft);
        matplot::save("/tmp/commit-count.png");
    }

    return 0;
}
