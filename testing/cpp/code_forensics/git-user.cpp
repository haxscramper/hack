#include <exception>
#include <string>
#include <git2.h>
#include <fmt/core.h>
#include <fmt/ranges.h>
#include <map>
#include <fstream>
#include <thread>
#include <mutex>
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
    // Check for provided predicate
    if (config.allow_path && !config.allow_path(path)) { return GIT_OK; }

    git_blame_options blameopts = GIT_BLAME_OPTIONS_INIT;
    blameopts.newest_commit     = oid;

    auto object = tree_entry_to_object(repo, entry);

    assert(object_type(object) == GIT_OBJECT_BLOB);

    auto relpath = std::string{root + path};
    auto blame   = blame_file(repo, relpath.c_str(), &blameopts);
    auto blob    = (git_blob*)object;
    int  i       = 0;
    int  line    = 1;

    bool              break_on_null_hunk = false;
    git_object_size_t rawsize            = blob_rawsize(blob);
    const char*       rawdata = (const char*)git_blob_rawcontent(blob);

    while (i < rawsize) {
        const char* eol = (const char*)memchr(
            rawdata + i, '\n', (size_t)(rawsize - i));
        char                  oid[10] = {0};
        const git_blame_hunk* hunk    = blame_get_hunk_byline(blame, line);

        if (break_on_null_hunk && !hunk) { break; }

        if (hunk != nullptr && hunk->final_signature != nullptr) {

            break_on_null_hunk   = true;
            gregorian::date date = posix_time::from_time_t(
                                       hunk->final_signature->when.time)
                                       .date();
            int period = config.get_period(date);

            ++stats.per_period[period].second;
            if (!stats.per_period[period].first) {
                stats.per_period[period].first = date;
            }

            //            std::cout << period << " [" << date << "] ";
            //            printf("%.*s\n", (int)(eol - rawdata - i),
            //            rawdata + i);
        }

        i = (int)(eol - rawdata + 1);
        line++;
    }

    blame_free(blame);

    ++stats.filecount;
    return GIT_OK;
}

commit_analysis_result process_commit(
    git_repository*      repo,
    git_oid              oid,
    const walker_config& config) {
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

        commit_free(commit);
        return stats;
    };

    return task();
}


#define REPO "/tmp/fusion"
#define GIT_SUCCESS 0

// git_blame* blame_file(git_repository*repo, const std::string& path,
// git_blame_options options) {

// }

int main() {
    libgit2_init();
    assert(libgit2_features() & GIT_FEATURE_THREADS);
    git_repository* repo = repository_open_ext(REPO, 0, nullptr);

    // Read HEAD on master
    char  head_filepath[512];
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


    git_oid      oid    = oid_fromstr(head_rev);
    git_revwalk* walker = revwalk_new(repo);

    revwalk_sorting(walker, GIT_SORT_TOPOLOGICAL);
    revwalk_push(walker, &oid);

    std::map<int, int>                  per_week;
    std::vector<commit_analysis_result> processed;

    gregorian::date start       = {2020, 1, 1};
    int             days_period = 30;
    walker_config   config{
          .get_period = [&](const gregorian::date& date) -> int {
            return (date - start).days() / days_period;
        },
          .check_date = [&](const gregorian::date& date) -> bool {
            return (date - start).days() % days_period == 0;
        }};


    //    int prev_period = 0;
    while (revwalk_next(&oid, walker) == GIT_SUCCESS) {

        git_commit* commit = commit_lookup(repo, &oid);
        // Convert from unix timestamp used by git to humane format
        gregorian::date date = posix_time::from_time_t(commit_time(commit))
                                   .date();
        auto diff = date - start;
        auto days = diff.days();
        //        ++per_week[days / 7];
        commit_free(commit);
        if (config.check_date(date)) {
            processed.push_back(process_commit(repo, oid, config));
        }
    }


    revwalk_free(walker);
    repository_free(repo);


    std::map<int, std::pair<gregorian::date, std::vector<int>>>
        period_burndown;
    for (auto& res : processed) {
        for (auto& [period, stats] : res.per_period) {
            period_burndown[period].second.push_back(stats.second);
            period_burndown[period].first = stats.first.value();
        }
    }

    for (auto& [period, lines] : period_burndown) {
        std::reverse(lines.second.begin(), lines.second.end());
        fmt::print("period {} lines {}\n", period, lines.second);
    }

    std::vector<std::vector<double>> Y;
    std::vector<std::string>         legend;
    for (auto& [period, stats] : period_burndown) {
        const auto& date  = stats.first;
        const auto& lines = stats.second;
        legend.push_back(gregorian::to_iso_extended_string(date));
        Y.push_back(std::vector<double>(lines.size()));

        for (auto count : lines) {
            Y.back().push_back(count);
        }
    }

    matplot::barstacked(Y);
    auto plot_legend = matplot::legend(legend);
    plot_legend->location(matplot::legend::general_alignment::topleft);

    if (false) {
        std::vector<int> weeks;
        std::vector<int> commits;

        std::ofstream csv{"/tmp/commit-count.csv"};

        for (auto& [week, count] : per_week) {
            csv << week << "," << count << "\n";
            weeks.push_back(week);
            commits.push_back(count);
        }
        matplot::plot(weeks, commits);
    }


    matplot::save("/tmp/commit-count.png");

    return 0;
}
