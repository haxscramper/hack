#include <exception>
#include <string>
#include <git2.h>
#include <fmt/core.h>
#include <map>
#include <fstream>

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

#define REPO "/tmp/Nim"
#define GIT_SUCCESS 0

int main() {
    libgit2_init();
    git_repository* repo = repository_open_ext(REPO, 0, nullptr);

    // Read HEAD on master
    char  head_filepath[512];
    FILE* head_fileptr;
    char  head_rev[41];

    strcpy(head_filepath, REPO);
    strcat(head_filepath, "/.git/refs/heads/devel");

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

    int             count = 0;
    gregorian::date start{2008, 1, 1};

    std::map<int, int> per_week;

    while (revwalk_next(&oid, walker) == GIT_SUCCESS) {
        git_commit* commit         = commit_lookup(repo, &oid);
        const char* commit_message = git::commit_message(commit);
        // Convert from unix timestamp used by git to humane format
        gregorian::date date = posix_time::from_time_t(commit_time(commit))
                                   .date();
        ++per_week[(date - start).days() / 7];
        commit_free(commit);
    }

    revwalk_free(walker);
    repository_free(repo);

    std::vector<int> weeks;
    std::vector<int> commits;

    std::ofstream csv{"/tmp/commit-count.csv"};

    for (auto& [week, count] : per_week) {
        csv << week << "," << count << "\n";
        weeks.push_back(week);
        commits.push_back(count);
    }

    matplot::plot(weeks, commits);
    matplot::save("/tmp/commit-count.png");

    return 0;
}
