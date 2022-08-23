#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <iostream>
#include <git2.h>
#include <fstream>
#include <optional>
#include <fstream>
#include <vector>
#include <string>
#include <array>
#include <map>

struct CommitInfo {
    git_oid oid;
};

using Graph = boost::
    adjacency_list<boost::vecS, boost::vecS, boost::directedS, CommitInfo>;

using GraphTraits = boost::graph_traits<Graph>;
using VDesc       = GraphTraits::vertex_descriptor;
using EDesc       = GraphTraits::edge_descriptor;


inline std::string oid_tostr(git_oid oid) {
    std::array<char, GIT_OID_HEXSZ + 1> result;
    git_oid_tostr(result.data(), sizeof(result), &oid);
    return std::string{result.data(), result.size() - 1};
}

int main(int argc, char* argv[]) {
    // init libgit
    git_libgit2_init();

    // -- open repo --
    const char* REPO_PATH = "/tmp/nimskull";

    git_repository* repo = nullptr;
    git_repository_open(&repo, REPO_PATH);

    // -- create revision walker --
    git_revwalk* walker = nullptr;
    git_revwalk_new(&walker, repo);

    // sort log by chronological order
    git_revwalk_sorting(walker, GIT_SORT_NONE);

    // start from HEAD
    git_revwalk_push_head(walker);

    // -- walk the walk --
    git_oid oid;

    std::ofstream log{"/tmp/revwalk_libgit2.log"};

    Graph                    g;
    std::optional<git_oid>   head;
    std::vector<std::string> ids;
    auto cmp = [](git_oid const& lhs, git_oid const& rhs) -> bool {
        return git_oid_cmp(&lhs, &rhs) < 0;
    };
    std::map<git_oid, VDesc, decltype(cmp)> oid_map;

    auto push_commit = [&g, &oid_map](git_oid const& oid) -> VDesc {
        auto iter = oid_map.find(oid);
        if (iter != oid_map.end()) {
            return iter->second;

        } else {
            VDesc vert   = boost::add_vertex(g);
            g[vert].oid  = oid;
            oid_map[oid] = vert;
            return vert;
        }
    };

    while (!git_revwalk_next(&oid, walker)) {
        if (!head) { head = oid; }
        auto current = push_commit(oid);

        // -- get the current commit --
        git_commit* commit = nullptr;
        git_commit_lookup(&commit, repo, &oid);
        for (int i = 0; i < git_commit_parentcount(commit); ++i) {
            auto parent = push_commit(*git_commit_parent_id(commit, i));
            auto [edge, ok] = boost::add_edge(current, parent, g);
        }
        // output
        ids.push_back(git_oid_tostr_s(&oid));

        // free the commit
        git_commit_free(commit);
    }

    std::cout << "Constructed graph with " << num_vertices(g)
              << " commits and " << num_edges(g) << " edges\n";

    std::ofstream out{"/tmp/graph.dot"};
    boost::write_graphviz(out, g, [&g](std::ostream& os, VDesc vert) {
        os << "[label=\"" << oid_tostr(g[vert].oid) << "\"]";
    });

    // -- clean up --
    git_revwalk_free(walker);
    git_repository_free(repo);

    std::cout << "walk completed\n";
    git_libgit2_shutdown();
    return 0;
}
