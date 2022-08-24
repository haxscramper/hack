#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/graphml.hpp>
#include <iostream>
#include <git2.h>
#include <fstream>
#include <optional>
#include <fstream>
#include <vector>
#include <set>
#include <string>
#include <array>
#include <map>

struct CommitInfo {
    git_oid oid;
};

struct EdgeInfo {
    bool is_main;
};

using Graph = boost::adjacency_list<
    boost::vecS,
    boost::vecS,
    boost::bidirectionalS,
    CommitInfo,
    EdgeInfo>;

using GraphTraits = boost::graph_traits<Graph>;
using VDesc       = GraphTraits::vertex_descriptor;
using EDesc       = GraphTraits::edge_descriptor;

inline std::string oid_tostr(git_oid oid) {
    std::array<char, GIT_OID_HEXSZ + 1> result;
    git_oid_tostr(result.data(), sizeof(result), &oid);
    return std::string{result.data(), result.size() - 1};
}

namespace bg {
using boost::in_degree;
using boost::in_edges;
using boost::num_vertices;
using boost::out_degree;
using boost::out_edges;
using boost::source;
using boost::target;
using boost::vertices;
}; // namespace bg


std::ostream& operator<<(std::ostream& out, git_oid const& oid) {
    out << oid_tostr(oid);
    return out;
}

std::istream& operator>>(std::istream& is, git_oid& in) { return is; }

bool operator==(git_oid const& lhs, git_oid const& rhs) {
    return git_oid_cmp(&lhs, &rhs) == 0;
}

bool operator<(git_oid const& lhs, git_oid const& rhs) {
    return git_oid_cmp(&lhs, &rhs) < 0;
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
    std::map<git_oid, VDesc> oid_map;

    auto get_desc = [&g, &oid_map](git_oid const& oid) -> VDesc {
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

    std::map<VDesc, VDesc> base_map;

    auto rec_parents = [&](vdesc       current,
                           git_commit* commit,
                           git_oid     now) {
        for (int i = 0; i < git_commit_parentcount(commit); ++i) {
            auto oid        = *git_commit_parent_id(commit, i);
            auto parent     = get_desc(oid);
            auto [edge, ok] = boost::add_edge(parent, current, g);
            if (i == 0) { g[edge].is_main = true; }
        }

        int main_in_count = 0;
        // std::cout << "-----";
        for (auto [begin, end] = bg::in_edges(current, g); begin != end;
             ++begin) {
            auto e = *begin;
            if (g[e].is_main) {
                // std::cout << g[bg::source(e, g)].oid << " --> "
                //           << g[bg::target(e, g)].oid << "\n";
                ++main_in_count;
            }
        }

        if (1 < main_in_count) {
            // std::cout << g[current].oid
            //           << " has more than one main incoming edge\n";
            assert(false);
        }
    };


    std::set<git_oid> rec_tried;
    while (!git_revwalk_next(&oid, walker)) {
        auto        current = get_desc(oid);
        git_commit* commit  = nullptr;
        rec_tried.insert(oid);
        git_commit_lookup(&commit, repo, &oid);
        rec_parents(current, commit, oid);

        git_commit_free(commit);
    }

    std::cout << "Constructed graph with " << num_vertices(g)
              << " commits and " << num_edges(g) << " edges\n";

    VDesc head;
    for (auto [begin, end] = boost::vertices(g); begin != end; ++begin) {
        VDesc v = *begin;
        if (boost::out_degree(v, g) == 0) {
            head = v;
            std::cout << "zero out degree commit: " << oid_tostr(g[v].oid)
                      << "\n";
        } else {
            int in_count = 0;
            for (auto [begin, end] = boost::in_edges(v, g); begin != end;
                 ++begin) {
                ++in_count;
            }
            if (in_count == 0) {
                std::cout << "zero degree in commit: "
                          << oid_tostr(g[v].oid) << "\n";
            }
        }
    }

    for (auto [begin, end] = boost::vertices(g); begin != end; ++begin) {
        auto v = *begin;
        assert(get_desc(g[v].oid) == v);
    }

    {
        std::ofstream out{"/tmp/graph.dot"};
        boost::write_graphviz(out, g, [&g](std::ostream& os, VDesc vert) {
            os << "[label=\"" << oid_tostr(g[vert].oid) << "\"]";
        });
    }

    {
        std::ofstream             out{"/tmp/graph.xml"};
        boost::dynamic_properties properties;
        properties.property("hash", get(&CommitInfo::oid, g));
        properties.property("is_main", get(&EdgeInfo::is_main, g));
        boost::write_graphml(out, g, properties, true);
    }


    std::vector<VDesc> main_path;
    VDesc              current = head;
    while (boost::in_degree(current, g) != 0) {
        main_path.push_back(current);
        for (auto [begin, end] = boost::in_edges(current, g); begin != end;
             ++begin) {
            auto e = *begin;
            if (g[e].is_main) { current = boost::source(e, g); }
        }
    }

    std::cout << "Main diff path has " << main_path.size() << " commits\n";
    std::set<VDesc> main_commits;
    int             merges_to_main   = 0;
    int             regular_in_main  = 0;
    int             starting_commits = 0;
    {
        for (auto commit : main_path) {
            if (1 < bg::in_degree(commit, g)) { ++merges_to_main; }
            if (1 == bg::in_degree(commit, g)) { ++regular_in_main; }
            if (0 == bg::in_degree(commit, g)) { ++starting_commits; }
            main_commits.insert(commit);
        }
        std::cout << regular_in_main << " regular commits in main\n";
        std::cout << merges_to_main << " merges to the main branch\n";
        std::cout << starting_commits << " commits with zero parents\n";
    }

    int merges_to_non_main = 0;
    {
        for (auto [begin, end] = bg::vertices(g); begin != end; ++begin) {
            auto v = *begin;
            if (main_commits.find(v) == main_commits.end() &&
                1 < bg::in_degree(v, g)) {
                ++merges_to_non_main;
            }
        }
        std::cout << merges_to_non_main << " merges to non-main branch\n";
    }

    int commits_outside = 0;
    {
        for (auto [begin, end] = bg::vertices(g); begin != end; ++begin) {
            auto v = *begin;
            if (main_commits.find(v) == main_commits.end() &&
                bg::in_degree(v, g) == 1) {
                ++commits_outside;
            }
        }
        std::cout << commits_outside
                  << " regular commits outside of the main branch\n";
    }

    std::cout << commits_outside + merges_to_non_main + merges_to_main +
                     regular_in_main + starting_commits
              << " sum via selection. " << bg::num_vertices(g)
              << " number of registered vertices\n";

    // -- clean up --
    git_revwalk_free(walker);
    git_repository_free(repo);

    std::cout << "walk completed\n";
    git_libgit2_shutdown();
    return 0;
}
