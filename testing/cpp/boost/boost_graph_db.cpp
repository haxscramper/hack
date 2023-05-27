#include <iostream>
#include <string>
#include <variant>
#include <optional>
#include <functional>
#include <vector>
#include <unordered_map>
#include <format>
#include <algorithm>
#include <fmt/core.h>
#include <fmt/color.h>
#include <algorithm>

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>

template <typename T>
using Vec = std::vector<T>;

using Str = std::string;

template <typename T>
using Opt = std::optional<T>;

using namespace boost;

struct EdgeProp {};
struct VertProp {};


using Graph = adjacency_list<
    vecS,
    vecS,
    bidirectionalS,
    VertProp,
    EdgeProp>;

struct QuerySystem {
    using GraphTraits     = boost::graph_traits<Graph>;
    using VertDesc        = typename GraphTraits::vertex_descriptor;
    using EdgeDesc        = typename GraphTraits::edge_descriptor;
    using VertBundledType = typename boost::vertex_bundle_type<
        Graph>::type;
    using EdgeBundledType = typename boost::edge_bundle_type<Graph>::type;
    using Value           = std::string;


    /// QueryResult object
    struct QueryResult {
        /// QueryResultKind enum
        enum class Kind
        {
            None,
            QrValue,
            Vertex
        };


        /// Structs representing each kind of
        struct None {};
        struct QrValue {
            Value value;
        };
        struct Vertex {
            VertDesc vertex;
        };

        std::variant<None, QrValue, Vertex> data;

        VertDesc getVertex() const {
            return std::get<Vertex>(data).vertex;
        }

        Value getValue() const { return std::get<QrValue>(data).value; }

        Kind getKind() const {
            switch (data.index()) {
                case 0: return Kind::None;
                case 1: return Kind::QrValue;
                case 2: return Kind::Vertex;
            }
        }


        QueryResult(decltype(data) const& value) : data(value) {}
    };

    struct Gremlin {
        Opt<Value>                                value;
        Opt<VertDesc>                             vertex;
        std::unordered_map<std::string, VertDesc> asMap;
        Gremlin() = default;
        Gremlin(Value const& value) : value(value) {}
        Gremlin(Opt<VertDesc> const& vertex) : vertex(vertex) {}
    };

    /// PipeRes object
    struct PipeRes {
        /// PipeRes Kind enum
        enum class Kind
        {
            Pull,
            Push,
            Eval,
            Done,
            False
        };

        /// Structs representing each kind of
        struct Pull {};
        struct Push {};
        struct Eval {
            Gremlin gremlin;
        };

        struct Done {};
        struct False {};

      public:
        std::variant<Pull, Push, Eval, Done, False> data;

        Kind getKind() const { return static_cast<Kind>(data.index()); }

        Eval const& getEval() { return std::get<Eval>(data); }

        PipeRes(decltype(data) const& value) : data(value) {}
        PipeRes(Gremlin const& gremlin) : data(Eval{gremlin}) {}
    };


    /// VertexFilter object
    struct VertexFilter {
        /// VertexFilterKind enum
        enum class Kind
        {
            None,
            IdSet,
            Property,
            Predicate
        };

        /// Structs representing each kind of
        struct None {
            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                return PipeRes(Gremlin());
            }
        };

        struct IdSet {
            std::vector<VertDesc> idSet;
            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                // FIXME original implementation mutated different parts of
                // the pipe as it went along, but I'm not sure if I want to
                // implement it in exactly the same way.
                if (!init) {
                    init = true;
                    for (auto id : idSet) {
                        vertices.push_back(id);
                    }
                }

                if (!vertices.empty()) {
                    Gremlin g;
                    g.vertex = std::optional<VertDesc>(vertices.back());
                    vertices.pop_back();
                    return PipeRes(PipeRes::Eval{g});
                } else {
                    // Not found any new vertices, returning
                    return PipeRes(PipeRes::Done());
                }
            }

          private:
            Vec<VertDesc> vertices;
            bool          init = false;
        };


        struct Predicate {
            std::function<bool(VertDesc)> predicate;

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                if (!gremlin) {
                    return PipeRes(PipeRes::Pull());
                } else if (predicate(*gremlin->vertex)) {
                    return PipeRes(PipeRes::Pull());
                } else {
                    return PipeRes(*gremlin);
                }
            }
        };

        std::variant<None, IdSet, Predicate> data;

        Kind getKind() const { return static_cast<Kind>(data.index()); }

        VertexFilter(decltype(data) const& value) : data(value) {}
        VertexFilter() {}

        PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
            return std::visit(
                [&](auto& filter) { return filter.run(gremlin, graph); },
                data);
        }
    };


    /// Pipe object
    struct Pipe {
        /// PipeKind enum
        enum class Kind
        {
            Property,
            Unique,
            Filter,
            OutNodes,
            InNodes,
            TakeN,
            AsTagged,    /// Tag pipe results
            MergeTagged, /// Merge tagged pipe results
            Back
        };

        /// Structs representing each kind of Pipe
        struct Property {
            Value                                             property;
            std::function<Opt<Value>(VertDesc, Graph const&)> getter;

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                if (!gremlin) {
                    return PipeRes(PipeRes::Pull());
                } else {
                    if (gremlin->vertex.has_value()) {
                        Opt<Value> eval = getter(*gremlin->vertex, graph);
                        if (eval) {
                            return PipeRes(*eval);
                        } else {
                            return PipeRes(PipeRes::False());
                        }
                    } else {
                        return PipeRes(PipeRes::False());
                    }
                }
            }
        };

        struct Unique {
            Vec<VertDesc> seen;

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                if (!gremlin.has_value() || !gremlin->vertex.has_value()
                    || std::find(
                           seen.begin(), seen.end(), *gremlin->vertex)
                           != seen.end()) {
                    return PipeRes(PipeRes::Pull());
                } else {
                    return PipeRes(*gremlin);
                }
            }
        };

        struct OutInNodes {
            Vec<EdgeDesc> edges;
            Opt<Gremlin>  gremlin;

            bool inDirection = true;

            Vec<EdgeDesc> getEdges(Graph const& graph) {
                Vec<EdgeDesc> result;
                if (inDirection) {
                    auto [it, it_end] = in_edges(*gremlin->vertex, graph);
                    for (; it != it_end; ++it) {
                        result.push_back(*it);
                    }

                } else {
                    auto [it, it_end] = out_edges(*gremlin->vertex, graph);
                    for (; it != it_end; ++it) {
                        result.push_back(*it);
                    }
                }
                return result;
            }

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                if (!gremlin.has_value() && edges.empty()) {
                    return PipeRes(PipeRes::Pull());
                }

                if (edges.empty()) {
                    this->gremlin = gremlin;
                    for (const auto& edge : getEdges(graph)) {
                        edges.push_back(edge);
                    }
                }

                if (edges.empty()) {
                    return PipeRes(PipeRes::Pull());
                }

                auto edge = edges.back();
                edges.pop_back();

                Gremlin res;
                if (inDirection) {
                    res.vertex = Opt<VertDesc>(source(edge, graph));
                } else {
                    res.vertex = Opt<VertDesc>(target(edge, graph));
                }
                return PipeRes(res);
            }
        };

        struct Filter {
            bool          init;
            Vec<VertDesc> vertices;
            VertexFilter  filter;

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                return filter.run(gremlin, graph);
            }
        };

        struct TakeN {
            int takeNum = 0;

            int taken = 0;

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                if (taken == takeNum) {
                    taken = 0;
                    return PipeRes(PipeRes::Done());
                } else {
                    if (!gremlin) {
                        return PipeRes(PipeRes::Pull());
                    } else {
                        ++taken;
                        return PipeRes(*gremlin);
                    }
                }
            }
        };

        struct AsTagged {
            Str asName;

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                if (!gremlin) {
                    return PipeRes(PipeRes::Pull());
                }

                Gremlin res       = *gremlin;
                res.asMap[asName] = gremlin->asMap.at(asName);

                return PipeRes(res);
            }
        };

        struct MergeTagged {
            Vec<Str>      mergeNames;
            Vec<VertDesc> vertices;

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                if (!gremlin) {
                    return PipeRes(PipeRes::Pull()); // query
                                                     // initialization
                }

                if (vertices.empty()) { // state initialization
                    for (const auto& [name, vertex] : gremlin->asMap) {
                        vertices.push_back(vertex);
                    }
                }

                if (vertices.empty()) {
                    return PipeRes(PipeRes::Pull()); // done with
                                                     // this batch
                }

                auto vertex = vertices.back();
                vertices.pop_back();

                return PipeRes(Gremlin(vertex));
            }
        };

        struct Back {
            Str backName;

            PipeRes run(Opt<Gremlin> const& gremlin, Graph const& graph) {
                if (!gremlin.has_value()) {
                    return PipeRes(PipeRes::Pull());
                } else {

                    Opt<VertDesc> asName;
                    auto back_target = gremlin->asMap.find(backName);
                    if (back_target != gremlin->asMap.end()) {
                        asName = gremlin->asMap.at(backName);
                    }

                    return PipeRes(Gremlin(asName));
                }
            }
        };


        std::variant<
            Property,
            Unique,
            OutInNodes,
            Filter,
            TakeN,
            AsTagged,
            MergeTagged,
            Back>
            data;

        Pipe(decltype(data) const& value) : data(value) {}
        Pipe() {}


        Kind    getKind() const { return static_cast<Kind>(data.index()); }
        PipeRes run(Opt<Gremlin> gremlin, Graph const& graph) {
            return std::visit(
                [&](auto& pipe) { return pipe.run(gremlin, graph); },
                data);
        }
    };

    struct Query {
        Vec<Pipe> program;

        Query& merge(Vec<Str> const& toMerge) {
            program.push_back(
                Pipe(Pipe::MergeTagged{.mergeNames = toMerge}));
            return *this;
        }

        Query& filter(VertexFilter const& filt) {
            program.push_back(Pipe(Pipe::Filter{.filter = filt}));
            return *this;
        }

        Query& start(VertDesc id) {
            VertexFilter::IdSet set{};
            set.idSet = Vec<VertDesc>{id};
            auto filt = VertexFilter(set);
            return filter(filt);
        }

        Query& outNodes() {
            program.push_back(
                Pipe(Pipe::OutInNodes{.inDirection = false}));
            return *this;
        }


        Query& inNodes() {
            program.push_back(Pipe(Pipe::OutInNodes{.inDirection = true}));
            return *this;
        }

        Vec<VertDesc> runForVertices(
            Graph const& graph,
            bool         traceExec = false) {
            auto          tmp = run(graph, traceExec);
            Vec<VertDesc> result;
            for (const auto& it : tmp) {
                result.push_back(it.getVertex());
            }

            return result;
        }

        /// Run a query on a graph
        Vec<QueryResult> run(Graph const& graph, bool traceExec = false) {
            const int max = static_cast<int>(program.size()) - 1;
            fmt::print("Executing query of {} steps\n", max);
            std::vector<Gremlin> results;
            PipeRes              res{PipeRes::False()};

            int done = -1;
            int pc   = max;

            while (done < max) {
                if (traceExec) {
                    fmt::print(
                        "Executing step {}, done={}, max={}\n",
                        pc,
                        done,
                        max);
                }

                std::optional<Gremlin> arg;
                if (res.getKind() == PipeRes::Kind::Eval) {
                    arg = std::get<PipeRes::Eval>(res.data).gremlin;
                }

                res = program[pc].run(arg, graph);
                if (res.getKind() == PipeRes::Kind::Pull) {
                    if (traceExec) {
                        fmt::print("  Result kind is 'pull'\n");
                    }
                    // Current pipe needs more input
                    res.data = PipeRes::False();
                    if (done < pc - 1) {
                        // Try previous filter - decrement PC and continue
                        // evaluation
                        --pc;
                        if (traceExec) {
                            fmt::print(
                                "  Previous filter at positon {} is not "
                                "completed\n",
                                pc);
                        }
                        continue;
                    } else {
                        if (traceExec) {
                            fmt::print(
                                "  Previous filter at position {} is "
                                "complete\n",
                                pc);
                        }
                        // Previous pipe is done
                        done = pc;
                    }
                } else if (res.getKind() == PipeRes::Kind::Done) {
                    if (traceExec) {
                        fmt::print("  Result kind is 'done'\n");
                    }
                    res.data = PipeRes::False();
                    done     = pc;
                }

                // Next step
                ++pc;

                if (max < pc) {
                    // Pipeline evaluation had some result
                    if (res.getKind() == PipeRes::Kind::Eval) {
                        if (traceExec) {
                            fmt::print(
                                "  Last step result was evaluated, "
                                "adding\n");
                        }
                        results.push_back(res.getEval().gremlin);
                    }

                    res.data = PipeRes::False();
                    --pc;
                }
            }

            Vec<QueryResult> result;
            for (const auto& gremlin : results) {
                if (gremlin.value.has_value()) {
                    result.push_back(QueryResult(
                        QueryResult::QrValue{gremlin.value.value()}));
                } else if (gremlin.vertex.has_value()) {
                    result.push_back(QueryResult(
                        QueryResult::Vertex{gremlin.vertex.value()}));
                } else {
                    result.push_back(QueryResult(QueryResult::None{}));
                }
            }

            return result;
        }
    };
};

#include <gtest/gtest.h>
#include <gmock/gmock.h>

Graph simpleGraph() {
    Graph graph;
    auto  v1   = add_vertex(graph);
    auto  v2   = add_vertex(graph);
    auto  edge = add_edge(v1, v2, graph);

    return graph;
}

Graph bigGraph() {
    Graph graph;
    auto  _id1 = add_vertex(graph);
    auto  _id2 = add_vertex(graph);
    auto  _id3 = add_vertex(graph);
    auto  _id4 = add_vertex(graph);
    auto  _id5 = add_vertex(graph);
    auto  _id6 = add_vertex(graph);

    add_edge(_id1, _id2, graph);
    add_edge(_id2, _id3, graph);
    add_edge(_id2, _id4, graph);
    add_edge(_id2, _id5, graph);
    add_edge(_id2, _id6, graph);
    add_edge(_id3, _id4, graph);
    add_edge(_id4, _id5, graph);
    add_edge(_id5, _id3, graph);
    add_edge(_id3, _id5, graph);
    add_edge(_id4, _id3, graph);
    add_edge(_id5, _id4, graph);
    add_edge(_id3, _id6, graph);
    add_edge(_id4, _id6, graph);
    add_edge(_id5, _id6, graph);
    add_edge(_id6, _id3, graph);
    add_edge(_id6, _id4, graph);
    add_edge(_id6, _id5, graph);

    return graph;
}

TEST(BasicApi, OutNode) {
    Graph graph = simpleGraph();

    auto result = QuerySystem::Query{}
                      .start(boost::vertex(0, graph))
                      .outNodes()
                      .run(graph);

    ASSERT_EQ(result.size(), 1);
}

TEST(BasicApi, StartReturns) {
    Graph graph = simpleGraph();

    auto v      = boost::vertex(0, graph);
    auto result = QuerySystem::Query{}.start(v).run(graph);

    ASSERT_EQ(result.size(), 1);
    ASSERT_EQ(result.at(0).getVertex(), v);
}

TEST(BasicApi, InNode) {
    Graph graph = simpleGraph();

    auto v      = boost::vertex(1, graph);
    auto result = QuerySystem::Query{}.start(v).inNodes().run(graph);

    ASSERT_EQ(result.size(), 1);
    ASSERT_EQ(result.at(0).getVertex(), boost::vertex(0, graph));
}

template <typename T>
Vec<T> sorted(Vec<T> const& in) {
    Vec<T> res = in;
    std::sort(res.begin(), res.end());
    return res;
}

TEST(BasicApi, NestedOutgoing) {
    auto g      = bigGraph();
    auto result = QuerySystem::Query{}
                      .start(vertex(0, g))
                      .outNodes()
                      .outNodes()
                      .runForVertices(g);

    ASSERT_EQ(result.size(), 4);
    ASSERT_EQ(
        result,
        sorted<decltype(vertex(3, g))>({
            vertex(2, g),
            vertex(3, g),
            vertex(4, g),
            vertex(5, g),
        }));
}

int main(int argc, char** argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
