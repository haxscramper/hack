#include <ogdf/basic/GraphAttributes.h>
#include <ogdf/fileformats/GraphIO.h>

// Example taken from https://github.com/ogdf/ogdf/issues/41

using namespace ogdf;

void addBends(
    GraphAttributes&                         GA,
    edge                                     e,
    std::vector<std::pair<double, double>>&& bends) {
    for (auto bend : bends) {
        GA.bends(e).pushBack(DPoint(bend.first, bend.second));
    }
}

int main() {
    Graph           G;
    GraphAttributes GA(
        G,
        GraphAttributes::nodeGraphics | GraphAttributes::nodeStyle
            | GraphAttributes::edgeGraphics | GraphAttributes::edgeStyle);
    GA.directed() = false;

    // lots of nodes...
    node w{G.newNode()};
    node v1{G.newNode()};
    node v2{G.newNode()};
    node v3{G.newNode()};
    node v0{G.newNode()};
    node s1{G.newNode()};
    node s2{G.newNode()};
    node s3{G.newNode()};
    node s4{G.newNode()};

    // and edges...
    edge w_w{G.newEdge(w, w)};
    edge w_s3{G.newEdge(w, s3)};
    edge s3_s2{G.newEdge(s3, s2)};
    edge s2_s1{G.newEdge(s2, s1)};
    edge s1_v0{G.newEdge(s1, v0)};
    edge v1_s1{G.newEdge(v1, s1)};
    edge v2_s2{G.newEdge(v2, s2)};
    edge v2_s4{G.newEdge(v2, s4)};
    edge v3_s3{G.newEdge(v3, s3)};
    edge w_v3{G.newEdge(w, v3)};
    edge w_v0_1{G.newEdge(w, v0)};
    edge w_v0_2{G.newEdge(w, v0)};
    edge w_s4{G.newEdge(w, s4)};
    edge s4_v1{G.newEdge(s4, v1)};

    // node shapes
    for (node v : {s1, s2, s3, s4}) {
        GA.shape(v)  = Shape::Ellipse;
        GA.height(v) = GA.width(v) = 7;
        GA.fillColor(v) = GA.strokeColor(v) = Color::Name::Black;
    }
    for (node v : {v0, v1, v2, v3, w}) {
        GA.shape(v)       = Shape::Rect;
        GA.width(v)       = 125;
        GA.height(v)      = 63;
        GA.fillColor(v)   = Color::Name::White;
        GA.strokeColor(v) = Color::Name::Black;
    }
    GA.width(w)  = 15;
    GA.height(w) = 95.5;

    // node positions
    GA.y(w)  = -200;
    GA.y(s3) = GA.y(s2) = GA.y(s1) = GA.y(w) - 33;
    GA.y(s4)                       = GA.y(s2) + 49;
    GA.y(v1) = GA.y(v2) = GA.y(v3) = GA.y(w) + 93;
    GA.y(v0)                       = GA.y(v1) + 79;
    double dist{(GA.y(s4) - GA.y(s2)) / 3};

    GA.x(w)  = 1000;
    GA.x(v3) = GA.x(s3) = GA.x(w) - 80;
    GA.x(v2) = GA.x(s2) = GA.x(s3) - 140;
    GA.x(v0) = GA.x(v1) = GA.x(s1) = GA.x(s2) - 140;
    GA.x(s4)                       = GA.x(s2) + dist;

    // edge styles
    for (edge e : G.edges) {
        GA.strokeWidth(e) = 1;
    }
    for (edge e : {w_s3, s3_s2, s2_s1, s1_v0, v1_s1, v2_s2, v3_s3}) {
        GA.strokeWidth(e) = 3;
    }
    GA.strokeType(w_w) = StrokeType::Dash;

    // and now the bends...
    double overlap(12);
    double leftbend(112);
    double padding(47);
    addBends(
        GA,
        w_w,
        {{GA.x(w), GA.y(s3) - 30},
         {GA.x(v0) - leftbend - padding, GA.y(s3) - 30},
         {GA.x(v0) - leftbend - padding, GA.y(v0) + padding},
         {GA.x(w), GA.y(v0) + padding}});
    addBends(GA, v2_s4, {{GA.x(s4), GA.y(v2)}});
    addBends(
        GA,
        w_v3,
        {{GA.x(w), GA.y(s4) + dist},
         {GA.x(w) + overlap, GA.y(s4) + dist},
         {GA.x(v3) + dist, GA.y(s4) + dist},
         {GA.x(v3) + dist, GA.y(v3)}});
    addBends(
        GA, w_s3, {{GA.x(w), GA.y(s3)}, {GA.x(w) + overlap, GA.y(s3)}});
    addBends(
        GA,
        s1_v0,
        {{GA.x(v0) - leftbend - dist, GA.y(s1)},
         {GA.x(v0) - leftbend - dist, GA.y(v0) + dist},
         {GA.x(v0), GA.y(v0) + dist}});
    addBends(
        GA,
        w_v0_1,
        {{GA.x(w), GA.y(s3) + dist},
         {GA.x(w) + overlap, GA.y(s3) + dist},
         {GA.x(v0) - leftbend, GA.y(s1) + dist},
         {GA.x(v0) - leftbend, GA.y(v0)}});
    addBends(
        GA,
        w_v0_2,
        {{GA.x(w), GA.y(s3) + 2 * dist},
         {GA.x(w) + overlap, GA.y(s3) + 2 * dist},
         {GA.x(v0) - leftbend + dist, GA.y(s1) + 2 * dist},
         {GA.x(v0) - leftbend + dist, GA.y(v0) - dist},
         {GA.x(v0), GA.y(v0) - dist}});
    addBends(
        GA, w_s4, {{GA.x(w), GA.y(s4)}, {GA.x(w) + overlap, GA.y(s4)}});
    addBends(
        GA,
        s4_v1,
        {{GA.x(v1) + dist, GA.y(s4)}, {GA.x(v1) + dist, GA.y(v1)}});

    GraphIO::write(GA, "pretty_edge_routing.tmp.svg", GraphIO::drawSVG);
    return 0;
}
