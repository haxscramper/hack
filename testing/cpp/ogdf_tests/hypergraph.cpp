#include <ogdf/fileformats/GraphIO.h>
#include <ogdf/hypergraph/HypergraphLayout.h>

using namespace ogdf;

int main() {
    Hypergraph H;

    H.readBenchHypergraph("hypergraph.txt");

    HypergraphAttributesES HA(H, EdgeStandardType::tree);
    HypergraphLayoutES     hlES;

    hlES.setProfile(HypergraphLayoutES::Profile::Normal);
    hlES.call(HA);

    GraphIO::write(HA.repGA(), "hypergraph.tmp.svg", GraphIO::drawSVG);

    return 0;
}
