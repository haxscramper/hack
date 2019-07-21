#include <QColor>
#include <QUuid>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphml.hpp>

using namespace boost;

class VertexDescriptor
{
  public:
    VertexDescriptor() {
        uuid = QUuid::createUuid();
    }

    QUuid uuid;
};


inline static std::ostream& operator<<(
    std::ostream& os,
    QUuid const&  uuid) {
    return os << uuid.toString(QUuid::WithoutBraces).toStdString();
}

inline static std::istream& operator>>(std::istream& is, QUuid& uuid) {
    // To unqote walue use `std::quoted`
    // To read whitespace use `std::ws`
    std::string buf;
    is >> buf;
    uuid = QUuid::fromString(QString::fromStdString(buf));
    return is;
}


using Graph = adjacency_list<setS, vecS, directedS, VertexDescriptor>;


int main(int argc, char* argv[]) {
    Graph              graph;
    dynamic_properties props;
    add_vertex(graph);
    props.property("uuid", get(&VertexDescriptor::uuid, graph));

    write_graphml(std::cout, graph, props);
}
