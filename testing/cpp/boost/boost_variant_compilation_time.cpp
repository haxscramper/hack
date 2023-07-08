#ifdef TEST_STD
#    include <variant>
using namespace std;
#else
#    include <boost/variant2/variant.hpp>
using namespace boost::variant2;
#endif

#define PACK int, float, char, bool, double
#define PACK10 PACK, PACK
#define PACK20 PACK10, PACK10
#define PACK40 PACK20, PACK20
#define PACK80 PACK40, PACK40
#define PACK160 PACK80, PACK80
#define PACK320 PACK160, PACK160
#define PACK640 PACK320, PACK320

int main() {
    variant<PACK640> a, b, c, d;
    visit([](auto x) {}, a);
}
