#include <initializer_list>

// Aggregate initialization
//
// Struct has no private/protected fields, is not derived from anything,
// has no user-defined constructors etc.
struct AggregateInit {
    int  fld1;
    char fld2;
};

/*

nim initalization via `initT(fld1: cint, fld2: cchar): AggregateInit
  {.importcpp: "AggregateInit({@})".}

*/

void aggregateParam(AggregateInit arg = {12, '1'}) {
}

struct ListInit {
    ListInit(std::initializer_list<int> arg) {
    }
};


/*

nim initalization via `initT(args: varargs[int]): AggregateInit
  {.importcpp: "AggregateInit({@})".}

But I need to figure out how `varargs` converts to `importcpp` exactly

*/


void initListParam(ListInit arg = {1, 2, 3, 4, 5}) {
}


int main() {
    aggregateParam();
    aggregateParam({12, '1'});
}
