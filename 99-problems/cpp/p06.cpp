#include <iostream>
#include <sstream>
#include <vector>

/*
#idea: create vector_subrange struct that represents subrange in some
 vector, add support for bidirectional iterators. Write functions that
 takes two ranges and zips them into zip_view - another contaier
 adapter. Write function for performing transform_all - it will
 perform transformation of each element in array and check if each
 result satisfies the predicate. It should be short-circuit.
*/

enum class kwd
{
    Direct,
    Reversed,
};
template <class Container>
struct vector_subrange {
    using value_type = typename Container::value_type;

    struct iterator : std::iterator<std::output_iterator_tag, int> {
        explicit iterator(vector_subrange& range, size_t _index = 0)
            : container(range), index(_index) {
        }

        int       operator*() const;
        iterator& operator++();
        iterator  operator++(int);

      private:
        size_t           index = 0;
        vector_subrange& container;
    };

    int at(size_t pos) {
        size_t vector_pos;
        if (is_reversed) {
            vector_pos = range.second - pos;
        }
        vector_pos = range.first + pos;

        if (vector_pos >= container.size()) {
            std::ostringstream out;
            out << "vector_subrange::at out of range (vec size: "
                << container.size() << " pos: " << vector_pos << "";

            throw std::out_of_range(out.str());
        }

        return container[vector_pos];
    }

    vector_subrange(
        std::vector<int>&         _container,
        std::pair<size_t, size_t> _range,
        kwd                       direction = kwd::Direct)
        : range(_range)
        , container(_container)
        , is_reversed(direction == kwd::Reversed) {
    }

  private:
    std::vector<int>&         container;
    size_t                    position;
    std::pair<size_t, size_t> range;
    bool                      is_reversed = false;
};

int vector_subrange::iterator::operator*() const {
    return container.at(index);
}


template <class Container1, class Container2>
struct zip_view {
    using value_type = std::pair<
        typename Container1::value_type&,
        typename Container2::value_type&>;

    struct iterator : std::iterator<std::output_iterator_tag, value_type> {

        explicit iterator(zip_view& _view, size_t _index = 0)
            : view(_view), index(_index) {
        }

        std::pair<int&, int&> operator*() const;
        iterator&             operator++();
        iterator              operator++(int);

      private:
        size_t    index = 0;
        zip_view& view;
    };

    zip_view(Container1& _first, Container2& _second)
        : containers(std::make_pair(_first, _second)) {
    }

    value_type at(size_t pos) {
        return std::make_pair(
            containers.first.at(pos), containers.second.at(pos));
    }


  private:
    std::pair<Container1&, Container2&> containers;
};

template <class Container, typename Transformer, typename Predicate>
auto transform_all() -> bool {
    return false;
}

using int_subr = vector_subrange<std::vector<int>>;

auto is_palindrome(std::vector<int>& data) -> bool {
    int_subr left(data, {0, data.size() / 2});
    int_subr right(
        data, {data.size() / 2, data.size()}, kwd::Reversed);

    zip_view<int_subr, int_subr> view(left, right);

    return false;
}


int main() {


    std::cout << "    Running main\n";
    return 0;
}
