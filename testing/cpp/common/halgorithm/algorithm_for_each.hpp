#ifndef ALGORITHM_FOR_EACH_HPP
#define ALGORITHM_FOR_EACH_HPP

#include <algorithm>
#include <functional>
#include <memory>
#include <unordered_map>
#include <vector>

namespace spt {
inline namespace algorithm {
    template <typename In, typename Out>
    std::vector<Out> vec_transform(
        const std::vector<In>&               in,
        const std::function<Out(const In&)>& map) {
        std::vector<Out> result;
        for (const auto& it : in) {
            result.push_back(map(it));
        }
        return result;
    }


    /*!
     * \brief Apply Function for each of items in range [first, last) and
     * supply index of item for each function call
     */
    template <class InputIt, class Function>
    void for_each_count(InputIt first, InputIt last, Function func) {
        uint index = 0;
        while (first != last) {
            func(index, *first);
            ++first;
            ++index;
        }
    }


    template <class Container, class Function>
    void for_each(Container& container, Function function) {
        std::for_each(container.begin(), container.end(), function);
    }


    /*!
     * \brief Call function with arguments in range [0, maxIndex)
     * \deprecated Useless
     */
    template <class Function>
    void for_index(uint maxIndex, Function func) {
        for (uint i = 0; i < maxIndex; ++i) {
            func(i);
        }
    }

    template <class InputIt, class T, class BinaryOperation, class Filter>
    T accumulate_if(
        InputIt         first,
        InputIt         last,
        T               init,
        BinaryOperation op,
        Filter          filter) {
        while (first != last) {
            if (filter(*first)) {
                init = op(/*std::move(*/ init /*)*/, *first);
            }
            first++;
        }

        return init;
    }


    /*!
     * \brief Soft container based of values from ScoreFunction.
     *
     * Complexity: \f$O(n)\f$ applications of score function. +
     * \f$O(n\log n)\f$ comparisons
     *
     * Space: \f$O(n)\f$ pairs `<Container::value_type, function retur
     * value>
     */
    template <
        class Container,    //
        class ScoreFunction //
        >
    std::unordered_map<
        typename Container::value_type, //
        typename std::result_of<
            ScoreFunction(typename Container::value_type)>::type>
        score_sort(
            Container&    container, ///< Reference to sorted container
            ScoreFunction score      ///< Score function
        ) {

        typedef typename Container::value_type ObjectType;
        typedef typename std::result_of<ScoreFunction(ObjectType)>::type
            ScoreType;

        //#======    Algorithm

        std::unordered_map<ObjectType, ScoreType> scores;
        for (const ObjectType& item : container) {
            scores[item] = score(item);
        }

        std::sort(
            container.begin(),
            container.end(),
            [&](const ObjectType& lhs, const ObjectType& rhs) {
                return scores[lhs] < scores[rhs];
            });

        return scores;
    }

    /*!
     * \brief Soft container based of values from ScoreFunction.
     *
     * Call scoreFunctions on each object in container and then retrieve
     * score val from each object using scoreRetrieve function. Return
     * result of invocation for each object in sequence
     *
     * **Approximate complexity**
     *
     * Time: \f$\mathcal{O}\left(n\log n + nf(m) \right)\f$ where
     * \f$f(m)\f$ - complexity of invocation of scoreFunction for object in
     * sequence
     *
     * Space \f$\mathcal{O}(n)\f$
     *
     * \todo Return Score map object instead of unordered map
     * \todo After moving to c++17 add conditional to keep or discard score
     * type map. It will allow to reduce required space more than in half
     * in cases where it is not required to get matches
     */
    template <
        class Container,     //
        class ScoreFunction, //
        class ScoreRetrieve  //
        >
    std::unordered_map<
        typename Container::value_type, //
        typename std::result_of<
            ScoreFunction(typename Container::value_type)>::type>
        score_type_sort(
            Container& container, ///< Reference to container for sorting
            ScoreFunction scoreFunctions, ///< Score function
            ScoreRetrieve scoreRetrieve   ///< Retrieve score value
        ) {

        typedef typename Container::value_type ObjectType;
        typedef typename std::result_of<ScoreFunction(ObjectType)>::type
                    ScoreType;
        typedef int Score;


        //#======    Algorithm
        std::unordered_map<ObjectType, ScoreType> scores;
        std::unordered_map<ObjectType, Score>     scoreVals;

        for (const ObjectType& item : container) {
            scores[item]    = scoreFunctions(item);
            scoreVals[item] = scoreRetrieve(scores[item]);
        }

        std::sort(
            container.begin(),
            container.end(),
            [&](const ObjectType& lhs, const ObjectType& rhs) {
                return scoreVals[lhs] < scoreVals[rhs];
            });

        return scores;
    }


    /*!
     * \brief Resize container to target size. If target size is bigger
     * then current size of the container insert elements generated by
     * generator on the back. If size of the container is bigger than
     * target size call container.resize()
     */
    template <class Container>
    void resize_assign(
        /// Container to insert values into
        Container& container,
        /// Target size
        size_t target_size,
        /// Generator function
        typename std::function<typename Container::value_type()>
            generator) {
        std::back_insert_iterator<Container> back_inserter(container);
        if (std::size(container) > target_size) {
            container.resize(target_size);
        } else {
            for (size_t initial_size = std::size(container);
                 initial_size <= target_size;
                 ++initial_size) {
                *(back_inserter) = generator();
            }
        }
    }
} // namespace algorithm
} // namespace spt

#endif // ALGORITHM_FOR_EACH_HPP
