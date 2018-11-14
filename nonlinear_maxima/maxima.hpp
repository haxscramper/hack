#include <algorithm>
#include <functional>
#include <iostream>
#include <iterator>
#include <random>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm.hpp>
#include <iterator>
#include <debug_support.hpp>


enum SolutionType
{
    Max,
    Min
};


struct Solution {
    double x   = 0;
    double y   = 0;
    double z   = 0;
    double res = 0;
};

using Functor = std::function<Solution(double, double, double)>;

template <class Container, class AverageType>
AverageType average(
    typename Container::iterator                                begin,
    typename Container::iterator                                end,
    std::function<AverageType(typename Container::value_type&)> getter) {
    if (std::distance(begin, end) == 0) {
        return 0;
    }

    AverageType sum;
    while (begin != end) {
        sum += getter(*begin);
        ++begin;
    }

    return sum / (std::distance(begin, end));
}

double average(std::vector<double> vec) {
    return std::accumulate(
               std::begin(vec), //
               std::end(vec),   //
               0.0)
           / static_cast<double>(vec.size());
}

struct SearchParams {
    int    x_start                 = 0;    ///< Initial x position
    int    y_start                 = 0;    ///< Initial y position
    int    z_start                 = 0;    ///< Initial z poistion
    float  top_take_percent        = 0.10; ///< Take top from list
    float  breed_take_percent      = 0.10; ///< Breed top from list
    size_t random_take_copies      = 10; ///< Take random around each point
    size_t initial_generation_size = 100;  ///< Size of first generation
    double random_distance         = 0.05; ///< Mutation sampling distance
    float  crossover_take_percent  = 0.05; ///< Take random % and crossover
    double mutation_chance         = 0.1;  ///< Take % and mutate
    float  tolerance_take_percent  = 0.05; ///< Top % to check tolerance
    double change_tolerance        = 0.1;  ///< Threshold change value
    int    new_random              = 40;
    double new_random_range        = 1000.0;
    size_t generation_count        = 10;
    size_t max_top_take            = 100;
    float  start_random_mupltiplier = 20;
};

double rand_double(double start, double end) {
    return start
           + static_cast<double>(rand())
                 / (static_cast<double>(RAND_MAX / (end - start)));
}

Solution generate_random_solution(Functor func, SearchParams& param) {
    return func(
        rand_double(-param.new_random_range, param.new_random_range),
        rand_double(-param.new_random_range, param.new_random_range),
        rand_double(-param.new_random_range, param.new_random_range));
}

void mutate(Solution& sln) {
    double& var = [&]() -> double& {
        switch (rand() % 3) {
            case 0: {
                return sln.x;
            } break;
            case 1: {
                return sln.y;
            } break;
            case 2: {
                return sln.z;
            } break;
        }
    }();

    var = var * (((rand() % 201) - 100) / 100.0);
};

Solution generate_child(Solution& lhs, Solution& rhs, Functor func) {
    Solution sln;
    sln.x   = average({lhs.x, rhs.x});
    sln.y   = average({lhs.y, rhs.y});
    sln.z   = average({lhs.z, rhs.z});
    sln.res = func(sln.x, sln.y, sln.z).res;
    return sln;
}

void crossover(Solution& lhs, Solution& rhs) {
    for (size_t i = 0; i < 5; ++i) {
        switch (rand() % 3) {
            case 0: {
                std::swap(lhs.x, rhs.x);
            } break;
            case 1: {
                std::swap(lhs.y, rhs.y);
            } break;
            case 2: {
                std::swap(lhs.z, rhs.z);
            } break;
        }
    }
}


Solution take_random_near(
    Solution& sln,
    double    random_distance,
    Functor   func) {
    double x = sln.x + rand_double(-random_distance, random_distance);
    double y = sln.y + rand_double(-random_distance, random_distance);
    double z = sln.z + rand_double(-random_distance, random_distance);
    return func(x, y, z);
}

double average(std::vector<Solution>& vec, double take) {
    return std::accumulate(
               vec.begin(),
               vec.begin() + take,
               0,
               [](double prev, Solution& sln) { //
                   return prev + sln.res;
               })
           / static_cast<size_t>(take);
}

Solution get_maxima(SearchParams param, Functor func, SolutionType type) {
    std::vector<Solution> input;
    std::vector<Solution> result;

    Solution init;
    init.x = param.x_start;
    init.y = param.y_start;
    init.z = param.z_start;


    for (size_t i = 0; i < param.initial_generation_size; ++i) {
        input.push_back(take_random_near(
            init,
            param.start_random_mupltiplier * param.random_distance,
            func));
    }


    for (size_t i = 0; i < param.generation_count; ++i) {
        for (Solution& sln : input) {
            for (size_t i = 0; i < param.random_take_copies; ++i) {
                result.push_back(
                    take_random_near(sln, param.random_distance, func));
            }
        }


        input.clear();

        result.erase(
            std::remove_if(
                result.begin(),
                result.end(),
                [](Solution& sln) { return std::isnan(sln.res); }),
            result.end());


        if (type == Solution::Max) {
            std::sort(
                result.begin(),
                result.end(),
                [](const Solution& lhs, const Solution& rhs) {
                    return lhs.res > rhs.res;
                });
        } else {
            std::sort(
                result.begin(),
                result.end(),
                [](const Solution& lhs, const Solution& rhs) {
                    return lhs.res < rhs.res;
                });
        }

        std::copy_n(
            result.begin(),
            std::min(
                static_cast<float>(param.max_top_take),
                param.top_take_percent * result.size()),
            std::back_inserter(input));

        for (size_t i = 0; i < param.breed_take_percent * input.size();
             ++i) {
            auto iter_1 = input.begin() + rand() % input.size();
            auto iter_2 = input.begin() + rand() % input.size();
            input.push_back(generate_child(*iter_1, *iter_2, func));
        }

        for (size_t i = 0; i < param.crossover_take_percent * input.size();
             ++i) {
            auto iter_1 = input.begin() + rand() % input.size();
            auto iter_2 = input.begin() + rand() % input.size();
            crossover(*iter_1, *iter_2);
        }

        result.clear();
    }


    return input.front();
}
