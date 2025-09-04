// Simple Strategy pattern utilities for demo
#pragma once
#include <vector>
#include <algorithm>

namespace strategy {

template<typename Input, typename Output>
struct IStrategy {
    virtual ~IStrategy() = default;
    virtual Output run(const Input& in) const = 0;
};

// Concrete strategy: quick sort using std::sort
struct QuickSortStrategy : IStrategy<std::vector<int>, std::vector<int>> {
    std::vector<int> run(const std::vector<int>& in) const override {
        std::vector<int> out = in;
        std::sort(out.begin(), out.end());
        return out;
    }
};

// Concrete strategy: reverse sort
struct ReverseSortStrategy : IStrategy<std::vector<int>, std::vector<int>> {
    std::vector<int> run(const std::vector<int>& in) const override {
        std::vector<int> out = in;
        std::sort(out.begin(), out.end(), std::greater<>());
        return out;
    }
};

// Context: Sorter that accepts a strategy pointer
struct Sorter {
    const IStrategy<std::vector<int>, std::vector<int>>* strategy;
    explicit Sorter(const IStrategy<std::vector<int>, std::vector<int>>* s) : strategy(s) {}
    std::vector<int> sort(const std::vector<int>& data) const {
        return strategy->run(data);
    }
};

} // namespace strategy
