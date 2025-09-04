#include "strategy/strategy.hpp"
#include <iostream>
#include <vector>

int main() {
    using namespace strategy;
    QuickSortStrategy qs;
    ReverseSortStrategy rs;

    std::vector<int> data{5,3,8,1,4};

    Sorter sorter_q(&qs);
    auto sorted_q = sorter_q.sort(data);
    std::cout << "QuickSort result: ";
    for (int v: sorted_q) std::cout << v << ' ';
    std::cout << '\n';

    Sorter sorter_r(&rs);
    auto sorted_r = sorter_r.sort(data);
    std::cout << "ReverseSort result: ";
    for (int v: sorted_r) std::cout << v << ' ';
    std::cout << '\n';

    return 0;
}
