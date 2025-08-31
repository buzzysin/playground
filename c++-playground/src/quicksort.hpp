#pragma once
#include <algorithm>
#include <iterator>

// QuickSort implementation for random-access iterators
template <typename RandomIt>
void quicksort(RandomIt first, RandomIt last) {
  // Base case: if the range is empty or has one element, do nothing
  if (first >= last || std::distance(first, last) < 2)
    return;

  // Choose the middle element as the pivot
  auto pivot = *(first + std::distance(first, last) / 2);
  RandomIt left = first;
  RandomIt right = last - 1;

  // Partition the range around the pivot
  while (left <= right) {
    // Move left iterator forward while elements are less than pivot
    while (*left < pivot) ++left;
    // Move right iterator backward while elements are greater than pivot
    while (*right > pivot) --right;
    if (left <= right) {
      // Swap elements that are on the wrong side of the pivot
      std::iter_swap(left, right);
      ++left;
      --right;
    }
  }

  // Recursively sort the partitions
  quicksort(first, right + 1); // Left partition
  quicksort(left, last);       // Right partition
}

// Functional-style QuickSort: returns a sorted copy of the input container
#include <vector>

template <typename Container>
Container quicksort_func(const Container& input) {
  if (input.size() < 2)
    return input;

  auto pivot = input[input.size() / 2];
  Container less, equal, greater;

  for (const auto& elem : input) {
    if (elem < pivot)
      less.push_back(elem);
    else if (elem > pivot)
      greater.push_back(elem);
    else
      equal.push_back(elem);
  }

  auto sorted_less = quicksort_func(less);
  auto sorted_greater = quicksort_func(greater);

  Container result;
  result.reserve(sorted_less.size() + equal.size() + sorted_greater.size());
  result.insert(result.end(), sorted_less.begin(), sorted_less.end());
  result.insert(result.end(), equal.begin(), equal.end());
  result.insert(result.end(), sorted_greater.begin(), sorted_greater.end());
  return result;
}