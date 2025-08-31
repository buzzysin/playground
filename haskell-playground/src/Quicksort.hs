module Quicksort (quickSort) where
import           Data.Maybe (fromJust)


quickSort :: Ord a => [a] -> [a]
quickSort []  = []
quickSort [x] = [x]
quickSort xs  = unIndex . quickSortImpl . index $ xs
  where
    -- implementation
    quickSortImpl :: (Eq a, Ord b) => [(a, b)] -> [(a, b)]
    quickSortImpl []   = []
    quickSortImpl [x]  = [x]
    quickSortImpl xs   = quickSortImpl less ++ (pivot : same) ++ quickSortImpl more
      where
        pivot       = fromJust $ xs `at` (length xs `div` 2)
        less        = [ x | x <- xs, getValue x <  getValue pivot ]
        same        = [ x | x <- xs, getValue x == getValue pivot
                                   , getIndex x /= getIndex pivot ]
        more        = [ x | x <- xs, getValue x >  getValue pivot ]

    -- helper aliases
    getIndex = fst
    getValue = snd
    index = zip [1 :: Int ..]
    unIndex = map getValue

    -- helper for getting the index safely
    []     `at` index = Nothing
    (x: _) `at` 0     = Just x
    (_:xs) `at` n     = xs `at` (n - 1)


