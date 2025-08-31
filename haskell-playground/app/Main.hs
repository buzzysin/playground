module Main where

import           Quicksort    (quickSort)
import           SimpleServer (startSimpleServer)

showcaseQuickSort :: IO ()
showcaseQuickSort = do
  let unsorted = [5, 3, 8, 1, 2]
  let sorted = quickSort unsorted
  putStrLn $ "=========[ Quicksort Showcase ]========="
  putStrLn $ "Unsorted: " ++ show unsorted
  putStrLn $ "Sorted: " ++ show sorted

main :: IO ()
main = do
  showcaseQuickSort
  startSimpleServer

