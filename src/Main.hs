module Main where

import           Data.Time (getCurrentTime)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24
import qualified Day25


day d = putStrLn "" >> putStrLn "" >> (print =<< getCurrentTime) >> putStrLn ( "Day " ++ d ++ " ")
half1 solution = putStr $ " half 1: " ++ show solution
half2 solution = putStrLn "" >> putStr (" half 2: " ++ show solution)

main = do
    day "1"
    half1 =<< Day01.solution1
    half2 =<< Day01.solution2

    day "2"
    half1 =<< Day02.solution1
    half2 =<< Day02.solution2

    day "3"
    half1 =<< Day03.solution1
    half2 =<< Day03.solution2

    day "4"
    half1 =<< Day04.solution1
    half2 =<< Day04.solution2

    day "5"
    half1 =<< Day05.solution1
    half2 =<< Day05.solution2

    day "6"
    half1 =<< Day06.solution1
    half2 =<< Day06.solution2

    day "7"
    half1 =<< Day07.solution1
    half2 =<< Day07.solution2

    day "8"
    half1 =<< Day08.solution1
    half2 =<< Day08.solution2

    day "9"
    half1 =<< Day09.solution1
    half2 =<< Day09.solution2

    day "10"
    half1 =<< Day10.solution1
    half2 =<< Day10.solution2

    day "11: Half 2 takes several hours"
    half1 Day11.solution1
    half2 Day11.solution2

    day "12"
    half1 =<< Day12.solution1
    half2 =<< Day12.solution2

    day "13"
    half1 =<< Day13.solution1
    half2 =<< Day13.solution2

    day "14"
    half1 Day14.solution1
    half2 Day14.solution2

    day "15"
    half1 =<< Day15.solution1
    half2 =<< Day15.solution2

    day "16"
    half1 =<< Day16.solution1
    half2 =<< Day16.solution2

    day "17"
    half1 =<< Day17.solution1
    half2 =<< Day17.solution2

    day "18"
    half1 =<< Day18.solution1
    half2 =<< Day18.solution2

    day "19"
    half1 =<< Day19.solution1
    half2 =<< Day19.solution2

    day "20"
    (solution1,solution2) <- Day20.solution
    half1 solution1
    half2 solution2

    day "21"
    half1 =<< Day21.solution1
    half2 =<< Day21.solution2

    day "22"
    half1 Day22.solution1
    half2 Day22.solution2

    day "23"
    half1 =<< Day23.solution1
    half2 =<< Day23.solution2

    day "24"
    half1 =<< Day24.solution1
    half2 =<< Day24.solution2

    day "25"
    half1 =<< Day25.solution1

    putStrLn ""
    print =<< getCurrentTime