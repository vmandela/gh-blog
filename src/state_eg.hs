{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

newtype Age = Age Integer deriving (Eq, Ord, Show, Num)

delay1 :: a -> a -> (a, a)
delay1 state input = (newstate, output)
        where output = state
              newstate = input

main :: IO ()
main = putStrLn "Hello World"

incrementAge :: Age -> Age
incrementAge x = x + (Age 1)

b = [1, 2 .. 10]

c = foldr (\x y -> x + y) 1 b

d = foldr1 (\x y -> x + y) b

e = foldr1 (\x y -> x - y) b

f = foldl1 (\x y -> x -y ) b