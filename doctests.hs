{-# OPTIONS_GHC -Wall -Werror #-}
module Main where
import Test.DocTest

main :: IO ()
main = doctest $ words "-pgmL markdown-unlit ConstraintUnions.lhs -XTypeApplications"
