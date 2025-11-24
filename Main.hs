{-# OPTIONS_GHC -Wall #-}
module Main where

import Yermolovych12

check :: String -> Bool -> IO ()
check name ok =
  putStrLn (name ++ ": " ++ if ok then "OK" else "FAIL")

main :: IO ()
main = do
  -- allSame
  check "allSame []"
    (allSame ([] :: [Int]) == True)
  check "allSame [9,9,9]"
    (allSame [9,9,9 :: Int] == True)
  check "allSame \"abc\""
    (allSame "abc" == False)

  -- remove
  check "remove 1 [(3,'a'),(1,'b'),(7,'a')]"
    (remove (1 :: Int) [(3 :: Int,'a'),(1,'b'),(7,'a')] == [(3,'a'),(7,'a')])
  check "remove 6 []"
    (remove (6 :: Int) ([] :: [(Int, Char)]) == [])

  -- lookUpAtt
  check "lookUpAtt \"temp\" header (table !! 0)"
    (lookUpAtt "temp" header (table !! 0) == "hot")

  -- removeAtt
  check "removeAtt \"temp\" header (table !! 0)"
    (removeAtt "temp" header (table !! 0)
       == ["sunny","high","calm","bad"])

  -- buildFrequencyTable
  check "buildFrequencyTable result fishingData"
    (buildFrequencyTable result fishingData
       == [("good",9),("bad",5)])
  check "buildFrequencyTable outlook fishingData"
    (buildFrequencyTable outlook fishingData
       == [("sunny",5),("overcast",4),("rainy",5)])
  check "buildFrequencyTable outlook (header,[])"
    (buildFrequencyTable outlook (header,[])
       == [("sunny",0),("overcast",0),("rainy",0)])

  -- nodes
  check "nodes fig1 == 18"
    (nodes fig1 == 18)

  -- evalTree
  check "evalTree fig1 header (table !! 5) == \"bad\""
    (evalTree fig1 header (table !! 5) == "bad")
  check "evalTree fig2 header (table !! 4) == \"good\""
    (evalTree fig2 header (table !! 4) == "good")

  -- partitionData
  check "partitionData fishingData outlook == outlookPartition"
    (partitionData fishingData outlook == outlookPartition)

  -- buildTree
  check "buildTree result nextAtt fishingData == fig1"
    (buildTree result nextAtt fishingData == fig1)
