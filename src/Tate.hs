module Tate (toTate) where

import Data.List(transpose)

type Mozisu = Int
type Haba = Int

toTate :: Mozisu -> Haba -> String -> String
toTate mozisu haba = 
  unlines.map (addSpace.reverse).transpose.makeTargetHaba haba.concatMap (makeSameLength mozisu).lines

addSpace :: String -> String
addSpace [] = []
addSpace (x:xs) = let en = fromEnum x
                   in if en>10 && en<150 then ' ':x:addSpace xs
                                         else x:addSpace xs

makeSameLength :: Int -> String -> [String]
makeSameLength ms str =
  let sln = length str
   in if sln>ms then take ms str:makeSameLength ms (drop ms str)
                else [str++replicate (ms-sln) ' ']

makeTargetHaba :: Int -> [String] -> [String]
makeTargetHaba _ [] = []
makeTargetHaba hb strs = let tls = (reverse.take hb.reverse) strs
                             mln = length$head strs
                             lng = length tls
                          in if lng<hb then strs ++ replicate (hb-lng) (replicate mln ' ') 
                                       else tls

