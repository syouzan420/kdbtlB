module Tate (toTate) where

import Data.List(transpose)

type Mozisu = Int
type Haba = Int
type ScrPos = Int

toTate :: Mozisu -> Haba -> ScrPos -> String -> String
toTate mozisu haba scp = 
  unlines.map (addSpace.reverse).transpose.makeTargetHaba haba scp.concatMap (makeSameLength mozisu).lines

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

makeTargetHaba :: Haba -> ScrPos -> [String] -> [String]
makeTargetHaba _ _ [] = []
makeTargetHaba hb scp strs = let tls = if scp<0 then (reverse.take hb.drop (-scp-1).reverse) strs
                                                else (take hb.drop scp) strs
                                 mln = length$head strs
                                 lng = length tls
                              in if lng<hb then strs ++ replicate (hb-lng) (replicate mln ' ') 
                                           else tls

