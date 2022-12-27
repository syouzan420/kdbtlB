module Myenai(enmAi) where

import Data.Map.Strict as M
import Mydata(Ply(..), Enm(..), Mes, Eai(..))

enmAi :: Mes -> Bool -> Int -> Ply -> [Enm] -> [Enm] -> (Mes,[Enm])
enmAi m _ _ _ [] enms = (m, enms)
enmAi m b r p (en:enms) nenms = enmAi nm b r p enms (nenms++[nenm])
  where (nm,nenm) = anenm m b r p en

anenm :: Mes -> Bool -> Int -> Ply -> Enm -> (Mes,Enm)
anenm m b r p e =
  let (Eai plp' atm' atn' tic' dam' dan' ipr' spr') = eai e
      pr = if (plp'==Nothing && not b) then ipr' else
            if (plp'/=Nothing && not b) then changePr dam' dan' spr' else changePr dam' dam' spr'
      ndan = if b then dam' else if (dan'==0) then dan' else dan'-1
      act = setAct r pr
   in (m,e)

changePr :: Int -> Int -> M.Map String Int -> M.Map String Int
changePr dm dn spr =
  let ug = case (M.lookup "ugoku" spr) of Just ug' -> ug'; _ -> 0;
      na = case (M.lookup "nageru" spr) of Just na' -> na'; _ -> 0;
      dr = div (na-ug) dm
   in M.adjust ((+) ((-dr)*dn)) "nageru" (M.adjust ((+) (dr*dn)) "ugoku" spr)

setAct :: Int -> M.Map String Int -> String
setAct r prl = let pl = toList prl
                in checkAct r 0 pl

checkAct :: Int -> Int -> [(String,Int)] -> String
checkAct _ _ [] = "noact"
checkAct r mi ((k,v):ls) =
  let ma = mi+v
   in if (r>=mi && r<ma) then k else checkAct r ma ls


