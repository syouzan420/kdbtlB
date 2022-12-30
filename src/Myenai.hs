module Myenai(enmAi,enTick) where

import Data.Map.Strict as M
import Mydata(Mana(..),T(..),Ta(..), Ply(..), Enm(..), Mes, Eai(..),Dr(..),youM)

enTick :: [Enm] -> [Enm]
enTick [] = []
enTick (e:es) =
  let (Eai _ atm' atn' tic' _ dan' _ _) = eai e
      natn = if (atn'==0) then atm' else atn'-1
      ntic = if (tic'==0) then 0 else tic'-1
      ndan = if (dan'==0) then 0 else dan'-1
   in e{eai=(eai e){atn=natn, tic=ntic, dan=ndan}}:(enTick es)

enmAi :: Mes -> Bool -> [Int] -> [Enm] -> Int -> [Maybe Mana] -> (Mes,[Maybe Mana])
enmAi m _ _ [] _ manas = (m, manas)
enmAi m b rs (en:enms) i nmns = enmAi nm b rs enms (i+1) (nmns++[nmn])
  where (nm,nmn) = anenm m b (rs!!i) i en

anenm :: Mes -> Bool -> Int -> Int -> Enm -> (Mes,Maybe Mana)
anenm m b r i e
  | atn' > 0 = (m,Nothing)
  | otherwise = 
    let (Eai plp' atm' _ tic' dam' dan' ipr' spr') = eai e
        pr = if (plp'==Nothing && not b) then ipr' else
             if (plp'/=Nothing && not b) then changePr dam' dan' spr' else changePr dam' dam' spr'
        ndan = if b then dam' else if (dan'==0) then dan' else dan'-1
        act = setAct r pr
        emana = actMana r i ex' plp' act
    in (m,emana)
  where atn' = atn$eai e; ex'=ex e

actMana :: Int -> Int -> Int -> Maybe (Int,Int,Int) -> String -> Maybe Mana
actMana r tg enx po act =
  let name = act ++ "*" ++ (show tg)
   in case act of
    "miru" -> case po of
                Nothing -> Just (Mana (T name (Dou [] ["Hou"] [] [])) youM)
                Just (_,x,w) -> 
                  let ddx = enx-x
                      dir = if(ddx>0) then Hd else Mg
                      nam = if(ddx>0) then "hidari" else "migi"
                   in Just (Mana (T name (Dou [] [] [] [T nam (Hou [(dir,abs ddx)])])) youM)
    "ugoku" -> undefined
    "nageru" -> undefined
    _ -> Nothing
                

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


