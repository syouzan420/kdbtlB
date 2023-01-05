module Myenai(enmAi,enTick) where

import Data.Map.Strict as M
import Mydata(Mana(..),T(..),Enm(..),Mes,Eai(..),(.>),toMana)

enTick :: Bool -> [Enm] -> [Enm]
enTick _ [] = []
enTick b (e:es) =
  let (Eai _ atm' atn' tic' dam' dan' _ _) = eai e
      natn = if (atn'==0) then atm' else atn'-1
      ntic = if (tic'==0) then 0 else tic'-1
      ndan = if b then dam' else if (dan'==0) then 0 else dan'-1
   in e{eai=(eai e){atn=natn, tic=ntic, dan=ndan}}:(enTick b es)

enmAi :: Mes -> Bool -> [Int] -> [Enm] -> Int -> [Maybe Mana] -> (Mes,[Maybe Mana])
enmAi m _ _ [] _ manas = (m, manas)
enmAi m b rs (en:enms) i nmns = enmAi nm b rs enms (i+1) (nmns++[nmn])
  where (nm,nmn) = anenm m b (rs!!i) i en

anenm :: Mes -> Bool -> Int -> Int -> Enm -> (Mes,Maybe Mana)
anenm m b r i e
  | atn' > 0 = (m,Nothing)
  | otherwise = 
    let (Eai plp' _ _ _ dam' dan' ipr' spr') = eai e
        pr = if (plp'==Nothing && not b) then ipr' else
             if (plp'/=Nothing && not b) then changePr dam' dan' spr' else changePr dam' dam' spr'
        act = setAct r pr
        drt = if (dan'==0) then 0 else (div 100 dam')*dan'
        emana = actMana r i eki' ex' plp' act drt
    in (m,emana)
  where atn' = atn$eai e; ex'=ex e; eki'=eki e

actMana :: Int -> Int -> Int -> Int -> Maybe (Int,Int,Int) -> String -> Int -> Maybe Mana
actMana r tg enk enx po act drt =
  let flg = if(r<50) then True else False 
      fto = if flg then 1 else (-1)
      tam = if flg then "hodama" else "mizutama"
      dir b = if b then "hidari" else "migi"
      pow = div (enk*r) 100
      henk = div enk 2
      pow' = if(pow==0) then 1 else if(pow>henk) then henk else pow
      coms = case po of
               Nothing -> case act of
                            "miru"   -> [act]
                            "ugoku"  -> if flg then ["hidari",act] else ["migi",act]
                            "nageru" -> [tam,"3",act]
                            _        -> ["noact"]
               Just (_,x,_) -> 
                 let ddx = enx - x
                  in case act of
                       "miru"   -> if(ddx==0) then [act] else [dir (ddx>0),show (abs ddx),act] 
                       "ugoku"  -> let ddx' = if(r>=drt) then ddx else
                                              if(ddx==0) then fto*2 else (-ddx*2)
                                   in [dir (ddx'>0),show (abs ddx'),act]
                       "nageru" -> if (ddx==0) then [tam,show pow',act]
                                               else [dir (ddx>0),show (abs ddx),tam,show pow',act]
                       _        -> ["noact"]
      res = Prelude.foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) []
                                                                (Prelude.map toMana coms)
   in (changeNa tg) <$> (case res of [] -> Nothing; (rs:[]) -> Just rs; _ -> Nothing) 
                
changeNa :: Int -> Mana -> Mana
changeNa tg (Mana (T na ta) yo) = 
  let (hna,nas) = case (words na) of [] -> ("",[""]); (hna':nas') -> (hna',nas')
   in Mana (T (hna++"*"++(show tg)++" "++(unwords nas)) ta) yo

changePr :: Int -> Int -> M.Map String Int -> M.Map String Int
changePr dm dn spr' =
  let ug = case (M.lookup "ugoku" spr') of Just ug' -> ug'; _ -> 0;
      na = case (M.lookup "nageru" spr') of Just na' -> na'; _ -> 0;
      dr = div (na-ug) dm
   in M.adjust ((+) ((-dr)*dn)) "nageru" (M.adjust ((+) (dr*dn)) "ugoku" spr')

setAct :: Int -> M.Map String Int -> String
setAct r prl = let pl = toList prl
                in checkAct r 0 pl

checkAct :: Int -> Int -> [(String,Int)] -> String
checkAct _ _ [] = "noact"
checkAct r mi ((k,v):ls) =
  let ma = mi+v
   in if (r>=mi && r<ma) then k else checkAct r ma ls


