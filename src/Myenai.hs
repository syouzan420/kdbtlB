module Myenai(enmAi,enTick) where

import Data.Map.Strict as M
import Data.Maybe(isNothing,isJust)
import Mydata(Mana(..),T(..),Enm(..),Mes,Eai(..),Ki,(.>),toMana,kiToList)


enTick :: Bool -> [Enm] -> [Enm]
enTick _ [] = []
enTick b (e:es) =
  let (Eai _ atm' atn' tic' dam' dan' _ _) = eai e
      natn = if atn'==0 then atm' else atn'-1
      ntic = if tic'==0 then 0 else tic'-1
      ndan 
        | b = dam'
        | dan'==0 = 0
        | otherwise = dan'-1
   in e{eai=(eai e){atn=natn, tic=ntic, dan=ndan}}:enTick b es

enmAi :: Mes -> Bool -> [Int] -> [Enm] -> Int -> [Maybe Mana] -> (Mes,[Maybe Mana])
enmAi m _ _ [] _ manas = (m, manas)
enmAi m b rs (en:enms) i nmns = enmAi nm b rs enms (i+1) (nmns++[nmn])
  where (nm,nmn) = anenm m b (rs!!i) i en

anenm :: Mes -> Bool -> Int -> Int -> Enm -> (Mes,Maybe Mana)
anenm m b r i e
  | atn' > 0 = (m,Nothing)
  | otherwise = 
    let (Eai plp' _ _ _ dam' dan' ipr' spr') = eai e
        pr 
         | (isNothing plp' && not b) = ipr'
         | (isJust plp' && not b) = changePr dam' dan' spr'
         | otherwise =  changePr dam' dam' spr'
        act = setAct r pr
        drt = if dan'==0 then 0 else div 100 dam'*dan'
        emana = actMana r i eki' ex' ew' plp' act drt
    in (m,emana)
  where atn' = atn$eai e; ex'=ex e; ew'=ew e; eki'=eki e

actMana :: Int -> Int -> Ki -> Int -> Int -> Maybe (Int,Int,Int) -> String -> Int -> Maybe Mana
actMana r tg enk enx enw po act drt =
  let flg = r<50 
      fto = if flg then 1 else (-1)
      tam = if flg then "hodama" else "mizutama"
      kit = if flg then 1 else 4
      kls = kiToList enk
      tki = kls!!kit
      dir b = if b then "hidari" else "migi"
      pow = div (tki*div r 2) 100
      henk = div tki 2
      pow' 
        | pow==0 = 1
        | pow>henk = if henk>0 then henk else 1
        | otherwise = pow
      coms = case po of
               Nothing -> case act of
                            "miru"   -> [act]
                            "ugoku"  -> if flg then ["hidari",act] else ["migi",act]
                            "nageru" -> [tam,show pow',act]
                            _        -> ["noact"]
               Just (_,x,_) -> 
                 let ddx = enx - x
                  in case act of
                       "miru"   -> if ddx==0 then [act] else [dir (ddx>0),show (abs ddx),act] 
                       "ugoku"  -> let ddx'
                                         | div r 2>=drt = ddx
                                         | ddx==0 = fto*enw*2
                                         | otherwise = div ddx (abs ddx)*enw*2
                                   in [dir (ddx'>0),show (abs ddx'),act]
                       "nageru" -> if ddx==0 then [tam,show pow',act]
                                               else [dir (ddx>0),show (abs ddx),tam,show pow',act]
                       _        -> ["noact"]
      res = Prelude.foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) []
                                                                (Prelude.map toMana coms)
   in changeNa tg <$> (case res of [] -> Nothing; [rs] -> Just rs; _ -> Nothing) 
                
changeNa :: Int -> Mana -> Mana
changeNa tg (Mana (T na ta) yo) = 
  let (hna,nas) = case words na of [] -> ("",[""]); (hna':nas') -> (hna',nas')
   in Mana (T (hna++"*"++show tg++" "++unwords nas) ta) yo

changePr :: Int -> Int -> M.Map String Int -> M.Map String Int
changePr dm dn spr' =
  let ug = case M.lookup "ugoku" spr' of Just ug' -> ug'; _ -> 0;
      na = case M.lookup "nageru" spr' of Just na' -> na'; _ -> 0;
      dr = div (na-ug) dm
   in M.adjust (((-dr)*dn) +) "nageru" (M.adjust ((dr*dn) +) "ugoku" spr')

setAct :: Int -> M.Map String Int -> String
setAct r prl = let pl = toList prl
                in checkAct r 0 pl

checkAct :: Int -> Int -> [(String,Int)] -> String
checkAct _ _ [] = "noact"
checkAct r mi ((k,v):ls) =
  let ma = mi+v
   in if r>=mi && r<ma then k else checkAct r ma ls

