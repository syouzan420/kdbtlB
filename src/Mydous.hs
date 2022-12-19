module Mydous(exeCom) where

import qualified Data.Map.Strict as M
import Data.List (intersect)
import Data.List.Split (splitOn)
import Mydata(T(..),Ta(..),Mana(..),State(..),Dr(..)
             ,Bu,Fun,Bul(..),Ply(..),Enm(..),(.>),toMana,maxY)

type Pos = (Int, Int, Int, String) -- y, x, width, name

exeCom :: String -> State -> State 
exeCom com s = let coms =  words com
                   manas = map toMana coms
                   res = foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) [] manas
                in if (res==[]) then s{mes=mes s++"ERROR!!\n"} else makeState s{mns=[]} res

makeState :: State -> [Mana] -> State
makeState st [] = st
makeState st (mn:manas) = makeState (applyMana st mn) manas 

applyMana :: State -> Mana -> State
applyMana st m@(Mana (T na (Dou _ _ ts1 ts2)) _) = 
  let nam = head$words na
      nms = splitOn "*" nam
      tg = if (length nms==1) then (-1) else (read (last nms))::Int
      fnc = M.lookup (head nms) funcName
      st' = st{mns = mns st ++ [m]}
      (nst,cs) = case fnc of
                    Nothing -> (st',0)
                    Just f  -> f ts1 ts2 st'
      enms = ens nst
      tki = if (tg==(-1)) then pki$pl$nst else eki$enms!!tg
      nen = if (tg>=0) then (enms!!tg){eki=tki-cs} else enms!!0 
      nens = if (tg>=0) then take tg enms ++ [nen] ++ drop (tg+1) enms else enms
      icast = tki > cs
   in if icast then if (tg==(-1)) then nst{pl=(pl nst){pki=tki-cs}} else nst{ens=nens}
               else st'{mes="not enough KI!"}
applyMana st m = st{mns= mns st ++ [m]}


funcName :: M.Map String Fun 
funcName = M.fromList [("nageru",nageru),("ugoku",ugoku),("miru",miru)]

miru :: Fun
miru [] [] st = (st{mes=seeToMes$lookingAt Ue (px$pl st) 1 1 (makePosLists st)},1)
miru [] ((T _ (Hou hus)):[]) st = (st{mes=seeToMes$lookingAt Ue dlt 3 1 (makePosLists st)},abs dlt)
  where (_,dlt) = calcDelta hus 1
miru _ _ st = (st,0)

makePosLists :: State -> [Pos] 
makePosLists st = [(py$pl st, px$pl st, pw$pl st, "player")]++
                  (map (\(Enm ena' _ _ _ _ ey' ex' ew' _) -> (ey',ex',ew',ena')) (ens st))

lookingAt :: Dr -> Int -> Int -> Int -> [Pos] -> [Pos]
lookingAt Ue ci wi fi psls = seeking 1 maxY ci wi fi psls
lookingAt Si ci wi fi psls = seeking (-1) 0 ci wi fi psls
lookingAt _ _ _ _ _ = []

seeking :: Int -> Int -> Int -> Int -> Int -> [Pos] -> [Pos]
seeking dr toi ci wi fri psls  
  | toi+dr == fri = []
  | otherwise = (foldl (\acc (y,x,w,na) -> let (cn,wd) = coinCide (x,w) (ci,wi) in 
      if(fri==y && (cn,wd)/=(0,0)) then acc++[(y,cn,wd,na)] else acc) [] psls)++
        seeking dr toi ci (wi+1) (fri+dr) psls

coinCide :: (Int, Int) -> (Int, Int) -> (Int, Int) 
coinCide (x,w) (x1,w1) =
  let s1 = [(x-w)..(x+w)]; s2 = [(x1-w1)..(x1+w1)]
      un = intersect s1 s2
   in if (un==[]) then (0,0) else let h = head un
                                      l = last un
                                   in (div (h+l) 2,div (l-h) 2)

seeToMes :: [Pos] -> String
seeToMes sis = concat$map (\(y,c,w,n) -> "dist: "++(show y)++" dir: "++
  (if (c>0) then "migi" else if (c<0) then "hidari" else "manaka")++(show c)++
    " haba"++(show w)++"---"++n++"\n") sis

ugoku :: Fun
ugoku [] _ st = (st{mes="No Direction"},0)
ugoku ((T _ (Hou hus)):[]) [] st = (st{pl=(pl st){pdx=dlt}},abs dlt)
  where (_,dlt) = calcDelta hus 1
ugoku ((T _ (Hou hus)):[]) ((T _ (Kaz kz)):[]) st = (st{pl=(pl st){pdx=dlt*sp}},(abs dlt)*sp)
  where (sp,dlt) = calcDelta hus kz 
ugoku _ _ st = (st,0)

nageru :: Fun
nageru [] _ st = (st{mes="No Tama"},0)
nageru ((T _ (Tam tm)):[]) [] st = (st{tms=(tms st)++(fst mkb)},snd mkb)
  where mkb = makeBullets tm [] 1 (getPlp st) 
nageru ((T _ (Tam tm)):[]) ((T _ (Hou hus)):[]) st = (st{tms=(tms st)++(fst mkb)},snd mkb)
  where mkb = makeBullets tm hus 1 (getPlp st) 
nageru ((T _ (Tam tm)):[]) ((T _ (Kaz kz)):[]) st = (st{tms=(tms st)++(fst mkb)},snd mkb)
  where mkb = makeBullets tm [] kz (getPlp st) 
nageru ((T _ (Tam tm)):[]) ((T _ (Hou hus)):(T _ (Kaz kz)):[]) st = (st{tms=(tms st)++(fst mkb)},snd mkb)
  where mkb = makeBullets tm hus kz (getPlp st) 
nageru ((T _ (Tam tm)):[]) ((T _ (Kaz kz)):(T _ (Hou hus)):[]) st = (st{tms=(tms st)++(fst mkb)},snd mkb)
  where mkb = makeBullets tm hus kz (getPlp st) 
nageru _ _ st = (st,0)

makeBullets :: [(Bu,Int)] -> [(Dr,Int)] -> Int -> (Int, Int) -> ([Bul],Int)
makeBullets [] _ _ _ = ([],0)
makeBullets ((b,s):bss) hus sp (y,x) = 
  let (dy, dx) = calcDelta hus sp
   in ((Bul{bt=b, bs=s, by=y, bx=x, bdy=dy, bdx=dx}):(fst mkb),(s*sp)+(snd mkb))
  where mkb = makeBullets bss hus sp (y,x)

getPlp :: State -> (Int, Int)
getPlp st = let p = pl st in (py p, px p)

calcDelta :: [(Dr,Int)] -> Int -> (Int, Int)
calcDelta hus sp = (sp, calcDx hus)
  where calcDx [] = 0
        calcDx ((dir,i):hs) 
          | dir==Mg = i + calcDx hs
          | dir==Hd = (-i) + calcDx hs
          | otherwise = calcDx hs
