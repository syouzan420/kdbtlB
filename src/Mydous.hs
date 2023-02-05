module Mydous(exeCom,applyMana) where

import qualified Data.Map.Strict as M
import Data.List (intersect)
import Data.List.Split (splitOn)
import Mydata(T(..),Ta(..),Mana(..),State(..),Dr(..)
             ,Bu(..),Fun,Ki(..),Bul(..),Ply(..),Enm(..),Eai(..),(.>),toMana,maxY,addKi,subKi,isKiLow)

type Pos = (Int, Int, Int, String) -- y, x, width, name

exeCom :: String -> State -> State 
exeCom com s = let coms =  words com
                   manas = map toMana coms
                   res = foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) [] manas
                in if null res then s{mes=mes s++"ERROR!!\n"} 
                                else makeState s{pl=(pl s){ing=False},mns=[]} res

makeState :: State -> [Mana] -> State
makeState = foldl applyMana  

applyMana :: State -> Mana -> State
applyMana st m@(Mana (T na (Dou _ _ ts1 ts2)) _) = 
  let nam = head$words na
      nms = splitOn "*" nam
      tg = if length nms==1 then (-1) else read (last nms)::Int
      fnc = M.lookup (head nms) funcName
      st' = st{mns = [m]}
      (nst,cs) = case fnc of
                    Nothing -> (st',Ki 0 0 0 0 0)
                    Just f  -> f tg ts1 ts2 st'
      enms = ens nst
      tki = if tg==(-1) then pki$pl nst else eki$enms!!tg
      kdif = subKi tki cs
      icast = not$isKiLow kdif
      nen = if tg>=0 then (enms!!tg){eki=kdif} else head enms 
      nens = if tg>=0 then take tg enms ++ [nen] ++ drop (tg+1) enms else enms
   in if icast then if tg==(-1) then nst{pl=(pl nst){pki=kdif}} else nst{ens=nens}
               else if tg==(-1) then st'{mes=mes st'++"not enough KI!\n"} else st'
applyMana st m = st{mns= [m]}

funcName :: M.Map String Fun 
funcName = M.fromList [("nageru",nageru),("ugoku",ugoku),("miru",miru)]

miru :: Fun
miru tg [] [] st 
  | tg==(-1) = (st{mes=mes st++seeToMes (lookingAt Ue (px p) 1 1 (makePosLists st)),
                   pl=lookList 0 p},Ki 1 0 0 0 0)
  | otherwise = (enMiru tg 0 1 1 st,Ki 1 0 0 0 0) 
  where p = pl st
miru tg [] [T _ (Hou hus)] st 
  | tg==(-1) = (st{mes=mes st++seeToMes (lookingAt Ue dlt 3 1 (makePosLists st)),
                   pl=lookList dlt (pl st)},Ki 1 0 0 0 0)
  | otherwise = (enMiru tg (-dlt) 3 1 st,Ki 1 0 0 0 0)
  where (_,dlt) = calcDelta hus 1
miru _ _ _ st = (st,Ki 0 0 0 0 0)

lookList :: Int -> Ply -> Ply
lookList dl p = p{look=makeLookList dl (px p) 1 1, ltc=20}

makeLookList :: Int -> Int -> Int -> Int -> [(Int,Int)]
makeLookList dl x w y 
  |y == maxY+1 = []
  |otherwise = makeLookList dl (if imy then x+dl else x) (if imy then w+1 else w) (y+1)++[(x+dl,w)]
  where imy = even y 

enMiru :: Int -> Int -> Int -> Int -> State -> State
enMiru tg ci wi fi st =
  let ens' = ens st
      e = ens'!!tg
      nci = if ci==0 then ex e else ci
      nplp = lookPlayer$lookingAt Si nci wi fi (makePosLists st)
      ne = e{eai=(eai e){plp=nplp}}
      nens = take tg ens' ++ [ne] ++ drop (tg+1) ens'
   in st{ens=nens}

lookPlayer :: [Pos] -> Maybe (Int, Int, Int)
lookPlayer [] = Nothing
lookPlayer ((y,x,w,"player"):_) = Just (y,x,w)
lookPlayer (_:ps) = lookPlayer ps

makePosLists :: State -> [Pos] 
makePosLists st = (py$pl st, px$pl st, pw$pl st, "player"):
                  map (\(Enm ena' _ _ _ _ ey' ex' ew' _ _) -> (ey',ex',ew',ena')) (ens st)

lookingAt :: Dr -> Int -> Int -> Int -> [Pos] -> [Pos]
lookingAt Ue ci wi fi psls = seeking 1 maxY ci wi fi psls
lookingAt Si ci wi fi psls = seeking (-1) 0 ci wi fi psls
lookingAt _ _ _ _ _ = []

seeking :: Int -> Int -> Int -> Int -> Int -> [Pos] -> [Pos]
seeking dr toi ci wi fri psls  
  | toi+dr == fri = []
  | otherwise = foldl (\acc (y,x,w,na) -> let (cn,wd) = coinCide (x,w) (ci,wi) in 
      if fri==y && (cn,wd)/=(0,0) then acc++[(y,cn,wd,na)] else acc) [] psls++
        seeking dr toi ci (wi+1) (fri+dr) psls

coinCide :: (Int, Int) -> (Int, Int) -> (Int, Int) 
coinCide (x,w) (x1,w1) =
  let s1 = [(x-w)..(x+w)]; s2 = [(x1-w1)..(x1+w1)]
      un = intersect s1 s2
   in if null un then (0,0) else let h = head un
                                     l = last un
                                  in (div (h+l) 2,div (l-h) 2)

seeToMes :: [Pos] -> String
seeToMes = concatMap (\(y,c,w,n) -> "dist: "++ show y++" dir: "
                     ++(if c>0 then "migi" else if c<0 then "hidari" else "manaka") 
                     ++show c++ " haba"++show w ++"---"++n++"\n")

ugoku :: Fun
ugoku _ [] _ st = (st{mes="No Direction"},Ki 0 0 0 0 0)
ugoku tg [T _ (Hou hus)] [] st 
  | tg==(-1) = (st{pl=(pl st){pdx=dlt}},Ki 0 1 0 0 0)
  | otherwise = (enUgoku tg (-dlt) 1 st,Ki 0 1 0 0 0)
  where (_,dlt) = calcDelta hus 1
ugoku tg [T _ (Hou hus)] [T _ (Kaz kz)] st
  | tg==(-1) = (st{pl=(pl st){pdx=dlt*sp}},Ki 0 1 0 0 sp)
  | otherwise = (enUgoku tg (-dlt) sp st,Ki 0 1 0 0 sp)
  where (sp,dlt) = calcDelta hus kz 
ugoku _ _ _ st = (st,Ki 0 0 0 0 0)

enUgoku :: Int -> Int -> Int -> State -> State
enUgoku tg dl sp st = 
  let ens' = ens st
      e = ens'!!tg
      nedx = dl*sp 
      ne = e{edx=nedx}
      nens = take tg ens' ++ [ne] ++ drop (tg+1) ens'
   in st{ens=nens}

nageru :: Fun
nageru _ [] _ st = (st{mes="No Tama"},Ki 0 0 0 0 0)
nageru tg [T _ (Tam tm)] t2 st =
  case t2 of
    []  -> nfun [] 1
    [T _ (Hou hus)] -> nfun hus 1
    [T _ (Kaz kz)] -> nfun [] kz
    [T _ (Hou hus),T _ (Kaz kz)] -> nfun hus kz
    [T _ (Kaz kz),T _ (Hou hus)] -> nfun hus kz
    _ -> (st,Ki 0 0 0 0 0)
  where dir = if tg==(-1) then 1 else (-1)
        mkb hus' kz' = makeBullets tm hus' (kz'*dir) (getPos tg st) 
        nfun hus' kz' = let mkb' = mkb hus' kz' in (mNage tg st{tms=tms st++fst mkb'},snd mkb')
nageru _ _ _ st = (st,Ki 0 0 0 0 0)

mNage :: Int -> State -> State
mNage (-1) st = showNage (-1) st{pl=(pl st){ing=True}}
mNage tg st = showNage tg st

showNage :: Int -> State -> State
showNage (-1) st = st{mes=mes st++"tama wo nageta!\n"}
showNage tg st = let e = ens st!!tg
                     ename = ena e
                  in st{mes=mes st++ename++" ga tama wo nageta!\n"}

makeBullets :: [(Bu,Int)] -> [(Dr,Int)] -> Int -> (Int, Int) -> ([Bul],Ki)
makeBullets [] _ _ _ = ([],Ki 0 0 0 0 0)
makeBullets ((b,s):bss) hus sp (y,x) = 
  let (dy, dx) = calcDelta hus sp
      nbmt 
        | dx==0 = 0
        | maxY>=dx = div maxY (abs dx)
        | otherwise = 1
      ndx
        | dx==0 = 0 
        | maxY>=dx = if dx>0 then 1 else (-1) 
        | otherwise = div dx maxY
      cs = case b of Ho -> Ki 0 s 0 0 (abs sp); Mi -> Ki 0 0 (abs sp) 0 s;
   in ((Bul{bt=b, bs=s, by=y, bx=x, bdy=dy, bdx=ndx, bmt=nbmt, btc=nbmt}):fst mkb
      ,addKi (snd mkb) cs)
  where mkb = makeBullets bss hus sp (y,x)

getPos :: Int -> State -> (Int, Int)
getPos (-1) st = let p = pl st in (py p, px p)
getPos tg st   = let e = ens st!!tg in (ey e, ex e)

calcDelta :: [(Dr,Int)] -> Int -> (Int, Int)
calcDelta hus sp = (sp, calcDx hus)
  where calcDx [] = 0
        calcDx ((dir,i):hs) 
          | dir==Mg = i + calcDx hs
          | dir==Hd = (-i) + calcDx hs
          | otherwise = calcDx hs
