module Mydous(exeCom,applyMana) where

import qualified Data.Map.Strict as M
import Data.List (intersect)
import Data.List.Split (splitOn)
import Mydata(T(..),Ta(..),Mana(..),State(..),Dr(..)
             ,Bu,Fun,Bul(..),Ply(..),Enm(..),Eai(..),(.>),toMana,maxY)

type Pos = (Int, Int, Int, String) -- y, x, width, name

exeCom :: String -> State -> State 
exeCom com s = let coms =  words com
                   manas = map toMana coms
                   res = foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) [] manas
                in if (res==[]) then s{mes=mes s++"ERROR!!\n"} 
                                else makeState s{pl=(pl s){ing=False},mns=[]} res

makeState :: State -> [Mana] -> State
makeState st [] = st
makeState st (mn:manas) = makeState (applyMana st mn) manas 

applyMana :: State -> Mana -> State
applyMana st m@(Mana (T na (Dou _ _ ts1 ts2)) _) = 
  let nam = head$words na
      nms = splitOn "*" nam
      tg = if (length nms==1) then (-1) else (read (last nms))::Int
      fnc = M.lookup (head nms) funcName
      st' = st{mns = [m]}
      (nst,cs) = case fnc of
                    Nothing -> (st',0)
                    Just f  -> f tg ts1 ts2 st'
      enms = ens nst
      tki = if (tg==(-1)) then pki$pl$nst else eki$enms!!tg
      nen = if (tg>=0) then (enms!!tg){eki=tki-cs} else enms!!0 
      nens = if (tg>=0) then take tg enms ++ [nen] ++ drop (tg+1) enms else enms
      icast = tki > cs
   in if icast then if (tg==(-1)) then nst{pl=(pl nst){pki=tki-cs}} else nst{ens=nens}
               else st'{mes=(mes st')++"not enough KI!\n"}
applyMana st m = st{mns= [m]}


funcName :: M.Map String Fun 
funcName = M.fromList [("nageru",nageru),("ugoku",ugoku),("miru",miru)]

miru :: Fun
miru tg [] [] st 
  | tg==(-1) = (st{mes=(mes st)++(seeToMes$lookingAt Ue (px$pl st) 1 1 (makePosLists st))},1)
  | otherwise = (enMiru tg 0 1 1 st, 1) 
miru tg [] ((T _ (Hou hus)):[]) st 
  | tg==(-1) = (st{mes=(mes st)++(seeToMes$lookingAt Ue dlt 3 1 (makePosLists st))},abs dlt)
  | otherwise = (enMiru tg (-dlt) 3 1 st, abs dlt)
  where (_,dlt) = calcDelta hus 1
miru _ _ _ st = (st,0)

enMiru :: Int -> Int -> Int -> Int -> State -> State
enMiru tg ci wi fi st =
  let ens' = ens st
      e = ens'!!tg
      nci = if (ci==0) then ex e else ci
      nplp = lookPlayer$lookingAt Si nci wi fi (makePosLists st)
      ne = e{eai=(eai e){plp=nplp}}
      nens = take tg ens' ++ [ne] ++ drop (tg+1) ens'
   in st{ens=nens}

lookPlayer :: [Pos] -> Maybe (Int, Int, Int)
lookPlayer [] = Nothing
lookPlayer ((y,x,w,"player"):_) = Just (y,x,w)
lookPlayer (_:ps) = lookPlayer ps

makePosLists :: State -> [Pos] 
makePosLists st = [(py$pl st, px$pl st, pw$pl st, "player")]++
                  (map (\(Enm ena' _ _ _ _ ey' ex' ew' _ _) -> (ey',ex',ew',ena')) (ens st))

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
ugoku _ [] _ st = (st{mes="No Direction"},0)
ugoku tg ((T _ (Hou hus)):[]) [] st 
  | tg==(-1) = (st{pl=(pl st){pdx=dlt}},abs dlt)
  | otherwise = (enUgoku tg (-dlt) 1 st,abs dlt)
  where (_,dlt) = calcDelta hus 1
ugoku tg ((T _ (Hou hus)):[]) ((T _ (Kaz kz)):[]) st
  | tg==(-1) = (st{pl=(pl st){pdx=dlt*sp}},(abs dlt)*sp)
  | otherwise = (enUgoku tg (-dlt) sp st,(abs dlt)*sp)
  where (sp,dlt) = calcDelta hus kz 
ugoku _ _ _ st = (st,0)

enUgoku :: Int -> Int -> Int -> State -> State
enUgoku tg dl sp st = 
  let ens' = ens st
      e = ens'!!tg
      nedx = dl*sp 
      ne = e{edx=nedx}
      nens = take tg ens' ++ [ne] ++ drop (tg+1) ens'
   in st{ens=nens}

nageru :: Fun
nageru _ [] _ st = (st{mes="No Tama"},0)
nageru tg ((T _ (Tam tm)):[]) t2 st =
  case t2 of
    []  -> nfun [] 1
    ((T _ (Hou hus)):[]) -> nfun hus 1
    ((T _ (Kaz kz)):[]) -> nfun [] kz
    ((T _ (Hou hus)):(T _ (Kaz kz)):[]) -> nfun hus kz
    ((T _ (Kaz kz)):(T _ (Hou hus)):[]) -> nfun hus kz
    _ -> (st,0)
  where dir = if(tg==(-1)) then 1 else (-1)
        mkb hus' kz' = makeBullets tm hus' (kz'*dir) (getPos tg st) 
        nfun hus' kz' = let mkb' = mkb hus' kz' in (mNage tg st{tms=(tms st)++(fst mkb')},snd mkb')
nageru _ _ _ st = (st,0)

mNage :: Int -> State -> State
mNage (-1) st = showNage (-1) st{pl=(pl st){ing=True}}
mNage tg st = showNage tg st

showNage :: Int -> State -> State
showNage (-1) st = st{mes=(mes st)++"tama wo nageta!\n"}
showNage tg st = let e = (ens st)!!tg
                     ename = ena e
                  in st{mes=(mes st)++ename++" ga tama wo nageta!\n"}

makeBullets :: [(Bu,Int)] -> [(Dr,Int)] -> Int -> (Int, Int) -> ([Bul],Int)
makeBullets [] _ _ _ = ([],0)
makeBullets ((b,s):bss) hus sp (y,x) = 
  let (dy, dx) = calcDelta hus sp
      nbmt = if (dx==0) then 0 else if (maxY>=dx) then div maxY dx else 1
      ndx = if (dx==0) then 0 else if(maxY>=dx) then 1 else div dx maxY
   in ((Bul{bt=b, bs=s, by=y, bx=x, bdy=dy, bdx=ndx, bmt=nbmt, btc=nbmt}):(fst mkb),(s*(abs sp))+(snd mkb))
  where mkb = makeBullets bss hus sp (y,x)

getPos :: Int -> State -> (Int, Int)
getPos (-1) st = let p = pl st 
                  in (py p, px p)
getPos tg st   = let e = (ens st)!!tg
                  in (ey e, ex e)

calcDelta :: [(Dr,Int)] -> Int -> (Int, Int)
calcDelta hus sp = (sp, calcDx hus)
  where calcDx [] = 0
        calcDx ((dir,i):hs) 
          | dir==Mg = i + calcDx hs
          | dir==Hd = (-i) + calcDx hs
          | otherwise = calcDx hs
