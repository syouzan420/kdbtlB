module Myfunc(doWithTime,takeMes,takePtic,takePki,vhData) where

import System.Random(randomRIO)
import Mydata(State(..), Mana, Ply(..), Enm(..), Bul(..), Mes, minX, maxX, maxY)
import Mydous(applyMana)
import Myenai(enmAi,enTick)

vhData :: State -> [(String,String,String)]
vhData st = let p = pl st
                xwl = look p
                stl = plyView st
             in (if(xwl==[]) then hLines stl else vhLines xwl stl)
                ++[("",mkPlyLine (px p) (getxPw p),"")]

hLines :: [String] -> [(String,String,String)]
hLines strl = map (\s -> (s,"","")) strl

vhLines :: [(Int,Int)] -> [String] -> [(String,String,String)]
vhLines [] _ = []
vhLines _ [] = []
vhLines ((x,w):vs) (str:xs) =
  let sln = length str
      ihp = x - w - minX
      ih = if(ihp<0) then 0 else ihp
      ilp = ihp + 2 * w
      il = if(ilp>sln+1) then sln else ilp
      t0 = take ih str
      t1 = drop ih (take (il+1) str)
      t2 = drop (il+1) str
   in (t0,t1,t2):(vhLines vs xs)

plyView :: State -> [String]
plyView st = plyViewLine maxY es ts
    where es = ens st; ts = tms st

plyViewLine :: Int -> [Enm] -> [Bul] -> [String]
plyViewLine 0 _ _ = [] 
plyViewLine i es ts = (mkViewLine enxs ewxs tmxs):(plyViewLine (i-1) es ts)
  where enxs = getxEn i es; ewxs = getxEw i es; tmxs = getxTm i ts

getxEn :: Int -> [Enm] -> [Int]
getxEn _ [] = []
getxEn y (e:es) = if(y==ey e) then (ex e):(getxEn y es) else getxEn y es 

getxTm :: Int -> [Bul] -> [Int]
getxTm _ [] = []
getxTm y (b:bss) = if(y==by b) then (bx b):(getxTm y bss) else getxTm y bss

getxEw :: Int -> [Enm] -> [Int]
getxEw _ [] = []
getxEw y (e:es) = let w = ew e; x = ex e; y' = ey e
                   in if(y==y') then (if(w==0) then [] else [(x-w)..(x-1)]++[(x+1)..(x+w)])++(getxEw y es)
                                else getxEw y es

getxPw :: Ply -> [Int]
getxPw p = let w = pw p; x = px p
            in if(w==0) then [] else [(x-w)..(x-1)]++[(x+1)..(x+w)]

mkViewLine :: [Int] -> [Int] -> [Int] -> String
mkViewLine enxs ewxs tmxs = 
  let stl = maxX - minX + 1
   in scanLine 't' minX tmxs $ scanLine '-' minX ewxs $ scanLine 'e' minX enxs $ replicate stl ' '

mkPlyLine :: Int -> [Int] -> String
mkPlyLine px' pwxs =
  let stl = maxX - minX + 1
   in scanLine '=' minX pwxs $ scanLine 'p' minX [px'] $ replicate stl ' '

scanLine :: Char -> Int -> [Int] -> String -> String
scanLine _ _ _ [] = ""
scanLine ch i xs (c:cs) = if (elem i xs) then [ch]++(scanLine ch (i+1) xs cs)
                                         else [c]++(scanLine ch (i+1) xs cs)

takeMes :: State -> String
takeMes st = mes st

takePtic :: State -> Int
takePtic st = ltc$pl st

takePki :: State -> Int
takePki st = pki$pl st

doWithTime :: State -> IO State 
doWithTime st@(State p es ts ms _) = do
  prs <- sequence$replicate (length es) (randomRIO (0::Int,99))
  let (ms_p,np,ts_p) = changePly ms p ts []
      (ms_e,es_e,ts_e) = changeEnms ms_p es ts_p [] []
      (ms_b,nts) = changeBuls ms_e ts_e []
      (nms,eman) = enmAi ms_b (ing np) prs es_e 0 []
      nes = enTick (ing np) es_e
      np' = np{ing=False}
      nms' = shortenMes nms 
      nst = if(pki (pl st)==0) then st else makeMState st{pl=np',ens=nes,tms=nts,mes=nms'} eman 
  return nst

shortenMes :: String -> String 
shortenMes ms = let lms = lines ms
                    lnms = length lms
                 in if(lnms>6) then unlines$drop (lnms-6) lms else unlines lms

makeMState :: State -> [Maybe Mana] -> State
makeMState st [] = st
makeMState st (mn:manas) = let nst = case mn of Nothing -> st; Just jm -> applyMana st jm
                            in makeMState nst manas

changePly :: Mes -> Ply -> [Bul] -> [Bul] -> (Mes,Ply,[Bul])
changePly m p [] bls = (m, normalPly p, bls)
changePly m p@(Ply pki' _ _ _ py' px' pw' pdx' _ _ _) (b@(Bul _ bs' by' bx' bdy' _ _ _):bss) bls =
  if (bdy'<0 && by'<=py' && bx'>=px'-pw' && bx'<=px'+pw') 
     then let npki = pki' - bs'*2
              ilose = npki <= 0
           in if ilose then (m++"you lose!\n", p{pki=0,pdx=0}, [])
                       else changePly (m++"attacked!\n")
                              (p{pki=npki,px=px'+dr,pdx=pdx'-dr}) bss bls
     else changePly m p bss (bls++[b])
                          where dr=if(pdx'>0) then 1 else if(pdx'<0) then (-1) else 0

normalPly :: Ply -> Ply
normalPly p@(Ply pki' pmki' prt' pmrt' _ px' _ pdx' _ look' ltc') =
  if(pdx'==0) then
    if(prt'<0 && pki'<pmki') then np{pki=pki'+1,prt=pmrt'}
                             else if(pki'<pmki') then np{prt=prt'-1} else np
              else np{px=px'+dr, pdx=pdx'-dr} 
                where dr=if(pdx'>0) then 1 else (-1)
                      nlook = if(ltc'==0) then [] else look'
                      nltc = if(ltc'/=0) then ltc'-1 else 0
                      np = p{look=nlook, ltc=nltc}

  
changeEnms :: Mes -> [Enm] -> [Bul] -> [Enm] -> [Bul] -> (Mes,[Enm],[Bul])
changeEnms m [] bls enms _ = (m, enms, bls)
changeEnms m (e:es) [] enms [] = changeEnms m es [] (enms++[normalEnm e]) []
changeEnms m (e:es) [] enms bls = changeEnms m es bls (enms++[e]) []
changeEnms m (e@(Enm ena' eki' _ _ _ ey' ex' ew' edx' _):es) 
             (b@(Bul _ bs' by' bx' bdy' _ _ _):bss) enms bls =
  if (bdy'>0 && by'>=ey' && bx'>=ex'-ew' && bx'<=ex'+ew') 
     then let neki = eki' - bs'*2
              idef = neki <= 0
           in if idef then changeEnms (m++ena'++"--defeat!\n") es bss enms bls
                      else changeEnms (m++ena'++"--hit!\n")
                            (e{eki=neki,ex=ex'+dr,edx=edx'-dr}:es) bss enms bls
     else changeEnms m (e:es) bss enms (bls++[b])
                          where dr=if(edx'>0) then 1 else if(edx'<0) then (-1) else 0

normalEnm :: Enm -> Enm 
normalEnm e@(Enm _ eki' emki' ert' emrt' _ ex' _ edx' _) =
  if(edx'==0) then
    if(ert'<0 && eki'<emki') then e{eki=eki'+1,ert=emrt'}
                             else if(eki'<emki') then e{ert=ert'-1} else e
              else e{ex=ex'+dr,edx=edx'-dr} 
                where dr=if(edx'>0) then 1 else (-1)

changeBuls :: Mes -> [Bul] -> [Bul] -> (Mes,[Bul])
changeBuls m [] bls = (m,bls) 
changeBuls m (b@(Bul _ bs' by' bx' bdy' bdx' bmt' btc'):bss) bls =
  let nby = by'+bdy'
      nbtc = if (btc'==0) then 0 else btc'-1
      nbx = if (nbtc==0) then bx'+bdx' else bx'
      nbtc' = if(nbtc==0) then bmt' else nbtc
   in if (nby>(maxY+bs') || nby<(0-bs'))
         then changeBuls m bss bls
         else changeBuls m bss (bls++[b{by=nby,bx=nbx,btc=nbtc'}])


