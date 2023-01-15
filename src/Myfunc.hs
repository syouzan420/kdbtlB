module Myfunc(doWithTime,takeMes,plyView) where

import System.Random(randomRIO)
import Mydata(State(..), Mana, Ply(..), Enm(..), Bul(..), Mes, minX, maxX, maxY)
import Mydous(applyMana)
import Myenai(enmAi,enTick)

plyView :: State -> String
plyView st = let es = ens st
                 ts = tms st
              in plyViewLine maxY es ts

plyViewLine :: Int -> [Enm] -> [Bul] -> String
plyViewLine 0 _ _ = ""
plyViewLine i es ts = let enxs = getxEn i es 
                          ewxs = getxEw i es
                          tmxs = getxTm i ts
                       in (mkViewLine enxs ewxs tmxs) ++ "\n" ++ (plyViewLine (i-1) es ts)

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

mkViewLine :: [Int] -> [Int] -> [Int] -> String
mkViewLine enxs ewxs tmxs = 
  let stl = maxX - minX + 1
   in scanLine 't' minX tmxs $ scanLine '-' minX ewxs $ scanLine 'e' minX enxs $ replicate stl ' '

scanLine :: Char -> Int -> [Int] -> String -> String
scanLine _ _ _ [] = ""
scanLine ch i xs (c:cs) = if (elem i xs) then [ch]++(scanLine ch (i+1) xs cs)
                                         else [c]++(scanLine ch (i+1) xs cs)

takeMes :: State -> String
takeMes st = mes st

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
changePly m p@(Ply pki' _ _ _ py' px' pw' pdx' _ _) (b@(Bul _ bs' by' bx' bdy' _ _ _):bss) bls =
  if (bdy'<0 && by'<=py' && bx'>=px'-pw' && bx'<=px'+pw') 
     then let npki = pki' - bs'
           in if (npki > 0)
                 then changePly (m++"attacked!\n")
                        (p{pki=pki'-bs'*2,px=px'+dr,pdx=pdx'-dr}) bss bls
                 else (m++"lose!\n", p{pki=0,pdx=0}, [])
     else changePly m p bss (bls++[b])
                          where dr=if(pdx'>0) then 1 else if(pdx'<0) then (-1) else 0

normalPly :: Ply -> Ply
normalPly p@(Ply pki' pmki' prt' pmrt' _ px' _ pdx' _ _) =
  if(pdx'==0) then
    if(prt'<0 && pki'<pmki') then p{pki=pki'+1,prt=pmrt'}
                             else if(pki'<pmki') then p{prt=prt'-1} else p
              else p{px=px'+dr, pdx=pdx'-dr} 
                where dr=if(pdx'>0) then 1 else (-1)

  
changeEnms :: Mes -> [Enm] -> [Bul] -> [Enm] -> [Bul] -> (Mes,[Enm],[Bul])
changeEnms m [] bls enms _ = (m, enms, bls)
changeEnms m (e:es) [] enms [] = changeEnms m es [] (enms++[normalEnm e]) []
changeEnms m (e:es) [] enms bls = changeEnms m es bls (enms++[e]) []
changeEnms m (e@(Enm ena' eki' _ _ _ ey' ex' ew' edx' _):es) 
             (b@(Bul _ bs' by' bx' bdy' _ _ _):bss) enms bls =
  if (bdy'>0 && by'>=ey' && bx'>=ex'-ew' && bx'<=ex'+ew') 
     then let neki = eki' - bs'
           in if (neki > 0) 
                 then changeEnms (m++ena'++"--hit!\n")
                    (e{eki=eki'-bs'*2,ex=ex'+dr,edx=edx'-dr}:es) bss enms bls
                 else changeEnms (m++ena'++"--defeat!\n") es bss enms bls
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


