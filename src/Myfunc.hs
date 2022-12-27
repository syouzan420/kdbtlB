module Myfunc(doWithTime) where

import System.Random(randomRIO)
import Mydata(State(..), Ply(..), Enm(..), Bul(..), Mes, maxY)
import Myenai(enmAi)

doWithTime :: State -> IO State 
doWithTime (State p es ts ms manas) = do
  pr <- randomRIO (0::Int,99)
  let ipn = ing p
      (ms_p,np,ts_p) = changePly ms p ts []
      (ms_a,es_a) = enmAi ms_p ipn pr np es []
      (ms_e,nes,ts_e) = changeEnms ms_a es_a ts_p [] []
      (nms,nts) = changeBuls ms_e ts_e []
  return (State np nes nts nms manas)

changePly :: Mes -> Ply -> [Bul] -> [Bul] -> (Mes,Ply,[Bul])
changePly m p [] bls = (m, normalPly p, bls)
changePly m p@(Ply pki' _ _ _ py' px' pw' pdx' _) (b@(Bul _ bs' by' bx' bdy' _):bss) bls =
  if (bdy'<0 && by'<=py' && bx'>=px'-pw' && bx'<=px'+pw') 
     then let npki = pki' - bs'
           in if (npki > 0)
                 then changePly (m++"attacked!\n")
                        (p{pki=pki'-bs',px=px'+dr,pdx=pdx'-dr}) bss bls
                 else (m++"lose!\n", p{pki=0,pdx=0}, [])
     else changePly m p bss (bls++[b])
                          where dr=if(pdx'>0) then 1 else if(pdx'<0) then (-1) else 0

normalPly :: Ply -> Ply
normalPly p@(Ply pki' pmki' prt' pmrt' _ px' _ pdx' _) =
  if(pdx'==0) then
    if(prt'<0 && pki'<pmki') then p{pki=pki'+1,prt=pmrt'}
                             else if(pki'<pmki') then p{prt=prt'-1} else p
              else p{px=px'+dr, pdx=pdx'-dr} 
                where dr=if(pdx'>0) then 1 else (-1)

  
changeEnms :: Mes -> [Enm] -> [Bul] -> [Enm] -> [Bul] -> (Mes,[Enm],[Bul])
changeEnms m [] bls enms _ = (m, enms, bls)
changeEnms m (e:es) [] enms [] = changeEnms m es [] (enms++[normalEnm e]) []
changeEnms m (e:es) [] enms bls = changeEnms m es bls (enms++[e]) []
changeEnms m (e@(Enm ena' eki' _ _ _ ey' ex' ew' edx' _):es) (b@(Bul _ bs' by' bx' bdy' _):bss) enms bls =
  if (bdy'>0 && by'>=ey' && bx'>=ex'-ew' && bx'<=ex'+ew') 
     then let neki = eki' - bs'
           in if (neki > 0) 
                 then changeEnms (m++ena'++"--hit!\n")
                    (e{eki=eki'-bs',ex=ex'+dr,edx=edx'-dr}:es) bss enms bls
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
changeBuls m (b@(Bul _ bs' by' bx' bdy' bdx'):bss) bls =
  let nby = by'+bdy'
   in if (nby>(maxY+bs') || nby<(0-bs'))
         then changeBuls m bss bls
         else changeBuls m bss (bls++[b{by=nby,bx=bx'+bdx'}])


