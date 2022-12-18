module Myfunc(exeCom, doWithTime) where

import Mydata(State(..), Mana, Ply(..), Enm(..), Bul(..), Mes
             ,toMana, applyMana, (.>), maxY)

exeCom :: String -> State -> Maybe State 
exeCom com s = let coms =  words com
                   manas = map toMana coms
                   res = foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) [] manas
                in if (res==[]) then Nothing else Just (makeState s{mns=[]} res)

makeState :: State -> [Mana] -> State
makeState st [] = st
makeState st (mn:manas) = makeState (applyMana st mn) manas 

doWithTime :: State -> State 
doWithTime (State p es ts ms manas) =
  let (ms',np,ts1) = changePly ms p ts []
      (ms'',nes,ts2) = changeEnms ms' es ts1 [] []
      (ms''',ts3) = changeBuls ms'' ts2 []
   in State np nes ts3 ms''' manas

changePly :: Mes -> Ply -> [Bul] -> [Bul] -> (Mes,Ply,[Bul])
changePly m p [] bls = (m, normalPly p, bls)
changePly m p@(Ply pki' _ _ _ py' px0' px1' pdx') (b@(Bul _ bs' by' bx' bdy' _):bss) bls =
  if (bdy'<0 && by'<=py' && bx'>=px0' && bx'<=px1') 
     then let npki = pki' - bs'
           in if (npki > 0)
                 then changePly (m++"attacked!\n")
                        (p{pki=pki'-bs',px0=px0'+dr,px1=px1'+dr,pdx=pdx'-dr}) bss bls
                 else (m++"lose!\n", p{pki=0,pdx=0}, [])
     else changePly m p bss (bls++[b])
                          where dr=if(pdx'>0) then 1 else if(pdx'<0) then (-1) else 0

normalPly :: Ply -> Ply
normalPly p@(Ply pki' pmki' prt' pmrt' _ px0' px1' pdx') =
  if(pdx'==0) then
    if(prt'<0 && pki'<pmki') then p{pki=pki'+1,prt=pmrt'}
                             else if(pki'<pmki') then p{prt=prt'-1} else p
              else p{px0=px0'+dr, px1=px1'+dr, pdx=pdx'-dr} 
                where dr=if(pdx'>0) then 1 else (-1)

  
changeEnms :: Mes -> [Enm] -> [Bul] -> [Enm] -> [Bul] -> (Mes,[Enm],[Bul])
changeEnms m [] bls enms _ = (m, enms, bls)
changeEnms m (e:es) [] enms [] = changeEnms m es [] (enms++[normalEnm e]) []
changeEnms m (e:es) [] enms bls = changeEnms m es bls (enms++[e]) []
changeEnms m (e@(Enm ena' eki' _ _ _ ey' ex0' ex1' edx'):es) (b@(Bul _ bs' by' bx' bdy' _):bss) enms bls =
  if (bdy'>0 && by'>=ey' && bx'>=ex0' && bx'<=ex1') 
     then let neki = eki' - bs'
           in if (neki > 0) 
                 then changeEnms (m++ena'++"--hit!\n")
                    (e{eki=eki'-bs',ex0=ex0'+dr,ex1=ex1'+dr,edx=edx'-dr}:es) bss enms bls
                 else changeEnms (m++ena'++"--defeat!\n") es bss enms bls
     else changeEnms m (e:es) bss enms (bls++[b])
                          where dr=if(edx'>0) then 1 else if(edx'<0) then (-1) else 0

normalEnm :: Enm -> Enm 
normalEnm e@(Enm _ eki' emki' ert' emrt' _ ex0' ex1' edx') =
  if(edx'==0) then
    if(ert'<0 && eki'<emki') then e{eki=eki'+1,ert=emrt'}
                             else if(eki'<emki') then e{ert=ert'-1} else e
              else e{ex0=ex0'+dr,ex1=ex1'+dr,edx=edx'-dr} 
                where dr=if(edx'>0) then 1 else (-1)

changeBuls :: Mes -> [Bul] -> [Bul] -> (Mes,[Bul])
changeBuls m [] bls = (m,bls) 
changeBuls m (b@(Bul _ bs' by' bx' bdy' bdx'):bss) bls =
  let nby = by'+bdy'
   in if (nby>(maxY+bs') || nby<(0-bs'))
         then changeBuls m bss bls
         else changeBuls m bss (bls++[b{by=nby,bx=bx'+bdx'}])


