{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Myapp(appMain,state) where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((%=),(.=),zoom,use)
import Control.Monad (void,forever)
import Control.Concurrent (threadDelay,forkIO)
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (App(..), showFirstCursor, customMain, halt
                 ,vScrollToEnd, ViewportScroll, viewportScroll)
import Brick.AttrMap (attrMap)
import Brick.Types (Widget(..), EventM, BrickEvent(..), ViewportType(..))
import Brick.Widgets.Core (str, strWrap, (<+>), (<=>), hLimit, vLimit, viewport)
import Brick.Widgets.Edit as E
import Brick.Widgets.Center as C
import Mydata(State(..), Mana, Ply(..), Enm(..), Bul(..), Mes(..)
             ,toMana, initstate, applyMana, (.>), maxY)

data Name = Edit | View | Com deriving (Ord, Show, Eq)

data CustomEvent = Logstate deriving Show

data St = St {_state :: State
             ,_stlog :: String
             ,_cmlog :: String
             ,_edit :: E.Editor String Name
             }

makeLenses ''St

drawUI :: St -> [Widget Name]
drawUI st = [ui]
    where a = (strWrap $ st^.stlog)
          c = (strWrap $ st^.cmlog)
          cm = viewport Com Vertical c
          v = viewport View Vertical a
          e1 = E.renderEditor (str.unlines) True (st^.edit)
          ui = C.center $
            (str "Com : " <+> (hLimit 60 $ vLimit 5 cm)) <=>
            str " " <=>
            (str "Inp :> " <+> (hLimit 60 $ vLimit 3 e1)) <=>
            str " " <=>
            (str "Log : "  <+> (hLimit 100 $ vLimit 7 v)) <=>
            str " " <=>
            str "Esc to quit."

vpScroll :: ViewportScroll Name
vpScroll = viewportScroll View

cmScroll :: ViewportScroll Name
cmScroll = viewportScroll Com

appEvent :: BrickEvent Name CustomEvent -> EventM Name St ()
appEvent e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent (V.EvKey V.KEnter []) -> do
          st <- use state
          ed <- use edit
          let con = E.getEditContents ed
              com = unlines con
              s' = exeCom com st
          case s' of
            Nothing -> stlog %= (++"Error!")
            Just js -> state .= js
          cmlog %= (++com)
          edit .= E.editor Edit (Just 1) ""
          vScrollToEnd cmScroll
        AppEvent Logstate -> do
          st <- use state 
          let nst = doWithTime st
          state .= nst 
          if (st/=nst) then stlog %= (++(show nst)++"\n") else return ()
          vScrollToEnd vpScroll
        ev -> zoom edit $ E.handleEditorEvent ev 


initialState :: St
initialState = St { _state = initstate
                  , _stlog = show initstate 
                  , _cmlog = ""
                  , _edit = E.editor Edit (Just 1) ""
                  }

theApp :: App St CustomEvent Name 
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

appMain :: IO ()
appMain = do
    chan <- newBChan 1

    void $ forkIO $ forever $ do
        writeBChan chan Logstate 
        threadDelay 500000

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) theApp initialState



exeCom :: String -> State -> Maybe State 
exeCom com s = let coms =  words com
                   manas = map toMana coms
                   res = foldl (\acc mn -> case mn of Just m' -> acc .> m'; _ -> acc) [] manas
                in if (res==[]) then Nothing else Just (makeState s{mns=[]} res)

makeState :: State -> [Mana] -> State
makeState st [] = st
makeState st (mn:manas) = makeState (applyMana st mn) manas 

doWithTime :: State -> State 
doWithTime (State p es ts s ms manas) =
  let (ms',np,ts1) = changePly ms p ts []
      (ms'',nes,ts2) = changeEnms ms' es ts1 [] []
      (ms''',ts3) = changeBuls ms'' ts2 []
   in State np nes ts3 s ms''' manas

changePly :: Mes -> Ply -> [Bul] -> [Bul] -> (Mes,Ply,[Bul])
changePly m p [] bls = (m, normalPly p, bls)
changePly (Mes m) p@(Ply pki' pmki' prt' pmrt' py' px0' px1' pdx') (b@(Bul _ bs' by' bx' bdy' _):bss) bls =
  if (bdy'<0 && by'<=py' && bx'>=px0' && bx'<=px1') 
     then let npki = pki' - bs'
           in if (npki > 0)
                 then changePly (Mes (m++"\nattacked!"))
                        (Ply (pki'-bs') pmki' prt' pmrt' py' (px0'+dr) (px1'+dr) (pdx'-dr)) bss bls
                 else (Mes (m++"\nlose!"), Ply 0 pmki' prt' pmrt' py' px0' px1' 0, [])
     else changePly (Mes m) (normalPly p) bss (bls++[b])
                          where dr=if(pdx'>0) then 1 else if(pdx'<0) then (-1) else 0

normalPly :: Ply -> Ply
normalPly p@(Ply pki' pmki' prt' pmrt' py' px0' px1' pdx') =
  if(pdx'==0) then
    if(prt'<0 && pki'<pmki') then Ply (pki'+1) pmki' pmrt' pmrt' py' px0' px1' pdx'
                             else if(pki'<pmki') then Ply pki' pmki' (prt'-1) pmrt' py' px0' px1' pdx'
                                                 else p
              else Ply pki' pmki' prt' pmrt' py' (px0'+dr) (px1'+dr) (pdx'-dr) 
                where dr=if(pdx'>0) then 1 else (-1)
  
changeEnms :: Mes -> [Enm] -> [Bul] -> [Enm] -> [Bul] -> (Mes,[Enm],[Bul])
changeEnms m [] bls enms _ = (m, enms, bls)
changeEnms m (e:es) [] enms [] = changeEnms m es [] (enms++[normalEnm e]) []
changeEnms m (e:es) [] enms bls = changeEnms m es bls (enms++[e]) []
changeEnms (Mes m) (e@(Enm eki' emki' ert' emrt' ey' ex0' ex1' edx'):es)
                    (b@(Bul _ bs' by' bx' bdy' _):bss) enms bls =
  if (bdy'>0 && by'>=ey' && bx'>=ex0' && bx'<=ex1') 
     then let neki = eki' - bs'
           in if (neki > 0) 
                 then changeEnms (Mes (m++"\nhit!"))
                    ((Enm (eki'-bs') emki' ert' emrt' ey' (ex0'+dr) (ex1'+dr) (edx'-dr)):es) bss enms bls
                 else changeEnms (Mes (m++"\ndefeat!")) es bss enms bls
     else changeEnms (Mes m) ((normalEnm e):es) bss enms (bls++[b])
                          where dr=if(edx'>0) then 1 else if(edx'<0) then (-1) else 0

normalEnm :: Enm -> Enm 
normalEnm e@(Enm eki' emki' ert' emrt' ey' ex0' ex1' edx') =
  if(edx'==0) then
    if(ert'<0 && eki'<emki') then Enm (eki'+1) emki' emrt' emrt' ey' ex0' ex1' edx'
                             else if(eki'<emki') then Enm eki' emki' (ert'-1) emrt' ey' ex0' ex1' edx'
                                                 else e
              else Enm eki' emki' ert' emrt' ey' (ex0'+dr) (ex1'+dr) (edx'-dr) 
                where dr=if(edx'>0) then 1 else (-1)

changeBuls :: Mes -> [Bul] -> [Bul] -> (Mes,[Bul])
changeBuls m [] bls = (m,bls) 
changeBuls (Mes m) ((Bul bt' bs' by' bx' bdy' bdx'):bss) bls =
  let nby = by'+bdy'
   in if (nby>(maxY+bs') || nby<(0-bs'))
         then changeBuls (Mes m) bss bls
         else changeBuls (Mes m) bss (bls++[Bul bt' bs' nby (bx'+bdx') bdy' bdx'])
  


