{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Myapp(appMain,state) where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((%=),(.=),zoom,use)
import Control.Monad (void,forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay,forkIO)
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Brick.Main (App(..), showFirstCursor, customMain, halt
                 ,vScrollToEnd, ViewportScroll, viewportScroll)
import Brick.AttrMap (attrMap, attrName)
import Brick.Types (Widget(..), EventM, BrickEvent(..), ViewportType(..))
import Brick.Widgets.Core (str, strWrap, (<+>), (<=>), hLimit, vLimit, viewport, withAttr)
import Brick.Widgets.Edit as E
import Brick.Util (fg)
import Brick.Widgets.Center as C
import Mydata(State,initstate)
import Myfunc(doWithTime,takeMes,plyView)
import Mydous(exeCom)

data Name = Edit | View | Coma | Mess | Stat deriving (Ord, Show, Eq)

data CustomEvent = Ticking deriving Show

data St = St {_state :: State
             ,_plview :: String
             ,_stlog :: String
             ,_cmlog :: String
             ,_mslog :: String
             ,_edit :: E.Editor String Name
             }

makeLenses ''St

drawUI :: St -> [Widget Name]
drawUI st = [ui]
    where s = (strWrap $ st^.stlog)
          c = (strWrap $ st^.cmlog)
          m = (strWrap $ st^.mslog)
          v = (strWrap $ st^.plview)
          cm = viewport Coma Vertical c
          ms = viewport Mess Vertical m
          sm = viewport Stat Vertical s
          vw = viewport View Vertical v
          e1 = E.renderEditor (str.unlines) True (st^.edit)
          ui = C.center $
            (str "Mes : " <+> (hLimit 40 $ vLimit 5 ms)) <+> 
            (str "View : " <+> (withAttr (attrName "watching") $ hLimit 30 $ vLimit 11 vw)) <=>
            str " " <=>
            (str "Com : " <+> (hLimit 40 $ vLimit 3 cm)) <=>
            str " " <=>
            (str "Inp :> " <+> (hLimit 60 $ vLimit 3 e1)) <=>
            str " " <=>
            (str "Log : "  <+> (hLimit 100 $ vLimit 8 sm)) <=>
            str " " <=>
            str "Esc to quit."

stScroll :: ViewportScroll Name
stScroll = viewportScroll Stat 

cmScroll :: ViewportScroll Name
cmScroll = viewportScroll Coma

msScroll :: ViewportScroll Name
msScroll = viewportScroll Mess 

appEvent :: BrickEvent Name CustomEvent -> EventM Name St ()
appEvent e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent (V.EvKey V.KEnter []) -> do
          st <- use state
          ed <- use edit
          let con = E.getEditContents ed
              com = unlines con
              nst = exeCom com st
          state .= nst
          mslog .= takeMes nst
          cmlog %= (++com)
          edit .= E.editor Edit (Just 1) ""
          vScrollToEnd cmScroll
          vScrollToEnd msScroll
        AppEvent Ticking -> do
          st <- use state 
          nst <- liftIO$doWithTime st
          state .= nst 
          mslog .= takeMes st
          plview .= plyView st
          if (st/=nst) then stlog %= (++(show nst)++"\n") else return ()
          vScrollToEnd stScroll
          vScrollToEnd msScroll
        ev -> zoom edit $ E.handleEditorEvent ev 


initialState :: St
initialState = St { _state = initstate
                  , _plview = ""
                  , _stlog = show initstate 
                  , _cmlog = ""
                  , _mslog = ""
                  , _edit = E.editor Edit (Just 1) ""
                  }

theApp :: App St CustomEvent Name 
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr 
                  [ (attrName "watching", fg V.cyan)
                  , (attrName "cantsee" , fg V.red)]
        }

appMain :: IO ()
appMain = do
    chan <- newBChan 1

    void $ forkIO $ forever $ do
        writeBChan chan Ticking 
        threadDelay 1000000

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) theApp initialState

