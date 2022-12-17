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
import Mydata(State(..), initstate)
import Myfunc(exeCom, doWithTime)

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
            (str "Log : "  <+> (hLimit 100 $ vLimit 8 v)) <=>
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

