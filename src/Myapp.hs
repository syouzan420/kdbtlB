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
import Brick.AttrMap (attrMap)
import Brick.Types (Widget(..), EventM, BrickEvent(..), ViewportType(..))
import Brick.Widgets.Core (str, strWrap, (<+>), (<=>), hLimit, vLimit, viewport)
import Brick.Widgets.Edit as E
import Brick.Widgets.Center as C
import Mydata(State(..),initstate)
import Myfunc(doWithTime)
import Mydous(exeCom)

data Name = Edit | View | Coma | Mess deriving (Ord, Show, Eq)

data CustomEvent = Logstate deriving Show

data St = St {_state :: State
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
          cm = viewport Coma Vertical c
          ms = viewport Mess Vertical m
          v = viewport View Vertical s
          e1 = E.renderEditor (str.unlines) True (st^.edit)
          ui = C.center $
            (str "Mes : " <+> (hLimit 60 $ vLimit 5 ms)) <=>
            str " " <=>
            (str "Com : " <+> (hLimit 60 $ vLimit 3 cm)) <=>
            str " " <=>
            (str "Inp :> " <+> (hLimit 60 $ vLimit 3 e1)) <=>
            str " " <=>
            (str "Log : "  <+> (hLimit 100 $ vLimit 8 v)) <=>
            str " " <=>
            str "Esc to quit."

vpScroll :: ViewportScroll Name
vpScroll = viewportScroll View

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
          mslog .= mes nst
          cmlog %= (++com)
          edit .= E.editor Edit (Just 1) ""
          vScrollToEnd cmScroll
          vScrollToEnd msScroll
        AppEvent Logstate -> do
          st <- use state 
          nst <- liftIO$doWithTime st
          state .= nst 
          mslog .= mes st
          if (st/=nst) then stlog %= (++(show nst)++"\n") else return ()
          vScrollToEnd vpScroll
          vScrollToEnd msScroll
        ev -> zoom edit $ E.handleEditorEvent ev 


initialState :: St
initialState = St { _state = initstate
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
        , appAttrMap = const $ attrMap V.defAttr []
        }

appMain :: IO ()
appMain = do
    chan <- newBChan 1

    void $ forkIO $ forever $ do
        writeBChan chan Logstate 
        threadDelay 3000000

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) theApp initialState

