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
import Brick.AttrMap (attrMap, attrName, AttrName)
import Brick.Types (Widget(..), EventM, BrickEvent(..), ViewportType(..))
import Brick.Widgets.Core (str, strWrap, (<+>), (<=>), hLimit, vLimit, viewport, withAttr, vBox)
import Brick.Widgets.Edit as E
import Brick.Util (fg)
import Brick.Widgets.Center as C
import Mydata(State,initstate)
import Myfunc(doWithTime,takeMes,takePtic,takePki,vhData)
import Mydous(exeCom)

data Name = Edit | View | Coma | Mess | Stat deriving (Ord, Show, Eq)

data CustomEvent = Ticking deriving Show

data St = St {_state :: State
             ,_vhdata :: [(String,String,String)]
             ,_pkiv :: Int
             ,_ptic :: Int
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
          vhd = st^.vhdata
          tic = st^.ptic
          kiv = st^.pkiv
          cm = viewport Coma Vertical c
          ms = viewport Mess Vertical m
          sm = viewport Stat Vertical s
          e1 = E.renderEditor (str.unlines) True (st^.edit)
          ui = C.center $
            (str "  ") <+> (
              ((str "Mes : " <+> (hLimit 40 $ vLimit 5 ms)) <=> 
              str " " <=>
              (str "Inp :> " <+> (hLimit 40 $ vLimit 3 e1)) <=>
              str " " <=>
              (str "Com : " <+> (withAttr atcm $ hLimit 40 $ vLimit 2 cm))) <+> 
                (vBox $ (widgetVH tic vhd)) <+>
                  (str " Ki : " <+> str (show kiv)) <=>
              str " " <=>
              (str "Log : "  <+> sm) <=>
              str " " <=>
              str "Esc to quit."
            ) <+> (str "  ")

atwa, athi, atcm :: AttrName
atwa = attrName "player"; athi = attrName "0"; atcm = attrName "command"


widgetVH :: Int -> [(String,String,String)] -> [Widget Name]
widgetVH _ [] = []
widgetVH _ [(_,t1,_)] = [(withAttr atwa $ str t1)] 
widgetVH i ((t0,t1,t2):xs) = ((withAttr athi $ str t0) <+>
                             (withAttr (attrName (show i)) $ str t1) <+>
                             (withAttr athi $ str t2)):(widgetVH i xs)

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
          vhdata .= vhData st
          ptic .= takePtic st
          pkiv .= takePki st
          if (st/=nst) then stlog .= show nst else return ()
          vScrollToEnd msScroll
        ev -> zoom edit $ E.handleEditorEvent ev 


initialState :: St
initialState = St { _state = initstate
                  , _ptic = 0
                  , _pkiv = 0
                  , _vhdata = []
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
        , appAttrMap = const $ attrMap V.defAttr (makeColors 20)
        }

makeColors :: Int -> [(AttrName, V.Attr)]
makeColors 0 = [(attrName "player", fg V.brightCyan)
               ,(attrName "0", fg V.black)
               ,(attrName "command", fg V.brightYellow)]
makeColors i = (attrName (show i), fg (V.rgbColor (i*10) (i*10) (i*10))):(makeColors (i-1))

appMain :: IO ()
appMain = do
    chan <- newBChan 1

    void $ forkIO $ forever $ do
        writeBChan chan Ticking 
        threadDelay 1000000

    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) theApp initialState

