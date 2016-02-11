{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class
import Data.ByteString.Char8
import Data.JSString
import Data.Monoid ((<>))
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.ClientRect
import GHCJS.DOM.Element
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.Types hiding (Event)
import GHCJS.Marshal (fromJSVal)
import GHCJS.Types
import GHCJS.DOM.Enums
import Reflex
import Reflex.Dom hiding (restore)

data DrawingAreaConfig t = DrawingAreaConfig
  { _drawingAreaConfig_clear  :: Event t ()
  , _drawingAreaConfig_radius :: Behavior t Double
  , _drawingAreaConfig_color  :: Behavior t String
  -- , _drawingAreaConfig_mouse  :: Dynamic t (Maybe (Double,Double))
  , _drawingAreaConfig_paint  :: Event t ()
  , _drawingAreaConfig_erase  :: Event t ()
  , _drawingAreaConfig_undo   :: Event t ()
  , _drawingAreaConfig_send   :: Event t ()
  }

data DrawingArea t = DrawingArea
  { _drawingArea_el    :: El t
  , _drawingArea_image :: Event t ByteString }

canvH, canvW :: Int
canvW = 200
canvH = 128


drawingArea :: MonadWidget t m => DrawingAreaConfig t -> m (DrawingArea t)
drawingArea cfg = do
  (cEl,_) <- elAttr' "canvas" ("id" =: "canvas"
                      <> "width"  =: show canvW
                      <> "height" =: show canvH) $ return ()

  let canvEl = (castToHTMLCanvasElement . _el_element) cEl

  Just ctx <- liftIO $ fromJSVal =<< getContext canvEl ("2d" :: JSString)
  performEvent_ $ (const $ liftIO (clearArea ctx canvEl)) <$> _drawingAreaConfig_clear cfg

  pixels <- performEvent (liftIO (getCanvas canvEl) <$ _drawingAreaConfig_send cfg)

  return  (DrawingArea cEl pixels)


relativeCoords :: MonadWidget t m => El t -> m (Dynamic t (Maybe (Double,Double)))
relativeCoords el = do
  -- Just cr <- liftIO (getBoundingClientRect (_el_element el))
  -- t <- getTop cr
  -- l <- getLeft cr
  -- liftIO $ print t
  -- let f l t (x,y) = Just (fromIntegral x - realToFrac l, fromIntegral y - realToFrac t)
  -- crEvents <- performEvent (liftIO (getBoundingClientRect $ _el_element el) <$> domEvent Mousemove el)

  let moveFunc (x,y) = do
        Just cr <- getBoundingClientRect (_el_element el)
        t <- fmap (floor) (getTop cr)
        l <- fmap (floor) (getLeft cr)
        return $ Just (fromIntegral $ x - l, fromIntegral $ y - t)
  p <- performEvent $ leftmost [return Nothing <$ domEvent Mouseleave el
                               , (fmap moveFunc (domEvent Mousemove el))]
  holdDyn Nothing p



getCanvas :: HTMLCanvasElement -> IO ByteString
getCanvas el = Data.ByteString.Char8.pack <$>
  toDataURL el (Nothing :: Maybe String)

clearArea :: CanvasRenderingContext2D -> HTMLCanvasElement -> IO ()
clearArea ctx canv = do
  save ctx
  setFillStyle ctx (Just $ CanvasStyle $
                    jsval ("rgba(255,255,255,1)" :: JSString))
  fillRect ctx 0 0 (realToFrac canvW) (realToFrac canvH)
  restore ctx

paint :: CanvasRenderingContext2D
      -> Double
      -> String
      -> (Double,Double)
      -> IO ()
paint ctx r c (x,y) = do
  save ctx
  setFillStyle ctx (Just $ CanvasStyle $ jsval (toJSString c))
  arc ctx (realToFrac x) (realToFrac y) (realToFrac r) 0 (2*pi) True
  fill ctx CanvasWindingRuleNonzero
  restore ctx

main :: IO ()
main = mainWidget $ mdo
  pb <- getPostBuild
  text "test"
  da <- drawingArea (DrawingAreaConfig pb (constant 10) (constant "rgba(100,100,0,1)") never never never never)
  coords <- relativeCoords (_drawingArea_el da)
  display coords
  return ()
