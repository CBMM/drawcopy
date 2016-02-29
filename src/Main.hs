{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecursiveDo       #-}

module Main where

import           GHCJS.DOM
import           GHCJS.DOM.Document
import           GHCJS.DOM.EventM
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.MouseEvent

import Control.Concurrent
import Control.Lens
import Control.Monad.Trans (liftIO)
import Control.Monad (liftM, unless, when)
import Control.Monad.IO.Class
import Data.Bitraversable (bisequence)
import Data.ByteString.Char8 hiding (filter, null)
import Data.Foldable hiding (filter, null)
import Data.Time
import Data.JSString hiding (filter, null)
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
import System.Random (StdGen, getStdGen)
import System.Random.MWC hiding (restore, save)

data DrawingAreaConfig t = DrawingAreaConfig
  { _drawingAreaConfig_clear  :: Event t ()
  , _drawingAreaConfig_radius :: Behavior t Double
  , _drawingAreaConfig_color  :: Behavior t String
  , _drawingAreaConfig_undo   :: Event t ()
  , _drawingAreaConfig_send   :: Event t ()
  }

defDAC :: Reflex t => DrawingAreaConfig t
defDAC = DrawingAreaConfig never (constant 10) (constant "black") never never

data DrawingArea t = DrawingArea
  { _drawingArea_el    :: El t
  , _drawingArea_image :: Event t ImageData }

canvH, canvW :: Int
canvW = 200
canvH = 128

data TimedCoord = TC !UTCTime !Int !Int
  deriving (Eq, Show)


data DrawingAreaState = DAState
  { _dasCurrentBuffer :: Maybe ImageData
  , _dasCanUndo       :: Bool
  , _dasCurrentStroke :: [TimedCoord]
  , _dasStrokes       :: [[TimedCoord]]
  , _dasUndoneStrokes :: [[TimedCoord]]
  , _dasStroking      :: Bool
  }

instance Show DrawingAreaState where
  show das = show $ _dasStrokes das

das0 :: DrawingAreaState
das0 = DAState Nothing False [] [] [] False

data DrawingAreaUpdate = DAMakePoint TimedCoord
                         -- ^ Add a timestamped point to the current stroke
                       | DAUndo
                         -- ^ Undo the last stroke
                       -- | DASetStroking (Maybe ImageData)
                       | DASetStroking Bool
                         -- ^ Start a new stroke (Just rasterize)
                         --   or end the current one (Nothing)
                       | DASetBackground ImageData

drawingAreaUpdate :: DrawingAreaUpdate -> DrawingAreaState -> DrawingAreaState
drawingAreaUpdate DAUndo d =
  if   _dasStroking d || not (_dasCanUndo d) then d -- Ignore UNDO mid-stroke
  else d { _dasCanUndo       = False
         , _dasStrokes       = strokes'
         , _dasUndoneStrokes = unstrokes' }
  where
    (strokes', unstrokes') = case _dasStrokes d of
      []     -> ([], _dasUndoneStrokes d)
      (x:xs) -> (xs, x : _dasUndoneStrokes d)
drawingAreaUpdate (DASetStroking True) d = -- (Just b))  d =  -- Click
  d { _dasStroking      = True }
drawingAreaUpdate (DASetStroking False) d = -- Nothing) d =    -- Unclick
  d { _dasCurrentStroke = []
    , _dasStroking      = False
    , _dasStrokes      =  _dasCurrentStroke d : _dasStrokes d
    }
drawingAreaUpdate (DAMakePoint p) d =
  d { _dasCurrentStroke = p : _dasCurrentStroke d }
drawingAreaUpdate (DASetBackground b) d =
  d { _dasCurrentBuffer = Just b }


drawingArea :: MonadWidget t m => DrawingAreaConfig t -> m (DrawingArea t)
drawingArea cfg = mdo

  (cEl,_) <- elAttr' "canvas" ("id" =: "canvas"
                      <> "width"  =: show canvW
                      <> "height" =: show canvH) $ return ()

  let canvEl = (castToHTMLCanvasElement . _el_element) cEl

  Just ctx <- liftIO $ fromJSVal =<< getContext canvEl ("2d" :: JSString)
  performEvent_ $ liftIO (clearArea ctx canvEl) <$ _drawingAreaConfig_clear cfg

  pixels <- performEvent (liftIO (getCanvasBuffer ctx canvEl) <$ _drawingAreaConfig_send cfg)

  dragPoints  <- wrapDomEvent (_el_element cEl) (onEventName Mousemove) getTimedMouseEventCoords'
  firstPoints <- wrapDomEvent (_el_element cEl) (onEventName Mousedown) getTimedMouseEventCoords'

  let pointEvents    = leftmost [ dragPoints, firstPoints ]

  let points = fmap DAMakePoint $ gate (_dasStroking <$> current state)pointEvents

  -- strokeStarts <- fmap (DASetStroking . Just) <$>
  --                 performEvent (ffor (domEvent Mousedown cEl) $ \_ ->
  --                                liftIO (getCanvasBuffer ctx canvEl))
  strokeStarts <- return $ DASetStroking True <$ domEvent Mousedown cEl

  -- stokeEnds  <- performEvent (ffor (leftmost [domEvent Mousedown  cEl
  --                                            ,domEvent Mouseleave cEl]) $
  --                             \_ -> liftIO 
  strokeEnds <- return $ DASetStroking False <$
                           leftmost [() <$ domEvent Mouseup    cEl
                                    ,domEvent Mouseleave cEl]
  -- undos <- never -- DAUndo <$ _drawingAreaConfig_undo cfg
  backgroundUpdates <- (fmap.fmap) DASetBackground $
    performEvent (ffor (tag (current state) strokeEnds) $ \s ->
    liftIO (recomputeBackground ctx canvEl s))

  state <- foldDyn drawingAreaUpdate das0
           (leftmost [points, strokeStarts, strokeEnds, backgroundUpdates])

  tInit <- liftIO getCurrentTime
  ticks <- tickLossy 0.03 tInit

  performEvent_ (ffor (tag (current state) ticks) $ \s ->
                  liftIO $ redraw ctx canvEl s)
  performEvent_ (ffor (tag (current state) strokeStarts) $ \s ->
                  liftIO (recomputeBackground ctx canvEl s) >> return ())

  -- display state

  return  (DrawingArea cEl pixels)

getMouseEventCoords' :: EventM e MouseEvent (Int,Int)
getMouseEventCoords' = do
  e <- event
  (x,y) <- bisequence (getClientX e, getClientY e)
  return (x,y)

getTimedMouseEventCoords' :: EventM e MouseEvent TimedCoord
getTimedMouseEventCoords' = do
  e <- event
  t <- liftIO getCurrentTime
  (x,y) <- bisequence (getClientX e, getClientY e)
  return $ TC t x y


relativeCoords :: MonadWidget t m => El t -> m (Dynamic t (Maybe (Double,Double)))
relativeCoords el = do

  let moveFunc (x,y) = do
        Just cr <- getBoundingClientRect (_el_element el)
        t <- fmap floor (getTop cr)
        l <- fmap floor (getLeft cr)
        return $ Just (fromIntegral $ x - l, fromIntegral $ y - t)
  p <- performEvent $ leftmost [return Nothing <$ domEvent Mouseleave el
                               , (fmap moveFunc (domEvent Mousemove el))]
  holdDyn Nothing p


getCanvasBuffer :: CanvasRenderingContext2D -> HTMLCanvasElement -> IO ImageData
getCanvasBuffer ctx el = do
  d <- getImageData ctx 0 0 (realToFrac canvW) (realToFrac canvH)
  maybe (Prelude.error "No imagedata") return d


clearArea :: CanvasRenderingContext2D -> HTMLCanvasElement -> IO ()
clearArea ctx canv = do
  save ctx
  setFillStyle ctx
    (Just $ CanvasStyle $ jsval ("rgba(255,255,255,1)" :: JSString))
  fillRect ctx 0 0 (realToFrac canvW) (realToFrac canvH)
  restore ctx


recomputeBackground :: CanvasRenderingContext2D
                    -> HTMLCanvasElement
                    -> DrawingAreaState
                    -> IO ImageData
recomputeBackground ctx canv das = do
  save ctx
  clearArea ctx canv
  let c = "hsla(100,50%,50%,1)"
  setStrokeStyle ctx (Just . CanvasStyle . jsval $ Data.JSString.pack c)
  forM_ (filter (not . null) (_dasCurrentStroke das : _dasStrokes das)) $ \((TC hT hX hY):ps) -> do
    moveTo ctx (fromIntegral hX) (fromIntegral hY)
    forM_ ps $ \(TC t1 x1 y1) -> do
      lineTo ctx (fromIntegral x1) (fromIntegral y1)
    stroke ctx
  Just bs <- getImageData ctx 0 0 (realToFrac canvW) (realToFrac canvH)
    -- Data.ByteString.Char8.pack <$>
    -- toDataURL el el (Nothing :: Maybe String)
  restore ctx
  return bs


redraw :: CanvasRenderingContext2D
       -> HTMLCanvasElement
       -> DrawingAreaState
       -> IO ()
redraw ctx canv das = do
  save ctx
  t <- getCurrentTime
  unless (Nothing == _dasCurrentBuffer das) $ putImageData ctx (_dasCurrentBuffer das) 0 0
  forM_ (Prelude.zip (_dasCurrentStroke das) (Prelude.tail $ _dasCurrentStroke das)) $ \(TC hT hX hY, TC hT' hX' hY') -> do
      beginPath ctx
      moveTo ctx (fromIntegral hX) (fromIntegral hY)
      let h = floor . (* 255) . (+ 0.5) . (/ 2) . sin . (2*pi *) $ realToFrac (diffUTCTime t hT)
          c = "hsla(" ++ show h ++ ",50%,45%,1)"
      setStrokeStyle ctx (Just . CanvasStyle . jsval $ Data.JSString.pack c)
      lineTo ctx (fromIntegral hX') (fromIntegral hY')
      closePath ctx
      stroke ctx
  restore ctx


main :: IO ()
main = mainWidget $ mdo
  pb <- getPostBuild
  text "test"
  -- da <- drawingArea (defDAC & _drawingAreaConfig_clear .~ pb)
  da <- drawingArea defDAC
  coords <- relativeCoords (_drawingArea_el da)
  display coords
  return ()
