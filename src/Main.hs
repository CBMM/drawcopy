{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}

module Main where


-------------------------------------------------------------------------------
import           Prelude hiding (map)
import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Trans (liftIO)
import           Control.Monad (liftM, liftM2, mzero, unless, when)
import           Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Encode.Pretty as A
import           Data.Bitraversable (bisequence)
import           Data.Bool (bool)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char (toLower)
import           Data.Foldable hiding (filter, null, foldl')
import           Data.Traversable hiding (filter, null)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.String.QQ
import           Data.Time
import           Data.JSString (JSString, pack)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           GHC.Generics
import           GHC.Int
import           GHCJS.DOM
import           GHCJS.DOM.CanvasRenderingContext2D
import           GHCJS.DOM.ClientRect
import           GHCJS.DOM.Element (getBoundingClientRect, touchStart, mouseDown,
                                    mouseMove, touchEnd, touchMove, mouseUp,focus)
import           GHCJS.DOM.Enums
import           GHCJS.DOM.EventM
import           GHCJS.DOM.HTMLCanvasElement
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.HTMLTextAreaElement (select)
import           GHCJS.Marshal (fromJSVal)
import           GHCJS.DOM.MouseEvent
import qualified GHCJS.DOM.Touch      as Touch
import qualified GHCJS.DOM.TouchEvent as Touch
import qualified GHCJS.DOM.TouchList  as Touch
import           GHCJS.DOM.Types hiding (Event)
import           GHCJS.Types (jsval)
import           Reflex hiding (select)
import           Reflex.Dom hiding (restore, select)
import           System.Random (StdGen, getStdGen)
import           System.Random.MWC hiding (restore, save)
-------------------------------------------------------------------------------
import           Tagging.User
import           Tagging.Stimulus
import           Tagging.Response


-------------------------------------------------------------------------------
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
  { _drawingArea_el      :: El t
  , _drawingArea_strokes :: Dynamic t [[TimedCoord]]
  , _drawingArea_image   :: Event t ImageData }

canvH, canvW :: Int
canvW = 300
canvH = 300

data TimedCoord = TC !UTCTime !Int !Int
  deriving (Eq, Show)

instance A.ToJSON TimedCoord where
  toJSON (TC t x y) = A.object
    [ "t" A..= t
    , "x" A..= x
    , "y" A..= y
    ]

instance A.FromJSON TimedCoord where
  parseJSON (A.Object o) = TC <$> o A..: "t"
                              <*> o A..: "x"
                              <*> o A..: "y"
  parseJSON _            = mzero

type TouchId = Word

data WidgetTouches t = WidgetTouches
  { _widgetTouches_touchStarts     :: Event   t (Map.Map TouchId TimedCoord)
  , _widgetTouches_touchMoves      :: Event   t (Map.Map TouchId TimedCoord)
  , _widgetTouches_touchEnds       :: Event   t (Map.Map TouchId TimedCoord)
  , _widgetTouches_currentStrokes  :: Dynamic t (Map.Map TouchId [TimedCoord])
  , _widgetTouches_finishedStrokes :: Dynamic t [[TimedCoord]]
  }


-- Auxiliary tag
data PointAction = PointsStart | PointsEnd | PointsMove | PointsClear
  deriving (Eq)

widgetTouches :: MonadWidget t m
              => El t
              -> Event t () -- Event to clear the touches history. TODO: Finisging a stroke should be an event with the finished strokes, rather than a dynamic that holds them until manual clearing like this.
              -> m (WidgetTouches t)
widgetTouches el clears = do

  let e = _el_element el

  starts      <- wrapDomEvent e (`on` touchStart) (cbStartOrEnd e)
  mousestarts <- wrapDomEvent e (`on` mouseDown)  (mouseHandler e)
  moves       <- wrapDomEvent e (`on` touchMove)  (cbStartOrEnd e)
  mousemoves  <- wrapDomEvent e (`on` mouseMove)  (mouseHandler e)
  ends        <- wrapDomEvent e (`on` touchEnd)   (cbStartOrEnd e)
  mouseends   <- wrapDomEvent e (`on` mouseUp)    (mouseHandler e)

  mouseisdown <- holdDyn False (leftmost [True <$ mousestarts, False <$ mouseends])

  strokes <- foldDyn modifyStrokes (mempty, mempty)
             (leftmost [fmap (PointsStart,) starts
                       ,fmap (PointsStart,) mousestarts
                       ,fmap (PointsMove,) moves
                       ,fmap (PointsMove,) (gate (current mouseisdown) mousemoves)
                       ,fmap (PointsEnd,) ends
                       ,fmap (PointsEnd,) mouseends
                       ,(PointsClear, mempty) <$ clears
                       ])

  currents  <- nubDyn <$> mapDyn fst strokes
  finisheds <- nubDyn <$> mapDyn snd strokes


  return $ WidgetTouches starts moves ends currents finisheds

  where
    cbStartOrEnd :: Element -> EventM e Touch.TouchEvent (Map.Map TouchId TimedCoord)
    cbStartOrEnd clientEl = do
      preventDefault
      e <- event
      Just cr <- getBoundingClientRect clientEl
      x0 <- floor <$> getLeft cr
      y0 <- floor <$> getTop  cr
      Just tl <- liftIO $ Touch.getChangedTouches e
      liftIO $ touchListToTCMap x0 y0 tl

    mouseHandler :: Element -> EventM e MouseEvent (Map.Map TouchId TimedCoord)
    mouseHandler clientEl = do
      preventDefault
      e <- event
      Just cr <- getBoundingClientRect clientEl
      x0 <- floor <$> getLeft cr
      y0 <- floor <$> getTop  cr
      t <- liftIO getCurrentTime
      (x,y) <- bisequence (getClientX e, getClientY e)
      return $ 0 =: TC t (x - x0) (y - y0)

    touchListToList :: TouchList -> IO [Touch]
    touchListToList tl = do
      n  <- Touch.getLength tl
      catMaybes <$> forM [0 .. pred n] (Touch.item tl)

    touchListToMap :: TouchList -> IO (Map.Map TouchId Touch)
    touchListToMap tl = fmap Map.fromList $ touchListToList tl >>=
      mapM (\t -> fmap (,t) (Touch.getIdentifier t))

    touchListToTCMap :: Int -> Int -> TouchList -> IO (Map.Map TouchId TimedCoord)
    touchListToTCMap x0 y0 tl = mapM (touchRelCoord x0 y0) =<< touchListToMap tl

    modifyStrokes :: (PointAction, Map.Map TouchId TimedCoord)
                  -> (Map.Map TouchId [TimedCoord], [[TimedCoord]])
                  -> (Map.Map TouchId [TimedCoord], [[TimedCoord]])
    modifyStrokes (PointsStart, new) (cur, old) =
      (Map.union (fmap (:[]) new) cur, old)
    modifyStrokes (PointsEnd, del) (cur, old) =
      let delEntries :: Map.Map TouchId [TimedCoord] = Map.filterWithKey (\k _ -> Map.member k del) cur
          insEntries :: [[TimedCoord]] = Map.elems $ fmap reverse delEntries
      in  (Map.difference cur delEntries, old ++ insEntries)
    modifyStrokes (PointsMove, new) (cur,old) =
      let cur' = Map.unionWith (++) (fmap (:[]) new) cur
      in  (cur', old)
    modifyStrokes (PointsClear, _) (cur, _) = (cur, mempty)



drawingArea :: MonadWidget t m => Event t () -> DrawingAreaConfig t -> m (DrawingArea t)
drawingArea touchClears cfg = mdo

  pb <- getPostBuild

  (cEl,_) <- elAttr' "canvas" ("id" =: "canvas"
                      <> "width"  =: show canvW
                      <> "height" =: show canvH) $ fin

  let canvEl = (castToHTMLCanvasElement . _el_element) cEl

  Just ctx <- liftIO $ fromJSVal =<< getContext canvEl ("2d" :: JSString)
  performEvent_ $ liftIO (clearArea ctx canvEl) <$ _drawingAreaConfig_clear cfg

  pixels <- performEvent (liftIO (getCanvasBuffer ctx canvEl) <$ _drawingAreaConfig_send cfg)

  touches <- widgetTouches cEl touchClears

  let s = _widgetTouches_finishedStrokes touches

  let strokeDone = updated s

  redrawGuardOver <- delay 0.0001 strokeDone
  redrawOk <- holdDyn True (leftmost [True <$ redrawGuardOver, False <$ strokeDone])

  bkgndDelay <- delay 0 strokeDone
  backgroundUpdates <- performEvent (ffor (tag (current s) bkgndDelay) $ \strks ->
    liftIO (recomputeBackground' ctx canvEl strks))
  background <- holdDyn Nothing $ fmap Just backgroundUpdates

  -- el "br" fin

  -- text "CURRENT TOUCHES"
  -- display ( _widgetTouches_currentStrokes touches)

  -- el "br" fin

  -- text "FINISHED TOUCHES"
  -- display ( _widgetTouches_finishedStrokes touches )


  tInit <- liftIO getCurrentTime
  ticks <- gate (current redrawOk) <$> tickLossy 0.03 tInit

  -- performEvent_ (ffor (tag ((,) <$> fmap Map.elems (current ( _widgetTouches_finishedStrokes touches)) <*> fmap Map.elems (current (_widgetTouches_currentStrokes touches))) ticks) $ \s ->
  --                 liftIO $ redraw'' ctx canvEl s)
  let -- redrawData :: Behavior t ([[TimedCoord]], Maybe ImageData)
      redrawData = (,) <$> fmap Map.elems (current $ _widgetTouches_currentStrokes touches)
                       <*> current background

  performEvent_ $ ffor (tag redrawData ticks) (\s -> liftIO $ redraw' ctx canvEl s)
  -- performEvent_ (ffor (tag ((,) <$> fmap Map.elems (current ( _widgetTouches_finishedStrokes touches)))
  --                           <*> current background) ticks $ \s ->
  --                 liftIO $ redraw' ctx canvEl s)

  return $ DrawingArea cEl s pixels

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

relativeCoords :: MonadWidget t m => El t -> m (Dynamic t (Maybe TimedCoord))
relativeCoords el = do
  let moveFunc (x,y) = do
        now <- liftIO getCurrentTime
        Just cr <- getBoundingClientRect (_el_element el)
        t <- fmap floor (getTop cr)
        l <- fmap floor (getLeft cr)
        return $ Just (TC now (fromIntegral $ x - l) (fromIntegral $ y - t))
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


-- recomputeBackground :: CanvasRenderingContext2D
--                     -> HTMLCanvasElement
--                     -> DrawingAreaState
--                     -> IO ImageData
-- recomputeBackground ctx canv das = do
--   save ctx
--   clearArea ctx canv
--   let c = "hsla(100,50%,50%,1)"
--   setStrokeStyle ctx (Just . CanvasStyle . jsval $ Data.JSString.pack c)
--   forM_ (filter (not . null) (_dasCurrentStroke das ++ _dasStrokes das)) $ \((TC hT hX hY):ps) -> do
--     moveTo ctx (fromIntegral hX) (fromIntegral hY)
--     forM_ ps $ \(TC t1 x1 y1) -> do
--       lineTo ctx (fromIntegral x1) (fromIntegral y1)
--     stroke ctx
--   Just bs <- getImageData ctx 0 0 (realToFrac canvW) (realToFrac canvH)
--     -- Data.ByteString.Char8.pack <$>
--     -- toDataURL el el (Nothing :: Maybe String)
--   restore ctx
--   return bs

recomputeBackground' :: CanvasRenderingContext2D
                     -> HTMLCanvasElement
                     -> [[TimedCoord]]
                     -> IO ImageData
recomputeBackground' ctx canv tc = do
  save ctx
  clearArea ctx canv
  let c = "hsla(100,50%,50%,1)"
  setStrokeStyle ctx (Just . CanvasStyle . jsval $ Data.JSString.pack c)
  forM_ (filter (not . null) tc) $ \((TC hT hX hY):ps) -> do
    moveTo ctx (fromIntegral hX) (fromIntegral hY)
    forM_ ps $ \(TC t1 x1 y1) -> do
      lineTo ctx (fromIntegral x1) (fromIntegral y1)
    stroke ctx
  Just bs <- getImageData ctx 0 0 (realToFrac canvW) (realToFrac canvH)
    -- Data.ByteString.Char8.pack <$>
    -- toDataURL el el (Nothing :: Maybe String)
  restore ctx
  return bs



redraw' :: CanvasRenderingContext2D
        -> HTMLCanvasElement
        -> ([[TimedCoord]], Maybe ImageData)
        -> IO ()
redraw' ctx canv (tc,bkg) = do
  save ctx
  t <- getCurrentTime
  case bkg of
    Just _  -> putImageData ctx bkg 0 0
    Nothing -> clearArea ctx canv
  -- TODO don't just take one
  forM_ tc $ \cs ->
    forM_ (Prelude.zip cs (Prelude.tail $ cs))
      $ \(TC hT hX hY , TC hT' hX' hY') -> do
        beginPath ctx
        moveTo ctx (fromIntegral hX) (fromIntegral hY)
        let h = floor . (* 255) . (^4) . (+ 0.75) . (/ 4) . sin . (2*pi *) $ realToFrac (diffUTCTime t hT)
            c = "hsla(" ++ show h ++ ",50%,45%,1)"
        setStrokeStyle ctx (Just . CanvasStyle . jsval $ Data.JSString.pack c)
        lineTo ctx (fromIntegral hX') (fromIntegral hY')
        closePath ctx
        stroke ctx
  restore ctx

redraw'' :: CanvasRenderingContext2D
         -> HTMLCanvasElement
         -> ([[TimedCoord]],[[TimedCoord]])
         -> IO ()
redraw'' ctx canv (old,cur) = do
  save ctx
  t <- getCurrentTime
  clearArea ctx canv
  -- TODO don't just take one
  forM_ old $ \cs ->
    forM_ (Prelude.zip cs (Prelude.tail $ cs))
      $ \(TC hT hX hY , TC hT' hX' hY') -> do
        beginPath ctx
        moveTo ctx (fromIntegral hX) (fromIntegral hY)
        let h = floor . (* 255) . (^4) . (+ 0.75) . (/ 4) . sin . (2*pi *) $ realToFrac (diffUTCTime t hT)
            c = "hsla(" ++ show h ++ ",50%,45%,1)"
        setStrokeStyle ctx (Just . CanvasStyle . jsval $ Data.JSString.pack c)
        lineTo ctx (fromIntegral hX') (fromIntegral hY')
        closePath ctx
        stroke ctx
  forM_ cur $ \cs ->
    forM_ (Prelude.zip cs (Prelude.tail $ cs))
      $ \(TC hT hX hY , TC hT' hX' hY') -> do
        beginPath ctx
        moveTo ctx (fromIntegral hX) (fromIntegral hY)
        let h = floor . (* 255) . (^4) . (+ 0.75) . (/ 4) . sin . (2*pi *) $ realToFrac (diffUTCTime t hT)
            c = "hsla(" ++ show h ++ ",50%,45%,1)"
        setStrokeStyle ctx (Just . CanvasStyle . jsval $ Data.JSString.pack c)
        lineTo ctx (fromIntegral hX') (fromIntegral hY')
        closePath ctx
        stroke ctx
  restore ctx



-- redraw :: CanvasRenderingContext2D
--        -> HTMLCanvasElement
--        -> DrawingAreaState
--        -> IO ()
-- redraw ctx canv das = do
--   save ctx
--   t <- getCurrentTime
--   case _dasCurrentBuffer das of
--     Just _  -> putImageData ctx (_dasCurrentBuffer das) 0 0
--     Nothing -> clearArea ctx canv
--   -- TODO don't just take one
--   forM_ (_dasCurrentStroke das) $ \cs ->
--     forM_ (Prelude.zip cs (Prelude.tail $ cs))
--       $ \(TC hT hX hY , TC hT' hX' hY') -> do
--         beginPath ctx
--         moveTo ctx (fromIntegral hX) (fromIntegral hY)
--         let h = floor . (* 255) . (^4) . (+ 0.75) . (/ 4) . sin . (2*pi *) $ realToFrac (diffUTCTime t hT)
--             c = "hsla(" ++ show h ++ ",50%,45%,1)"
--         setStrokeStyle ctx (Just . CanvasStyle . jsval $ Data.JSString.pack c)
--         lineTo ctx (fromIntegral hX') (fromIntegral hY')
--         closePath ctx
--         stroke ctx
--   restore ctx


type Result = [[TimedCoord]]


questionTagging :: MonadWidget t m
                => (Assignment, StimulusSequence, StimSeqItem)
                -> m (Dynamic t Result)
questionTagging (asgn, stimseq, ssi) = do
  case ssiStimulus ssi of
    A.String picUrl -> do
      el "div" $ question (constDyn $ T.unpack picUrl) never
    _ -> text "Unable to fetch stimulus image" >> return (constDyn [])

question :: MonadWidget t m => Dynamic t String -> Event t () -> m (Dynamic t [[TimedCoord]])
question imgUrl touchClears = elAttr "div" ("class" =: "question") $ do
  da <- elClass "div" "drawing-area" $ do
    el "p" $ text "Your Copy"
    drawingArea touchClears defDAC
  el "div" $ do
    el "p" $ text "Goal Picture"
    imgAttrs <- mapDyn (\i -> "class" =: "goal-img" <> "src" =: i) imgUrl
    elDynAttr "img" imgAttrs fin
  return $ _drawingArea_strokes da

-----------------------------------------------------
-- Custom trial & result types for standalone mode --
-----------------------------------------------------

data ExperimentState t = ExperimentState
  { _esSubject :: Dynamic t String
  , _esPicSrcs :: Dynamic t [String]
  }

data Response = Response
  { _rSubject   :: String
  , _rImage     :: String
  , _rStartTime :: UTCTime
  , _rEndTime   :: UTCTime
  , _rStrokes   :: [[TimedCoord]]
  } deriving (Show, Generic)

instance A.ToJSON Response where
  toJSON = A.genericToJSON
           A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 2}

instance A.FromJSON Response where
  parseJSON = A.genericParseJSON
              A.defaultOptions {A.fieldLabelModifier = fmap toLower . drop 2}

main :: IO ()
main = mainWidgetWithHead appHead $ mdo
  t0 <- liftIO getCurrentTime
  showSettings <- toggle True =<< bootstrapButton "cog"
  showResults <- toggle False =<< bootstrapButton "th-list"
  clearResults <- switchPromptly never =<< dyn =<<
                  forDyn showResults (bool (return never) (responsesWidget responses))

  picIndex <- foldDyn ($) 0 $
              leftmost [ const 0 <$ updated (_esPicSrcs es)
                       , succ    <$ submits
                       ]
  es <- elClass "div" "settings" $ settings showSettings

  stimTime <- holdDyn t0 =<< performEvent (liftIO getCurrentTime <$ updated picIndex)

  stimulus <- combineDyn (\i srcs -> if length srcs > 0
                                     then srcs !! mod i (length srcs)
                                     else "")
                         picIndex
                         (_esPicSrcs es)

  strokes  <- question stimulus (() <$ submits)

  trialMetadata <- $(qDyn [| ( $(unqDyn [| _esSubject es |]),
                               $(unqDyn [| stimulus      |]),
                               $(unqDyn [| stimTime      |]),
                               $(unqDyn [| strokes       |]))
                           |])

  submitClicks <- button "Submit"
  submits <- performEvent
    (ffor (tag (current trialMetadata) submitClicks) $ \(subj,stm,tStm,strk) -> do
        tNow <- liftIO getCurrentTime
        return $ Response subj stm tStm tNow strk
    )

  -- responses <- foldDyn (:) [] submits
  responses <- foldDyn ($) [] (leftmost [fmap (:) submits, const [] <$ clearResults])

  return ()

settings :: MonadWidget t m => Dynamic t Bool -> m (ExperimentState t)
settings vis = do
  settingAttrs <- forDyn vis $ bool ("style" =: "display:none;") mempty
  elDynAttr "div" settingAttrs $ do
    es <- elClass "div" "settings-fields" $ do
      nm   <- bootstrapLabeledInput "Subject" "subejct"
              (\a -> value <$> textInput (def & attributes .~ constDyn a))

      el "br" fin
      pics <- bootstrapLabeledInput "Images" "images"
              (\a -> value <$> textArea (def & attributes .~ constDyn ("id" =: "results" <> a)
                                             & textAreaConfig_initialValue .~ defaultPics))
      pics' <- mapDyn Prelude.lines pics
      return $ ExperimentState nm pics'
    elClass "div" "settings-preview" $
      dyn =<< (forDyn (_esPicSrcs es) $ \pics ->
                (forM_  pics
                 (\src -> elAttr
                          "img" ("class" =: "preview-pic" <> "src" =: src)
                          fin)))
    return es

responsesWidget :: MonadWidget t m => Dynamic t [Response] -> m (Event t ())
responsesWidget resps = divClass "responses" $ do
  pb <- getPostBuild
  rtext <- mapDyn (BSL.unpack . A.encodePretty) resps

  (sel',clear) <- divClass "results-buttons" $ do
   sel <- elClass "button" "btn btn-large results-select" $ do
     b <- bootstrapButton "screenshot"
     text "Select all"
     return b
   clear <- elClass "button" "btn btn-large results-remove" $ do
     b <- bootstrapButton "remove"
     text "Reset results"
     return b
   return (sel,clear)

  el "br" fin
  sel <- delay 0.1 sel'

  ta <- _textArea_element <$> textArea
        (def & textAreaConfig_setValue .~ updated rtext)
--         (def & textAreaConfig_setValue .~ tag (current rtext) pb)
  performEvent_ ((liftIO (putStrLn "TEST") >> focus ta >> select ta) <$ leftmost [pb, () <$ updated rtext, sel])
  return clear


bootstrapButton :: MonadWidget t m => String -> m (Event t ())
bootstrapButton glyphShortname = (domEvent Click . fst) <$>
  elAttr' "span" ("class" =: (prfx <> glyphShortname)) (return ())
  where prfx = "glyphicon glyphicon-"

------------------------------------------------------------------------------
-- | An externally-prodded validating label & text-input
bootstrapLabeledInput :: forall t m.MonadWidget t m
                      => String
                      -> String
                      -> (Map.Map String String -> m (Dynamic t String))
                      -- ^ function that uses arg as attributes in widget
                      -> m (Dynamic t String)
bootstrapLabeledInput label idattr textwidget =
  elClass "div" "form-group" $ mdo
    elAttr "label" ("for" =: idattr) $ text label
    let attrs = "type"  =: "text" <>
                "id"    =: idattr <>
                "class" =: "form-control"
    textwidget attrs

appHead :: MonadWidget t m => m ()
appHead = do
  elAttr "link" ("rel" =: "stylesheet"
              <> "type" =: "text/css"
              <> "href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css")
    fin
  elAttr "link" ("href" =: "drawcopy.css"
              <> "rel"  =: "stylesheet"
              <> "type" =: "text/css") fin
  el "style" (text appStyle)

appStyle :: String
appStyle = [s|
.settings > div {
  background-color: gray;
  display: flex;
}

.settings-fields {
  padding: 20px;
  width: 50%;
}

.settings-preview{
  padding:20px;
  width: 50%;
}

#images { resize: vertical; }

.preview-pic {
  height: 40px;
  margin: 4px;
}

|] ++ unlines [
 ".question div canvas{"
 , "  height: "     ++ show canvH ++ "px;"
 , "  min-height: " ++ show canvH ++ "px;"
 , "  max-height: " ++ show canvH ++ "px;"
 , "  width: "      ++ show canvW ++ "px;"
 , "  min-width: "  ++ show canvW ++ "px;"
 , "  max-width: "  ++ show canvW ++ "px;"
 ,"}\n\n"] ++ unlines [
 ".question div img{"
 , "  height: "     ++ show canvH ++ "px;"
 , "  min-height: " ++ show canvH ++ "px;"
 , "  max-height: " ++ show canvH ++ "px;"
 , "  width: "      ++ show canvW ++ "px;"
 , "  min-width: "  ++ show canvW ++ "px;"
 , "  max-width: "  ++ show canvW ++ "px;"
 ,"}"]

defaultPics :: String
defaultPics = unlines
  [ "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character01/0709_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character02/0710_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character03/0711_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character04/0712_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character05/0713_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character06/0714_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character07/0715_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character08/0716_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character09/0717_01.png"
  , "https://s3.amazonaws.com/lakecharacters/Alphabet_of_the_Magi/character10/0718_01.png"
  ]


relativizedCoord :: Int -> Int -> TimedCoord -> TimedCoord
relativizedCoord x0 y0 (TC t x y) =
  TC t (x - x0) (y - y0)

touchCoord :: Touch -> IO TimedCoord
touchCoord touch = do
  t <- getCurrentTime
  (x,y) <- bisequence (Touch.getClientX touch, Touch.getClientY touch)
  return $ TC t x y

touchRelCoord :: Int -> Int -> Touch -> IO TimedCoord
touchRelCoord x0 y0 tch = do
  relativizedCoord x0 y0 <$> touchCoord tch



corner :: Reflex t => El t -> IO (Int,Int)
corner w = do
  bRect <- getBoundingClientRect (_el_element w)
  case bRect of
    Nothing -> error "Error getting element corner"
    Just b  -> do
      top   <- fmap floor (getTop  b)
      left  <- fmap floor (getLeft b)
      return (top,left)


-- data DrawingAreaState = DAState
--   { -- _dasCurrentBuffer :: Maybe ImageData
--     _dasCanUndo       :: Bool
--   , _dasCurrentStroke :: [[TimedCoord]]
--   , _dasStrokes       :: [[TimedCoord]]
--   , _dasUndoneStrokes :: [[TimedCoord]]
--   , _dasStroking      :: Bool
--   }

-- instance Show DrawingAreaState where
--   show das = show $ _dasStrokes das

-- das0 :: DrawingAreaState
-- das0 = DAState False [] [] [] False

-- data DrawingAreaUpdate = DAMakePoint TimedCoord
--                          -- ^ Add a timestamped point to the current stroke
--                        | DAUndo
--                          -- ^ Undo the last stroke
--                          -- | DASetStroking (Maybe ImageData)
--                        | DASetStroking Bool
--                          -- ^ Start a new stroke (Just rasterize)
--                          --   or end the current one (Nothing)
--  --                       | DASetBackground ImageData
--                        | DAOverwriteCurrentStrokes [[TimedCoord]]
--                        | DAAppendToOldStrokes [[TimedCoord]]

-- instance Show DrawingAreaUpdate where
--   show (DAMakePoint tc) = "DAMakePoint " ++ show tc
--   show (DAUndo) = "DAUndo"
--   show (DASetStroking b) = "DASetStroking " ++ show b
-- --  show (DASetBackground _) = "DASetBackground <image>"
--   show (DAOverwriteCurrentStrokes tc) = "DAOverwriteCurrentStrokes " ++ show tc
--   show (DAAppendToOldStrokes tc) = "DAAppendToOldStrokes " ++ show tc

-- drawingAreaUpdate :: DrawingAreaUpdate -> DrawingAreaState -> DrawingAreaState
-- drawingAreaUpdate DAUndo d =
--   if   _dasStroking d || not (_dasCanUndo d) then d -- Ignore UNDO mid-stroke
--   else d { _dasCanUndo       = False
--          , _dasStrokes       = strokes'
--          , _dasUndoneStrokes = unstrokes' }
--   where
--     (strokes', unstrokes') = case _dasStrokes d of
--       []     -> ([], _dasUndoneStrokes d)
--       (x:xs) -> (xs, x : _dasUndoneStrokes d)
-- drawingAreaUpdate (DASetStroking True) d = -- (Just b))  d =  -- Click
--   d { _dasStroking      = True }
-- drawingAreaUpdate (DASetStroking False) d = -- Nothing) d =    -- Unclick
--   d { _dasCurrentStroke = []
--     , _dasStroking      = False
--     , _dasStrokes      =  _dasCurrentStroke d ++ _dasStrokes d
--     }
-- drawingAreaUpdate (DAMakePoint p) d =
--   d { _dasCurrentStroke = (p : head (_dasCurrentStroke d)) : tail (_dasCurrentStroke d) }
-- -- drawingAreaUpdate (DASetBackground b) d =
-- --   d { _dasCurrentBuffer = Just b }
-- drawingAreaUpdate (DAOverwriteCurrentStrokes cur) d =
--   d { _dasCurrentStroke = cur }
-- drawingAreaUpdate (DAAppendToOldStrokes old) d =
--   d { _dasStrokes = _dasStrokes d ++ old }



-- taggingInteractionWidget :: forall t m.MonadWidget t m => m ()
-- taggingInteractionWidget = elAttr "div" ("class" =: "interaction") $ mdo
--   pb <- getPostBuild

--   let requestTriggers = leftmost [pb, () <$ sendResult]
--   assignments <- getAndDecode ("/api/fullposinfo" <$ requestTriggers)
--   da <- widgetHold (drawingArea defDAC >>= \d -> text "No image" >> return (_drawingArea_strokes d))
--                    (fmap question (fmapMaybe id assignments))

--   submits <- button "Send"
--   sendResult <- performRequestAsync $
--     ffor (tag (current $ joinDyn da) submits) $ \(r :: Result) ->
--       XhrRequest "POST" "/api/response?advance" $
--       XhrRequestConfig ("Content-Type" =: "application/json")
--       Nothing Nothing Nothing (Just . BSL.unpack $ A.encode
--                               (ResponsePayload (A.toJSON r)))

--   fin


-- taggingMain :: IO ()
-- taggingMain = mainWidget interactionWidget

main' :: IO ()
main' = mainWidgetWithHead appHead $ do
  da <- drawingArea never defDAC
  fin

main'' :: IO ()
main'' = mainWidget $ text "Hello"

fin :: MonadWidget t m => m ()
fin = return ()
