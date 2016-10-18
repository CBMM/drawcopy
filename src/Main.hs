{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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
import           Data.Default (Default(..))
import           Data.Foldable hiding (filter, null, foldl')
import           Data.Proxy
import           Data.Traversable hiding (filter, null)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, isJust)
import           Data.String.QQ
import           Data.Time
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           GHC.Int
import           GHCJS.DOM
import           GHCJS.DOM.Blob
import           GHCJS.DOM.CanvasRenderingContext2D
import           GHCJS.DOM.ClientRect
import           GHCJS.DOM.Element (touchStart, mouseDown,
                                    mouseMove, touchEnd, touchMove, mouseUp,focus)
import           GHCJS.DOM.Enums
import           GHCJS.DOM.EventM
import           GHCJS.DOM.HTMLCanvasElement
import           GHCJS.DOM.HTMLDocument
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.HTMLTextAreaElement (select)
import qualified GHCJS.DOM.MouseEvent

import           GHCJS.DOM.Types hiding (Event)
import           GHCJS.DOM.URL
import           Reflex hiding (select)
import           Reflex.Dom hiding (restore, select, Window, Element, preventDefault)
import           System.Random (StdGen, getStdGen, randomRIO)
import           System.Random.MWC hiding (restore, save)
#ifdef ghcjs_HOST_OS
import           Data.JSString (JSString, pack)
import           GHCJS.Marshal (fromJSVal, toJSVal_aeson)
import           GHCJS.Types (JSVal, jsval)
import           GHCJS.DOM.Element (getBoundingClientRect)
import           GHCJS.DOM.Location (getSearch)
import           GHCJS.DOM.Touch      (getIdentifier,getClientX,getClientY)
import           GHCJS.DOM.TouchEvent (TouchEvent, getChangedTouches)
import           GHCJS.DOM.TouchList   (getLength,item)
import           GHCJS.DOM.Window (getLocation)
#endif
import           Servant.API
import           Servant.Reflex
-- import           Servant.Common.BaseUrl


-------------------------------------------------------------------------------
data DrawingAreaConfig t = DrawingAreaConfig
  { _drawingAreaConfig_clear  :: Event t ()
  , _drawingAreaConfig_radius :: Behavior t Double
  , _drawingAreaConfig_color  :: Behavior t T.Text
  , _drawingAreaConfig_undo   :: Event t ()
  , _drawingAreaConfig_send   :: Event t ()
  }

defDAC :: Reflex t => DrawingAreaConfig t
defDAC = DrawingAreaConfig never (constant 10) (constant "black") never never

data DrawingArea t = DrawingArea
  { _drawingArea_el      :: El t
  , _drawingArea_strokes :: Dynamic t [[TimedCoord]]
  , _drawingArea_image   :: Dynamic t T.Text }

canvH, canvW :: Int
canvW = 500
canvH = 500

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

mouseGetClientX :: MouseEvent -> EventM e MouseEvent Int
mouseGetClientX = GHCJS.DOM.MouseEvent.getClientX

mouseGetClientY :: MouseEvent -> EventM e MouseEvent Int
mouseGetClientY = GHCJS.DOM.MouseEvent.getClientY

-- Auxiliary tag
data PointAction = PointsStart | PointsEnd | PointsMove | PointsClear
  deriving (Eq)

widgetTouches :: MonadWidget t m
              => El t
              -> Event t () -- Event to clear the touches history. TODO: Finisging a stroke should be an event with the finished strokes, rather than a dynamic that holds them until manual clearing like this.
              -> m (WidgetTouches t)
widgetTouches el clears = do

  let e =  _element_raw el

  starts      <- wrapDomEvent e (`on` touchStart) (cbStartOrEnd e)
  mousestarts <- wrapDomEvent e (`on` mouseDown)  (mouseHandler e)
  moves       <- wrapDomEvent e (`on` touchMove)  (cbStartOrEnd e)
  mousemoves  <- wrapDomEvent e (`on` mouseMove)  (mouseHandler e)
  ends        <- wrapDomEvent e (`on` touchEnd)   (cbStartOrEnd e)
  mouseends   <- wrapDomEvent e (`on` mouseUp)    (mouseHandler e)
  afterEnds   <- delay 0 $ leftmost [ends, mouseends]

  mouseisdown <- holdDyn False (leftmost [True <$ mousestarts, False <$ mouseends])

  strokes <- foldDyn modifyStrokes (mempty, mempty)
             (leftmost [fmap (PointsStart,) starts
                       ,fmap (PointsStart,) mousestarts
                       ,fmap (PointsMove,) moves
                       ,fmap (PointsMove,) (gate (current mouseisdown) mousemoves)
                       ,fmap (PointsMove,) ends
                       ,fmap (PointsMove,) mouseends
                       ,fmap (PointsEnd,) afterEnds
                       ,(PointsClear, mempty) <$ clears
                       ])

  let currents  = uniqDyn $ fst <$> strokes
      finisheds = uniqDyn $ snd <$> strokes


  return $ WidgetTouches starts moves ends currents finisheds

  where
    -- cbStartOrEnd :: Element -> EventM e TouchEvent (Map.Map TouchId TimedCoord)
    cbStartOrEnd clientEl = do
      preventDefault
      e <- event
      Just cr <- getBoundingClientRect clientEl
      x0 :: Int <- floor <$> getLeft cr
      y0 :: Int <- floor <$> getTop  cr
      Just tl <- liftIO $ getChangedTouches e
      liftIO $ touchListToTCMap x0 y0 tl

    -- mouseHandler :: Element -> EventM e MouseEvent (Map.Map TouchId TimedCoord)
    mouseHandler clientEl = do
      preventDefault
      rnd <- liftIO $ return 0 -- randomRIO (-0.01, 0.01)
      e <- event
      Just cr <- getBoundingClientRect clientEl
      x0 :: Int <- (floor . (+ rnd)) <$> getLeft cr
      y0 :: Int <- (floor . (+ rnd)) <$> getTop  cr
      t <- liftIO getCurrentTime
      (x,y) <- bisequence (mouseGetClientX e, mouseGetClientY e)
      return $ 0 =: TC t (x - x0) (y - y0)

    touchListToList :: TouchList -> IO [Touch]
    touchListToList tl = do
      n  <- getLength tl
      catMaybes <$> forM [(0::Word) .. pred n] (item tl)

    touchListToMap :: TouchList -> IO (Map.Map TouchId Touch)
    touchListToMap tl = fmap Map.fromList $ touchListToList tl >>=
      mapM (\t -> fmap (,t) (getIdentifier t))

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
      in  (Map.difference cur delEntries, old <> insEntries)
    modifyStrokes (PointsMove, new) (cur,old) =
      let cur' = Map.unionWith (<>) (fmap (:[]) new) cur
      in  (cur', old)
    modifyStrokes (PointsClear, _) (cur, _) = (cur, mempty)



drawingArea :: MonadWidget t m => Event t () -> DrawingAreaConfig t -> m (DrawingArea t)
drawingArea touchClears cfg = mdo

  pb <- getPostBuild

  (cEl,_) <- elAttr' "canvas" ("id" =: "canvas"
                      <> "width"  =: (T.pack . show) canvW
                      <> "height" =: (T.pack . show) canvH) $ blank

  let canvEl = (castToHTMLCanvasElement . _element_raw) cEl

  Just ctx <- liftIO $ fromJSVal =<< getContext canvEl ("2d" :: String)
  performEvent_ $ liftIO (clearArea ctx canvEl) <$ _drawingAreaConfig_clear cfg

  -- pixels <- performEvent (liftIO (getCanvasBuffer ctx canvEl) <$ _drawingAreaConfig_send cfg)

  touches <- widgetTouches cEl touchClears

  pixels <- holdDyn T.empty =<< performEvent (liftIO (toDataURL canvEl (Just ("image/jpeg" :: String))) <$
                                              leftmost [_drawingAreaConfig_send cfg
                                                       ,_drawingAreaConfig_clear cfg
                                                       ,pb
                                                       ,() <$ updated (_widgetTouches_finishedStrokes touches)])

  let s = _widgetTouches_finishedStrokes touches

  let strokeDone = updated s

  redrawGuardOver <- delay 0.0001 strokeDone
  redrawOk <- holdDyn True (leftmost [True <$ redrawGuardOver, False <$ strokeDone])

  bkgndDelay <- delay 0 strokeDone
  backgroundUpdates <- performEvent (ffor (tag (current s) bkgndDelay) $ \strks ->
    liftIO (recomputeBackground ctx canvEl strks))
  background <- holdDyn Nothing $ fmap Just backgroundUpdates

  tInit <- liftIO getCurrentTime
  ticks <- gate (current redrawOk) <$> tickLossy 0.03 tInit

  let redrawData = (,) <$> fmap Map.elems
                           (current $ _widgetTouches_currentStrokes touches)
                       <*> current background

  performEvent_ $ ffor (tag redrawData ticks) (liftIO . redraw ctx canvEl)

  return $ DrawingArea cEl s pixels



getCanvasBuffer :: CanvasRenderingContext2D -> HTMLCanvasElement -> IO ImageData
getCanvasBuffer ctx el = do
  d <- getImageData ctx 0 0 (realToFrac canvW) (realToFrac canvH)
  maybe (Prelude.error "No imagedata") return d


clearArea :: CanvasRenderingContext2D -> HTMLCanvasElement -> IO ()
clearArea ctx canv = do
  save ctx
  setFillStyle ctx
    (Just $ CanvasStyle $ jsval (pack "rgba(255,255,255,1)"))
  fillRect ctx 0 0 (realToFrac canvW) (realToFrac canvH)
  restore ctx


recomputeBackground :: CanvasRenderingContext2D
                    -> HTMLCanvasElement
                    -> [[TimedCoord]]
                    -> IO ImageData
recomputeBackground ctx canv tc = do
  save ctx
  clearArea ctx canv
  let c = "hsla(100,50%,0%,1)"
  forM_ tc (drawStroke ctx)
  Just bs <- getImageData ctx 0 0 (realToFrac canvW) (realToFrac canvH)
  restore ctx
  return bs

drawStroke :: CanvasRenderingContext2D -> [TimedCoord] -> IO ()
drawStroke ctx tcs = do
  save ctx
  setStrokeStyle ctx (Just . CanvasStyle . jsval . pack $ ("black" :: String))
  setLineWidth ctx 20
  setLineCap ctx $ Just ("round" :: String)
  beginPath ctx >> case tcs of
    [] -> return ()
    [TC t x y] -> do
      moveTo ctx (fromIntegral x) (fromIntegral y)
      lineTo ctx (fromIntegral $ x + 1) (fromIntegral y)
    [TC t0 x0 y0, TC t1 x1 y1]
      | max (x1 - x0) (y1 - y0) < 1 -> do
          moveTo ctx (fromIntegral x0) (fromIntegral y0)
          lineTo ctx (fromIntegral $ x0 + 1) (fromIntegral y0)
      | otherwise -> do
          moveTo ctx (fromIntegral x0) (fromIntegral y0) >> lineTo ctx (fromIntegral x1) (fromIntegral y1)
    (TC t0 x0 y0 : css) -> do
      moveTo ctx (fromIntegral x0) (fromIntegral y0)
      forM_ css $ \(TC t x y) -> lineTo ctx (fromIntegral x) (fromIntegral y)
  -- closePath ctx
  stroke ctx
  restore ctx


redraw :: CanvasRenderingContext2D
       -> HTMLCanvasElement
       -> ([[TimedCoord]], Maybe ImageData)
       -> IO ()
redraw ctx canv (tc,bkg) = do
  save ctx
  setStrokeStyle ctx (Just . CanvasStyle . jsval . pack $ ("black" :: String))
  setLineWidth ctx 20
  case bkg of
    Just _  -> putImageData ctx bkg 0 0
    Nothing -> clearArea ctx canv
  forM_ tc $ drawStroke ctx


question :: MonadWidget t m
         => Dynamic t T.Text
         -> Event t ()
         -> Bool
         -> m (Dynamic t (T.Text, [[TimedCoord]]))
question imgUrl touchClears overlap = elAttr "div" (mayOverlapClass "question") $ do

  elAttr "div" (mayOverlapClass "example-area") $ do
    divClass "goal-header" $ divClass "frog-div" $
      elAttr "img" ("src" =: "static/frogpencil.jpg") blank
    let imgAttrs = (\i -> "class" =: "goal-img" <> "src" =: i) <$> imgUrl
    elDynAttr "img" imgAttrs blank

  da <- elAttr "div" (mayOverlapClass "drawing-area") $ do
    divClass "draw-header" $ if   overlap
                             then blank
                             else divClass "you-div" $ el "span" $ text "You"
    drawingArea touchClears defDAC

  return $ (,) <$> _drawingArea_image da <*> _drawingArea_strokes da

  where mayOverlapClass baseClass = "class" =: (baseClass <> bool "" " overlap" overlap)



-----------------------------------------------------
-- Custom trial & result types for standalone mode --
-----------------------------------------------------

data ExperimentState t = ExperimentState
  { _esSubject :: Dynamic t T.Text
  , _esPicSrcs :: Dynamic t [T.Text]
  }

data Response = Response
  { _rSubject   :: T.Text
  , _rImage     :: T.Text
  , _rStartTime :: UTCTime
  , _rEndTime   :: UTCTime
  , _rStrokes   :: [[TimedCoord]]
  , _rOverlap   :: Bool
  , _rJpeg      :: T.Text
  } deriving (Show, Generic)

instance A.ToJSON Response where
  toJSON = A.genericToJSON
           A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 2}

instance A.FromJSON Response where
  parseJSON = A.genericParseJSON
              A.defaultOptions {A.fieldLabelModifier = fmap toLower . drop 2}

main :: IO ()
main = mainWidgetWithHead appHead $ run

run :: forall t m. MonadWidget t m => m ()
run = mdo
  let dbClient u = client dbApi (Proxy :: Proxy m)
                   (constDyn (BaseFullUrl Https u 443 "/"))
      dbKeyParam = (Right . DBBearer <$> dbKey)
      (dbList :<|> _) = dbClient "api.dropbox.com" dbKeyParam
  params <- liftIO $ currentWindow >>= \(Just win) -> getQueryParams win
  let overlap = let l = Map.lookup "overlap" params
                in  not (l == Nothing || l == Just "false")
  t0 <- liftIO getCurrentTime

  (settingsButton, showResults) <- divClass "button-bank" $ do
    settingsBtn <- bootstrapButton "cog"
    showResults <- toggle False =<< bootstrapButton "th-list"
    return (settingsBtn, showResults)

  showSettings <- foldDyn ($) True $ leftmost [const False <$ closeSettings
                                              ,not <$ settingsButton]

  picIndex <- foldDyn ($) 0 $
              leftmost [ const 0 <$ updated (_esPicSrcs es)
                       , const 0 <$ clearResults
                       , succ    <$ submits
                       ]

  stimTime <- holdDyn t0 =<< performEvent (liftIO getCurrentTime <$ updated picIndex)

  let stimulus = zipDynWith (\i srcs -> if length srcs > 0
                                        then srcs !! mod i (length srcs)
                                        else "")
                 picIndex
                 (_esPicSrcs es)

  (strokes, submitClicks) <- divClass "interaction" $ do
    strokes <- question stimulus (() <$ submits) overlap
    btn <- fmap fst $ elAttr' "button" ("class" =: "submit-button btn btn-default-btn-lg") $ bootstrapButton "ok-circle"
    let submitClicks = domEvent Click btn
    return (strokes, submitClicks)

  let settingsAts = ("class" =: "settings" <>) . bool ("style" =: "display:none") mempty <$> showSettings
  (es, closeSettings, dbx, dbKey) <- elDynAttr "div" settingsAts settings

  clearResults <- switchPromptly never =<< dyn
                  ((bool (return never)
                                      (responsesWidget dbClient dbKeyParam responses)) <$> showResults)


  let trialMetadata = (,,,,) <$> _esSubject es <*> stimulus <*> stimTime <*> pure overlap <*> strokes

  submits <- performEvent
    (ffor (tag (current trialMetadata) submitClicks) $ \(subj,stm,tStm,ovlp,(imgdata, strk)) -> do
        tNow <- liftIO getCurrentTime
        return $ Response subj stm tStm tNow strk ovlp imgdata
    )

  responses <- foldDyn ($) [] (leftmost [fmap (:) submits, const [] <$ clearResults])

  return ()


#ifdef ghcjs_HOST_OS
enblobURL :: A.ToJSON a => a -> IO String
enblobURL a = fmap GHCJS.DOM.Types.fromJSString $ js_enblobURL =<< toJSVal_aeson (A.toJSON a)

foreign import javascript unsafe "b = new Blob([JSON.stringify($1)],{type: 'application/json'}); $r = (window.URL ? URL : webkitURL).createObjectURL(b)"
  js_enblobURL :: JSVal -> IO JSString
#else
enblobURL :: A.ToJSON a => a -> IO String
enblobURL = undefined
#endif

data DropboxConnection = DBConn JSVal


#ifdef ghcjs_HOST_OS
dbConn :: T.Text -> IO DropboxConnection
dbConn key = js_dbConn (toJSString key) >>= return . DBConn

foreign import javascript unsafe "new Dropbox({accessToken: $1})" js_dbConn :: JSString -> IO JSVal

checkConn :: DropboxConnection -> IO Bool
checkConn (DBConn c) = js_checkConn c

dbSend :: DropboxConnection -> T.Text -> T.Text -> IO Bool
dbSend (DBConn c) fname payload = js_dropboxSend c (toJSString fname) (toJSString payload)

-- dbList :: DropboxConnection -> T.Text -> IO [T.Text]
-- dbList (DBConn c) dirname = fromJSVal_aeson <$> js_dblist c (toJSString dirname)

-- foreign import javascript interruptible "($1).filesListFolder($2).then(resp => $c(_.map(resp.entries, (e => e.name))));"
--   js_dblist :: JSString -> JSVal

foreign import javascript interruptible "($1).filesUpload({path:'/' + ($2) + '.json', contents: $3}).then(function(r){ $c(true); }, function(e) { $c(false);});"
  js_dropboxSend :: JSVal -> JSString -> JSString -> IO Bool

foreign import javascript interruptible "($1).filesListFolder({path:''}).then(function(r){ $c(true); }, function(e){ $c(false); });"
  js_checkConn :: JSVal -> IO Bool
#else
dbConn :: T.Text -> IO DropboxConnection
dbConn = undefined

checkConn :: DropboxConnection -> IO Bool
checkConn = undefined

dbSend :: DropboxConnection -> T.Text -> T.Text -> IO Bool
dbSend = undefined
#endif


settings :: MonadWidget t m => m (ExperimentState t, Event t (), Dynamic t (Maybe DropboxConnection), Dynamic t T.Text)
settings = do
  pb <- getPostBuild
  (es,dbx,dbKey) <- elClass "div" "settings-top" $ do
    (es,dbx,dbKey) <- elClass "div" "settings-fields" $ do
      (dbx,dbKey) <- divClass "dropbox-connection" $ do
        dbKey <- bootstrapLabeledInput "Dropbox Token" "token"
                 (\a -> value <$> textInput (def & attributes .~ constDyn a))
        connect <- button "Connect"
        conns <- performEvent (ffor (tagPromptlyDyn dbKey connect) $ \k -> liftIO $ do
                                  c  <- dbConn k
                                  ok <- checkConn c
                                  return $ bool Nothing (Just c)  ok
                              )
        conn <- holdDyn Nothing conns
        dynText (fmap (("Good Conn:" <>) . T.pack . show . isJust) conn)
        return (conn,dbKey)
      nm   <- bootstrapLabeledInput "Subject" "subejct"
              (\a -> value <$> textInput (def & attributes .~ constDyn a))

      el "br" blank
      pics <- bootstrapLabeledInput "Images" "images"
              (\a -> value <$> textArea (def & attributes .~ constDyn ("id" =: "results" <> a)
                                             & textAreaConfig_initialValue .~ defaultPics))
      let pics' = T.lines <$> pics
      return (ExperimentState nm pics',dbx,dbKey)
    elClass "div" "settings-preview" $
      dyn $ (\pics ->
                (forM_  pics
                 (\src -> elAttr
                          "img" ("class" =: "preview-pic" <> "src" =: src)
                          blank))) <$> (_esPicSrcs es)
    return (es,dbx,dbKey)

  closeButton <- fmap (domEvent Click . fst) $ elAttr' "button" ("class" =: "settings-ok btn btn-default-btn-lg") $ bootstrapButton "ok-circle"
  return (es,closeButton,dbx,dbKey)

responsesWidget :: forall t m.MonadWidget t m
                => (T.Text -> Client t m DropboxApi)
                -> Dynamic t (Either T.Text DropboxBearer)
                -> Dynamic t [Response]
                -> m (Event t ())
responsesWidget endpoints dbKey resps = divClass "responses" $ do
  pb <- getPostBuild
  let (dbList :<|> _      :<|> _   ) = endpoints "api.dropbox.com" dbKey
      (_      :<|> dbDown :<|> dbUp) = endpoints "content.dropboxapi.com" dbKey
      rtext = (T.pack . BSL.unpack . A.encodePretty) <$> resps

  (refresh,clear,send) <- divClass "results-buttons" $ do
    refresh <- fmap (domEvent Click . fst) $ elAttr' "button" ("class" =: "btn btn-large results-select") $ do
      b <- bootstrapButton "screenshot"
      text "Refresh"
    clear <- fmap (domEvent Click . fst) $ elAttr' "button" ("class" =: "btn btn-large results-remove") $ do
      b <- bootstrapButton "remove"
      text "Reset results"
    send  <- fmap (domEvent Click . fst) $ elAttr' "button" ("class" =: "btn btn-large results-send") $ do
      b <- bootstrapButton "send"
      text "Send results"
    return (refresh,clear,send)


  let timeToName :: UTCTime -> T.Text
      timeToName t = T.pack $ formatTime defaultTimeLocale "drawcopy-%F-%H.%M.%S.json" t
  ts <- performEvent (ffor send $ \() -> liftIO getCurrentTime)
  newFilename <- traceDyn "name" <$>  holdDyn "file.json" (timeToName <$> ts)
  dbUp (((\n -> Right (DBUp ("/" <> n) Upload_add True False)) <$> newFilename)) (Right <$> resps) (() <$ updated newFilename)

  otherDataFiles <- holdDyn [] . fmap _dbesEntries . fmapMaybe reqSuccess =<<
    dbList (constDyn . Right $ DBReq "") (leftmost [refresh, pb])

  let ddEntries = ffor otherDataFiles $ \es ->
           (Nothing =: "Current")
        <> Map.fromList ((\(DBEntry n p _ _ _ _ _) -> (Just p, n)) <$> es)
  datSel :: Dynamic t (Maybe T.Text) <- value <$> dropdown Nothing ddEntries def
  otherData <- dbDown (fmap (maybe (Left "No filename") (Right . DBReq)) datSel)
                      (() <$ fmapMaybe id (updated datSel))
  dataPreview <- holdDyn [] $ leftmost [updated resps
                                       ,fmapMaybe reqSuccess otherData
                                       ,tagPromptlyDyn resps (updated datSel)
                                       ]

  dyn $ ffor dataPreview $ \dp -> elAttr "div" ("class" =: "previews" <> "style" =: "display: flex; flex-wrap:wrap;") $
    for_ dp $ \(Response subj img _ _ _ _ rImg) -> do
      elAttr "div" ("class" =: "response-preview" <> "style" =: "margin: 3px;") $ do
        elAttr "img" ("src" =: img)  blank
        elAttr "img" ("src" =: rImg <> "style" =: "width:105px;") blank

  return clear

note :: e -> Maybe a -> Either e a
note e Nothing  = Left e
note _ (Just x) = Right x

bootstrapButton :: MonadWidget t m => T.Text -> m (Event t ())
bootstrapButton glyphShortname = (domEvent Click . fst) <$>
  elAttr' "span" ("class" =: (prfx <> glyphShortname)) (return ())
  where prfx = "glyphicon glyphicon-"

------------------------------------------------------------------------------
-- | An externally-prodded validating label & text-input
bootstrapLabeledInput :: forall t m.MonadWidget t m
                      => T.Text
                      -> T.Text
                      -> (Map.Map T.Text T.Text -> m (Dynamic t T.Text))
                      -- ^ function that uses arg as attributes in widget
                      -> m (Dynamic t T.Text)
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
    blank
  elAttr "script" ("scr" =: "https://cdnjs.cloudflare.com/ajax/libs/dropbox.js/2.2.1/Dropbox-sdk.min.js")
    blank
  elAttr "link" ("href" =: "static/drawcopy.css"
              <> "rel"  =: "stylesheet"
              <> "type" =: "text/css") blank
  el "style" (text appStyle)

appStyle :: T.Text
appStyle =  T.unlines [
 ".goal-img, canvas{"
 , "  height: "     <> (T.pack . show) canvH <> "px;"
 , "  min-height: " <> (T.pack . show) canvH <> "px;"
 , "  max-height: " <> (T.pack . show) canvH <> "px;"
 , "  width: "      <> (T.pack . show) canvW <> "px;"
 , "  min-width: "  <> (T.pack . show) canvW <> "px;"
 , "  max-width: "  <> (T.pack . show) canvW <> "px;"
 ,"}\n\n"
 ]

defaultPics' :: T.Text
defaultPics' = T.unlines
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

defaultPics :: T.Text
defaultPics = T.unlines
  ["https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten6.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten5.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten49.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten47.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten44.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten41.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten40.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten38.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten37.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten23.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten19.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten16.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten17.png"
  ,"https://s3.amazonaws.com/lakecharacter/Omniglot_letters/Drawcopy/handwritten18.png"
  ]

relativizedCoord :: Int -> Int -> TimedCoord -> TimedCoord
relativizedCoord x0 y0 (TC t x y) =
  TC t (x - x0) (y - y0)

touchCoord :: Touch -> IO TimedCoord
touchCoord touch = do
  t <- getCurrentTime
  (x,y) <- bisequence (getClientX touch, getClientY touch)
  return $ TC t x y

touchRelCoord :: Int -> Int -> Touch -> IO TimedCoord
touchRelCoord x0 y0 tch = do
  relativizedCoord x0 y0 <$> touchCoord tch


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
  blank

main'' :: IO ()
main'' = mainWidget $ text "Hello"

getQueryParams :: Window -> IO (Map.Map String String)
getQueryParams w = do
  Just loc <- getLocation w
  qs <- getSearch loc
  return . Map.mapKeys T.unpack . Map.map (T.unpack . T.drop 1)
         . Map.fromList . fmap (T.breakOn "=") . T.splitOn "&" . T.dropWhile (== '?') $ T.pack qs

dbApi :: Proxy DropboxApi
dbApi = Proxy


data DropboxBearer = DBBearer T.Text

instance ToHttpApiData DropboxBearer where
  toQueryParam (DBBearer t) = "Bearer " <> t

data DBEntry = DBEntry
  { _dbeName            :: T.Text
  , _dbePath_lower      :: T.Text
  , _dbeId              :: T.Text
  , _dbeClient_modified :: UTCTime
  , _dbeServer_modified :: UTCTime
  , _dbeRev             :: T.Text
  , _dbeSize            :: Int
  } deriving (Show, Generic)

instance A.FromJSON DBEntry where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 4 }


data DBReq = DBReq {
  _dbrPath :: T.Text
  } deriving (Generic)

instance A.ToJSON DBReq where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 4 }

instance ToHttpApiData DBReq where
  toQueryParam = T.decodeUtf8 . BSL.toStrict . A.encode

data DBEntries = DBEntries {
    _dbesEntries :: [DBEntry]
  , _dbesCursor :: T.Text
  , _dbesHas_more :: Bool
  } deriving (Generic, Show)

instance A.FromJSON DBEntries where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 5 }

data DBUp = DBUp {
    _dbupPath :: T.Text
  , _dbupMode :: UploadMode
  , _dbupAutorename :: Bool
  , _dbupMute :: Bool
  } deriving (Eq, Ord, Show, Generic)

data UploadMode =  Upload_add | Upload_overwrite | Upload_update
  deriving (Eq, Show, Read, Ord, Generic)

instance A.ToJSON UploadMode where
  toJSON = A.String . T.drop 7 . T.pack . show

instance ToHttpApiData DBUp where
  toQueryParam = T.decodeUtf8 . BSL.toStrict . A.encode

instance A.ToJSON DBUp where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = fmap toLower . drop 5 }

instance Default DBUp where
  def = DBUp "/file.txt" Upload_add True False

instance MimeRender OctetStream [Response] where
  mimeRender _ val = A.encode val

type DropboxApi = "2" :> "files" :> Header "Authorization" DropboxBearer :>
  ("list_folder" :> ReqBody '[JSON] DBReq :> Post '[JSON] DBEntries
  :<|> "download" :> Header "Dropbox-API-Arg" DBReq :> Post '[JSON] [Response]
  :<|> "upload" :> Header "Dropbox-API-Arg" DBUp :> ReqBody '[OctetStream] [Response] :> Post '[JSON] DBEntry)

#ifdef ghcjs_HOST_OS
#else
data ImageData
data TouchList
data JSString
data CanvasStyle = CanvasStyle JSString
data CanvasRenderingContext2D
data ClientBoundingRect
-- data Window
getLeft :: ClientBoundingRect -> m Float
getLeft = undefined
getTop :: ClientBoundingRect -> m Float
getTop = undefined
getIdentifier = undefined
pack = undefined
getClientX = undefined
getClientY = undefined
getContext = undefined
getImageData = undefined
safe = undefined
setFillStyle = undefined
jsval = undefined
fillRect = undefined
restore = undefined
getBoundingClientRect = undefined
beginPath = undefined
fromJSVal = undefined
putImageData = undefined
save = undefined
moveTo = undefined
lineTo = undefined
setStrokeStyle = undefined
closePath = undefined
stroke = undefined
item = undefined
getChangedTouches = undefined
getLength = undefined
setLineWidth = undefined
setLineCap = undefined
getLocation = undefined
getSearch = undefined
data URL
newURL :: String -> IO URL
newURL = undefined
data JSVal
newtype BlobPropertyBag = BlobPropertyBag JSVal
newBlob' = undefined
toJSVal_aeson = undefined
revokeObjectURL = undefined
createObjectURL = undefined
toDataURL :: HTMLCanvasElement -> Maybe String -> IO T.Text
toDataURL = undefined
#endif
