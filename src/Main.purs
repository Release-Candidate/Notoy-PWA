-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Main.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
-- | # Module Main
-- |
-- | Main entry point of the app.
module Main
  ( main
  ) where

import Prelude
import App.Constants (appElementId)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Note (KeyWordArray(..), Note(..), defaultNote, fromShared, noteKeyId)
import Data.Options (Options, defaultOptions)
import Data.String (Pattern(..), split, trim)
import Data.URL (noteUrlFromString, noteUrlToString)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Helpers.Browser (getCurrentUrl, loadFromLocalStorage, saveToLocalStorage)
import Partial.Unsafe (unsafePartial)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Internal.Types (Event)
import Web.HTML (Window, window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Window (toEventTarget)
import Web.URL (searchParams)
import Web.URL.URLSearchParams (get)

-- | Main entry point of the app.
main :: Effect Unit
main =
  HA.runHalogenAff do
    HA.awaitLoad
    appEl <- HA.selectElement (QuerySelector appElementId)
    let
      app = unsafePartial $ fromJust appEl
    runUI component unit app

{-------------------------------------------------------------------------------
| The App's state.
-}
type State
  = { options :: Options
    , note :: Note
    }

initialState :: forall input. input -> State
initialState _ =
  { options: defaultOptions
  , note: defaultNote
  }

{-------------------------------------------------------------------------------
| The app's actions, called if an event occurred.
-}
data Action
  = Initialize
  | ShareEvent Event
  | TitleChanged String
  | URLChanged String
  | KeywordsChanged String
  | ShortDescChanged String
  | LongDescChanged String
  | DownloadNote

component ::
  forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  let
    Note
      { title: noteTitle
    , url: noteURL
    , keywords: noteKeywords
    , shortDesc: noteShortDesc
    , longDesc: noteLongDesc
    } = state.note
  in
    HH.div_
      [ HH.div [ HP.id "title" ]
          [ HH.label [ HP.for "titleText" ]
              [ HH.span_ [ HH.text "Title:" ]
              , HH.br_
              , HH.input
                  [ HP.id "titleText"
                  , HP.type_ HP.InputText
                  , HP.min 50.0
                  , HP.value $ fromMaybe "" noteTitle
                  , HE.onValueInput \st -> TitleChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "url" ]
          [ HH.label [ HP.for "pageURL" ]
              [ HH.span_ [ HH.text "URL:" ]
              , HH.br_
              , HH.input
                  [ HP.id "pageURL"
                  , HP.type_ HP.InputUrl
                  , HP.min 50.0
                  , HP.value $ fromMaybe "" $ map noteUrlToString noteURL
                  , HE.onValueInput \st -> URLChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "keywords" ]
          [ HH.label [ HP.for "keyWords" ]
              [ HH.span_ [ HH.text "Keywords (comma separated):" ]
              , HH.br_
              , HH.input
                  [ HP.id "keyWords"
                  , HP.type_ HP.InputText
                  , HP.min 50.0
                  , HP.placeholder "keyword1, key word 2, Keyword 3"
                  , HP.value $ fromMaybe "" $ map show noteKeywords
                  , HE.onValueInput \st -> KeywordsChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "description" ]
          [ HH.label [ HP.for "descriptionText" ]
              [ HH.span_ [ HH.text "Short description:" ]
              , HH.br_
              , HH.textarea
                  [ HP.id "descriptionText"
                  , HP.rows 5
                  , HP.cols 60
                  , HP.value $ fromMaybe "" noteShortDesc
                  , HE.onValueInput \st -> ShortDescChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "detailed_text" ]
          [ HH.label [ HP.for "detailedDescription" ]
              [ HH.span_ [ HH.text "Detailed description:" ]
              , HH.br_
              , HH.textarea
                  [ HP.id "detailedDescription"
                  , HP.rows 5
                  , HP.cols 60
                  , HP.value $ fromMaybe "" noteLongDesc
                  , HE.onValueInput \st -> LongDescChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "save" ]
          [ HH.button
              [ HP.id "saveButton"
              , HE.onClick \_ -> DownloadNote
              ]
              [ HH.text "Save" ]
          ]
      ]

handleAction ::
  forall output m.
  MonadAff m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    win <- H.liftEffect $ window
    (loadedNote :: Maybe Note) <- H.liftEffect $ loadFromLocalStorage win noteKeyId
    case loadedNote of
      Nothing -> H.liftEffect $ log $ "No Note loaded!"
      Just savedNote -> do
        currState <-
          H.modify \state ->
            { note: savedNote
            , options: state.options
            }
        H.liftEffect $ log $ "Loaded Note: " <> show currState.note
    H.subscribe' \_ ->
      eventListener
        domcontentloaded
        (toEventTarget win)
        (\e -> Just $ ShareEvent e)
  ShareEvent e -> do
    win <- H.liftEffect $ window
    handleShare win e
  TitleChanged st -> do
    win <- H.liftEffect $ window
    newState <-
      H.modify \state ->
        let
          Note
            { url: oldUrl
          , keywords: oldKeywords
          , shortDesc: oldShortDesc
          , longDesc: oldLongDesc
          } = state.note
        in
          { note:
              Note
                { title: (Just st)
                , url: oldUrl
                , keywords: oldKeywords
                , shortDesc: oldShortDesc
                , longDesc: oldLongDesc
                }
          , options: state.options
          }
    H.liftEffect $ saveToLocalStorage win newState.note
  URLChanged st -> do
    win <- H.liftEffect $ window
    newState <-
      H.modify \state ->
        let
          Note
            { title: oldTitle
          , keywords: oldKeywords
          , shortDesc: oldShortDesc
          , longDesc: oldLongDesc
          } = state.note
        in
          { note:
              Note
                { title: oldTitle
                , url: noteUrlFromString st
                , keywords: oldKeywords
                , shortDesc: oldShortDesc
                , longDesc: oldLongDesc
                }
          , options: state.options
          }
    H.liftEffect $ saveToLocalStorage win newState.note
  KeywordsChanged st -> do
    win <- H.liftEffect $ window
    newState <-
      H.modify \state ->
        let
          Note
            { title: oldTitle
          , url: oldUrl
          , shortDesc: oldShortDesc
          , longDesc: oldLongDesc
          } = state.note
        in
          { note:
              Note
                { title: oldTitle
                , url: oldUrl
                , keywords: Just $ KeyWordArray $ map trim $ split (Pattern ",") st
                , shortDesc: oldShortDesc
                , longDesc: oldLongDesc
                }
          , options: state.options
          }
    H.liftEffect $ saveToLocalStorage win newState.note
  ShortDescChanged st -> do
    win <- H.liftEffect $ window
    newState <-
      H.modify \state ->
        let
          Note
            { title: oldTitle
          , url: oldUrl
          , keywords: oldKeywords
          , longDesc: oldLongDesc
          } = state.note
        in
          { note:
              Note
                { title: oldTitle
                , url: oldUrl
                , keywords: oldKeywords
                , shortDesc: Just st
                , longDesc: oldLongDesc
                }
          , options: state.options
          }
    H.liftEffect $ saveToLocalStorage win newState.note
  LongDescChanged st -> do
    win <- H.liftEffect $ window
    newState <-
      H.modify \state ->
        let
          Note
            { title: oldTitle
          , url: oldUrl
          , keywords: oldKeywords
          , shortDesc: oldShortDesc
          } = state.note
        in
          { note:
              Note
                { title: oldTitle
                , url: oldUrl
                , keywords: oldKeywords
                , shortDesc: oldShortDesc
                , longDesc: Just st
                }
          , options: state.options
          }
    H.liftEffect $ saveToLocalStorage win newState.note
  DownloadNote -> pure unit

shareTargetFields ::
  { text :: String
  , title :: String
  , url :: String
  }
shareTargetFields = { title: "title", url: "url", text: "text" }

{-------------------------------------------------------------------------------
| Event handler for the share event (`domcontentloaded`).
|
| Called, when a website has been shared with this app.
|
| On Android, the data of the shared URL is as follows:
| Title: Hacker News
| URL is empty.
| Text: https://news.ycombinator.com/news
-}
handleShare ::
  forall output m.
  MonadAff m =>
  Window -> Event -> H.HalogenM State Action () output m Unit
handleShare win _ = do
  url <- H.liftEffect $ getCurrentUrl unit
  case url of
    Just u -> do
      let
        toSearch = searchParams u

        sharedTitle = get shareTargetFields.title toSearch

        sharedUrl = get shareTargetFields.url toSearch

        maybeURL = noteUrlFromString =<< sharedUrl

        sharedText = get shareTargetFields.text toSearch

        note = fromShared sharedTitle maybeURL sharedText
      case note of
        Note
          { title: Nothing
        , url: Nothing
        , keywords: Nothing
        , shortDesc: Nothing
        , longDesc: Nothing
        } -> pure unit
        _ -> do
          H.liftEffect $ saveToLocalStorage win note
          H.modify_ \state ->
            { note: note
            , options: state.options
            }
          H.liftEffect $ log $ "Got shared note: " <> show note
    Nothing -> pure unit
