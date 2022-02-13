-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Constants.purs
-- Date:     06.Jan.2022
--
-- =============================================================================
-- | Module App.Constants, various constants used in the app.
module App.Constants
  ( appElementId
  , downloadAttr
  , hiddenURLId
  , hrefAttr
  , localeToTranslation
  , noteTextGetPosition
  , noteTextKeywords
  , noteTextLongDescription
  , noteTextPosition
  , noteTextSave
  , noteTextShare
  , noteTextShortDescription
  , noteTextTitle
  , noteTextUrl
  , optionsTextFormat
  , optionsTextMarkdown
  , optionsTextOrgMode
  , optionsTextPosLookup
  , optionsTextText
  , optionsTextTimestamp
  , optionsTextYAML
  , translateToDe
  , translateToEn
  , translateToEo
  , translateToSK
  ) where

import Prelude
import Data.DateTimeFormat (Locale(..))
import Data.String (take, toLower)

{-------------------------------------------------------------------------------
| HTML attribute `download`.
-}
downloadAttr :: String
downloadAttr = "download"

{-------------------------------------------------------------------------------
| HTML attribute `href`.
-}
hrefAttr :: String
hrefAttr = "href"

{-------------------------------------------------------------------------------
| The id of the HTML div to render the app to.
|
| "app"
-}
appElementId :: String
appElementId = "app"

{-------------------------------------------------------------------------------
| The id of the hidden `a` HTML element used to download the note.
|
| "hiddenURL"
-}
hiddenURLId ∷ String
hiddenURLId = "hiddenURL"

{-------------------------------------------------------------------------------
| A record for internationalization texts.
-}
type Translation
  = { en :: String
    , de :: String
    , sk :: String
    , eo :: String
    }

localeToTranslation ∷ Locale → (Translation → String)
localeToTranslation (Locale localeStr) = case toLower $ take 2 localeStr of
  "en" -> translateToEn
  "de" -> translateToDe
  "sk" -> translateToSK
  "eo" -> translateToEo
  _ -> translateToEn

{-------------------------------------------------------------------------------
| Return the English translation.
-}
translateToEn :: Translation -> String
translateToEn trans = trans.en

{-------------------------------------------------------------------------------
| Return the German translation.
-}
translateToDe :: Translation -> String
translateToDe trans = trans.de

{-------------------------------------------------------------------------------
| Return the Slovak translation.
-}
translateToSK :: Translation -> String
translateToSK trans = trans.sk

{-------------------------------------------------------------------------------
| Return the Esperanto translation.
-}
translateToEo :: Translation -> String
translateToEo trans = trans.eo

noteTextTitle :: Translation
noteTextTitle =
  { en: "Title:"
  , de: "Überschrift:"
  , eo: "Titolo:"
  , sk: "Názov:"
  }

noteTextUrl :: Translation
noteTextUrl =
  { en: "Link:"
  , de: "Link:"
  , eo: "Ligilo:"
  , sk: "Link:"
  }

noteTextKeywords :: Translation
noteTextKeywords =
  { en: "Keywords (comma separated):"
  , de: "Schlagworte (getrennt mit Beistrichen):"
  , eo: "Etikedoj (separitaj per komoj):"
  , sk: "Kľúčové slová (oddelené čiarkami):"
  }

noteTextPosition :: Translation
noteTextPosition =
  { en: "Location:"
  , de: "Ort:"
  , eo: "Loko:"
  , sk: "Pozícia:"
  }

noteTextGetPosition :: Translation
noteTextGetPosition =
  { en: "Get position"
  , de: "Position abfragen"
  , eo: "Demandi lokon"
  , sk: "Get position"
  }

optionsTextFormat :: Translation
optionsTextFormat =
  { en: "Note file format"
  , de: "Format der erstellten Notiz:"
  , eo: "Formato de la noto:"
  , sk: "Zvolený formát poznámok:"
  }

noteTextShortDescription :: Translation
noteTextShortDescription =
  { en: "Short description:"
  , de: "Kurze Beschreibung:"
  , eo: "Mallonga priskribo:"
  , sk: "Krátky popis:"
  }

noteTextLongDescription :: Translation
noteTextLongDescription =
  { en: "Detailed description:"
  , de: "Ausführliche Beschreibung:"
  , eo: "Detala priskribo:"
  , sk: "Podrobný popis:"
  }

noteTextSave :: Translation
noteTextSave =
  { en: "Save"
  , de: "Speichern"
  , eo: "Konservi"
  , sk: "Uložiť"
  }

noteTextShare :: Translation
noteTextShare =
  { en: "Share"
  , de: "Teilen"
  , eo: "Diskonigi"
  , sk: "Zdieľať"
  }

optionsTextMarkdown :: Translation
optionsTextMarkdown =
  { en: "Markdown (Obsidian, Joplin, Zettlr)"
  , de: "Markdown (Obsidian, Joplin, Zettlr)"
  , sk: "Markdown (Obsidian, Joplin, Zettlr)"
  , eo: "Markdown (Obsidian, Joplin, Zettlr)"
  }

optionsTextOrgMode :: Translation
optionsTextOrgMode =
  { en: "Org-Mode (Emacs)"
  , de: "Org-Mode (Emacs)"
  , sk: "Org-Mode (Emacs)"
  , eo: "Org-Mode (Emacs)"
  }

optionsTextText :: Translation
optionsTextText =
  { en: "Plain Text"
  , de: "Nur Text"
  , sk: "Text"
  , eo: "Text"
  }

optionsTextTimestamp :: Translation
optionsTextTimestamp =
  { en: "Add current date to note?"
  , de: "Aktuelles Datum zur Notiz hinzufügen?"
  , eo: "Ĉu aldoni hodiaŭan daton al la noto?"
  , sk: "Potrebujete pridať aktuálny dátum?"
  }

optionsTextPosLookup :: Translation
optionsTextPosLookup =
  { en: "Look the position up on BigData?"
  , de: "Den Ortsnamen auf BigData abfragen?"
  , sk: "Look the position up on BigData?"
  , eo: "Look the position up on BigData?"
  }

optionsTextYAML :: Translation
optionsTextYAML =
  { en: "Add YAML front matter (metadata for e.g. Pandoc)?"
  , de: "YAML Front Matter (YAML Metadata für Pandoc) hinzufügen?"
  , eo: "Ĉu aldoni 'YAML Front Matter' (YAML metadatumojn por Pandoc) al la noto?"
  , sk: "Pridať YAML Front Matter (YAML meta data pre Pandoc)?"
  }
