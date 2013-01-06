{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- TODO rss feed

--
-- License     : BSD
-- Maintainer  : Sergey Bushnyak, sergey.bushnyak@sigrlami.eu
-- Stability   : experimental
-- Portability : GHC
--
--
-- Entry point for site publishing

module Main where

import Data.Monoid
import Data.Maybe
import Data.Time
import Data.Time.Format
import System.Locale
import Control.Monad
import Control.Applicative
import Data.Maybe
import Control.Monad.Trans
import Control.Error
import Text.Printf
import Hakyll
import Text.JSON
import qualified Data.Map as Map

----------------------------------------------------------------------

posts = do
  posts <- loadAll "posts/*.md"
  tmpl <- loadBody "templates/post.html"
  let ctx = defaultContext
  applyTemplateList tmpl ctx $ recentFirst posts

main :: IO ()
main = hakyll $ do

  -- Read templates
  match "templates/*" $ compile templateCompiler

  -- render css files
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  -- render images
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  -- render index page
  match "index.html" $ do
    route idRoute
    compile $
      makeItem "" >>=
      loadAndApplyTemplate "templates/index.html"
         (constField "fevents" "" <>
          constField "pevents" "" <>
          defaultContext) >>=
      loadAndApplyTemplate
        "templates/default.html"
        defaultContext

  match "posts/*" $ compile pandocCompiler

  match "blog.html" $ do
    route idRoute
    compile $ do {
      posts <- posts;

      makeItem "" >>=

      loadAndApplyTemplate "templates/posts.html"
        (constField "posts" posts <> defaultContext) >>=

      loadAndApplyTemplate "templates/default.html"
        defaultContext
    }

  {-
  match "posts/*.md" $ do
    route $ setExtension "html"
    compile $ pandocCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext
  -}
  match "events.js" $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*.md"
      tmpl <- loadBody "templates/eventDescription.tmpl"
      events <-
        liftM (makeEventList . catMaybes) $ forM posts $ \p -> runMaybeT $ do
          mdata <- lift $ getMetadata $ itemIdentifier p
          dateStr <- hoistMaybe $ Map.lookup "eventDate" mdata
          let
            date :: Day
            date = parseDate (itemIdentifier p) dateStr
            prettyDate = formatTime ourLocale "%-d %B %Y" date
            ymdDate = formatDate date
          description <-
            liftM itemBody $
            lift $
            applyTemplate tmpl (constField "url" "todo" <> constField "date" prettyDate <> constField "datetime" ymdDate <> defaultContext) p
          return (date, description)
      loadAndApplyTemplate
        "templates/events.js"
        (constField "events" events)
        =<< makeItem ()

parseDate loc str =
  fromMaybe err $ parseTime defaultTimeLocale "%Y-%m-%d" str
  where err = error $ printf "Bad date in %s: %s"

formatDate date = formatTime defaultTimeLocale "%Y-%m-%d" date

makeEventList :: [(Day, String)] -> String
makeEventList events =
  encode $ flip map events $ \(date, description) ->
    toJSObject
      [("eventDescription", description)
      ,("eventDate", formatDate date)]

ourLocale = defaultTimeLocale
  { months = [(m,m) | m <- ms] }
  where
  ms =
    [ "января"
    , "февраля"
    , "марта"
    , "апреля"
    , "мая"
    , "июня"
    , "июля"
    , "августа"
    , "сентября"
    , "октября"
    , "ноября"
    , "декабря"
    ]
