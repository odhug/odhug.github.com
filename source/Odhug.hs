{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
--
-- License     : BSD
-- Maintainer  : Sergey Bushnyak, sergey.bushnyak@sigrlami.eu
-- Stability   : experimental
-- Portability : GHC
--
-- Entry point for site publishing

module Main where

import Control.Applicative
import Control.Error hiding (left)
import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import Data.Maybe
import Data.String.Utils (replace)
import Data.Time
import Data.Time.Format
import Hakyll
import System.Locale
import Text.JSON
import Text.Printf
import Clay
import Prelude hiding (div, span)
import qualified Data.Map as Map

----------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

  match "templates/*" $ compile templateCompiler

  match "css/*" $ do
    route idRoute
    compile $ compressCssCompiler

  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "js/**" $ do
    route idRoute
    compile copyFileCompiler

  match "about.md" $ compile $ pandocCompiler

  match "posts/*.md" $ version "pandoc" $
    compile $ pandocCompiler

  match "posts/*.md" $ do
    route $ setExtension "html"
    compile $ do
      i <- getUnderlying
      html <- load $ setVersion (Just "pandoc") i   
      (return html { itemIdentifier = i })
      >>= loadAndApplyTemplate "templates/post.html" (postUrlCtx <> defaultContext)
      >>= loadAndApplyTemplate "templates/indefault.html" defaultContext   
      >>= relativizeUrls  

  create ["index.html"] $ do
    route idRoute
    compile $ do {
      about  <- loadBody "about.md";
      images <- images;
      makeItem "" >>=
      loadAndApplyTemplate "templates/index.html"
         (constField "images" images <>
          constField "about" about <>
          defaultContext) >>=
      loadAndApplyTemplate
        "templates/default.html"
        defaultContext
       >>= relativizeUrls
    }

  create ["blog.html"] $ do
    route idRoute
    compile $ do {
      posts <- posts;
      makeItem "" >>= 
      loadAndApplyTemplate "templates/posts.html"
        (constField "posts" posts <> defaultContext)  >>=
      loadAndApplyTemplate "templates/default.html"
        defaultContext
       >>= relativizeUrls
    }

  create ["js/events.js"] $ do
    route idRoute
    compile $ do
      posts <- loadAll ("posts/*.md" .&&. hasNoVersion)
      tmpl <- loadBody "templates/event.tpl"
      events <-
        liftM (makeEventList . catMaybes) $ forM posts $ \p -> runMaybeT $ do
          mdata <- lift $ getMetadata $ itemIdentifier p
          dateStr <- hoistMaybe $ Map.lookup "eventDate" mdata
          let
            date :: Day
            date = parseDate (itemIdentifier p) dateStr
            prettyDate = formatTime ourLocale "%-d %B %Y" date
            ymdDate = formatDate date
          url <-
            lift $ postUrl p
          description <-
            liftM itemBody $
            lift $
            applyTemplate tmpl (constField "url" url <>
                                constField "date" prettyDate <>
                                constField "datetime" ymdDate <> defaultContext) p
          return (date, description)
      loadAndApplyTemplate
        "templates/events.tpl"
        (constField "events" events)
        =<< makeItem ()

  -- render forum
  create ["forum.html"] $ do
  route idRoute
  compile $ makeItem "" >>=
            loadAndApplyTemplate "templates/forum.html"
            defaultContext >>=
            loadAndApplyTemplate "templates/default.html"
            defaultContext

  -- rss feed
  -- create ["feed.rss"] $ do
  -- route idRoute
  -- compile $ makeItem "" >>=
  --           loadAndApllyTemplate "templates/feed.tpl"
  --           defaultContext


----------------------------------------------------------------------

postUrl p =
  fmap (maybe "" toUrl) .
  getRoute .
  setVersion Nothing $
  itemIdentifier p

postUrlCtx = field "postUrl" postUrl

posts = do
  posts <- loadAll ("posts/*.md" .&&. hasVersion "pandoc")
  tmpl <- loadBody "templates/post-item.html"
  let ctx = postUrlCtx <> defaultContext
  applyTemplateList tmpl ctx $ recentFirst posts

images = do
  images <- loadAll "images/promo/*";
  imgTpl <- loadBody "templates/image-item.html";
  let imageCtx :: Context CopyFile
      imageCtx = mconcat
               [ urlField "url"
               , missingField  -- For better error messages
               ]
  images' <- applyTemplateList imgTpl imageCtx images
  return  $ replace "src=\"/" "src=\"./" images'

parseDate loc str =
  fromMaybe err $ parseTime defaultTimeLocale "%Y-%m-%d" str
  where err = error $ printf "Bad date in %s: %s"

formatDate date = formatTime defaultTimeLocale "%Y-%m-%d" date

makeEventList :: [(Day, String)] -> String
makeEventList events =
  encode $ flip Prelude.map events $ \(date, description) ->
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

