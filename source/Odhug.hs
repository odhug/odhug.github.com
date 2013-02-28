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

import qualified Data.Map as Map

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
import Data.List (isPrefixOf)
import Prelude hiding (div, span)

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

  match "posts/*.md" $
    version "pandoc" $
    compile pandocCompiler

  match "posts/*.md" $ do
    route   $ setExtension "html"
    compile $ do
      item <- getUnderlying
      html <- load $ setVersion (Just "pandoc") item
      return html { itemIdentifier = item }
        >>= loadAndApplyTemplate "templates/post.html" ( postUrlCtx <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do 
      about  <- loadBody "about.md"
      images <- images
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html"
          (constField "images" images <> constField "about" about <> defaultContext) 
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["blog.html"] $ do
    route idRoute
    compile $ do 
      posts <- posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html" (constField "posts" posts <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["js/events.js"] $ do
    route idRoute
    compile $ do 
      posts  <- loadAll ("posts/*.md" .&&. hasNoVersion)
      tpl    <- loadBody "templates/event.tpl"
      events <- loadEvents posts tpl
      makeItem ""
        >>= loadAndApplyTemplate "templates/events.tpl" (constField "events" events)

  create ["forum.html"] $ do
  route idRoute
  compile $ makeItem ""
    >>= loadAndApplyTemplate "templates/forum.html" defaultContext
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls

  create ["rss.xml"] $ do
    route idRoute
    let descContext = field "description" (return . itemBody)
    compile $
      loadAll ("posts/*.md" .&&. hasVersion "pandoc") >>=
      renderRss feedConf (field "url" postUrl <> descContext <> defaultContext)

----------------------------------------------------------------------

postUrl :: Item a -> Compiler String
postUrl item = fmap (maybe "" toUrl) . getRoute . setVersion Nothing $ itemIdentifier item
  
postUrlCtx :: Context a
postUrlCtx = field "postUrl" postUrl
  
-- generate posts list to use in blog page
posts :: Compiler String
posts = do
  posts <- loadAll ("posts/*.md" .&&. hasVersion "pandoc")
  tmpl  <- loadBody "templates/post-item.html"
  applyTemplateList tmpl (postUrlCtx <> defaultContext) $ recentFirst posts

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

loadEvents :: [Item String] -> Template -> Compiler String
loadEvents posts tmpl = do
  liftM (makeEventList . catMaybes) $ forM posts $ \item -> runMaybeT $ do 
    mdata   <- lift $ getMetadata $ itemIdentifier item
    dateStr <- hoistMaybe $ Map.lookup "eventDate" mdata
    let
      date :: Day
      date = parseDate (itemIdentifier item) dateStr
      prettyDate = formatTime ourLocale "%-d %B %Y" date
      ymdDate = formatDate date
    url <- lift $ postUrl item
    description <- liftM itemBody $ lift $
      applyTemplate tmpl ( constField "url" url <>
                           constField "date" prettyDate <>
                           constField "datetime" ymdDate <> defaultContext) item
      -- NB: this relativizeUrlsWith call relies on the fact that we only
      -- show URLs at the root
      >>= return . fmap (relativizeUrlsWith ".")
    return (date, description)

makeEventList :: [(Day, String)] -> String
makeEventList events =
  encode $ flip Prelude.map events $ \(date, description) ->
    toJSObject
      [("eventDescription", description)
      ,("eventDate", formatDate date)]

parseDate :: Identifier -> String -> Day
parseDate loc str =
  fromMaybe err $ parseTime defaultTimeLocale "%Y-%m-%d" str
  where err = error $ printf "Bad date in %s: %s"

formatDate date = formatTime defaultTimeLocale "%Y-%m-%d" date

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

feedConf = FeedConfiguration
  { feedTitle = "Odessa Haskell User Group"
  , feedDescription = "OdHUG news and announcements"
  , feedAuthorName = "OdHUG"
  , feedAuthorEmail = ""
  , feedRoot = "http://odhug.github.com/site"
  }
