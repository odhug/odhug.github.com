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
--import CopyImage
--import Text.Atom.Feed

----------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

  -- Read templates
  match "templates/*" $ compile templateCompiler

  -- render css files
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler
  
  -- render images  
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "about.md" $ compile $ pandocCompiler

  -- render index page
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
    }

  match "posts/*" $ compile pandocCompiler

  create ["blog.html"] $ do
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
  create ["events.js"] $ do
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
            applyTemplate tmpl (constField "url" "todo" <> 
                                constField "date" prettyDate <> 
                                constField "datetime" ymdDate <> defaultContext) p
          return (date, description)
      loadAndApplyTemplate
        "templates/events.js"
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

posts = do
  posts <- loadAll "posts/*.md"
  tmpl <- loadBody "templates/post.html"
  let ctx = defaultContext
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

carouselS :: Css
carouselS =  ".carousel" 
             & do
               border    solid  (px 10)  white
               boxShadow (em 0) (em 0.3) (em 0.8) black
               margin    (px 0) auto auto auto
               padding   (px 0) auto auto auto

--horizontalS :: Integer -> Css
--horizontalS size = ".horizontal" 
--              & do
--                width    (px size) 
--                position relative
--                overflow hidden

--hItemS :: Integer -> Css
--hItemS size = ".horizontal .item"
--                $ do
--                  width  (px size)  
--                  margin 0 auto auto auto;
--                  float  left;

--hItemsS :: Integer -> Css
--hItemsS max = ".horizontal .items"
--              & do
--                width (px max)
--                animation hscroll 20s infinite

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

