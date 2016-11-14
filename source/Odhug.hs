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

import           Control.Applicative
import           Control.Error hiding (left)
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import           Data.List (isPrefixOf)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String.Utils (replace)
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Format
import           Hakyll
import           Hakyll.Core.Metadata
import           Prelude hiding (div, span)
import           System.Locale hiding (defaultTimeLocale, months)
import           Text.JSON
import           Text.Pandoc.Options
import           Text.Printf

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

  match "about.md" $ compile $ ourPandocCompiler

  match "aboutf.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/about.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html" 
      (constField "title" tAbout <> defaultContext)
      >>= relativizeUrls

  match "posts/*.md" $
    version "pandoc" $
    compile ourPandocCompiler

  match "posts/*.md" $ do
    route   $ setExtension "html"
    compile $ do
      item <- getUnderlying
      html <- load $ setVersion (Just "pandoc") item
      return html { itemIdentifier = item }
        >>= loadAndApplyTemplate "templates/post.html"
        ( postUrlCtx <> defaultContext)
        >>= loadAndApplyTemplate "templates/defaultn.html"
        ( titleCtx <> defaultContext)
        >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do 
      about  <- loadBody "about.md"
      images <- images
      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html"
          (constField "images" images <> constField "about" about <> defaultContext) 
        >>= loadAndApplyTemplate "templates/default.html"
          (constField "title" tIndex <> defaultContext)
        >>= relativizeUrls

  create ["blog.html"] $ do
    route idRoute
    compile $ do 
      posts <- posts
      makeItem ""
        >>= loadAndApplyTemplate "templates/posts.html"
        (constField "posts" posts <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html"
        (constField "title" tBlog <> defaultContext)
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
      >>= loadAndApplyTemplate "templates/default.html"
      (constField "title" tForum <> defaultContext)
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

titleCtx :: Context a
titleCtx =
  field "title" $ \item ->
  do
    let identifier = itemIdentifier item
    metadata <- getMetadata $ identifier
    let title =
          case HM.lookup "title" metadata of
            Nothing    -> ""
            Just value ->  -- this is not actual value, case it again
              case value of
                String val -> "OdHUG - " ++ (T.unpack val)
                _          -> ""
    return title

-- generate posts list to use in blog page
posts :: Compiler String
posts = do
  posts <- loadAll ("posts/*.md" .&&. hasVersion "pandoc")
  tmpl  <- loadBody "templates/post-item.html"
  applyTemplateList tmpl (postUrlCtx <> defaultContext) =<< recentFirst posts

images = do
  images <- loadAll "images/promo/*";
  imgTpl <- loadBody "templates/image-item.html";
  let imageCtx :: Context CopyFile
      imageCtx = mconcat
               [ urlField "url" 
               , missingField  -- For better error messages
               ]
  aimage  <- applyTemplate     imgTpl imageCtx $ head images
  images' <- applyTemplateList imgTpl imageCtx $ tail images
  return $ replace "src=\"/" "src=\"./"
         $ (replace "item" "active item" $ itemBody aimage) ++ images'

loadEvents :: [Item String] -> Template -> Compiler String
loadEvents posts tmpl = do
  liftM (makeEventList . catMaybes) $ forM posts $ \item -> runMaybeT $ do 
    mdata   <- lift $ getMetadata $ itemIdentifier item
    dateStr <- hoistMaybe $ case HM.lookup "eventDate" mdata of
                              Nothing    -> Nothing
                              Just value ->  -- this is not actual value, case it again
                                case value of
                                  String val -> Just $ T.unpack val
                                  _          -> Nothing
    let date :: Day
        date = parseDate (itemIdentifier item) dateStr
        prettyDate = formatTime ourLocale "%-d %B %Y" date
        ymdDate    = formatDate date
        
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
  Text.JSON.encode $ flip Prelude.map events $ \(date, description) ->
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
  , feedRoot = "haskell.od.ua"
  }

ourPandocCompiler =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerEmailObfuscation = NoObfuscation }

tIndex = "OdHUG"
tBlog  = "OdHUG — блог"
tForum = "OdHUG — форум"
tAbout = "OdHUG - о нас"

