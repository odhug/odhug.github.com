{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

--
-- License     : BSD
-- Maintainer  : Sergey Bushnyak, sergey.bushnyak@sigrlami.eu
-- Stability   : experimental
-- Portability : GHC
--
--
-- Entry point for site publishing

module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (&&&), (***), (+++), (|||), arr, second)
import Data.Monoid (mempty, mconcat)
import Data.Either (rights)
import Data.Maybe
import Data.Time
import Data.Time.Format
import System.Locale
import Control.Category (id)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Text.Blaze.Html (Html, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.ByteString (ByteString)

import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import qualified Data.ByteString as BS

import Hakyll

----------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

  -- render css files
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  -- renser images
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  -- render blog posts
  match "posts/*" $ do
    route   $ setExtension ".html"
    compile $ pageCompiler
      >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
      >>> renderTagsField "prettytags" (fromCapture "tags/*")
      >>> applyTemplateCompiler "templates/post.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

  -- render blog posts list
  match  "blog.html" $ route idRoute
  create "blog.html" $ constA mempty
    >>> arr (setField "title" "Официальный блог OdHUG")
    >>> requireAllA "posts/*" addPostList
    >>> applyTemplateCompiler "templates/posts.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

  -- render index page
  match "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Home")
    >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
    >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . chronological) >>> addPostList)
    >>> applyTemplateCompiler "templates/index.html"
    >>> applyTemplateCompiler "templates/default.html"
    >>> relativizeUrlsCompiler

  -- Tags
  create "tags" $
    requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

  -- Add a tag list compiler for every tag
  match "tags/*" $ route $ setExtension ".html"
  metaCompile $ require_ "tags"
    >>> arr tagsMap
    >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

  -- Render RSS feed
  match "rss.xml" $ route idRoute
  create "rss.xml" $
    requireAll_ "posts/*"
      >>> mapCompiler (arr $ copyBodyToField "description")
      >>> renderRss feedConfiguration

  -- Read templates
  match "templates/*" $ compile templateCompiler
  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/item.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "OdHUG RSS feed"
    , feedDescription = "Official OdHUG channel"
    , feedAuthorName  = "OdHUG"
    , feedAuthorEmail = ""
    , feedRoot        = "odhug.github.com"
    }





