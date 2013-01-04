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
import Hakyll

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
    compile $ do
       dummyItem <- makeItem ""
       body <-
         loadAndApplyTemplate "templates/index.html"
          ( constField "fevents" "" <>
            constField "pevents" "" <>
            defaultContext) dummyItem
       loadAndApplyTemplate
        "templates/default.html"
        (constField "body" (itemBody body) <> defaultContext)
        dummyItem

  match "posts/*" $ compile pandocCompiler

  match "blog.html" $ do
    route idRoute
    compile $ do
      posts <- posts
      dummyItem <- makeItem ""
      blog <-
        loadAndApplyTemplate "templates/posts.html"
          (constField "posts" posts <> defaultContext)
          dummyItem
      loadAndApplyTemplate "templates/default.html"
        (constField "body" (itemBody blog) <> defaultContext)
        dummyItem
