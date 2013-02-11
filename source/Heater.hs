{-# LANGUAGE OverloadedStrings #-}

import Clay

import Prelude hiding (div,span,(**))
import Data.Monoid

greenlink = "#82BD10"
stonegrey = "#606060"
cloudblack = "#363636"
elephantgrey = "#303240"
abstractgrey = "#9b9b9b"

main = putCss $
  do ".loud" ? color black
     ".leftinfo" ?
       do float sideLeft
          margin 0 2 1 0          
     ".rightinfo" ?
       do float sideRight
          margin 0 0 1 2
     ".small" ?
       do fontSize (em 0.8)
          marginBottom (em 1.875)
          lineHeight (em 1.875)
     ".large" ?
       do fontSize (em 1.2)
          lineHeight (em 2.5)
          marginBottom (em 1.25)
     ".hide" ? display none
     ".hidden" ? display none
     ".highlight" ? background white
     ".last" ?
       do marginRight 0
          paddingRight 0

     "nav" ** h1 ? textAlign (alignSide sideCenter)
     "header, footer" ?
       do display block
          clear both
     "header" ?
       do ".home" &
           do zIndex 90
              position fixed
              left 0
              marginTop 0
              marginLeft (px 10)
              backgroundImage (url "/images/logo.png")
              backgroundRepeat noRepeat
              fontFamily ["sans-serif"]
              width (px 200)
              height (px 200)
          h1 ?
            do a ?
                do height (px 135)
                   display block
                   lineHeight (px 80)
               marginBottom (em 0.5)
     "footer" ?
       do marginLeft 10
          marginRight auto
          borderTop solid (px 1) "#aaaaaa"
          marginTop (em 1)
          p ?
            do fontSize (em 0.8)
               lineHeight (em 1.1)
               color "#d4d4d4"
               padding 1 0 0 0
          "#twitter" & padding 0 0 0 22
          p ** a ? color "#555555"
     "footer" ** p ** ".right" ?
       do float sideRight
          textAlign (alignSide sideRight)
          paddingRight 10
     "article" ?
       do marginBottom (em 1)
          color "#404040"
          fontSize (em 1.15)
          width  (px 800)
          h1 ?
            do margin 0 0 25 0
               color elephantgrey
          (h2 ** a) <> (h3 ** a) ? color elephantgrey
          p ?
            do fontSize (em 0.4)
               color grey
               ".meta" &
                 do fontSize (em 0.9)
                    fontStyle italic
                    color "#d4d4d4"
                    margin 1 0 0 0
                    a ?
                      do color "#555555"
          "figure" ?
            do margin 0 2 1 0
     "section" ?
       do "#page" & img ?
             do float sideLeft
                marginLeft (em 1.5)
                textAlign (alignSide sideCenter)
          "#page" & h1 ?
             do margin 0 0 1 0
                color stonegrey

     ol ? "#toc" & li ?
       do p ?
            do marginBottom (em 1)
               fontSize (em 1.0)
               color "#333333"
          h2 ?
            do fontSize (em 1.3)
               lineHeight (em 1.2)
               margin 0 0 0 0
               padding 0 0 0 0
     ol ? "#toc" & body ? ".postlist" & ul ?
       do padding 0 0 0 0
               
     "#abstract" ? abstract
     "#posts" ? posts
     "#htmltagcloud" ? htmltagcloud
     "#mobile" ? mobile

     "#posts" ** h1 ?
       do marginTop (em 0.5)
          color stonegrey
          
abstract :: Css
abstract =
  do fontSize (em 1.35)
     marginBottom 35
     color abstractgrey

posts :: Css
posts =
  do width (px 800)

htmltagcloud :: Css
htmltagcloud =
  do width auto
     fontFamily ["Lucida Sans","Lucida Grande","Lucida Sans Unicode","sans-serif"]
     margin 0 0 0 0
     padding 0 0 0 0
     lineHeight (em 1.9)
     wordSpacing normal
     letterSpacing normal
     textTransform none
     fontSize (em 0.7)
     color "#ACC1F3"
     a ?
       do
         ":link" &
           do borderRadius (px 20)
              borderWidth 0
              backgroundColor cloudblack
              color "#fff"
              padding 3 11 3 0
         ":visited" &
           do textDecoration none
              color white
         ":hover" &
           do color white
              backgroundColor greenlink
         ":active" &
           do color white
              backgroundColor "#03d"

mobile :: Css
mobile =
  do ".column" &
      do width (px 350)
         marginLeft (px 10)
         marginTop (px 10)
         marginBottom (px 10)
         backgroundImage (url "/images/logo.png")
         backgroundRepeat noRepeat