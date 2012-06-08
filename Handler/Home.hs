{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Home where

import Import
import Handler.Utils

--- home
getHomeR :: Handler RepHtml
getHomeR = do
  thisUserRole <- getThisUserRole
  let auth = thisUserRole [AdminRole, ResidentRole]
  defaultLayout $ do
             setTitle "Welcome to media library!"
             $(widgetFile "home")

