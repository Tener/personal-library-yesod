{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.User where

import Import
import Control.Arrow
import qualified Data.Text

import Handler.Utils

-- user

userForm proto = renderTable $ userAForm proto

userAForm :: (Maybe User) -> AForm App App User
userAForm proto = User
   <$> areq textField "Identifier" (fmap userIdent proto)
   <*> areq (selectFieldList roles) "Role" (fmap userRole proto)
   <*> aopt textField "Name" (fmap userName proto)
   <*> aopt textField "Address" (fmap userAddress proto)
   <*> aopt textField "Telephone" (fmap userTelephone proto)
       where
         roles :: [(Text,Role)]
         roles = map (Data.Text.pack . showRole &&& id) [minBound..maxBound] -- buahahahah

getUserViewR :: UserId -> Handler RepHtml
getUserViewR uid = do
  user :: User <- runDB $ get404 uid
  giveAdminToMyself
  isAdmin <- checkAdminUser
  isMyself <- checkYourself uid
  (fwidget, enctype) <- generateFormPost (userForm (Just user))
  defaultLayout $ do
      setTitle $ toHtml $ "User " ++ show (userIdent user)
      $(widgetFile "user-view")

postUserEditR :: UserId -> Handler RepHtml
postUserEditR uid = do
  user :: User <- runDB $ get404 uid
  ((result, _), _) <- runFormPost (userForm (Just user))
  case result of
    FormSuccess new -> runDB $ replace uid new
    _ -> return ()
  redirect (UserViewR uid)


getUserAllR :: Handler RepHtml
getUserAllR = do
  users :: [Entity User] <- runDB $ selectList [] [Asc UserId]
  defaultLayout $ do
      setTitle "A list of all users."
      $(widgetFile "users-all")
