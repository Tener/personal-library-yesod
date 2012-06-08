{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.User where

import Import
import Control.Arrow
import qualified Data.Text

import Handler.Utils
import Handler.File(deleteFile)

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

--- delete user

userDeleteForm = renderTable (const <$> areq areYouSureField "Are you sure?" (Just False))
  where areYouSureField = check isSure boolField
        isSure False = Left ("You must be sure to delete a user" :: Text)
        isSure True = Right True

getUserDeleteR ::UserId ->  Handler RepHtml
getUserDeleteR key = do
  user :: User <- runDB $ get404 key
  (fwidget, enctype) <- generateFormPost userDeleteForm
  defaultLayout $ do
      setTitle "Deleting a user."
      $(widgetFile "user-delete")

postUserDeleteR ::UserId ->  Handler RepHtml
postUserDeleteR key = do
  user :: User <- runDB $ get404 key
  ((result,fwidget), enctype) <- runFormPost userDeleteForm

  case result of
    FormSuccess _ -> do
                files <- runDB $ selectList [FileSender ==. key] []
                mapM_ deleteFile files
                runDB $ do
                      deleteWhere [RentTakenBy ==. key]
                      updateWhere [RentAuthorizedBy ==. Just key] [RentAuthorizedBy =. Nothing]
                      deleteWhere [ReviewUser ==. key]
                      delete key
                defaultLayout [whamlet|
                               <p> <strong>User deleted.</strong> |]
    _ -> defaultLayout $ do
              setTitle "Deleting a user."
              $(widgetFile "user-delete")

