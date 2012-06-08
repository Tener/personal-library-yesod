{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Rent where

import Import
import Data.Time
import Control.Monad (when)

import Handler.Utils

-- widgets

assetRentFormWidget :: AssetId -> UserId -> Handler Widget
assetRentFormWidget aid uid = do
  now <- liftIO getCurrentTime
  users :: [Entity User] <- runDB $ selectList [] []

  form1 <- generateFormPost (renderDivs $ rentForm'guestUser now aid uid)
  form2 <- generateFormPost (renderDivs $ rentForm'sameUser now aid uid)
  form3 <- generateFormPost (renderDivs $ rentForm'forOtherUser now aid uid users)
  
  free <- checkAssetFree aid
  isAdmin <- checkAdminUser
  isGuest <- checkGuestUser
  return $(widgetFile "asset-rent-form-widget")

assetRentFormWidgetR :: AssetId -> Handler Widget
assetRentFormWidgetR aid = do
  uid'ent <- requireAuth
  let uid = (entityKey uid'ent) 
  assetRentFormWidget aid uid

assetRentFormWidgetM :: AssetId -> Handler (Maybe Widget)
assetRentFormWidgetM aid = do
  maid <- maybeAuth
  case maid of
    Nothing -> return Nothing
    Just uid'ent -> do
        let uid = (entityKey uid'ent) 
        w <- (assetRentFormWidget aid uid)
        return (Just w)

-- forms

rentForm'sameUser :: UTCTime -> AssetId -> UserId -> AForm App App Rent
rentForm'sameUser now aid uid = 
    Rent aid uid (Just uid) (Just (utctDay now)) Nothing
     <$> areq textareaField "Comment" (Just (Textarea "comment here"))
     <*> pure now

rentForm'guestUser :: UTCTime -> AssetId -> UserId -> AForm App App Rent
rentForm'guestUser now aid uid = 
    Rent aid uid Nothing Nothing Nothing
     <$> areq textareaField "Comment" (Just (Textarea "comment here"))
     <*> pure now

rentForm'forOtherUser :: UTCTime -> AssetId -> UserId -> [Entity User] -> AForm App App Rent
rentForm'forOtherUser now aid uid other'users = 
    (\ other -> Rent aid other (Just uid) (Just (utctDay now)) Nothing)
     <$> areq (selectFieldList users'opt) "User" Nothing
     <*> areq textareaField "Comment" (Just (Textarea "comment here"))
     <*> pure now
 
     where
       users'opt :: [(Text,UserId)]
       users'opt = map one'user (filter (\user -> entityKey user /= uid) other'users)
       one'user ent = ((userIdent $ entityVal ent), (entityKey ent))

-- handlers

getRentViewR :: RentId -> Handler RepHtml
getRentViewR rid = do
  rent <- runDB $ get404 rid
  role <- getThisUserRole

  let getThisUserId = do
        mu <- maybeAuth
        case mu of
          Nothing -> return (const False)
          Just user -> return (\uid -> uid == entityKey user)
  checkSelf <- getThisUserId

  let isAdmin = role [AdminRole]
      isAdminOrResident = role [AdminRole, ResidentRole]
      canUserSee uid = checkSelf uid || role [AdminRole, ResidentRole]


  defaultLayout $ do
    setTitle "View rental"
    $(widgetFile "rental-view")

postRentDeleteR, getRentDeleteR :: RentId -> Handler RepHtml
postRentDeleteR rid = do
  rent <- runDB $ get404 rid
  ((result,fwidget), enctype) <- runFormPost rentalDeleteForm
  case result of
    FormSuccess _yesPerformDelete -> do
                         runDB $ delete rid
                         defaultLayout [whamlet|
                                        <p> <strong>Rental of an <a href=@{AssetViewR (rentWhat rent)}>item</a> deleted.</strong> |]
    _ -> defaultLayout $ do
            setTitle "Deleting a rental."
            $(widgetFile "rental-delete")

getRentDeleteR rid = do
  rent <- runDB $ get404 rid
  (fwidget, enctype) <- generateFormPost rentalDeleteForm
  defaultLayout $ do
    setTitle "Deleting a rental."
    $(widgetFile "rental-delete")

--

postRentAuthorizeR, getRentAuthorizeR :: RentId -> Handler RepHtml
postRentAuthorizeR rid = do
  rent <- runDB $ get404 rid
  ((result,fwidget), enctype) <- runFormPost rentalAuthorizeForm
  today <- utctDay `fmap` liftIO getCurrentTime
  uid <- fmap entityKey requireAuth
  case result of
    FormSuccess _yesPerformAuthorize -> do
                         runDB $ replace rid (rent { rentStart = Just today, rentAuthorizedBy = Just uid })
                         defaultLayout [whamlet|
                                        <p> <strong>Rental of an <a href=@{AssetViewR (rentWhat rent)}>item</a> authorized.</strong> |]
    _ -> defaultLayout $ do
            setTitle "Deleting a rental."
            $(widgetFile "rental-authorize")

getRentAuthorizeR rid = do
  rent <- runDB $ get404 rid
  (fwidget, enctype) <- generateFormPost rentalAuthorizeForm
  defaultLayout $ do
    setTitle "Deleting a rental."
    $(widgetFile "rental-authorize")


--
requireRentUnfinished rent = do
  when (rentFinish rent /= Nothing) (invalidArgs ["The item has already been returned."])

postRentReturnR :: RentId -> Handler RepHtml
postRentReturnR rid = do
  rent <- runDB $ get404 rid
  requireRentUnfinished rent
  
  today <- utctDay `fmap` liftIO getCurrentTime
  ((result,fwidget), enctype) <- runFormPost rentalReturnForm
  case result of
    FormSuccess _yesPerformDelete -> do
                         runDB $ replace rid (rent { rentFinish = Just today })
                         defaultLayout [whamlet|
                                        <p> <strong><a href=@{AssetViewR (rentWhat rent)}>Item</a> returned on #{show today}.</strong> |]
    _ -> defaultLayout $ do
            setTitle "Returning an item."
            $(widgetFile "rental-return")

getRentReturnR :: RentId -> Handler RepHtml
getRentReturnR rid = do
  rent <- runDB $ get404 rid
  requireRentUnfinished rent
  (fwidget, enctype) <- generateFormPost rentalReturnForm
  defaultLayout $ do
    setTitle "Returning an item."
    $(widgetFile "rental-return")
--

-- rental forms


rentalReturnForm = renderTable (const <$> areq areYouSureField "Do you want to mark that item as returned?" (Just False))
  where areYouSureField = check isSure boolField
        isSure False = Left ("You must be sure to proceed." :: Text)
        isSure True = Right True

rentalDeleteForm = renderTable (const <$> areq areYouSureField "Are you sure?" (Just False))
  where areYouSureField = check isSure boolField
        isSure False = Left ("You must be sure to delete a rental." :: Text)
        isSure True = Right True

rentalAuthorizeForm = renderTable (const <$> areq areYouSureField "Do you want to authorize this rental? It will be marked as rented." (Just False))
  where areYouSureField = check isSure boolField
        isSure False = Left ("You must be sure to proceed." :: Text)
        isSure True = Right True

---------

checkAssetFree :: AssetId -> Handler Bool
checkAssetFree aid = do
  rents <- runDB $ selectList [RentWhat ==. aid, RentFinish ==. Nothing] []
  return (null rents)

requireAssetFree :: AssetId -> Handler ()
requireAssetFree aid = do
  f <- checkAssetFree aid
  when (not f) (invalidArgs ["This asset is currently unavailable."])

-- getRentNewGuestR :: AssetId -> Handler RepHtml
-- getRentNewGuestR aid = do
--   asset :: Asset <- runDB $ get404 aid
--   user <- requireAuth
--  
--   undefined

postRentNewGuestR :: AssetId -> Handler RepHtml
postRentNewGuestR aid = do
  _asset :: Asset <- runDB $ get404 aid
  user <- requireAuth
  guest <- checkGuestUser
  when (not guest) (invalidArgs ["You must be a guest user to issue this form."])
  requireAssetFree aid
  -- proceed
  now <- liftIO getCurrentTime
  ((result, _), _) <- runFormPost (renderDivs $ rentForm'guestUser now aid (entityKey user))
  case result of
    FormSuccess new -> runDB $ (insert new >> return ())
    _ -> return ()
  redirect (AssetViewR aid)

postRentNewOtherR :: AssetId -> Handler RepHtml
postRentNewOtherR aid = do
  users :: [Entity User] <- runDB $ selectList [] []
  _asset :: Asset <- runDB $ get404 aid
  user <- requireAuth
  let uid = entityKey user
  guest <- checkGuestUser
  when guest (invalidArgs ["Guest users cannot issue this form."])
  requireAssetFree aid
  -- proceed
  now <- liftIO getCurrentTime
  ((result, _), _) <- runFormPost (renderDivs $ rentForm'forOtherUser now aid uid users)
  case result of
    FormSuccess new -> runDB $ (insert new >> return ())
    _ -> return ()
  redirect (AssetViewR aid)

---

getRentNewR :: AssetId -> Handler RepHtml
getRentNewR aid = do
  _asset :: Asset <- runDB $ get404 aid
  asset'free <- checkAssetFree aid
  wg <- assetRentFormWidgetR aid
  defaultLayout $ 
       if not asset'free 
        then [whamlet| This asset is currently rented. |]
        else [whamlet| Issue new rent here. <br> ^{wg} |]

postRentNewR :: AssetId -> Handler RepHtml
postRentNewR aid = do
  _asset :: Asset <- runDB $ get404 aid
  requireAssetFree aid
  wg <- assetRentFormWidgetR aid
  uid <- fmap entityKey requireAuth
  now <- liftIO getCurrentTime
  ((result, _), _) <- runFormPost (renderDivs $ rentForm'sameUser now aid uid)
  case result of
      FormSuccess rent -> do
                  _rid <- runDB $ insert rent
                  redirect (AssetViewR aid)
      _ -> defaultLayout $ [whamlet| Issue new rent here. <br> ^{wg} |]
