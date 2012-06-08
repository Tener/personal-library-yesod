{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Asset where

import Import

import qualified Data.Text
import Database.Persist.GenericSql

import Handler.Utils
import Handler.Rent (assetRentFormWidgetM)
import Handler.Review (reviewForm)
import Handler.File (deleteFile)

-- assets

assetAForm :: (Maybe Asset) -> [(Text,AssetId)] -> AForm App App Asset
assetAForm proto otherAssets = Asset
   <$> areq textField "Name" (fmap assetName proto)
   <*> areq textareaField "Description" (fmap assetDescription proto)
   <*> areq (selectFieldList kinds) "Kind" (fmap assetKind proto)
   <*> aopt (selectFieldList otherAssets) "Next asset" (fmap assetNext proto)
       where
         kinds :: [(Text,AssetKind)]
         kinds = [("music",AssetMusic),("book",AssetBook),("movie",AssetMovie)]

assetForm :: Maybe Asset -> [(Text,AssetId)] -> Html -> MForm App App (FormResult Asset, Widget)
assetForm proto other = renderTable $ assetAForm proto other

-----

assetRentedWidget :: AssetId -> Handler Widget
assetRentedWidget aid = do
--  rentals <- runDB $ selectList [RentWhat ==. aid] [Asc RentId]

  role <- getThisUserRole

  let getThisUserId = do
        mu <- maybeAuth
        case mu of
          Nothing -> return (const False)
          Just user -> return (\uid -> uid == entityKey user)
  checkSelf <- getThisUserId
  

  let isAdmin = role [AdminRole]
      canAuthorize = role [AdminRole, ResidentRole]
      canUserSee uid = checkSelf uid || role [AdminRole, ResidentRole]

--  isAdmin

  let query = Data.Text.concat [
              "SELECT DISTINCT ??, ??,                                                   ", 
              "                CASE                                                      ", 
              "                 WHEN rent.authorized_by IS NULL THEN NULL                ", 
              "                 ELSE u2.ident END                                        ", 
              "  FROM rent,                                                              ", 
              "       public.user,                                                       ", 
              "       public.user as u2                                                  ", 
              "  WHERE rent.what = ?                                                     ", 
              "    AND rent.taken_by = public.user.id                                    ", 
              "    AND (rent.authorized_by = u2.id OR rent.authorized_by IS NULL)        ",
              "  ORDER BY rent.id                                                        "]

      query' = "SELECT DISTINCT ??,??,NULL FROM rent, public.user WHERE what = ?"

  results' <- runDB (do
             ret <- rawSql query [unKey aid]
             return (ret :: [(Entity Rent, Entity User, Single (Maybe Text))]))

  let results = map (\ (a,b,Single c) -> (a,b,c)) results'

  return $(widgetFile "asset-rented-widget")

----


getAssetViewR :: AssetId -> Handler RepHtml
getAssetViewR aid = do
  asset :: Asset <- runDB $ get404 aid
  displayUserWidget <- mkDisplayUserWidget

  grpElems :: [Entity AssetGroupElement] <- runDB $ selectList [AssetGroupElementAsset ==. aid] [Desc AssetGroupElementAsset]
  let agrpIds = map (assetGroupElementGroup . entityVal) grpElems
  assetGroupsAll :: [Entity AssetGroup] <- runDB $ selectList [] [Desc AssetGroupId]
  reviews :: [Entity Review] <- runDB $ selectList [ReviewWhat ==. aid] [Desc ReviewId]
  files :: [Entity File] <- runDB $ selectList [FileAsset ==. aid] [Desc FileId]

  let assetGroupsNot = filter (\ ent -> not ((entityKey ent) `elem` agrpIds)) assetGroupsAll
      assetGroupsIn = filter (\ ent -> ((entityKey ent) `elem` agrpIds)) assetGroupsAll

  rentedWidget <- assetRentedWidget aid
  rentWidget <- assetRentFormWidgetM aid

  let mkRevForm (Just user) = fmap Just (generateFormPost (renderTable (reviewForm (entityKey user) aid)))
      mkRevForm Nothing = return Nothing
  m'form'review <- mkRevForm =<< maybeAuth

  thisUserRole <- getThisUserRole
  let canModifyGroups = thisUserRole [AdminRole, ResidentRole, FriendRole]
      canPostReviews = thisUserRole [AdminRole, ResidentRole, FriendRole, GuestRole]
      canDelReviews = thisUserRole [AdminRole]
      canUploadFiles = thisUserRole [AdminRole, ResidentRole, FriendRole]
      canDeleteFiles = thisUserRole [AdminRole]
      canDeleteAsset = thisUserRole [AdminRole]


  defaultLayout $ do
           setTitle $ toHtml $ "Asset " ++ show (assetName asset)
           $(widgetFile "asset-view")

prepareAssetsToList assets = map (\a -> (formatAsset a, entityKey a)) assets

formatAsset a = Data.Text.concat [assetName (entityVal a), "(id=", (Data.Text.pack . showPeristentKey . unKey . entityKey $ a) , ")"]

getAssetAllGeneric title filter'lst = do
  assets :: [Entity Asset] <- runDB $ selectList filter'lst [Asc AssetId]
  (fwidget, enctype) <- generateFormPost (assetForm Nothing $ prepareAssetsToList assets)

  thisUserRole <- getThisUserRole
  let auth = thisUserRole [AdminRole, ResidentRole]

  defaultLayout $ do
      setTitle title
      $(widgetFile "asset-all-view")

getAssetAllMusicR :: Handler RepHtml
getAssetAllMusicR = getAssetAllGeneric "A list of all music assets." [AssetKind ==. AssetMusic]

getAssetAllMoviesR :: Handler RepHtml
getAssetAllMoviesR = getAssetAllGeneric "A list of all movie assets." [AssetKind ==. AssetMovie]

getAssetAllBooksR :: Handler RepHtml
getAssetAllBooksR = getAssetAllGeneric "A list of all book assets." [AssetKind ==. AssetBook]

getAssetAllR :: Handler RepHtml
getAssetAllR = getAssetAllGeneric "A list of all assets." []

getAssetNewR :: Handler RepHtml
getAssetNewR = do
  assets :: [Entity Asset] <- runDB $ selectList [] [Asc AssetId]
  (fwidget, enctype) <- generateFormPost (assetForm Nothing $ prepareAssetsToList assets)
  defaultLayout $ do
      setTitle "A new asset."
      $(widgetFile "asset-new")

postAssetNewR :: Handler RepHtml
postAssetNewR = do
  assets :: [Entity Asset] <- runDB $ selectList [] [Asc AssetId]
  ((result, fwidget), enctype) <- runFormPost (assetForm Nothing $ prepareAssetsToList assets)
  case result of
    FormSuccess asset -> do
                  aid <- runDB $ insert asset
                  redirect (AssetViewR aid)
    _ -> defaultLayout $ do
                  setTitle "A new asset."
                  $(widgetFile "asset-new")

getAssetEditR :: AssetId -> Handler RepHtml
getAssetEditR aid = do 
  asset <- runDB $ get404 aid
  assets :: [Entity Asset] <- runDB $ selectList [] [Asc AssetId]
  (fwidget, enctype) <- generateFormPost (assetForm (Just asset) $ prepareAssetsToList assets)

  defaultLayout $ do
      setTitle "Edit existing asset."
      $(widgetFile "asset-edit")

postAssetEditR ::AssetId ->  Handler RepHtml
postAssetEditR aid = do
  asset :: Asset <- runDB $ get404 aid
  assets :: [Entity Asset] <- runDB $ selectList [] [Asc AssetId]
  ((result, fwidget), enctype) <- runFormPost (assetForm (Just asset) $ prepareAssetsToList assets)
  case result of
    FormSuccess asset'new -> do
                  runDB $ replace aid asset'new
                  redirect (AssetViewR aid)
    _ -> defaultLayout $ do
       setTitle "Edit existing asset."
       $(widgetFile "asset-edit")

assetDeleteForm = renderTable (const <$> areq areYouSureField "Are you sure?" (Just False))
  where areYouSureField = check isSure boolField
        isSure False = Left ("You must be sure to delete an asset" :: Text)
        isSure True = Right True

getAssetDeleteR ::AssetId ->  Handler RepHtml
getAssetDeleteR aid = do
  asset :: Asset <- runDB $ get404 aid
  (fwidget, enctype) <- generateFormPost assetDeleteForm
  defaultLayout $ do
      setTitle "Deleting an asset."
      $(widgetFile "asset-delete")

postAssetDeleteR ::AssetId ->  Handler RepHtml
postAssetDeleteR aid = do
  asset :: Asset <- runDB $ get404 aid
  ((result,fwidget), enctype) <- runFormPost assetDeleteForm

  case result of
    FormSuccess _ -> do
                files <- runDB $ selectList [FileAsset ==. aid] []
                mapM_ deleteFile files
                runDB $ do
                      deleteWhere [AssetGroupElementAsset ==. aid]
                      deleteWhere [RentWhat ==. aid]
                      deleteWhere [ReviewWhat ==. aid]
                      delete aid
                defaultLayout [whamlet|
                               <p> <strong>Asset deleted.</strong> |]
    _ -> defaultLayout $ do
              setTitle "Deleting an asset."
              $(widgetFile "asset-delete")
