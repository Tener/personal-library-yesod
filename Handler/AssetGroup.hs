{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.AssetGroup where

import Import
import Handler.Utils

-- asset groups

assetGroupAForm :: (Maybe AssetGroup) -> AForm App App AssetGroup
assetGroupAForm proto = AssetGroup
   <$> areq textField "Name" (fmap assetGroupName proto)
   <*> areq textareaField "Description" (fmap assetGroupDescription proto)

assetGroupForm :: Maybe AssetGroup -> Html -> MForm App App (FormResult AssetGroup, Widget)
assetGroupForm proto = renderTable $ assetGroupAForm proto

areYouSureDeleteForm = renderTable (const <$> areq areYouSureField "Are you sure?" (Just False))
  where areYouSureField = check isSure boolField
        isSure False = Left ("You must be sure to delete an item." :: Text)
        isSure True = Right True

getAssetGroupDeleteR :: AssetGroupId -> Handler RepHtml
getAssetGroupDeleteR gid = do
  asset :: AssetGroup <- runDB $ get404 gid
  (fwidget, enctype) <- generateFormPost areYouSureDeleteForm
  defaultLayout $ do
      setTitle "Deleting an asset."
      $(widgetFile "asset-group-delete")
    
postAssetGroupDeleteR :: AssetGroupId -> Handler RepHtml
postAssetGroupDeleteR gid = do
  asset :: AssetGroup <- runDB $ get404 gid
  elems <- runDB $ selectList [AssetGroupElementGroup ==. gid] []
  ((result,fwidget), enctype) <- runFormPost areYouSureDeleteForm
  case result of
    FormSuccess _yesPerformDelete -> do
                         runDB $ do
                              mapM_ (delete . entityKey) elems
                              delete gid
                         defaultLayout [whamlet|
                                        <p> <strong>Asset group deleted.</strong> |]
    _ -> defaultLayout $ do
              setTitle "Deleting an asset group."
              $(widgetFile "asset-group-delete")

getAssetGroupNewR :: Handler RepHtml
getAssetGroupNewR = do
  (fwidget, enctype) <- generateFormPost (assetGroupForm Nothing)
  defaultLayout $ do
      setTitle "A new asset group."
      $(widgetFile "asset-group-new")

postAssetGroupNewR :: Handler RepHtml
postAssetGroupNewR = do
  ((result, fwidget), enctype) <- runFormPost (assetGroupForm Nothing)
  case result of
    FormSuccess assetgroup -> do
                  aid <- runDB $ insert assetgroup
                  redirect (AssetGroupViewR aid)
    _ -> defaultLayout $ do
           setTitle "A new asset group."
           $(widgetFile "asset-group-new")

getAssetGroupEditR :: AssetGroupId -> Handler RepHtml
getAssetGroupEditR gid = do
  grp :: AssetGroup <- runDB $ get404 gid
  (fwidget, enctype) <- generateFormPost (assetGroupForm (Just grp))
  defaultLayout $ do
      setTitle "Edit asset group."
      $(widgetFile "asset-group-edit")

postAssetGroupEditR :: AssetGroupId -> Handler RepHtml
postAssetGroupEditR gid = do
  grp :: AssetGroup <- runDB $ get404 gid
  ((result, fwidget), enctype) <- runFormPost (assetGroupForm (Just grp))
  case result of
    FormSuccess new -> do
                  runDB $ replace gid new
                  redirect (AssetGroupViewR gid)
    _ -> defaultLayout $ do
            setTitle "Edit asset group."
            $(widgetFile "asset-group-edit")


getAssetGroupViewAllR :: Handler RepHtml
getAssetGroupViewAllR = do
  grps :: [Entity AssetGroup] <- runDB $ selectList [] [Asc AssetGroupId]
  (fwidget, enctype) <- generateFormPost (assetGroupForm Nothing)
  thisUserRole <- getThisUserRole
  let auth = thisUserRole [AdminRole, ResidentRole]
  defaultLayout $ do
      setTitle $ "All asset groups."
      $(widgetFile "asset-group-all-view")

getAssetGroupViewR :: AssetGroupId -> Handler RepHtml
getAssetGroupViewR gid = do
  grp :: AssetGroup <- runDB $ get404 gid
  elems <- runDB $ selectList [AssetGroupElementGroup ==. gid] []
  assetsAll :: [Entity Asset] <- runDB $ selectList [] [Asc AssetId]

  thisUserRole <- getThisUserRole
  let canEdit = thisUserRole [AdminRole, ResidentRole, FriendRole]
      canDelete = thisUserRole [AdminRole, ResidentRole]

      usedAssetKeys = map (assetGroupElementAsset . entityVal) elems

      assetsUsed = filter (\ ent -> (entityKey ent) `elem` usedAssetKeys ) assetsAll
      assetsUnused = filter (\ ent -> not ((entityKey ent) `elem` usedAssetKeys) ) assetsAll
      
  defaultLayout $ do
      setTitle $ toHtml $ "Viewing asset group " ++ show (assetGroupName grp)
      $(widgetFile "asset-group-view")
