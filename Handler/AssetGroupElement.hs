{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.AssetGroupElement where

import Import
-- import Handler.Utils


-- asset group elements

postAssetGroupElementNewR :: AssetGroupId -> AssetId -> Handler RepHtml
postAssetGroupElementNewR gid aid = do
  _grp :: AssetGroup <- runDB $ get404 gid
  _ass :: Asset <- runDB $ get404 aid
  _el <- runDB $ insertUnique (AssetGroupElement aid gid)
  defaultLayout $ [whamlet| \<a href=@{AssetViewR aid}>Asset</a> added to <a href=@{AssetGroupViewR gid}>asset group</a>. |]

postAssetGroupElementDeleteUniqueR :: AssetGroupId -> AssetId -> Handler RepHtml
postAssetGroupElementDeleteUniqueR gid aid = do
  _grp :: AssetGroup <- runDB $ get404 gid
  _ass :: Asset <- runDB $ get404 aid
  eid :: Entity AssetGroupElement <- runDB $ getBy404 (UniqueAssetGroupElement aid gid)
  runDB $ delete (entityKey eid)
  defaultLayout $ [whamlet| \<a href=@{AssetViewR aid}>Asset</a> removed from <a href=@{AssetGroupViewR gid}>asset group</a>. |]

postAssetGroupElementDeleteR :: AssetGroupElementId -> Handler RepHtml
postAssetGroupElementDeleteR eid = do
  _el :: AssetGroupElement <- runDB $ get404 eid
  runDB $ delete eid
  defaultLayout $ [whamlet| Asset removed from asset group. |]

getAssetGroupElementViewR :: AssetGroupElementId -> Handler RepHtml
getAssetGroupElementViewR eid = do
  AssetGroupElement aid gid <- runDB $ get404 eid
  grp :: AssetGroup <- runDB $ get404 gid
  ass :: Asset <- runDB $ get404 aid
  defaultLayout $ [whamlet| 
<p>  
     Asset: <a href=@{AssetViewR aid}>#{show ass}</a>
<p>
     Group: <a href=@{AssetGroupViewR gid}>#{show grp}</a>
|]
