{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Review where

import Import
import qualified Data.Text

-- import Handler.Utils

-- reviews

reviewForm :: UserId -> AssetId -> AForm App App Review
reviewForm uid aid = Review aid 
                          <$> areq textareaField "Review" Nothing
                          <*> areq (radioFieldList ratings) "Rating" (Just 3)
                          <*> pure uid
    where
      ratings = [ ((Data.Text.pack . show $ i),i) | i <- [1..5] :: [Int] ]

reviewDeleteForm = renderTable (const <$> areq areYouSureField "Are you sure?" (Just False))
  where areYouSureField = check isSure boolField
        isSure False = Left ("You must be sure to delete a review." :: Text)
        isSure True = Right True

postReviewNewR :: AssetId -> Handler RepHtml
postReviewNewR aid = do
  _asset :: Asset <- runDB $ get404 aid
  uid <- fmap entityKey requireAuth
  ((result, _), _) <- runFormPost (renderDivs $ reviewForm uid aid)
  case result of
      FormSuccess review -> do
                       _revId <- runDB $ insert review
                       return ()
      _ -> return ()
  redirect (AssetViewR aid)

postReviewDeleteR, getReviewDeleteR :: ReviewId -> Handler RepHtml
postReviewDeleteR rid = do
  review <- runDB $ get404 rid
  ((result,fwidget), enctype) <- runFormPost reviewDeleteForm
  case result of
    FormSuccess _yesPerformDelete -> do
                         runDB $ delete rid
                         defaultLayout [whamlet|
                                        <p> <strong>Review of an <a href=@{AssetViewR (reviewWhat review)}>item</a> deleted.</strong> |]
    _ -> defaultLayout $ do
            setTitle "Deleting a review."
            $(widgetFile "review/delete")

getReviewDeleteR rid = do
  review <- runDB $ get404 rid
  (fwidget, enctype) <- generateFormPost reviewDeleteForm
  defaultLayout $ do
    setTitle "Deleting a review."
    $(widgetFile "review/delete")
