{-# LANGUAGE ScopedTypeVariables #-}

{-

config/models part:

File
    asset AssetId
    sender UserId
    path String
    comment Textarea
    originalName Text
    originalContentType Text
    size Int64
    deriving Show

-}

module Handler.File where

import Import
import Handler.Utils

import System.Random
import System.FilePath
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding
import Data.Text.Encoding.Error
import qualified Data.Text
import qualified Network.Wai.Parse as WAI
import qualified Data.List
import System.Directory
import qualified System.PosixCompat -- file sizes
import Data.Int (Int64)

-- upload

uploadDirectory :: FilePath -- FIXME: make this configurable
uploadDirectory = "/var/tmp/personallibrary/incoming"

randomFileName :: IO FilePath
randomFileName = do
  createDirectoryIfMissing True uploadDirectory -- hack?

  fname'base <- replicateM 20 (randomRIO ('a','z'))
  let fname = uploadDirectory </> fname'base <.> "bin"
  return fname

fileUploadForm :: Form (FileInfo, Textarea)
fileUploadForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textareaField "What's on the file?" Nothing

getFileNewR :: AssetId -> Handler RepHtml
getFileNewR aid = do
  _asset <- runDB $ get404 aid
  (formWidget, formEnctype) <- generateFormPost fileUploadForm
  defaultLayout $ do
       setTitle "Upload new file."
       $(widgetFile "file-new")
  
getFileSize :: FilePath -> IO Int64
getFileSize fp = do
  stat <- System.PosixCompat.getFileStatus fp
  let System.PosixCompat.COff size = System.PosixCompat.fileSize stat
  return size

postFileNewR' :: AssetId -> Handler RepHtml
postFileNewR' aid = do
  _asset <- runDB $ get404 aid
  user <- requireAuth
  ((result, formWidget), formEnctype) <- runFormPost fileUploadForm
  case result of
    FormSuccess (fi,info) -> do
                 fn <- liftIO randomFileName
                 liftIO (LBS.writeFile fn (fileContent fi))
                 fsize <- liftIO $ getFileSize fn
                 let newFile = File aid (entityKey user) fn info (fileName fi) (fileContentType fi) fsize
                 fid <- runDB $ insert newFile
                 redirect (FileViewR fid)
    _ -> return ()

  defaultLayout $ do
       setTitle "Upload new file."
       $(widgetFile "file-new")

postFileNewR :: AssetId -> Handler RepHtml
postFileNewR aid = do
  _asset <- runDB $ get404 aid
  user <- requireAuth
  wreq <- waiRequest
  yreq <- getRequest
  (params, files) <- lift $ WAI.parseRequestBody WAI.tempFileBackEnd wreq

  let lookupWaiPar p = Data.List.lookup (Data.Text.Encoding.encodeUtf8 p) params
      lookupWaiFile f = Data.List.lookup (Data.Text.Encoding.encodeUtf8 f) files

      token = lookupWaiPar "_token"
      comment = lookupWaiPar "f3" -- FIXME -- hard coded string
      file = lookupWaiFile "f2" -- FIXME -- hard coded string

      tokenOK = (fmap Data.Text.Encoding.encodeUtf8 . reqToken $ yreq) == token

  -- when (not tokenOK) (redirect $ FileNewR aid) -- FIXME -- is this correct way to handle it?

  case (comment,file, tokenOK) of
    (Just comm, Just waiFileInfo, True) -> do
      fn <- liftIO randomFileName -- generate new name
      liftIO $ renameFile (WAI.fileContent waiFileInfo) fn -- rename received file
      fsize <- liftIO $ getFileSize fn -- get file size
      let bs2t = Data.Text.Encoding.decodeUtf8With lenientDecode
          comment'ta = Textarea (bs2t comm)
          uid = entityKey user
          declared'fname = bs2t (WAI.fileName waiFileInfo)
          declared'ctype = bs2t (WAI.fileContentType waiFileInfo)
          
          new = File aid uid fn comment'ta declared'fname declared'ctype fsize
      fid <- runDB $ insert new
      redirect (FileViewR fid)

    _ -> do
      (formWidget, formEnctype) <- generateFormPost fileUploadForm
      defaultLayout $ do
             setTitle "Upload new file."
             $(widgetFile "file-new")

-- view attributes

getFileViewR :: FileId -> Handler RepHtml
getFileViewR fid = do
  linkUser <- mkDisplayUserWidget
  file <- runDB $ get404 fid
  defaultLayout $ do
       setTitle "File description."
       $(widgetFile "file-view")
  
-- download

getFileGetR :: FileId -> Handler RepHtml
getFileGetR fid = do
  file <- runDB $ get404 fid
  redirect (FileGetNameR fid (fileOriginalName file))

getFileGetNameR :: FileId -> Text -> Handler RepHtml
getFileGetNameR fid _name = do
  file <- runDB $ get404 fid
  setHeader "Content-Disposition" (Data.Text.concat ["attachment; filename=\"",(fileOriginalName file), "\";"])
  setHeader "X-Content-Type-Options" "nosniff"
  let encodeContentType ct = Data.Text.Encoding.encodeUtf8 ct
      fct = encodeContentType $ fileOriginalContentType file
  sendFile (fct) (filePath file)

-- delete

-- getFileDeleteR :: FileId -> Handler RepHtml
-- getFileDeleteR fid = do
--   defaultLayout $ do
--        setTitle "Delete file."
--        $(widgetFile "file-delete")
--  
-- postFileDeleteR :: FileId -> Handler RepHtml
-- postFileDeleteR fid = do
--   defaultLayout $ do
--        setTitle "Delete file."
--        $(widgetFile "file-delete")

fileDeleteForm = renderTable (const <$> areq areYouSureField "Are you sure?" (Just False))
  where areYouSureField = check isSure boolField
        isSure False = Left ("You must be sure to delete a file" :: Text)
        isSure True = Right True

getFileDeleteR :: FileId ->  Handler RepHtml
getFileDeleteR fid = do
  file :: File <- runDB $ get404 fid
  (fwidget, enctype) <- generateFormPost fileDeleteForm
  defaultLayout $ do
      setTitle "Deleting a file."
      $(widgetFile "file-delete")

-- | delete file from db and disk.
deleteFile :: Entity File -> Handler ()
deleteFile (Entity fid file) = do
  -- delete from db
  runDB $ delete fid
  -- delete from disk
  liftIO (removeFile (filePath file))


postFileDeleteR :: FileId ->  Handler RepHtml
postFileDeleteR fid = do
  file :: File <- runDB $ get404 fid
  ((result,fwidget), enctype) <- runFormPost fileDeleteForm

  case result of
    FormSuccess _ -> do
        deleteFile (Entity fid file)
        defaultLayout [whamlet|
                       <p> <strong>File deleted.</strong> |]
    _ -> defaultLayout $ do
              setTitle "Deleting a file."
              $(widgetFile "file-delete")
