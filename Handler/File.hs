{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Handler.File where

import Import
import Handler.Utils

import Codec.Archive.Streaming
import Control.Monad
import Data.Digest.CRC32 (crc32)
import Data.Int (Int64)
import Data.Text.Encoding.Error
import Data.Text.Format
import Data.Time
import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath
import System.Random
import qualified Blaze.ByteString.Builder.ByteString as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Network.Wai.Parse as WAI
import qualified System.Locale
import qualified System.PosixCompat -- file sizes

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
       $(widgetFile "file/new")
  
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
                 let crc = crc32 (fileContent fi)
                 fsize <- liftIO $ getFileSize fn
                 let newFile = File aid (entityKey user) fn info (fileName fi) (fileContentType fi) fsize crc
                 fid <- runDB $ insert newFile
                 redirect (FileViewR fid)
    _ -> return ()

  defaultLayout $ do
       setTitle "Upload new file."
       $(widgetFile "file/new")

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
      crc <- liftIO (fmap crc32 (LBS.readFile fn))
      let bs2t = Data.Text.Encoding.decodeUtf8With lenientDecode
          comment'ta = Textarea (bs2t comm)
          uid = entityKey user
          declared'fname = bs2t (WAI.fileName waiFileInfo)
          declared'ctype = bs2t (WAI.fileContentType waiFileInfo)
          
          new = File aid uid fn comment'ta declared'fname declared'ctype fsize crc
      fid <- runDB $ insert new
      redirect (FileViewR fid)

    _ -> do
      (formWidget, formEnctype) <- generateFormPost fileUploadForm
      defaultLayout $ do
             setTitle "Upload new file."
             $(widgetFile "file/new")

-- view attributes

getFileViewR :: FileId -> Handler RepHtml
getFileViewR fid = do
  linkUser <- mkDisplayUserWidget
  file <- runDB $ get404 fid
  defaultLayout $ do
       setTitle "File description."
       $(widgetFile "file/view")
  
-- download

getFileGetR :: FileId -> Handler RepHtml
getFileGetR fid = do
  file <- runDB $ get404 fid
  redirect (FileGetNameR fid (fileOriginalName file))

getFileGetNameR :: FileId -> Text -> Handler RepPlain -- FIXME: figure out right reply content type
getFileGetNameR fid _name = do
  file <- runDB $ get404 fid
  setHeader "Content-Disposition" (Data.Text.concat ["attachment; filename=\"",(fileOriginalName file), "\";"])
  setHeader "X-Content-Type-Options" "nosniff"
  let encodeContentType ct = Data.Text.Encoding.encodeUtf8 ct
      fct = encodeContentType $ fileOriginalContentType file
  sendFile (fct) (filePath file)

-- batch download

getFilesForAssetGetR :: AssetId -> Handler RepPlain
getFilesForAssetGetR key = do
  asset <- runDB $ get404 key
  now <- liftIO getCurrentTime
  now'posix <- liftIO getPOSIXTime

  files <- runDB $ selectList [FileAsset ==. key] [Asc FileId]

  let lt2lbs t = t2lbs . Data.Text.Lazy.toStrict $ t 
      t2lbs t = LBS.fromChunks . singletonList . Data.Text.Encoding.encodeUtf8 $ t
      singletonList x = [x]

      readFileFromDisk (Entity _ file) = LBS.readFile (filePath file)
      fileEntry (Entity fid file) = 
        let name = Data.Text.Format.format "{}/{}" ((Shown fid), (fileOriginalName file))
            -- fixme: error/warning for too big files 
            size = fromIntegral . fileSize $ file
        
            e = Entry { eRelativePath = read . show $ name
                       , eCompressionMethod = NoCompression
                       , eLastModified = round now'posix
                       , eCRC32 = fileCrc file
                       , eCompressedSize = size
                       , eUncompressedSize = size
                       , eExtraField = LBS.empty
                       , eFileComment = t2lbs $ unTextarea $ fileComment file
                       , eInternalFileAttributes = 0
                       , eExternalFileAttributes = 0
                       }
                        in e

  files'lazy <- liftIO $ mapM readFileFromDisk files
  let entries = map fileEntry files

  let comment = Data.Text.Format.format "File bundle for asset {} - {}" (Shown key, assetName asset)
      archive =
         Archive
         { zComment = lt2lbs comment
         , zSignature = Nothing
         , zEntries = entries
         }

--  let now'fmt = Data.Time.formatTime System.Locale.defaultTimeLocale "%s" now
--      fname = Data.Text.Format.format "asset_{}_file_bundle_{}.zip" (Shown key, now'fmt)
--      cont'disp = Data.Text.Encoding.encodeUtf8 $ Data.Text.Format.format "attachment; filename=\"{}\";" [fname]
  setHeader "Content-Disposition" "attachment; filename=archive-all.zip;" -- (Data.Text.Lazy.toStrict cont'disp)
  setHeader "X-Content-Type-Options" "nosniff"

--  let h1 = ("Content-Disposition","attachment; filename=archive-all.zip;") -- fixme: provide filename different for each asset
--      h2 = ("X-Content-Type-Options","nosniff")
--      -- h3 = ("Content-Length","") -- fixme: calculate exact actual content length.
--  
--  sendWaiResponse (responseLBS ok200 [h1,h2] (fromArchive files'lazy archive))

  let content = ContentBuilder (Builder.fromLazyByteString (fromArchive files'lazy archive)) Nothing -- fixme: calculate response length
  return (RepPlain content)
  

-- delete

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
      $(widgetFile "file/delete")

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
              $(widgetFile "file/delete")
