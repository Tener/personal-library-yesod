{-# LANGUAGE CPP #-}
------------------------------------------------------------------------
-- |
-- Module      : Codec.Archive.ZipHack
-- Copyright   : John MacFarlane, Krzysztof Skrzętnicki
-- License     : GPL 2 (see LICENSE)
--
-- Maintainer  : John MacFarlane < jgm at berkeley dot edu >
-- Stability   : unstable
-- Portability : so far only tested on GHC
--
-- The zip-archive library provides functions for creating, modifying,
-- and extracting files from zip archives.
--
-- Certain simplifying assumptions are made about the zip archives: in
-- particular, there is no support for encryption, zip files that span
-- multiple disks, ZIP64, OS-specific file attributes, or compression
-- methods other than Deflate.  However, the library should be able to
-- read the most common zip archives, and the archives it produces should
-- be readable by all standard unzip programs.
--
-- As an example of the use of the library, a standalone zip archiver
-- and extracter, Zip.hs, is provided in the source distribution.
--
-- For more information on the format of zip archives, consult
-- <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>
------------------------------------------------------------------------

-- this is hacked version of module Codec.Archive.Zip.
-- Entry's eCompressedData field is removed. instead the compressed files are passed as an extra parameter where needed.
-- only fromArchive is available.

module Codec.Archive.ZipHack
--       (
-- 
--       -- * Data structures
--         Archive (..)
--       , Entry (..)
--       , CompressionMethod (..)
--       , ZipOption (..)
--       , emptyArchive
-- 
--       -- * Pure functions for working with zip archives
--       , toArchive
--       , fromArchive
--       , filesInArchive
--       , addEntryToArchive
--       , deleteEntryFromArchive
--       , findEntryByPath
--       , fromEntry
--       , toEntry
-- 
--       -- * IO functions for working with zip archives
--       , readEntry
--       , writeEntry
--       , addFilesToArchive
--       , extractFilesFromArchive
-- 
--       ) where

where

import System.Time ( toUTCTime, addToClockTime, CalendarTime (..), ClockTime (..), TimeDiff (..) )
import Data.Bits ( shiftL, shiftR, (.&.) )
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.List ( nub, find )
import Text.Printf
import System.FilePath
import System.Directory ( doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing )
import Control.Monad ( when, unless, zipWithM, liftM )
import System.Directory ( getModificationTime )
import System.IO ( stderr, hPutStrLn )
import qualified Data.Digest.CRC32 as CRC32
import qualified Data.Map as M

import Prelude

#ifndef _WINDOWS
import System.Posix.Files ( setFileTimes )
#endif

-- from bytestring
import qualified Data.ByteString.Lazy as B

-- from utf8-string
import Data.ByteString.Lazy.UTF8 ( toString, fromString )

-- from zlib
import qualified Codec.Compression.Zlib.Raw as Zlib

------------------------------------------------------------------------

-- | Structured representation of a zip archive, including directory
-- information and contents (in lazy bytestrings).
data Archive = Archive
                { zEntries                :: [Entry]              -- ^ Files in zip archive
                , zSignature              :: Maybe B.ByteString   -- ^ Digital signature
                , zComment                :: B.ByteString         -- ^ Comment for whole zip archive
                } deriving (Read, Show)

-- | Representation of an archived file, including content and metadata.
data Entry = Entry
               { eRelativePath            :: FilePath            -- ^ Relative path, using '/' as separator
               , eCompressionMethod       :: CompressionMethod   -- ^ Compression method
               , eLastModified            :: Integer             -- ^ Modification time (seconds since unix epoch)
               , eCRC32                   :: Word32              -- ^ CRC32 checksum
               , eCompressedSize          :: Word32              -- ^ Compressed size in bytes
               , eUncompressedSize        :: Word32              -- ^ Uncompressed size in bytes
               , eExtraField              :: B.ByteString        -- ^ Extra field - unused by this library
               , eFileComment             :: B.ByteString        -- ^ File comment - unused by this library
               , eInternalFileAttributes  :: Word16              -- ^ Internal file attributes - unused by this library
               , eExternalFileAttributes  :: Word32              -- ^ External file attributes (system-dependent)
--               , eCompressedData          :: B.ByteString        -- ^ Compressed contents of file
               } deriving (Read, Show, Eq)

type EntryContents = B.ByteString

-- | Compression methods.
data CompressionMethod = Deflate
                       | NoCompression
                       deriving (Read, Show, Eq)

-- | Options for 'addFilesToArchive' and 'extractFilesFromArchive'.
data ZipOption = OptRecursive               -- ^ Recurse into directories when adding files
               | OptVerbose                 -- ^ Print information to stderr
               deriving (Read, Show, Eq)

-- | A zip archive with no contents.
emptyArchive :: Archive
emptyArchive = Archive
                { zEntries                  = []
                , zSignature              = Nothing
                , zComment                = B.empty }

-- | Writes an 'Archive' structure to a raw zip archive (in a lazy bytestring).
fromArchive :: [EntryContents] -> Archive -> B.ByteString
fromArchive cont = runPut . putArchive cont

-- | Returns a list of files in a zip archive.
filesInArchive :: Archive -> [FilePath]
filesInArchive = (map eRelativePath) . zEntries


--------------------------------------------------------------------------------
-- Internal functions for reading and writing zip binary format.

-- | Perform a sequence of actions until one returns Nothing;
-- return list of results.
many :: Monad m => m (Maybe a) -> m [a]
many p = do
  r <- p
  case r of
       Just x  ->  many p >>= return . (x:)
       Nothing -> return []

-- Note that even on Windows, zip files use "/" internally as path separator.
zipifyFilePath :: FilePath -> String
zipifyFilePath path =
  let dir = takeDirectory path
      fn  = takeFileName path
      (_drive, dir') = splitDrive dir
      -- note: some versions of filepath return ["."] if no dir
      dirParts = dropWhile (==".") $ splitDirectories dir'
  in  (concat (map (++ "/") dirParts)) ++ fn


-- | MSDOS datetime: a pair of Word16s (date, time) with the following structure:
--
-- > DATE bit     0 - 4           5 - 8           9 - 15
-- >      value   day (1 - 31)    month (1 - 12)  years from 1980
-- > TIME bit     0 - 4           5 - 10          11 - 15
-- >      value   seconds*        minute          hour
-- >              *stored in two-second increments
--
data MSDOSDateTime = MSDOSDateTime { msDOSDate :: Word16
                                   , msDOSTime :: Word16
                                   } deriving (Read, Show, Eq)

-- | Epoch time corresponding to the minimum DOS DateTime (Jan 1 1980 00:00:00).
minMSDOSDateTime :: Integer
minMSDOSDateTime = 315532800

-- | Convert a clock time to a MSDOS datetime.  The MSDOS time will be relative to UTC.
epochTimeToMSDOSDateTime :: Integer -> MSDOSDateTime
epochTimeToMSDOSDateTime epochtime | epochtime < minMSDOSDateTime =
  epochTimeToMSDOSDateTime minMSDOSDateTime
  -- if time is earlier than minimum DOS datetime, return minimum
epochTimeToMSDOSDateTime epochtime =
  let ut = toUTCTime (TOD epochtime 0)
      dosTime = toEnum $ (ctSec ut `div` 2) + shiftL (ctMin ut) 5 + shiftL (ctHour ut) 11
      dosDate = toEnum $ ctDay ut + shiftL (fromEnum (ctMonth ut) + 1) 5 + shiftL (ctYear ut - 1980) 9
  in  MSDOSDateTime { msDOSDate = dosDate, msDOSTime = dosTime }

-- | Convert a MSDOS datetime to a 'ClockTime'.
msDOSDateTimeToEpochTime :: MSDOSDateTime -> Integer
msDOSDateTimeToEpochTime (MSDOSDateTime {msDOSDate = dosDate, msDOSTime = dosTime}) =
  let seconds = fromIntegral $ 2 * (dosTime .&. 0O37)
      minutes = fromIntegral $ (shiftR dosTime 5) .&. 0O77
      hour    = fromIntegral $ shiftR dosTime 11
      day     = fromIntegral $ dosDate .&. 0O37
      month   = fromIntegral $ ((shiftR dosDate 5) .&. 0O17) - 1
      year    = fromIntegral $ shiftR dosDate 9
      timeSinceEpoch = TimeDiff
               { tdYear = year + 10, -- dos times since 1980, unix epoch starts 1970
                 tdMonth = month,
                 tdDay = day - 1,  -- dos days start from 1
                 tdHour = hour,
                 tdMin = minutes,
                 tdSec = seconds,
                 tdPicosec = 0 }
      (TOD epochsecs _) = addToClockTime timeSinceEpoch (TOD 0 0)
  in  epochsecs


-- A zip file has the following format (*'d items are not supported in this implementation):
--
-- >   [local file header 1]
-- >   [file data 1]
-- >   [data descriptor 1*]
-- >   .
-- >   .
-- >   .
-- >   [local file header n]
-- >   [file data n]
-- >   [data descriptor n*]
-- >   [archive decryption header*]
-- >   [archive extra data record*]
-- >   [central directory]
-- >   [zip64 end of central directory record*]
-- >   [zip64 end of central directory locator*]
-- >   [end of central directory record]
--
-- Files stored in arbitrary order.  All values are stored in
-- little-endian byte order unless otherwise specified.
--
--  Central directory structure:
--
-- >   [file header 1]
-- >   .
-- >   .
-- >   .
-- >   [file header n]
-- >   [digital signature]
--
--  End of central directory record:
--
-- >   end of central dir signature    4 bytes  (0x06054b50)
-- >   number of this disk             2 bytes
-- >   number of the disk with the
-- >   start of the central directory  2 bytes
-- >   total number of entries in the
-- >   central directory on this disk  2 bytes
-- >   total number of entries in
-- >   the central directory           2 bytes
-- >   size of the central directory   4 bytes
-- >   offset of start of central
-- >   directory with respect to
-- >   the starting disk number        4 bytes
-- >   .ZIP file comment length        2 bytes
-- >   .ZIP file comment       (variable size)

putArchive :: [EntryContents] -> Archive -> Put
putArchive contents archive = do
  let entriesCount = fromIntegral $ length $ zEntries archive
      headersSize = sum $ map fileHeaderSize $ zEntries archive
      comment = zComment archive
      sig = zSignature archive
      localFileSizes = map localFileSize $ zEntries archive
      offsets = scanl (+) 0 localFileSizes
      cdOffset = last offsets
      dictionary = runPut (zipWithM putFileHeader offsets (zEntries archive) >> return ())

  entriesCount `seq` headersSize `seq` comment `seq` sig `seq` localFileSizes `seq` offsets `seq` cdOffset `seq` dictionary `seq` return ()
  zipWithM putLocalFile contents (zEntries archive)
--  mapM_ putLocalFile $ zEntries archive
  putLazyByteString dictionary
  putDigitalSignature $ sig
  putWord32le 0x06054b50
  putWord16le 0 -- disk number
  putWord16le 0 -- disk number of central directory
  putWord16le $ entriesCount -- number of entries this disk
  putWord16le $ entriesCount -- number of entries
  putWord32le $ headersSize  -- size of central directory
  putWord32le $ fromIntegral cdOffset                    -- offset of central dir
  putWord16le $ fromIntegral $ B.length $ comment
  putLazyByteString $ comment


fileHeaderSize :: Entry -> Word32
fileHeaderSize f =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 +
    fromIntegral (B.length $ fromString $ zipifyFilePath $ eRelativePath f) +
    B.length (eExtraField f) + B.length (eFileComment f)

localFileSize :: Entry -> Word32
localFileSize f =
  fromIntegral $ 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 +
    fromIntegral (B.length $ fromString $ zipifyFilePath $ eRelativePath f) +
    B.length (eExtraField f) + (fromIntegral (eCompressedSize f)) -- B.length (eCompressedData f) -- seriously? asking for length of LAZY bytestring? why make it lazy then?

-- Local file header:
--
-- >    local file header signature     4 bytes  (0x04034b50)
-- >    version needed to extract       2 bytes
-- >    general purpose bit flag        2 bytes
-- >    compression method              2 bytes
-- >    last mod file time              2 bytes
-- >    last mod file date              2 bytes
-- >    crc-32                          4 bytes
-- >    compressed size                 4 bytes
-- >    uncompressed size               4 bytes
-- >    file name length                2 bytes
-- >    extra field length              2 bytes
--
-- >    file name (variable size)
-- >    extra field (variable size)
--
-- Note that if bit 3 of the general purpose bit flag is set, then the
-- compressed size will be 0 and the size will be stored instead in a
-- data descriptor record AFTER the file contents. The record normally
-- begins with the signature 0x08074b50, then 4 bytes crc-32, 4 bytes
-- compressed size, 4 bytes uncompressed size.

putLocalFile :: EntryContents -> Entry -> Put
putLocalFile cont f = do
  putWord32le 0x04034b50
  putWord16le 20 -- version needed to extract (>=2.0)
  putWord16le 2  -- general purpose bit flag (max compression)
  putWord16le $ case eCompressionMethod f of
                     NoCompression -> 0
                     Deflate       -> 8
  let modTime = epochTimeToMSDOSDateTime $ eLastModified f
  putWord16le $ msDOSTime modTime
  putWord16le $ msDOSDate modTime
  putWord32le $ eCRC32 f
  putWord32le $ eCompressedSize f
  putWord32le $ eUncompressedSize f
  putWord16le $ fromIntegral $ B.length $ fromString
              $ zipifyFilePath $ eRelativePath f
  putWord16le $ fromIntegral $ B.length $ eExtraField f
  putLazyByteString $ fromString $ zipifyFilePath $ eRelativePath f
  putLazyByteString $ eExtraField f
  putLazyByteString $ cont -- eCompressedData f

-- File header structure:
--
-- >    central file header signature   4 bytes  (0x02014b50)
-- >    version made by                 2 bytes
-- >    version needed to extract       2 bytes
-- >    general purpose bit flag        2 bytes
-- >    compression method              2 bytes
-- >    last mod file time              2 bytes
-- >    last mod file date              2 bytes
-- >    crc-32                          4 bytes
-- >    compressed size                 4 bytes
-- >    uncompressed size               4 bytes
-- >    file name length                2 bytes
-- >    extra field length              2 bytes
-- >    file comment length             2 bytes
-- >    disk number start               2 bytes
-- >    internal file attributes        2 bytes
-- >    external file attributes        4 bytes
-- >    relative offset of local header 4 bytes
--
-- >    file name (variable size)
-- >    extra field (variable size)
-- >    file comment (variable size)

putFileHeader :: Word32        -- ^ offset
              -> Entry
              -> Put
putFileHeader offset local = do
  putWord32le 0x02014b50
  putWord16le 0  -- version made by
  putWord16le 20 -- version needed to extract (>= 2.0)
  putWord16le 2  -- general purpose bit flag (max compression)
  putWord16le $ case eCompressionMethod local of
                     NoCompression -> 0
                     Deflate       -> 8
  let modTime = epochTimeToMSDOSDateTime $ eLastModified local
  putWord16le $ msDOSTime modTime
  putWord16le $ msDOSDate modTime
  putWord32le $ eCRC32 local
  putWord32le $ eCompressedSize local
  putWord32le $ eUncompressedSize local
  putWord16le $ fromIntegral $ B.length $ fromString
              $ zipifyFilePath $ eRelativePath local
  putWord16le $ fromIntegral $ B.length $ eExtraField local
  putWord16le $ fromIntegral $ B.length $ eFileComment local
  putWord16le 0  -- disk number start
  putWord16le $ eInternalFileAttributes local
  putWord32le $ eExternalFileAttributes local
  putWord32le offset
  putLazyByteString $ fromString $ zipifyFilePath $ eRelativePath local
  putLazyByteString $ eExtraField local
  putLazyByteString $ eFileComment local

--  Digital signature:
--
-- >     header signature                4 bytes  (0x05054b50)
-- >     size of data                    2 bytes
-- >     signature data (variable size)

putDigitalSignature :: Maybe B.ByteString -> Put
putDigitalSignature Nothing = return ()
putDigitalSignature (Just sig) = do
  putWord32le 0x08064b50
  putWord16le $ fromIntegral $ B.length sig
  putLazyByteString sig

