{-# LANGUAGE FlexibleInstances #-}

module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Store
import Database.Persist.Quasi
import Data.Time
import Data.Int (Int64)

data Role = AdminRole | ResidentRole | FriendRole | GuestRole
    deriving (Show, Read, Eq, Enum, Bounded)
derivePersistField "Role"

data AssetKind = AssetBook | AssetMovie | AssetMusic
    deriving (Show, Read, Eq, Enum)
derivePersistField "AssetKind"

showAssetKind :: AssetKind -> Html
showAssetKind AssetBook = "book" 
showAssetKind AssetMusic = "music" 
showAssetKind AssetMovie = "movie" 

showRole :: Role -> String
showRole AdminRole = "admin"
showRole ResidentRole = "resident"
showRole FriendRole = "friend"
showRole GuestRole = "guest"

showKey :: Key backend entity -> String
showKey key = case unKey key of
                (PersistInt64 ident) -> show ident
                _ -> error "Not a key - not a PersistInt64 value"

-- to have delete cascade add 'mkDeleteCascade' below
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
