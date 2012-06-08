{-# LANGUAGE TupleSections, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Handler.Utils where

import Import
import qualified Data.List
import qualified Data.Text
import Database.Persist.Store
import Data.Prefix.Units
import Data.Int (Int64)
import Text.Printf

import Control.Monad (when)

-- rendering file sizes

renderFileSize' :: Int64 -> Text
renderFileSize' size = Data.Text.pack (showValueWith unitSymbol (Left FormatSiKMGT) (fromInteger $ toInteger size :: Double))

renderFileSize :: Int64 -> Text
renderFileSize size = (flip Data.Text.snoc 'B') $ 
    Data.Text.pack $ case formatValue (Left FormatSiKMGT) (fromInteger $ toInteger size :: Double) of
                       (_, Nothing) -> show size -- wihout symbol we simply show base size -- integral number of bytes
                       (a, Just un) -> printf "%.2f%s" a (unitSymbol un) -- otherwise we show approximate number of bytes


-- rendering data

renderUserW :: Entity User -> Widget
renderUserW ent =
    let uid = entityKey ent
        usr = entityVal ent
    in [whamlet| <a href=@{UserViewR uid}> #{userIdent usr} |]


mkDisplayUserWidget :: Handler (UserId -> Widget)
mkDisplayUserWidget = do
  users :: [Entity User] <- runDB $ selectList [] [Asc UserId]

  role <- getThisUserRole
  let getThisUserId = do
        mu <- maybeAuth
        case mu of
          Nothing -> return (const False)
          Just user -> return (\uid -> uid == entityKey user)
  checkSelf <- getThisUserId
  let canUserSee uid = checkSelf uid || role [AdminRole, ResidentRole]

      -- we assume that user id passed is already checked for validity.
      getUser :: UserId -> User
      getUser uid = case Data.List.lookup uid (map entToPair users) of
                      Nothing -> error "mkDisplayUserWidget: Invalid user id passed"
                      Just user -> user

  return (\ uid -> [whamlet|
$with user <- getUser uid
 $if canUserSee uid
    <a href=@{UserViewR uid}>#{userIdent user}
 $else
    #{userIdent user} |])

--- formatting

entToPair ent = ((entityKey ent),(entityVal ent))
formatKey k = Data.Text.concat ["id=", (Data.Text.pack . showPeristentKey . unKey $ k)]
showOrNone (Just x) = x
showOrNone Nothing = "not set"
showPeristentKey (PersistInt64 k) = show k
showPeristentKey _ = error "bad value for showPeristentKey"

--- helpers

nothing Nothing = True
nothing _ = False

--- authentication 

checkUserLogged :: Handler Bool
checkUserLogged = fmap (not . nothing) maybeAuth

getThisUserRole :: Handler ([Role] -> Bool)
getThisUserRole = do
  mu <- maybeAuth
  case mu of
    Nothing -> return (const False)
    Just user -> return ((userRole $ entityVal $ user) `elem`)

checkAdminUser :: Handler Bool
checkAdminUser = do
  maid <- maybeAuth
  return $ case maid of
             Nothing -> False
             Just ent -> AdminRole == (userRole . entityVal $ ent)

checkGuestUser :: Handler Bool
checkGuestUser = do
  maid <- maybeAuth
  return $ case maid of
             Nothing -> False
             Just ent -> GuestRole == (userRole . entityVal $ ent)

checkYourself :: UserId -> Handler Bool
checkYourself uid = do
  maid <- maybeAuth
  return $ case maid of
             Nothing -> False
             Just ent -> uid == (entityKey $ ent)

----- hacks

giveAdminToMyself :: Handler ()
giveAdminToMyself = do
  user <- maybeAuth
  case user of
    Nothing -> return ()
    Just (Entity ukey udata) -> 
        when (userIdent udata == "gtener@gmail.com") $ do
                      runDB $ replace ukey (udata { userRole = AdminRole })
                      $(logDebug) "User fixed."
                      return ()
                                  
