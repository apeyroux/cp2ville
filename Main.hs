{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty as S
import Data.Aeson 
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.Text.Lazy as TL
import GHC.Generics

data Ville = Ville { villeInse :: String
                   , villeName :: String
                   , villeCP :: String } deriving (Show, Eq, Read, Generic)

instance ToJSON Ville where
  toJSON (Ville inse name cp) =
    object ["name"  .= name
           , "inse" .= inse
           , "cp"   .= cp]

instance FromRow Ville where
  fromRow = Ville <$> field <*> field <*> field

instance ToRow Ville where
  toRow d = [ toField (villeInse d)
            , toField (villeName d)
            , toField (villeCP d)]

main :: IO ()
main = do
  c <- liftIO $ connect defaultConnectInfo { connectHost = "db", connectDatabase = "villes" }
  scotty 3000 $ do
    get "/" $ do
      p <- params
      case p of
        [("cp",cp)] -> do
          villes <- liftIO $ cp2ville c (TL.unpack cp)
          villes2json villes
        [("ville", ville)] -> do
          villes <- liftIO $ ville2cp c (TL.unpack ville)
          villes2json villes
        _ -> do
          S.text $ "?cp=92320\n?ville=chatillon"
  where
    ville2cp c ville = query c "select inse,name,cp from villes where UPPER(name) LIKE (UPPER(?) || '%')" [(ville)] :: IO [Ville]
    cp2ville c cp = query c "select inse,name,cp from villes where cp = ?" [(cp)] :: IO [Ville]
    villes2json v = case v of
      [ville] -> S.json $ ville
      _ -> S.json $ v
