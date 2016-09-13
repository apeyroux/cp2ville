{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty as S
import Data.Aeson 
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Network.HTTP.Types.Status
import Data.Text.Lazy as TL
import GHC.Generics

data Ville = Ville { villeInse :: String
                   , villeName :: String
                   , villeLigne5 :: String
                   , villeCP :: String } deriving (Show, Eq, Read, Generic)

instance ToJSON Ville where
  toJSON (Ville inse name ligne5 cp) =
    object ["name"  .= name
           , "ligne5" .= ligne5
           , "inse" .= inse
           , "cp"   .= cp]

instance FromRow Ville where
  fromRow = Ville <$> field <*> field <*> field <*> field

instance ToRow Ville where
  toRow d = [ toField (villeInse d)
            , toField (villeName d)
            , toField (villeLigne5 d)
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
          S.text $ "usage:\n\n- ?cp=92320\n- ?ville=chatillon"
  where
    ville2cp c ville = query c "select inse,name,cp,ligne5 from villes where UPPER(name) LIKE (UPPER(?) || '%')" [(ville)] :: IO [Ville]
    cp2ville c cp = query c "select inse,name,cp,ligne5 from villes where cp = ?" [(cp)] :: IO [Ville]
    villes2json v = case v of
      [ville] -> S.json $ ville
      _ -> do
        status status404
        S.json $ v
