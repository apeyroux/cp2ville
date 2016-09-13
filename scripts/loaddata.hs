#!/usr/bin/env stack
-- stack --resolver lts-6.15 --install-ghc runghc

{-# LANGUAGE OverloadedStrings #-}

import Text.SSV
import Database.PostgreSQL.Simple
import Data.List.Extra

data Ville = Ville { villeInse :: String
                   , villeName :: String
                   , villeLigne5 :: String
                   , villeCP :: String } deriving (Show, Eq, Read)

entry2ville :: [String] -> Maybe Ville
entry2ville e = case e of
  [] -> Nothing
  [inse, nom, cp, _, ligne5] -> Just (Ville (trim inse) (trim nom) (trim cp) (trim ligne5))
  _ -> Nothing


insert2db :: Connection -> Maybe Ville -> IO ()
insert2db c v = do
  case v of
    Just ville -> do
      _ <- execute c "insert into villes (inse,name,ligne5,cp) values (?,?,?,?)" [villeInse ville
                                                                        , villeName ville
                                                                        , villeLigne5 ville
                                                                        , villeCP ville]
      return ()
    Nothing -> return ()
             
main :: IO()
main = do
  c <- connect defaultConnectInfo { connectHost = "127.0.0.1", connectDatabase = "villes" }
  fcsv <- readFile "laposte_hexasmal.csv"
  mapM_ ((insert2db c) . entry2ville) $ readSSV format fcsv
  close c
  where
    format = csvFormat { ssvFormatSeparator = ';' }
