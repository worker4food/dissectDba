{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Prelude                         hiding (FilePath)

import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8      as C
import           Filesystem.Path
import qualified Filesystem.Path.CurrentOS       as FS
import           Options.Generic
import           System.Directory

import           Decode
import           PropList

data Action w
    = Dump
        { dba :: w ::: Maybe FilePath <?> "Location (file or directory) of 1Cv7.DBA, default '.'"
        , out :: w ::: Maybe FilePath <?> "Output file, default stdout"
        }
    | Compile
        { src :: w ::: Maybe FilePath <?> "Source file, default stdin"
        , out :: w ::: Maybe FilePath <?> "Output file, default stdout"
        }
    | Set
        { dba        :: w ::: Maybe FilePath <?> "Location (file or directory) of 1Cv7.DBA, default '.'"
        , server, db :: w ::: Maybe String   <?> "Update appropriate filed in <dba-file>"
        , uid, pwd   :: w ::: Maybe String   <?> "Update appropriate filed in <dba-file>"
        , crcAuto    :: w ::: Bool           <?> "Calculate checksum from <dba-dir>/usrdef/users.usr"
        , crcFile    :: w ::: Maybe FilePath <?> "Calculate checksum from specified file, overrides `crc-auto` switch"
        , crc        :: w ::: Maybe String   <?> "Set checksum to specified value, overrides `crc-auto` and `crc-file` switches"
        }
        deriving Generic

instance ParseRecord (Action Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (Action Unwrapped)

defaultFile = "1Cv7.DBA" :: FilePath

main :: IO ()
main = unwrapRecord "1Cv7.DBA manipulation tool" >>= \case
    Compile srcFile outFile -> do
        source <- readFileOrStd srcFile
        let compiled = (eitherResult . parse propList) source
        case compiled of
          Left err -> fail err
          Right _  -> writeFileOrStd outFile (encode source)

    Dump dbaFile outFile -> do
        file  <- guessDBAFile dbaFile
        bytes <- decode <$> readFileOrStd (Just file)
        writeFileOrStd outFile bytes

    Set {..} -> do
        dbaFile <- guessDBAFile dba
        source  <- decode <$> readFileOrStd (Just dbaFile)

        let srcValuesE = (eitherResult . parse propList) source
        srcValues <- either fail pure srcValuesE

        crc' <- case (crc, crcFile, crcAuto) of
            (Just val, _, _)  -> pure (Just val)

            (_, Just file, _) -> do
                crcE <- checkSumFromFile file
                pure $ either (const Nothing) Just crcE

            (_, _, True)      -> do
                let crcFile' = directory dbaFile </> "usrdef" </> "users.usr"
                crcE <- checkSumFromFile crcFile'
                pure $ either (const Nothing) Just crcE

            _                 -> pure Nothing

        let labels = Just <$> ["Server", "DB", "UID", "PWD", "Checksum"] :: [Maybe String]
        let values = catMaybes $ zipWith (liftM2 (,)) labels [server, db, uid, pwd, crc']

        let destValues = map (toOutStr . replaceK values) srcValues
        let destStr    = C.pack $ "{" ++ mconcat (intersperse "," destValues) ++ "}"

        writeFileOrStd (Just dbaFile) (encode destStr)
  where
    readFileOrStd (Just f) = C.readFile $ FS.encodeString f
    readFileOrStd Nothing  = C.getContents

    writeFileOrStd (Just f) = C.writeFile $ FS.encodeString f
    writeFileOrStd Nothing  = C.putStr

    checkSumFromFile = C.readFile . FS.encodeString >=> pure . mkChecksum

    guessDBAFile Nothing  = getCurrentDirectory >>= pure . FS.decodeString >>= pure . (</> defaultFile)
    guessDBAFile (Just f) = do
        isDir <- doesDirectoryExist $ FS.encodeString f
        pure $ if isDir
                  then f </> defaultFile
                  else f

    replaceK kvs (k, v) = case lookup k kvs of
        Just v' -> (k, v')
        Nothing -> (k, v)

    toOutStr :: (String, String) -> String
    toOutStr (k, v) = "{\"" ++ k ++ "\",\"" ++ v ++ "\"}"
