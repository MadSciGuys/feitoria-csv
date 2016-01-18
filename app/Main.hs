{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Data.List (sort, transpose)

import Data.Word

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Feitoria.CSV
import Feitoria

import System.Environment
import System.Directory

import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
    [mode, csvIn, tyInFile, tmpOutFile, feitOutFile] <- getArgs
    tys <- getCellTypes tyInFile
    -- The following CSV parser is not RFC compliant, but is practically
    -- compatible with our data sources.
    csvCells
        <- case mode of
            "directory" -> do
                files <- getDirectoryContents csvIn
                    >>= filterM doesFileExist
                    >>= mapM canonicalizePath 
                mapM (fmap BL.lines . BL.readFile) (sort files)
            "file" ->
                transpose <$> map (BL.split ',') <$> BL.lines <$> BL.readFile csvIn
    putTable
        (LazyTable
            (TableHeader 0x01 $ T.pack csvIn)
            (map (uncurry toLazyColumn) $ zip tys csvCells))
        tmpOutFile
        feitOutFile

getCellTypes :: FilePath -> IO [CellType]
getCellTypes file = map toCellType <$> lines <$> readFile file
    where
        toCellType :: String -> CellType
        toCellType "uint"     = TypeUInt
        toCellType "int"      = TypeInt
        toCellType "double"   = TypeDouble
        toCellType "datetime" = TypeDateTime
        toCellType "string"   = TypeString
        toCellType _          = error "Invalid cell type seen in type file."

toLazyColumn :: CellType -> [BL.ByteString] -> LazyColumn
toLazyColumn ty (x:xs) =
    LazyColumn
        (ColumnHeader (T.decodeUtf8 $ BL.toStrict x) ty 0)
        (map (toCell ty) xs)

toCell :: CellType -> BL.ByteString -> Maybe Cell
toCell _  str | str == "" = Nothing
toCell ty str = (Just . ($ str)) $
    case ty of
        TypeUInt     -> parseUInt
        TypeInt      -> parseInt
        TypeDouble   -> parseDouble
        TypeDateTime -> parseDateTime
        TypeString   -> parseString
        TypeBinary _ -> error "Binary type invalid CSV type."
        TypeBoolean  -> error "Boolean type invalid CSV type."
        TypeArray _  -> error "Array type invalid CSV type."
    where
        readByteString :: Read a => BL.ByteString -> a
        readByteString = read . T.unpack . T.decodeUtf8 . BL.toStrict

        parseInt      = CellInt    . readByteString
        parseUInt     = CellUInt   . readByteString
        parseDouble   = CellDouble . readByteString
        parseString   = CellString . T.decodeUtf8 . BL.toStrict
        parseDateTime = error "DateTime is a TODO."
