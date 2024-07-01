{-# OPTIONS -Wno-missing-signatures #-}

module Main (main) where
import Control.Exception
import Control.Monad
import Control.Monad.Random
import Data.List
import Options.Applicative
import System.Directory
import System.FilePath
import System.Process
import System.IO.Error
import System.Random
import System.Random.Shuffle

main = do
    Config{..} <- execParser $ info (configParser <**> helper)
        ( fullDesc
       <> progDesc "Run recursive dir dar build test" )

    pwd <- getCurrentDirectory
    let newScaffoldFolderName = "../" <> folderName <> "/nesting-example"
    createDirectoryIfMissing True newScaffoldFolderName
    setCurrentDirectory newScaffoldFolderName
    let sourceDirName = if sourceDirectoryNameIsDaml then "daml" else "."
    forM_ [1..recursionLimit] $ \i -> do
        createDirectory' ("nested-" <> show i) >>= setCurrentDirectory
        createDirectoryIfMissing True sourceDirName
        writeFile "daml.yaml" $ unlines $
                    [ "sdk-version: " <> sdkVer
                    , "name: nested" <> show i
                    , "version: 0.0.1"
                    , ("source: " <> sourceDirName)
                    , "build-options:"
                    , "- --target=1.16"
                    , "dependencies:"
                    , "- daml-prim"
                    , "- daml-stdlib"]
                    ++ if i == recursionLimit then [] else
                      ["data-dependencies:"]
                    ++ ["- ./nested-" <> show (i+1) <> darRoot <> show (i+1) <> "-0.0.1.dar"]
        writeFile (sourceDirName <> "/Source" <> show i <> ".daml") $ unlines $
                    [ "module Source" <> show i <> " where"]
                    ++  ["import Source" <> show (i + 1) | i /= recursionLimit]
                    ++
                    ["a" <> show i <> " : Int"
                    , "a" <> show i <> " = 2 + " <> if i == recursionLimit then "9" else "a" <> show (i+1)
                    ]

    setCurrentDirectory $ pwd </> ".." </> folderName
    removeFileIfExists "multi-package.yaml"
    dependencies <-
               map (fst . splitFileName) . words <$> readProcess "find" [".", "-name","daml.yaml"] []
                  >>= evalRandIO . shuffleM

    writeFile "multi-package.yaml" $ unlines $
                    [ "packages:" ]
                    ++
                    (("- " <> ) <$> dependencies)

      where darRoot = "/.daml/dist/nested"

createDirectory' d = do {createDirectory d; return d}
removeDirectoryRecursiveIfExists d = doesDirectoryExist d >>= flip when (removeDirectoryRecursive d)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

data Config = Config {recursionLimit :: Int, folderName :: String, sourceDirectoryNameIsDaml :: Bool, sdkVer :: String}

folder :: Parser String
folder = strOption
  (  long "folder"
  <> short 'f'
  <> metavar "FOLDER"
  <> help "Scaffold output folder" )

sdkVerP :: Parser String
sdkVerP = strOption
  ( long "sdkversion"
  <> short 's'
  <> help "Scaffold sdk version" )

sourceDirectoryNameIsDamlP :: Parser Bool
sourceDirectoryNameIsDamlP =
  switch ( long "useDamlName" <> short 'u' <> help "Whether to use daml as source dir name" )

configParser :: Parser Config
configParser = Config
    <$> option auto
        ( long "ddepth"
       <> short 'd'
       <> metavar "DIRDEPTH"
       <> help "Directory depth" )
    <*> folder
    <*> sourceDirectoryNameIsDamlP
    <*> sdkVerP
