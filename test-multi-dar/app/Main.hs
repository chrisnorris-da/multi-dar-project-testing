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

sdkVer = "2.8.0-snapshot.20231026.12262.0.vb12eb2ad"

main = do
    Config{..} <- execParser $ info (configParser <**> helper)
        ( fullDesc
       <> progDesc "Run recursive dir dar build test" )

    pwd <- getCurrentDirectory
    setCurrentDirectory "../example1/my-dep"
    removeDirectoryRecursiveIfExists "nested-1"
    forM_ [1..recursionLimit] $ \i -> do
        createDirectory' ("nested-" <> show i) >>= setCurrentDirectory
        writeFile "daml.yaml" $ unlines $
                    [ "sdk-version: " <> sdkVer
                    , "name: nested" <> show i
                    , "version: 0.0.1"
                    , "source: ."
                    , "build-options:"
                    , "- --target=1.15"
                    , "dependencies:"
                    , "- daml-prim"
                    , "- daml-stdlib"]
                    ++ if i == recursionLimit then [] else
                      ["data-dependencies:"]
                    ++ ["- ./nested-" <> show (i+1) <> darRoot <> show (i+1) <> "-0.0.1.dar"]
        writeFile ("Source" <> show i <> ".daml") $ unlines $
                    [ "module Source" <> show i <> " where"]
                    ++  ["import Source" <> show (i + 1) | i /= recursionLimit]
                    ++
                    ["a" <> show i <> " : Int"
                    , "a" <> show i <> " = 2 + " <> if i == recursionLimit then "9" else "a" <> show (i+1)
                    ]

    setCurrentDirectory $ pwd </> ".." </> "example1"
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

newtype Config = Config {recursionLimit :: Int}

configParser :: Parser Config
configParser = Config
    <$> option auto
        ( long "ddepth"
       <> short 'd'
       <> metavar "DIRDEPTH"
       <> help "Directory depth" )
