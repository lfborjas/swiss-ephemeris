{-# LANGUAGE NamedFieldPuns #-}
module SwissEphemeris.CmdLine where

import Data.Time
import System.Directory
import Options.Applicative
import SwissEphemeris.Precalculated
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Text.Read (readMaybe)
import SwissEphemeris

data Options = Options
  { ephe4Dir :: FilePath
  , ephePath :: FilePath
  , subcommand :: SubCommand
  }

data GenerateOptions = GenerateOptions
  { genStart :: EphemerisBlockNumber, genNFiles :: Int }

data QueryOptions = QueryOptions
  { day :: Day, withFallback :: Bool }

data SubCommand
  = Generate EphemerisBlockNumber Int
  | Query Day Bool


main :: IO ()
main = do
  Options {ephe4Dir, ephePath, subcommand } <- execParser optsParser
  absoluteEphe4dir <- makeAbsolute ephe4Dir
  absoluteEphePath <- makeAbsolute ephePath
  setEphe4Path absoluteEphe4dir
  
  withEphemerides absoluteEphePath $ do
    case subcommand of
      Generate block files -> generateEphemeris block files
      Query day withFallback -> queryEphemeris day withFallback
      
generateEphemeris :: EphemerisBlockNumber -> Int -> IO ()
generateEphemeris block fileCount = pure ()

queryEphemeris :: Day -> Bool -> IO ()
queryEphemeris queryDay allowFallback = do
  let (d, m, y) = toGregorian queryDay  
      julian    = julianDay y m (fromIntegral d) 0.0
  pure ()

dayReader :: ReadM Day
dayReader = eitherReader $ \arg ->
  case iso8601ParseM arg of
    Nothing -> Left $ "Invalid date: " <> arg
    Just tag -> Right tag

blockNumberReader :: ReadM EphemerisBlockNumber
blockNumberReader = eitherReader $ \arg ->
  case readMaybe arg >>= mkEphemerisBlockNumber of
    Nothing -> Left $ "Invalid block number: " <> arg <> " (must be between -20 and 300)"
    Just bn -> Right bn

generateParser :: Parser SubCommand
generateParser =
  Generate
    <$> option blockNumberReader (long "ephe-block" <> short 'b')
    <*> option auto (long "number-of-files" <> short 'n')
    
queryParser :: Parser SubCommand
queryParser =
  Query  
    <$> option dayReader (long "day" <> short 'd')
    <*> option auto (long "allow-fallback" <> short 'f')

generateCommand :: Mod CommandFields SubCommand
generateCommand = 
  command "generate" (info generateParser (progDesc "Generate precalculated ephemeris files"))

queryCommand :: Mod CommandFields SubCommand
queryCommand = 
  command "query"  (info queryParser (progDesc "Query an existing precalculated ephemeris file"))
  
mainOptions :: Parser Options
mainOptions =
  Options 
    <$> strOption (long "base-directory" <> short 'o' <> help "location of precalculated sep4_* files") 
    <*> strOption (long "ephe-directory" <> short 'e' <> help "location of ephemeris data") 
    <*> hsubparser (generateCommand <> queryCommand) 
    
optsParser :: ParserInfo Options
optsParser =
  info
    (helper <*> mainOptions)
    (fullDesc <> progDesc "Generate or query precalculated ephe4-style ephemeris.")
