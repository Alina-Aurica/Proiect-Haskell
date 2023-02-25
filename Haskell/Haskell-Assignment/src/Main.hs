module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  DB.save DB.empty
  return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  database <- DB.load
  case database of 
    Success db ->
      let 
        first = fmap (DB.findFirst (\x -> entryId x == getOptId getOpts)) database
      in
        case first of
          Success entry -> 
            case entry of
              Just en -> putStrLn $ entrySnippet en
              Nothing -> putStrLn "No first entry found"
          _ -> putStrLn "No first entry found"
    Error err -> putStrLn "Failed to load DB"
  --return ()

showEntry :: TestableMonadIO m => [Entry] -> m ()
showEntry entryLst =
  case entryLst of
    [] -> return ()
    all@(x:xs) -> putStrLn ((show . FmtEntry) x) >> showEntry xs


-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  database <- DB.load
  case database of
    Success db ->
      let
        allEntry = fmap (DB.findAll (\x -> Entry.Entry.matchedByAllQueries (searchOptTerms searchOpts) x)) database
      in
        case allEntry of
          Success e 
            | e == [] -> putStrLn "No entries found"
            | otherwise -> showEntry e
          _ -> putStrLn "No entries found"
    Error err -> putStrLn "Failed to load DB"

  --return ()


-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  database <- DB.load
  source <- readFile (addOptFilename addOpts)

  case database of
    Success e -> 
      let 
        databaseWithInsert = DB.insertWith (\id -> makeEntry id source addOpts) databaseEmpty where databaseEmpty = getSuccess database DB.empty
        queryExist = DB.findFirst(\elem -> entrySnippet elem == source) (getSuccess database DB.empty)
      in

      case queryExist of
        Just entry -> Prelude.mapM_ putStrLn (["Entry with this content already exists: ", (show (FmtEntry entry))])
        _ -> do
                DB.save databaseWithInsert
                return ()

    _ -> putStrLn "Failed to load DB"

  return ()
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }
  

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
