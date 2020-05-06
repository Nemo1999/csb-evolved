module Warrier.Make
    ( makeWarrier
    )
where

import           Control.Monad
import           Data.List
import           System.Directory

makeWarrier :: FilePath -> IO String
makeWarrier srcPath = do
    let files =
            [ "Wapons.txt"
            , "Data/Vec2.hs"
            , "GameRule.hs"
            , "GameSim.hs"
            , "Player.hs"
            , "Player/Instances.hs"
            , "Player/GA.hs"
            , "Util.hs"
            ]

    modNames                <- map filePathToModPath <$> scanHsFiles srcPath

    (pragmas, imports, lns) <-
        unzip3
            <$> mapM
                    (\file -> procSrcLine modNames . lines <$> readFile
                        (srcPath ++ "/" ++ file)
                    )
                    files

    return $ unlines (concat pragmas ++ concat imports ++ concat lns)

scanHsFiles :: FilePath -> IO [FilePath]
scanHsFiles path = do
    dirItems <- listDirectory path
    files    <- forM dirItems $ \item -> do
        isDir <- doesDirectoryExist $ path ++ "/" ++ item
        if isDir
            then map ((item ++ "/") ++) <$> scanHsFiles (path ++ "/" ++ item)
            else if isSuffixOf ".hs" item then return [item] else return []

    return $ concat files

filePathToModPath :: FilePath -> String
filePathToModPath filePath = map repl noPrefix
  where
    noPrefix = take (length filePath - length ".hs") filePath

    repl '/' = '.'
    repl c   = c

procSrcLine :: [String] -> [String] -> ([String], [String], [String])
procSrcLine modNames = fst . foldl f (([], [], []), ([], False))
  where
    f ((pragmas, imports, dstLns), (prefixes, False)) srcLn =
        case parseSrcLine srcLn of
            Pragma -> ((srcLn : pragmas, imports, dstLns), (prefixes, False))
            ModuleWhere    -> ((pragmas, imports, dstLns), (prefixes, False))
            Module         -> ((pragmas, imports, dstLns), (prefixes, True))
            Import modName -> if modName `elem` modNames
                then ((pragmas, imports, dstLns), (prefixes, False))
                else ((pragmas, imports ++ [srcLn], dstLns), (prefixes, False))
            ImportQualified modName shortName -> if modName `elem` modNames
                then
                    ( (pragmas, imports, dstLns)
                    , ((shortName ++ ".") : prefixes, False)
                    )
                else ((pragmas, imports ++ [srcLn], dstLns), (prefixes, False))
            _ ->
                ( (pragmas, imports, dstLns ++ [removeAllSubStr prefixes srcLn])
                , (prefixes, False)
                )
    f (out, (prefixes, True)) srcLn = case parseSrcLine srcLn of
        Where -> (out, (prefixes, False))
        _     -> (out, (prefixes, True))

data SrcLine = Pragma | ModuleWhere | Module | Where | Import String | ImportQualified String String | Other deriving (Show, Read)

parseSrcLine :: String -> SrcLine
parseSrcLine ln
    | "{-#" `isPrefixOf` ln = Pragma
    | ("module" : _) <- wds = if "where" `elem` wds then ModuleWhere else Module
    | "where" `elem` wds = Where
    | ["import", "qualified", modName, "as", shortName] <- wds = ImportQualified
        modName
        shortName
    | ["import", "qualified", modName] <- wds = ImportQualified modName modName
    | ("import" : modName : _) <- wds = Import
    $ takeWhile (not . (== '(')) modName
    | otherwise = Other
    where wds = filter (not . null) $ words ln

removeAllSubStr :: [String] -> String -> String
removeAllSubStr _ [] = []
removeAllSubStr needles haystack @ (h : hs) =
    case find (\n -> isPrefixOf n haystack) needles of
        Just needle -> removeAllSubStr needles $ drop (length needle) haystack
        Nothing     -> h : removeAllSubStr needles hs
