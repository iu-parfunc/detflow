{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DetmakeLib where

import qualified Data.ByteString.Char8 as B
import           Data.Char (isSpace)
import           Data.Foldable (foldl')
import           Data.Function (on)
import qualified Data.List as L
import           Data.List (groupBy, sortBy)
import           Data.Makefile
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S

import           Safe (headDef)

import           System.Directory
import           System.FilePath

import           Text.Regex
-- import           Text.Show.Pretty (ppShow)

findMakefile :: IO (Maybe FilePath)
findMakefile = findFile ["."] "Makefile"

trim :: B.ByteString -> B.ByteString
trim = snd . B.span isSpace . fst . B.spanEnd isSpace

splitPostFacto :: Makefile -> Makefile
splitPostFacto (Makefile entrs) = Makefile (map splitEntry entrs)
  where
    splitEntry :: Entry -> Entry
    splitEntry (Rule tgt deps cmds) = Rule tgt
                                           (concatMap splitDependency deps)
                                           cmds
    splitEntry e = e

    splitDependency :: Dependency -> [Dependency]
    splitDependency (Dependency dep) = map Dependency $ B.words dep

-- You can very well have two targets with the same name:
--
--  foo: bar
--  foo: baz
--
-- provided that only one of them has any commands.
combineTargets :: Makefile -> Makefile
combineTargets (Makefile entrs) =
  let equalEntryGroups = equivalenceClasses
        (\case Rule tgt _ _ -> Left tgt
               e            -> Right e) entrs
  in Makefile $
     map (\equalEntries ->
             let entr:_ = equalEntries
                 areEntries = case entr of
                                    Rule{} -> True
                                    _      -> False
             in if areEntries
                  then let allCmds = map (\case Rule _ _ cmds -> cmds
                                                _             -> error "Shouldn't happen") equalEntries
                           neCmds = filter (not . null) allCmds
                       in case entr of
                                 Rule tgt _ _
                                   -> Rule tgt
                                           (concatMap (\(Rule _ deps _) -> deps) equalEntries)
                                           (headDef [] neCmds)
                                 _ -> error "Shoudn't happen"

                  else entr)
         equalEntryGroups

deriving instance Ord Dependency
deriving instance Ord Entry
deriving instance Ord Target
deriving instance Ord Command

equivalenceClasses :: Ord b => (a -> b) -> [a] -> [[a]]
equivalenceClasses rep = groupBy ((==) `on` rep) . sortBy (compare `on` rep)

trimMakefile :: Makefile -> Makefile
trimMakefile (Makefile entrs) = Makefile (map trimEntry entrs)
  where
    trimEntry :: Entry -> Entry
    trimEntry (Rule tgt deps cmds) = Rule (trimTarget tgt)
                                          (concatMap trimDependency deps)
                                          (map trimCommand cmds)
    trimEntry (Assignment lhs rhs) = Assignment (trim lhs) (trim rhs)

    trimTarget :: Target -> Target
    trimTarget (Target tgt) = Target (trim tgt)

    trimCommand :: Command -> Command
    trimCommand (Command cmd) = Command (trim cmd)

    trimDependency :: Dependency -> [Dependency]
    trimDependency (Dependency dep) = map Dependency $ B.words $ trim dep

partitionEntries :: [Entry] -> ( [(B.ByteString, B.ByteString)]
                               , [(Target, [Dependency], [Command])]
                               )
partitionEntries = partitionWith go
  where
    go (Assignment to from) = Left (to, from)
    go (Rule tgt deps cmds) = Right (tgt, deps, cmds)

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([],[])
partitionWith f (x:xs) = case f x of
                         Left  b -> (b:bs, cs)
                         Right c -> (bs, c:cs)
    where (bs,cs) = partitionWith f xs

-- This (naïvely?) assumes that all assignments are declared in a
-- top-down order.
toRegexes :: [(B.ByteString, B.ByteString)] -> Map String String
toRegexes = foldl' go $ M.fromList [ ("MAKE", "detmake") ]
  where
    go :: Map String String -> (B.ByteString, B.ByteString) -> Map String String
    go prevs (lhs, rhs) =
      let rhs' = replace prevs rhs
      in M.insert (B.unpack lhs) (B.unpack rhs') prevs

replace :: Map String String -> B.ByteString -> B.ByteString
replace prevs = go
  where
    go :: B.ByteString -> B.ByteString
    go bs = case matchRegexAll (mkRegex "(\\$\\([^\\(\\)]+?\\))|(\\$\\{[^\\{\\}]+?\\})") (B.unpack bs) of
              Nothing -> bs
              Just (_, sub, _, _)
                -> let sub'       = init (drop 2 sub)
                       openBrace  = sub !! 1
                       closeBrace = last sub
                       subEscaped = "\\$\\" ++ [openBrace] ++ sub' ++ "\\" ++ [closeBrace]
                   in case M.lookup sub' prevs of
                        Nothing  -> go $ subRegexByteString (mkRegex subEscaped) "" bs
                                    {-
                                    error $ "Unknown: " ++ sub
                                         ++ "\nMap: " ++ ppShow prevs
                                         ++ "\nsub': " ++ sub'
                                         ++ "\nWhole string: " ++ B.unpack bs
                                    -}
                        Just new -> go $ subRegexByteString (mkRegex subEscaped) new bs

-- Gross hack that lets us deal with the targets like .c.o
handleSuffixRules :: Map String String -> Makefile -> Makefile
handleSuffixRules regexMap (Makefile entrs) =
  let targetNames = mapMaybe (\case Rule (Target tgt) _ _ -> Just tgt
                                    Assignment{}          -> Nothing) entrs

      isTarget d = M.member d deps

      deps :: M.Map B.ByteString ([Command], S.Set B.ByteString)
      deps = M.fromList [ (tgt, (cmds, S.fromList (L.map fromDep deps')))
                        | Rule (Target tgt) deps' cmds <- entrs, not (null cmds) ]

      -- All the targets that will (transitively) be built:
      reachable :: [B.ByteString]
      reachable = S.toList $ go S.empty targetNames
        where
          go acc [] = acc
          go acc (x:xs) =
            case M.lookup x deps of
              Nothing -> go (S.insert x acc) xs -- We'll deal with this later.
              Just (_,alldeps) -> let fresh = alldeps `S.difference` acc
                                  in go (S.insert x (acc `S.union` fresh)) (S.toList fresh ++ xs)

      !reachableFiles = filter (not . isTarget) reachable

      suffixRules :: M.Map String (String, [Command])
      suffixRules = M.fromList $ mapMaybe getSuffixRule entrs

      getSuffixRule :: Entry -> Maybe (String, (String, [Command]))
      getSuffixRule (Rule (Target (B.unpack -> '.':rest@(x:xs))) _ coCmds)
        | x /= '.', '.' `elem` xs, (suf1, '.':suf2) <- splitExtension rest
        = Just (suf2, (suf1, coCmds))
      getSuffixRule _ = Nothing

      newRules :: [(Target, [Dependency], [Command])]
      newRules = go S.empty reachableFiles
        where
          go :: S.Set B.ByteString -> [B.ByteString] -> [(Target, [Dependency], [Command])]
          go seen (x:xs)
            | not (x `S.member` seen)
            , strTgt <- B.unpack x
            , hasExtension strTgt
            , tgtBaseName  <- takeBaseName strTgt
            , '.':tgtExt <- takeExtension strTgt
            , Just (depExt, coCmds) <- tgtExt `M.lookup` suffixRules
            = let dep = B.pack $ tgtBaseName <.> depExt
              in (Target x, [Dependency dep], coCmds)
                 : go (x `S.insert` seen) (xs ++ [dep])
          go seen (_:xs) = go seen xs
          go _    []     = []

      fromDep :: Dependency -> B.ByteString
      fromDep (Dependency bs) = bs

  in Makefile $ map (\(tgt, deps', cmds) -> subRule True regexMap tgt deps' cmds) newRules
            ++ filter (\x -> case getSuffixRule x of
                               Just{}  -> False
                               Nothing -> True)
                      entrs
  {-
  let suffixRules = mapMaybe getSuffixRule entrs
   in Makefile $ foldl' (\entrsSoFar entr ->
                   foldl' (\entr' (suf1, suf2, coCmds) ->
                     handleEntry suf1 suf2 coCmds entr')
                       entr suffixRules:entrsSoFar) [] entrs
  where
    getSuffixRule :: Entry -> Maybe (Char, Char, [Command])
    getSuffixRule (Rule (Target (B.unpack -> ['.', suf1, '.', suf2])) _ coCmds)
      = Just (suf1, suf2, coCmds)
    getSuffixRule _ = Nothing

    handleEntry :: Char -> Char -> [Command] -> Entry -> Entry
    handleEntry suf1 suf2 coCmds (Rule tgt@(Target tgt') deps@[Dependency dep] _)
      | strTgt <- B.unpack tgt'
      , strDep <- B.unpack dep
      , hasExtension strTgt && takeExtension strTgt == ['.', suf2]
      , hasExtension strDep && takeExtension strDep == ['.', suf1]
      = Rule tgt deps coCmds
    handleEntry _ _ _ e = e
-}

subMakefile :: Bool -- Should this expand suffix rules?
            -> Makefile
            -> (Map String String, Makefile)
subMakefile expSufRules (Makefile entrs) =
  let (assns, rules) = partitionEntries entrs
      regexMap = toRegexes assns

      go :: (Target, [Dependency], [Command]) -> Entry
      go (tgt, deps, cmds) = subRule expSufRules regexMap tgt deps cmds

  in (regexMap, Makefile $ map go rules)

subRule :: Bool
        -> Map String String
        -> Target -> [Dependency] -> [Command] -> Entry
subRule expSufRules regexMap tgt deps cmds
  = subAssns expSufRules regexMap
  $ Rule tgt deps
  $ subVars expSufRules tgt deps cmds

subRegexByteString :: Regex -> String -> B.ByteString -> B.ByteString
subRegexByteString re new within
  = B.pack $ subRegex re (B.unpack within) new

-- Assignment substitution (e.g. replace $(CC) with gcc)
subAssns :: Bool -- Should this expand suffix rules?
         -> Map String String
         -> Entry -> Entry
subAssns expSufRules prevs (Rule tgt deps cmds)
  = let subTgt = subTarget prevs tgt
    in Rule subTgt
            (subThroughSuffixRules expSufRules subTgt (subDependency prevs) deps)
            (subThroughSuffixRules expSufRules subTgt (subCommand    prevs) cmds)
subAssns _ _ e = e

subThroughSuffixRules :: Bool -> Target -> (a -> a) -> [a] -> [a]
subThroughSuffixRules expSufRules tgt substituter things
  | Target (B.unpack -> '.':rest@(x:xs)) <- tgt
  , x /= '.', '.' `elem` xs, (_, '.':_) <- splitExtension rest
  = if expSufRules
    then map substituter things
    else things
subThroughSuffixRules _ _ substituter things = map substituter things

subTarget :: Map String String -> Target -> Target
subTarget prevs (Target tgt) = Target (replace prevs tgt)

subDependency :: Map String String -> Dependency -> Dependency
subDependency prevs (Dependency dep) = Dependency (replace prevs dep)

subCommand :: Map String String -> Command -> Command
subCommand prevs (Command cmd) = Command (replace prevs cmd)

-- Replace the variables $@, $<, and $^ in a Rule's Command.
subVars :: Bool -> Target -> [Dependency] -> [Command] -> [Command]
subVars expSufRules t@(Target tgt) deps = subThroughSuffixRules expSufRules t go
  where
    deps' :: [String]
    deps' = map (\(Dependency dep) -> B.unpack dep) deps

    go :: Command -> Command
    go (Command cmd)
      = Command
      $ subRegexByteString (mkRegex "\\$\\*") (takeBaseName $ B.unpack tgt)
      $ subRegexByteString (mkRegex "\\$@") (B.unpack tgt)
      $ (if null deps' then id else subRegexByteString (mkRegex "\\$<") (head deps'))
      $ subRegexByteString (mkRegex "\\$\\^") (unwords deps') cmd
