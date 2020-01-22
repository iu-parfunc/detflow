{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module PermSpec
    -- (main, spec)
    where

import           Control.Monad (replicateM)
import           Control.Monad.DetIO.Perms

import           Data.Either (isRight)
import qualified Data.Map.Strict as M
import qualified Data.List as L

import           System.FilePath
import           Test.Hspec hiding (example)
import           Test.Hspec.QuickCheck (prop)
import           Test.HUnit
import           Test.QuickCheck (Arbitrary(..), Gen, generate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Join, lattice properties" $ do
    it "join_empty1" (addPerms0 `shouldSatisfy` (\(Right _) -> True))
    it "join_empty2" (addPerms1 `shouldSatisfy` (\(Right _) -> True))
    it "join_empty2" (addPerms2 `shouldSatisfy` (\(Right _) -> True))

  describe "Perms representation" $ do
    it "normPerms1" (normPerms (Node (R 1) (M.fromList [("a",Has (R 1))]))
                     `shouldBe` (Has (R 1)))
    it "normPerms2" (normPerms (Node (R 0) (M.fromList [("a",Has (R 0))]))
                     `shouldBe` (Has (R 0)))
    it "normPerms3" (let x = Node (R 1) (M.fromList [("a",Has (R 0))]) in
                     normPerms x `shouldBe` x)
    it "normPerms4" (let x = Node (R 0) (M.fromList [("a",Has (R 1))]) in
                     normPerms x `shouldBe` x)
    -- Duplicated from above:
    it "normPerms5" (normPerms (Node (RW 1) (M.fromList [("a",Has (RW 1))]))
                     `shouldBe` (Has (RW 1)))
    it "normPerms6" (normPerms (Node (RW 0) (M.fromList [("a",Has (RW 0))]))
                     `shouldBe` (Has (RW 0)))
    it "normPerms7" (let x = Node (RW 1) (M.fromList [("a",Has (RW 0))]) in
                     normPerms x `shouldBe` x)
    it "normPerms8" (let x = Node (RW 0) (M.fromList [("a",Has (RW 1))]) in
                     normPerms x `shouldBe` x)
    -- And mixed R/RW:
    it "normPerms9" (let x = Node (R 1) (M.fromList [("a",Has (RW 1))]) in
                     normPerms x `shouldBe` x)
    -- This one is not possible to encounter at runtime:
    it "normPerms10" (let x = Node (RW 1) (M.fromList [("a",Has (R 1))]) in
                     normPerms x `shouldBe` x)


  describe "checkout/addPerms arithmetic" $ do
    it "checkout/addPerms_inverse" case_addPerms1
    prop "prop_addPerms_self"    prop_addPerms_self

  describe "Can read/write" $ do
    prop "prop_canRead1"          prop_canRead1
    prop "prop_canRead2"          prop_canRead2
    prop "prop_canRead_allPerms"  prop_canRead_allPerms
    prop "prop_cantRead1"         prop_cantRead1
    prop "prop_cantRead_prefix"   prop_cantRead_prefix
    prop "prop_canWrite1"         prop_canWrite1
    prop "prop_canWrite2"         prop_canWrite2
    prop "prop_canWrite_allPerms" prop_canWrite_allPerms
    prop "prop_cantWrite1"        prop_cantWrite1
    prop "prop_cantWrite_prefix"  prop_cantWrite_prefix

    prop "prop_canExtend_parent"  prop_canExtend_parent

-- Example starting permissions for DetIO:
example :: Perms
example = Node noPerm
          (M.fromList [ ("input",  Has (R 1) )
                      , ("output", Has (RW 1)) ])

parent, child :: Perms
(parent,child) = pr
 where
  Right pr = checkout (mkPermR "/input/a") example

case_addPerms1 :: Assertion
case_addPerms1 = assertEqual "checkout/join inverse"
               (Right example)
               (normPerms <$> addPerms' parent child)

_prop_checkoutR_addPerms_inverse :: Perms -> PathPerm -> Bool
_prop_checkoutR_addPerms_inverse p1 pp =
    case checkout pp p1 of
      Right (a,b) -> Right p1 == addPerms' a b
      Left e -> error e

-- TODO more props:
-- prop total order of Frac

-- prop inverses: finish checkout/join inverses


fromPP :: PathPerm -> Perms
fromPP x = let IP y = fromPathPerm x in y

prop_addPerms_self :: PathPerm -> Bool
prop_addPerms_self pa = isRight $ addPerms' (fromPP pa) (fromPP pa)

-- FIXME: this is diverging:
_prop_temp :: Perms -> Bool
_prop_temp p1 = isRight $ addPerms' emptyPerms p1


-- _prop_addPerms_self2 p1 = isRight $ addPerms p1 p1

-- _prop_addPerms_commutative = ...

prop_canRead1 :: PathPerm -> Bool
prop_canRead1 pa = canRead (pathPermToPath pa) (fromPP pa)

prop_canRead2 :: PathPerm -> Bool
prop_canRead2 pa = canRead (pathPermToPath pa)
                   (fromRight $ addPerms' emptyPerms (fromPP pa))

prop_canRead_allPerms :: PathPerm -> Bool
prop_canRead_allPerms pa = canRead (pathPermToPath pa) allPerms

prop_cantRead1 :: PathPerm -> Bool
prop_cantRead1 pa = not $ canRead (pathPermToPath pa) emptyPerms


-- Cannot read a prefix of the dir we have permission on.
prop_cantRead_prefix :: PathPerm -> Bool
prop_cantRead_prefix pp | ls@(_:_) <- pathparts pp =
                          let prefixPath = "/" ++ concat (L.intersperse "/" (init ls)) in
                          not $ canRead prefixPath (fromPP pp)
                        | otherwise = True


-- Tedious duplication:
----------------------------------------
prop_canWrite1 :: PathPerm -> Bool
prop_canWrite1 pp =
    canWrite (pathPermToPath pp)
             (fromPP (WritePath (pathparts pp)))

prop_canWrite2 :: PathPerm -> Bool
prop_canWrite2 pa = canWrite (pathPermToPath pa)
                   (fromRight $ addPerms' emptyPerms (fromPP pa))

prop_canWrite_allPerms :: PathPerm -> Bool
prop_canWrite_allPerms pa = canWrite (pathPermToPath pa) allPerms

prop_cantWrite1 :: PathPerm -> Bool
prop_cantWrite1 pa = not $ canWrite (pathPermToPath pa) emptyPerms

prop_cantWrite_prefix :: PathPerm -> Bool
prop_cantWrite_prefix pp | (_:ls) <- pathparts pp =
                           not $ canWrite (pathPermToPath pp{pathparts=ls}) (fromPP pp)
                         | otherwise = True

----------------------------------------

-- | If we checkout two write perms on subdirs, both CAN extend the
-- parent.
prop_canExtend_parent :: PathPerm -> Bool
prop_canExtend_parent ReadPath{} = True
prop_canExtend_parent pp@WritePath{} =
    let path = pathPermToPath pp
        child1 = path </> "a"
        child2 = path </> "b"
        p1     = fromPP pp
        Right (p2,cp1) = checkout (mkPermRW child1) p1
        Right (p3,cp2) = checkout (mkPermRW child2) p2
    in
    canExtendDir path cp1
    && canExtendDir path cp2
    && canExtendDir path p3   -- Parent retains ability too.
    && canWrite path p1
    && not (canWrite path p3) -- Can't DELETE that directory.
    && not (canWrite path cp1) -- Child can't delete either
    && not (canWrite path cp2)

-- failing :: Bool
-- failing = prop_cantWrite_prefix (mkPermRW "/c")


{-

ase_fail1 :: Either String Perms
ase_fail1 = sub example "/input/a" W

-}

example_gen_atoms :: IO ()
example_gen_atoms =
  mapM_ print =<<
  (replicateM 10 $ generate (arbitrary :: Gen PathPerm))

example_gen_perms :: IO Perms
example_gen_perms = generate (arbPerm2 3)

perms1 :: Perms
perms1 = fromPP (mkPermRW "/a/s/z/k/o/s/b/f/k/a/i/a/x/m/a/a/a/a/t/u")

example_addPerms :: Either String Perms
example_addPerms = allPerms `addPerms` perms1

fromRight :: Either a b -> b
fromRight x = let (Right y) = x in y


addPerms0 :: Either String Perms
addPerms0 =
    fromPP (mkPermR "/input") `addPerms'`
    emptyPerms

addPerms1 :: Either String Perms
addPerms1 = do
    x <- fromPP (mkPermR "/input") `addPerms'`
         fromPP (mkPermRW "/output")
    x `addPerms` emptyPerms

addPerms2 :: Either String Perms
addPerms2 = addPerms parent child
 where
  parent = Node { this = R 0,
                  children = M.fromList [("input",Has (R 1)),("output",Has (RW 1))]}
  child = Has (R 0)
