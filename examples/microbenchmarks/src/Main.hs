{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Concurrent (getNumCapabilities)
import           Control.Concurrent.Async (replicateConcurrently_)
import           Control.Monad
import           Control.Monad.DetIO        as D
import           Control.Monad.DetIO.Unsafe (fullNonDet)

import           Criterion.Main
import           Criterion.Types

import           Data.Int (Int64)
-- import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as T

-- import qualified Prelude as P
import           Prelude hiding (putStrLn)

import           System.IO.Silently (silence)
import           System.Process as Proc

theText :: Text
theText = "Hello, World!"

-- | Bound the prints per fork if we want to limit memory usage.
printsPerFork :: Maybe Int64
printsPerFork = Nothing
-- printsPerFork = Just 1000

{-# INLINE fanOut #-}
fanOut :: forall m. Monad m
       => (Maybe Int64)
       -> (forall x. m x -> IO x)
       -> (forall x. Int -> m x -> m ())
       -> (m ())   -- The actual work to do.
       -> Int64 -> IO ()
fanOut printsPerForkLimit toIO replicateAction putIt n = do
  caps <- getNumCapabilities
  silence $ toIO $
    case printsPerForkLimit of
      Nothing -> replicateAction caps $ putNText putIt n
      Just limit -> do 
        let (q,r) = quotRem n limit
        replicateAction (fromIntegral q) $
          replicateAction caps $ putNText putIt limit
        replicateAction caps $ putNText putIt r

{-# INLINE putNText #-}
putNText :: Monad m => (m ()) -> Int64 -> m ()
putNText doOneAction n = replicateM_ (fromIntegral n) $ doOneAction

{-# INLINE replicateDetIO_ #-}
replicateDetIO_ :: Int -> DetIO a -> DetIO ()
replicateDetIO_ n a = do
  thrds <- replicateM n $ forkIO a
  void $ joinThreads thrds

{-# INLINE mkDriver #-}
mkDriver :: Monad m => m String -> m ()
mkDriver fn =
  do str <- fn
     -- _ <- evaluate (length str)
     length str `seq` return ()
    
main :: IO ()
main = defaultMain    
    -- Perform N prints on all of numCapabalities threads:
    [ bench "Text.putStrLn" $
        Benchmarkable $ fanOut ppf id replicateConcurrently_ (T.putStrLn theText)
    , bench "DetIO.putTextLn_det" $
        Benchmarkable $ fanOut ppf runDetIO replicateDetIO_ (putTextLn theText)
    , bench "DetIO.putTextLn_nondet" $
        Benchmarkable $ fanOut ppf (\x -> runDetIO $ fullNonDet *> x)
                            replicateDetIO_ (putTextLn theText)
    
    , bench "Process.readProcess" $
        Benchmarkable $ fanOut ppf id replicateConcurrently_
                          (mkDriver (Proc.readProcess "./a.out" [] ""))

    , bench "DetIO.readProcess_det" $
        Benchmarkable $ fanOut ppf runDetIO replicateDetIO_
                          (mkDriver (D.readProcess "./a.out" [] ""))
    , bench "DetIO.readProcess_nondet" $
        Benchmarkable $ fanOut ppf (\x -> runDetIO $ fullNonDet *> x)
                          replicateDetIO_
                          (mkDriver (D.readProcess "./a.out" [] ""))
    ]
 where ppf = printsPerFork 
