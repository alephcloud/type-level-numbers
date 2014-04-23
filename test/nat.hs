{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
import Control.Applicative
#ifdef TEMPLATE_HASKELL
import Language.Haskell.TH
#endif
import System.Exit

import TestNat


main :: IO ()
main = do
#ifdef TEMPLATE_HASKELL
  plus  <- sequence $(listE (testAdd <$> [0..8] <*> [0..8]))
  minus <- sequence $(listE [testSub n m | m <- [0..8], n <- [m..8]])
  mult  <- sequence $(listE (testMul <$> [0..8] <*> [0..8]))
  case and $ plus ++ minus ++ mult of
    True  -> exitSuccess
    False -> exitFailure
#else
  return ()
#endif
