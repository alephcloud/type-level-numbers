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
  plus  <- sequence $(listE (testAddZ <$> [-9..9] <*> [-9..9]))
  minus <- sequence $(listE (testSubZ <$> [-9..9] <*> [-9..9]))
  mult  <- sequence $(listE (testMulZ <$> [-9..9] <*> [-9..9]))
  case and $ plus ++ minus ++ mult of
    True  -> exitSuccess
    False -> exitFailure
#else
  return ()
#endif
