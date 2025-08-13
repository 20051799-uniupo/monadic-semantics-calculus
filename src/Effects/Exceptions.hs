{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Exceptions (
    ExceptionSig(..)
) where

import Lib

data ExceptionSig = Throw

-- instance Sig ExceptionSig where
--     arity Throw = 1
-- 
-- instance MonSem (Either e) ExceptionSig  where
--   run Throw [e] = Left e
