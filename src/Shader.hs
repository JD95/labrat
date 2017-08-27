{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}

module Shader where

import GHC.Prim
import GHC.TypeLits
import GHC.Natural
import Data.Proxy
import Data.Monoid
import Control.Monad
import Linear
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import qualified Graphics.GL.Core43 as GL
import Foreign.Storable
import Data.Array.MArray
import Data.Array.Storable
import Data.Array.IO
import Data.HVect hiding (Nat)

class ShaderInput (p :: (* -> *) -> * -> *) where
  layout :: Storable (t a) => p t a -> Natural
  name :: Storable (t a) => p t a -> String
  value :: Storable (t a) => p t a -> t a

newtype ShaderVal (p :: Nat) (n :: Symbol) t a = ShaderVal (t a)

instance (KnownNat p, KnownSymbol n) => ShaderInput (ShaderVal p n) where
  layout (_ :: ShaderVal p n t a) = fromIntegral $ natVal (Proxy :: Proxy p)
  name (_ :: ShaderVal p n t a) = symbolVal (Proxy :: Proxy n)
  value (ShaderVal t) = t

