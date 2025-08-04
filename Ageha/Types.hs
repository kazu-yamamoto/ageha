module Ageha.Types where

import Data.ByteString

-- | Symmetric key.
newtype Key = Key ByteString deriving (Eq, Show)

-- | Nonce or initial vector.
newtype Nonce = Nonce ByteString deriving (Eq, Show)

-- | Associate/Additional Data.
newtype AAD = AAD ByteString deriving (Eq, Show)
