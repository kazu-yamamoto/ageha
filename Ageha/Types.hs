module Ageha.Types where

import Data.ByteString

-- | Symmetric key.
newtype Key = Key {getKey :: ByteString} deriving (Eq, Show)

-- | Nonce or initial vector.
newtype Nonce = Nonce {getNonce :: ByteString} deriving (Eq, Show)

-- | Associate/Additional Data.
newtype AAD = AAD {getAAD :: ByteString} deriving (Eq, Show)
