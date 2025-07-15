{-# LANGUAGE OverloadedStrings #-}

module Ageha.Hash where

import Data.ByteString (ByteString)

class HashAlgorithm a where
    -- | Associated type for the block size of the hash algorithm
    hashBlockSize :: a -> Int

    -- | Associated type for the digest size of the hash algorithm
    hashDigestSize :: a -> Int

    hashName :: a -> ByteString

data SHA256 = SHA256 deriving (Eq, Show)

instance HashAlgorithm SHA256 where
    hashBlockSize _ = 64
    hashDigestSize _ = 32
    hashName _ = "SHA-256"

data SHA384 = SHA384 deriving (Eq, Show)

instance HashAlgorithm SHA384 where
    hashBlockSize _ = 128
    hashDigestSize _ = 48
    hashName _ = "SHA-384"

data SHA512 = SHA512 deriving (Eq, Show)

instance HashAlgorithm SHA512 where
    hashBlockSize _ = 128
    hashDigestSize _ = 64
    hashName _ = "SHA-512"
