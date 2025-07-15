{-# LANGUAGE OverloadedStrings #-}

module Ageha.KDF.HKDF where

import Botan.Low.KDF
import Data.ByteString (ByteString)
import System.IO.Unsafe (unsafePerformIO)

import Ageha.Hash

type IKM = ByteString
type SALT = ByteString
type Info = ByteString
type OKM = ByteString

data PRK = PRK ByteString

hkdfExtract :: HashAlgorithm h => h -> SALT -> IKM -> PRK
hkdfExtract h salt ikm = PRK prk
  where
    size = hashDigestSize h
    name = hashName h
    prk = unsafePerformIO $ kdf (hkdf_extract name) size ikm salt ""

hkdfExpand :: HashAlgorithm h => h -> PRK -> Info -> Int -> OKM
hkdfExpand h (PRK prk) info len = okm
  where
    name = hashName h
    okm = unsafePerformIO $ kdf (hkdf_expand name) len prk "" info
