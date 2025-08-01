{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Ageha.Cipher.AEAD (
    -- * AEAD
    pattern AES128GCM,
    pattern AES256GCM,
    pattern ChaCha20Poly1305,

    -- * Encryption
    aeadInitEncrypt,
    aeadEncrypt,

    -- * Decryption
    aeadInitDecrypt,
    aeadDecrypt,
) where

import Botan.Low.Cipher
import Data.ByteString (ByteString)

pattern AES128GCM :: AEADName
pattern AES128GCM = "AES-128/GCM"

pattern AES256GCM :: AEADName
pattern AES256GCM = "AES-256/GCM"

aeadInitEncrypt
    :: CipherName
    -> ByteString
    -- ^ Key
    -> IO Cipher
aeadInitEncrypt c key = do
    code <- cipherInit c Encrypt
    cipherSetKey code key
    return code

aeadInitDecrypt
    :: CipherName
    -> ByteString
    -- ^ Key
    -> IO Cipher
aeadInitDecrypt c key = do
    code <- cipherInit c Decrypt
    cipherSetKey code key
    return code

aeadEncrypt
    :: Cipher
    -> ByteString
    -- ^ Nonce
    -> ByteString
    -- ^ Associate data
    -> ByteString
    -- ^ Plain text
    -> IO ByteString
    -- ^ Cipher text including an authentication tag
aeadEncrypt enc nonce aad plain = do
    cipherSetAssociatedData enc aad
    cipherStart enc nonce
    cipherEncrypt enc plain

aeadDecrypt
    :: Cipher
    -> ByteString
    -- ^ Nonce
    -> ByteString
    -- ^ Associate data
    -> ByteString
    -- ^ Cipher text including an authentication tag
    -> IO ByteString
    -- ^ Plain text
aeadDecrypt dec nonce aad cipher = do
    cipherSetAssociatedData dec aad
    cipherStart dec nonce
    cipherDecrypt dec cipher
