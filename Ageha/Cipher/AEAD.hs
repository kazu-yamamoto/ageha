{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Ageha.Cipher.AEAD (
    -- * AEAD
    AEADName,
    pattern AES128GCM,
    pattern AES256GCM,
    pattern ChaCha20Poly1305,

    -- * Encryption
    aeadInitEncrypt,
    aeadEncrypt,
    AEADEncrypter,

    -- * Decryption
    aeadInitDecrypt,
    aeadDecrypt,
    AEADDecrypter,

    -- * Types
    Key (..),
    Nonce (..),
    AAD (..),
) where

---------------------------------------------------------------

import Botan.Low.Cipher
import Data.ByteString (ByteString)

import Ageha.Types

---------------------------------------------------------------

pattern AES128GCM :: AEADName
pattern AES128GCM = "AES-128/GCM"

pattern AES256GCM :: AEADName
pattern AES256GCM = "AES-256/GCM"

newtype AEADEncrypter = AEADEncrypter Cipher
newtype AEADDecrypter = AEADDecrypter Cipher

---------------------------------------------------------------

aeadInitEncrypt
    :: CipherName
    -> Key
    -> IO AEADEncrypter
aeadInitEncrypt cn (Key key) = do
    code <- cipherInit cn Encrypt
    cipherSetKey code key
    return $ AEADEncrypter code

aeadInitDecrypt
    :: CipherName
    -> Key
    -> IO AEADDecrypter
aeadInitDecrypt cn (Key key) = do
    code <- cipherInit cn Decrypt
    cipherSetKey code key
    return $ AEADDecrypter code

aeadEncrypt
    :: AEADEncrypter
    -> Nonce
    -> AAD
    -> ByteString
    -- ^ Plain text
    -> IO ByteString
    -- ^ Cipher text including an authentication tag
aeadEncrypt (AEADEncrypter enc) (Nonce nonce) (AAD aad) pt = do
    cipherSetAssociatedData enc aad
    cipherStart enc nonce
    cipherEncrypt enc pt

aeadDecrypt
    :: AEADDecrypter
    -> Nonce
    -> AAD
    -> ByteString
    -- ^ Cipher text including an authentication tag
    -> IO ByteString
    -- ^ Plain text
aeadDecrypt (AEADDecrypter dec) (Nonce nonce) (AAD aad) ct = do
    cipherSetAssociatedData dec aad
    cipherStart dec nonce
    cipherDecrypt dec ct
