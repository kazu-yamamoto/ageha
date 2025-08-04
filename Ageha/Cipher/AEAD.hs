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
    AEADEncrypter,

    -- * Decryption
    aeadInitDecrypt,
    aeadDecrypt,
    AEADDecrypter,
) where

import Botan.Low.Cipher
import Data.ByteString (ByteString)

pattern AES128GCM :: AEADName
pattern AES128GCM = "AES-128/GCM"

pattern AES256GCM :: AEADName
pattern AES256GCM = "AES-256/GCM"

newtype AEADEncrypter = AEADEncrypter Cipher
newtype AEADDecrypter = AEADDecrypter Cipher

aeadInitEncrypt
    :: CipherName
    -> ByteString
    -- ^ Key
    -> IO AEADEncrypter
aeadInitEncrypt c key = do
    code <- cipherInit c Encrypt
    cipherSetKey code key
    return $ AEADEncrypter code

aeadInitDecrypt
    :: CipherName
    -> ByteString
    -- ^ Key
    -> IO AEADDecrypter
aeadInitDecrypt c key = do
    code <- cipherInit c Decrypt
    cipherSetKey code key
    return $ AEADDecrypter code

aeadEncrypt
    :: AEADEncrypter
    -> ByteString
    -- ^ Nonce
    -> ByteString
    -- ^ Associate data
    -> ByteString
    -- ^ Plain text
    -> IO ByteString
    -- ^ Cipher text including an authentication tag
aeadEncrypt (AEADEncrypter enc) nonce aad plain = do
    cipherSetAssociatedData enc aad
    cipherStart enc nonce
    cipherEncrypt enc plain

aeadDecrypt
    :: AEADDecrypter
    -> ByteString
    -- ^ Nonce
    -> ByteString
    -- ^ Associate data
    -> ByteString
    -- ^ Cipher text including an authentication tag
    -> IO ByteString
    -- ^ Plain text
aeadDecrypt (AEADDecrypter dec) nonce aad cipher = do
    cipherSetAssociatedData dec aad
    cipherStart dec nonce
    cipherDecrypt dec cipher
