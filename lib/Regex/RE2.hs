{-# LANGUAGE ForeignFunctionInterface #-}

-- |
--
-- TODO: write docs
module Regex.RE2
	( Pattern
	, Error
	, Options
	, defaultOptions
	, compile
	, replace
	, replaceAll
	, extract
	, quoteMeta
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import           Data.String (IsString, fromString)
import           Foreign.C
import           System.IO.Unsafe (unsafePerformIO)

data Pattern = Pattern

instance Eq Pattern where

instance Show Pattern where

instance IsString Pattern where

data Options = Options

defaultOptions :: Options
defaultOptions = undefined

data Error = Error

compile :: Options -> B.ByteString -> Either Error Pattern
compile = undefined

replace :: Pattern -> B.ByteString -> B.ByteString -> (B.ByteString, Bool)
replace = undefined

replaceAll :: Pattern -> B.ByteString -> B.ByteString -> (B.ByteString, Integer)
replaceAll = undefined

extract :: Pattern -> B.ByteString -> B.ByteString -> Maybe B.ByteString
extract = undefined

quoteMeta :: B.ByteString -> B.ByteString
quoteMeta input = unsafePerformIO $ do
	cstr <- B.unsafeUseAsCString input c_quote_meta
	B.unsafePackMallocCString cstr

foreign import ccall "haskell-re2.h haskell_re2_quote_meta"
	c_quote_meta :: CString -> IO CString
