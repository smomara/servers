{-# LANGUAGE OverloadedStrings #-}

module InsecureSocketsLayer where

import Control.Monad (unless)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, isDigit, ord)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)
import Debug.Trace (traceShowM)
import Network.Simple.TCP

data CipherOp
  = Reverse
  | Xor Word8
  | XorPos
  | Add Word8
  | AddPos
  deriving (Show, Eq)

type CipherSpec = [CipherOp]

parseCipherSpec :: ByteString -> Maybe CipherSpec
parseCipherSpec bs = go (BS.unpack bs)
 where
  go [] = Nothing -- needs to end with 00
  go (0x00 : _) = Just []
  go (0x01 : rest) = (Reverse :) <$> go rest
  go (0x02 : n : rest) = (Xor n :) <$> go rest
  go (0x03 : rest) = (XorPos :) <$> go rest
  go (0x04 : n : rest) = (Add n :) <$> go rest
  go (0x05 : rest) = (AddPos :) <$> go rest
  go _ = Nothing

reverseBits :: Word8 -> Word8
reverseBits n = foldl (\acc i -> (acc `shiftL` 1) .|. ((n `shiftR` i) .&. 1)) 0 [0 .. 7]

getPositions :: Integer -> [Word8]
getPositions startPos = map (fromIntegral . (`mod` 256)) [startPos ..]

applyOp :: Integer -> CipherOp -> [Word8] -> [Word8]
applyOp startPos op = case op of
  Reverse -> map reverseBits
  Xor n -> map (`xor` n)
  XorPos -> zipWith xor positions
  Add n -> map (+ n)
  AddPos -> zipWith (+) positions
 where
  positions = getPositions startPos

applyRevOp :: Integer -> CipherOp -> [Word8] -> [Word8]
applyRevOp startPos op = case op of
  Add n -> map (subtract n)
  AddPos -> zipWith subtract positions
  _ -> applyOp startPos op -- XOR and Reverse are their own inverses
 where
  positions = getPositions startPos

encodeCipher :: Integer -> CipherSpec -> ByteString -> ByteString
encodeCipher startPos ops bs =
  BS.pack $ foldl (flip $ applyOp startPos) (BS.unpack bs) ops

decodeCipher :: Integer -> CipherSpec -> ByteString -> ByteString
decodeCipher startPos ops bs =
  BS.pack $ foldl (flip $ applyRevOp startPos) (BS.unpack bs) (reverse ops)

isNoOpCipher :: CipherSpec -> Bool
isNoOpCipher ops = all (\b -> foldl (flip $ applyOp 0) [b] ops == [b]) [0 .. 255]

parseRequest :: ByteString -> [(Int, String)]
parseRequest bs =
  map parseToy $
    BS.split (fromIntegral $ ord ',') $
      if BS.last bs == fromIntegral (ord '\n')
        then BS.init bs
        else bs

splitOn :: Word8 -> [Word8] -> [[Word8]]
splitOn delim = go
 where
  go [] = []
  go xs =
    let (part, rest) = break (== delim) xs
     in part : case rest of
          [] -> []
          (_ : xs') -> go xs'

parseToy :: ByteString -> (Int, String)
parseToy bs =
  let (nums, _) = BS.span (isDigit . chr . fromIntegral) bs
      num = read $ map (chr . fromIntegral) $ BS.unpack nums
   in (num, map (chr . fromIntegral) $ BS.unpack bs)

findMaxToy :: [(Int, String)] -> String
findMaxToy = snd . maximumBy (comparing fst)

processLine :: Socket -> CipherSpec -> ByteString -> IO ()
processLine sock cipherSpec line = do
  let toys = parseRequest line
      maxToy = findMaxToy toys
      response = BS.pack $ map (fromIntegral . ord) (maxToy ++ "\n")
      encodedResponse = encodeCipher 0 cipherSpec response
  send sock encodedResponse

handleClient :: Socket -> CipherSpec -> Integer -> ByteString -> IO ()
handleClient sock cipherSpec pos buffer = do
  maybeBytes <- recv sock 4096
  case maybeBytes of
    Nothing -> return () -- Client disconnected
    Just bytes -> do
      let decoded = decodeCipher pos cipherSpec bytes
          newBuffer = buffer <> decoded
          newPos = pos + toInteger (BS.length bytes)
      if BS.null newBuffer
        then return ()
        else
          if BS.last newBuffer == fromIntegral (ord '\n')
            then do
              mapM_ (processLine sock cipherSpec) $
                filter (not . BS.null) $
                  BS.split (fromIntegral $ ord '\n') newBuffer
              handleClient sock cipherSpec newPos BS.empty
            else handleClient sock cipherSpec newPos newBuffer

runInsecureSocketsLayer :: ServiceName -> IO ()
runInsecureSocketsLayer port = serve (Host "0.0.0.0") port $ \(sock, _) -> do
  maybeBytes <- recv sock 128
  case maybeBytes >>= parseCipherSpec of
    Nothing -> return () -- Invalid cipher spec
    Just cipherSpec ->
      unless (isNoOpCipher cipherSpec) $
        handleClient sock cipherSpec 0 BS.empty
