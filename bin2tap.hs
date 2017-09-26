module Main (main) where

import Data.Bits
import qualified Data.ByteString as BS
import Data.Char
import Data.Word
import System.Environment
import System.Posix
import Text.Read

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)

-- Split an unsigned 16 bit int into a pair) of unsigned 8 bit ints
splitWord16_8 :: Word16 -> [Word8]
splitWord16_8 a = [fromIntegral a, fromIntegral (shiftR a 8)]

-- String to Word8 array
stringData :: String -> [Word8]
stringData s = map (\c -> fromIntegral (ord c)) s

-- For building the spectrum format header block data
headerData :: Word8 -> String -> Word16 -> Word16 -> Word16 -> [Word8]
headerData datatype name length ext1 ext2 =
    [datatype] ++ stringData name ++ splitWord16_8 length ++ splitWord16_8 ext1 ++ splitWord16_8 ext2

-- Calculates the block's checksum byte
checkSum :: [Word8] -> Word8
checkSum blockData = foldl xor 0 blockData

-- Takes a block of data, adds the flag (for header or data) and prepends
-- the .tap format length field
block :: Word8 -> [Word8] -> BS.ByteString
block flag blockData = let payload = [flag] ++ blockData ++ [checkSum ([flag] ++ blockData)] in
    BS.pack (splitWord16_8 (fromIntegral (length payload)) ++ payload)

-- helpers for the different types of blocks
headerBlock, dataBlock :: [Word8] -> BS.ByteString
headerBlock blockData = block 0 blockData
dataBlock blockData = block 255 blockData

-- a helper for code headers (type 3 with special magic number in ext2 field)
codeHeader :: String -> Word16 -> Word16 -> [Word8]
codeHeader name length startaddr = headerData 3 name length startaddr 32768

-- a helper for code headers in .tap format
codeHeaderBlock :: String -> Word16 -> Word16 -> BS.ByteString
codeHeaderBlock name length startaddr = headerBlock (codeHeader name length startaddr)

-- limit checks for start address
rangedAddr :: Int -> Maybe Int
rangedAddr addr =
    if addr >= 0 && addr <= 65535 then Just addr
    else Nothing

-- for parsing address arg
parseAddr :: String -> Maybe Int
parseAddr addr = maybe Nothing rangedAddr (readMaybe addr)

-- for parsing name arg (must be <= 10 length)
parseName :: String -> Maybe String
parseName name = let l = length name in
    if l > 10 then Nothing
    else Just (name ++ replicate (10-l) ' ')

-- for parsing all the command line arguments
parseArgs :: [String] -> Maybe (Maybe Int, Maybe String, String)
parseArgs args = let l = length args in
    if l /= 3 then Nothing
    else Just (parseAddr (args !! 0), parseName (args !! 1), args !! 2)

invalidArgs :: a
invalidArgs = error ("Usage: bin2tap addr name file" ++
    "    where addr is the start address," ++
    "    name is the tape file name" ++
    "    and file is the local filename")

invalidName :: a
invalidName = error "Tape file name must be no more than 10 characters"

invalidAddr :: a
invalidAddr = error "Address must be an integer in the range 0 to 65535 (0x0000 to 0xFFFF)"

-- Main conversion function
tapeConversion :: Maybe Int -> Maybe String -> String -> IO ()
tapeConversion _addr _name filename = do
    -- Various IO happens here...
    -- validate arguments
    addr <- maybe invalidAddr return _addr;
    name <- maybe invalidName return _name;
    -- get the size of the binary
    filelen <- getFileSize filename;
    -- get the contents of the binary
    filedata <- BS.readFile filename;
    -- save a new file that contains...
    BS.writeFile outfilename (
        -- ...a header block for the binary data...
        codeHeaderBlock name (fromIntegral filelen) (fromIntegral addr)
        `BS.append`
        -- ...and the binary data block itself.
        dataBlock (BS.unpack filedata)
        )
    where outfilename = filename ++ ".tap"

main :: IO ()
main = getArgs >>= maybe invalidArgs (\(addr, name, file) -> tapeConversion addr name file) . parseArgs
