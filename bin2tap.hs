import System.Environment
import Text.Read
import Data.Int
import Data.Word
import Data.Bits
import Data.Char
import qualified Data.ByteString as BS
import System.Posix

getFileSize :: String -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)


splitWord16_8 a = [(fromIntegral a :: Word8), (fromIntegral (shiftR a 8) :: Word8)]

stringData :: String -> [Word8]
stringData s = map (\c -> fromIntegral (ord c) :: Word8) s

headerData :: Word8 -> String -> Word16 -> Word16 -> Word16 -> [Word8]
headerData datatype name length ext1 ext2 =
    [datatype] ++ (stringData name) ++ (splitWord16_8 length) ++ (splitWord16_8 ext1) ++ (splitWord16_8 ext2)

checkSum :: [Word8] -> Word8
checkSum blockData = foldl xor 0 blockData

block :: Word8 -> [Word8] -> BS.ByteString
block flag blockData = let payload = [flag] ++ blockData ++ [checkSum ([flag] ++ blockData)] in
    BS.pack (splitWord16_8 (fromIntegral (length (payload)) :: Word16) ++ payload)

headerBlock blockData = block 0 blockData
dataBlock blockData = block 255 blockData

codeHeader :: String -> Word16 -> Word16 -> [Word8]
codeHeader name length startaddr = headerData 3 name length startaddr 32768

codeHeaderBlock :: String -> Word16 -> Word16 -> BS.ByteString
codeHeaderBlock name length startaddr = headerBlock (codeHeader name length startaddr)

rangedAddr :: Int -> (Maybe Int)
rangedAddr addr =
    if addr >= 0 && addr <= 65535 then Just addr
    else Nothing

parseAddr :: String -> Maybe Int
parseAddr addr = maybe Nothing rangedAddr (readMaybe addr :: Maybe Int)

parseName :: String -> Maybe String
parseName name = let l = length name in
    if l > 10 then Nothing
    else Just (name ++ replicate (10-l) ' ')

parseArgs :: [String] -> Maybe (Maybe Int, Maybe String, String)
parseArgs args = let l = length args in
    if l /= 3 then Nothing
    else Just (parseAddr (args !! 0), parseName (args !! 1), args !! 2)

invalidArgs = error ("Usage: bin2tap addr name file" ++
    "    where addr is the start address," ++
    "    name is the tape file name" ++
    "    and file is the local filename")

invalidName = error "Tape file name must be no more than 10 characters"

invalidAddr = error "Address must be an integer in the range 0 to 65535 (0x0000 to 0xFFFF)"

tapeConversion :: (Maybe Int) -> (Maybe String) -> String -> IO ()
tapeConversion _addr _name filename = do
    addr <- maybe invalidAddr return _addr;
    name <- maybe invalidName return _name;
    filelen <- getFileSize filename;
    filedata <- BS.readFile filename;
    BS.writeFile outfilename (
        codeHeaderBlock name (fromIntegral filelen :: Word16) (fromIntegral addr :: Word16)
        `BS.append`
        dataBlock (BS.unpack filedata)
        )
    where outfilename = filename ++ ".tap"

main = getArgs >>= maybe invalidArgs (\(addr, name, file) -> tapeConversion addr name file) . parseArgs
