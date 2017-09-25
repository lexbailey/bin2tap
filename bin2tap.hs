import System.Environment
import Text.Read

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
    print outfilename;
    where outfilename = filename ++ ".tap"

main = getArgs >>= maybe invalidArgs (\(addr, name, file) -> tapeConversion addr name file) . parseArgs
