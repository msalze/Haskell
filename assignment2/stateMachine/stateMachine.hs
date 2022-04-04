import Data.ByteString (elemIndex, isPrefixOf, putStrLn)
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )
import qualified Control.Monad
-- import Data.String ()

-- openFile:: FilePath  -> [String]
-- openFile path = do

data StateTest = StateTest {name:: String, text:: String}
data Transition = Transition {tname:: String, from:: String, to:: String }

findType:: String -> String
findType line = do
    return "shit"

-- takes line and returns line without whitespaces in beginning
lstrip:: [Char] -> [Char]
lstrip text = do
    let first = head text
    Control.Monad.when (isSpace first) $ do
            let rest = tail text
            return (lstrip rest)
    return text
    -- return text

findStart:: [String] -> [String]
findStart lines = do
    let start = head lines
    let start_cleaned = lstrip start
    let boolean = "@" `Data.List.isPrefixOf` start_cleaned
    if boolean
        then do
            findType start_cleaned
        else do
            findStart (tail lines)


-- findOccurence:: String -> String -> String 
-- findOccurence line character = do
--     if elemIndex line character == 0
--         then return line
--         else return "" 

printContent:: String -> IO ()
printContent lines = do
    Prelude.putStrLn lines


main :: IO ()
main = do
    let path = "../../description2/vending.machine"
    content <- Prelude.readFile path
    let linesOfFiles = lines content
    mapM_ Prelude.putStrLn linesOfFiles

    Prelude.putStrLn "Hello, World!\n"