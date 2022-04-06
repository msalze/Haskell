import Data.ByteString (elemIndex, isPrefixOf, putStrLn)
import Data.List ( isPrefixOf )
import Data.Char ( isSpace )
import qualified Control.Monad
import Data.Typeable
-- import Data.String ()

-- openFile:: FilePath  -> [String]
-- openFile path = do

data StateTest = StateTest {name:: String, text:: String}
data Transition = Transition {tname:: String, from:: String, to:: String }

findType:: String -> String
findType line = "anything"

-- takes line and returns line without whitespaces in beginning
-- lstrip:: [Char] -> [Char]
-- lstrip text = 
--     let first = head text
--     Control.Monad.when (isSpace first) $
--             let rest = tail text
--             return (lstrip rest)
--     return text
--     -- return text




-- findFirstNonSpace:: String -> String -> String 
-- findFirstNonSpace line character = do
--     if elemIndex line character == 0
--         then return line
--         else return "" 

-- how many lines contain the state
findEnd:: [String] -> Int
findEnd [] = 0
findEnd (x:xs)
        | containsEndState x = 1
        | otherwise = 1 + findEnd xs

findStart:: [String] -> [[String]]
findStart [] = []
findStart (x:xs)
        | "@" `Data.List.isPrefixOf` (dropWhile (== ' ') x) = (fst test) : (findStart $ snd test)
        | otherwise = findStart xs
        where 
            test = (splitAt (findEnd (x:xs)) (x:xs))
            -- start = 
            -- end = 

-- maybe use dropWhile instead?
-- findFirstNonSpace:: String -> String 
-- findFirstNonSpace [] = []
-- findFirstNonSpace line
--         | head line == ' ' = findFirstNonSpace (tail line)
--         | otherwise = line

-- if curly brace is in there
containsEndState:: String -> Bool  
containsEndState [] = False
containsEndState line
        | head line == '}' = True
        | otherwise = containsEndState (tail line)
-- findStart:: [String] -> SsdStart (tail lines)

-- printContent:: String -> IO ()
-- printContent lines = do
--     Prelude.putStrLn lines


main :: IO ()
main = do
    let path = "../../description2/vending.machine"
    content <- Prelude.readFile path
    let linesOfFiles = lines content
    -- mapM_ Prelude.putStrLn linesOfFiles

    print (typeOf $ head linesOfFiles)
    let var = return $ findFirstNonSpace (head linesOfFiles)
    let temp = findStart linesOfFiles
    print (typeOf var)
    print (typeOf temp)
    Prelude.putStrLn $ head var
    Prelude.putStrLn $ show temp

    Prelude.putStrLn "Hello, World!\n"