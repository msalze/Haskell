import Data.ByteString (elemIndex, isPrefixOf, putStrLn)
import Data.List
import Data.Char ( isSpace )
import qualified Control.Monad
import Data.Typeable

data StateTest = StateTest String TypeOfState [String] -- name type text
    deriving Show
data TypeOfState = Start | Middle | End
    deriving Show

getName:: StateTest -> String 
getName (StateTest name _ _) = name

getType:: StateTest -> TypeOfState
getType (StateTest _ t _) = t

getText:: StateTest -> [String]
getText (StateTest _ _ text) = text

-- creates a StateTest from 
initState:: [String] -> StateTest
initState str = StateTest (extractStateName $ head str) (findType $ head str) (findText str)

-- returns the type that the state definition has
findType:: String -> TypeOfState
findType (x:xs)
    | "*" `Data.List.isPrefixOf` (dropWhile (== ' ') xs) = Start
    | "+" `Data.List.isPrefixOf` (dropWhile (== ' ') xs) = End
    | otherwise = Middle

-- extracts the text that should be printed
-- TODO: different if only one line!!
findText:: [String] -> [String]
findText str = ((tail cleaned) : (firstLast str)) ++ [(init cleanedEnd)]
    where 
        cleaned = (dropWhile (/= '{') $ head str)
        cleanedEnd = (dropWhileEnd (/= '}') $ last str)

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

extractStateName:: String -> String
extractStateName (x:xs) = cleanedEnd
    where
        cleanedStart = dropWhile (== '+') (dropWhile (== '*') xs)
        cleanedEnd = init (dropWhileEnd (/= '{') cleanedStart)

createStates:: [[String]] -> [StateTest]
createStates [] = []
createStates (x:xs) = initState x : createStates xs


data Transition = Transition String String String String -- start name end text
    deriving Show

getTransitionStart:: Transition -> String
getTransitionStart (Transition t _ _ _) = t

getTransitionName:: Transition -> String
getTransitionName (Transition _ t _ _) = t

getTransitionEnd:: Transition -> String
getTransitionEnd (Transition _ _ t _) = t

getTransitionText:: Transition -> String
getTransitionText (Transition _ _ _ t) = t

initTransition:: String -> Transition
initTransition str = Transition (start) (firstLast name) (end) (text)
    where
        start = filter (/=' ') (init (dropWhileEnd (/= '(') $ tail str))
        name = dropWhileEnd (/= ')') (dropWhileEnd (/= ':') (dropWhile (/= '(') str))
        end = filter (/=' ') (firstLast (dropWhileEnd (/= ':') $ dropWhile (/= ')') str))
        text = tail $ dropWhile (/= ':') str



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

-- if curly brace is in there
containsEndState:: String -> Bool  
containsEndState [] = False
containsEndState line
        | head line == '}' = True
        | otherwise = containsEndState (tail line)

findTransitions:: [String] -> [String]
findTransitions [] = []
findTransitions (x:xs)
        | ">" `Data.List.isPrefixOf` (dropWhile (== ' ') x) = x : (findTransitions xs)
        | otherwise = findTransitions xs

main :: IO ()
main = do
    let path = "../../description2/vending.machine"
    content <- Prelude.readFile path
    let linesOfFiles = lines content
    -- mapM_ Prelude.putStrLn linesOfFiles

    let states = findStart linesOfFiles
    -- Prelude.putStrLn $ show states

    let statesConv = createStates states
    
    Prelude.putStrLn $ show statesConv
    

    let transitions = findTransitions linesOfFiles
    print (typeOf transitions)
    Prelude.putStrLn $ show transitions

    let transConv = initTransition $ head transitions

    Prelude.putStrLn $ show transConv
