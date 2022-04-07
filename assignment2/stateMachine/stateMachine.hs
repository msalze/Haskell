import Data.ByteString (elemIndex, isPrefixOf, putStrLn)
import Data.List
import Data.Char ( isSpace )
import qualified Control.Monad
import Data.Typeable
import System.Exit
import System.Environment

-- State definition and its functions
data StateTest = StateTest String TypeOfState [String] -- name type text
    deriving Show
data TypeOfState = Start | Middle | End
    deriving (Show, Eq)

getName:: StateTest -> String 
getName (StateTest name _ _) = name

getType:: StateTest -> TypeOfState
getType (StateTest _ t _) = t

getText:: StateTest -> [String]
getText (StateTest _ _ text) = text

printText:: StateTest -> IO()
printText state = do
    mapM_ Prelude.putStrLn $ getText state

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
findText [x] = cleaned : []
    where
        cleanedStart = (dropWhile (/= '{') $ x)
        cleaned = firstLast (dropWhileEnd (/= '}') $ cleanedStart)

findText str = ((tail cleaned) : (firstLast str)) ++ [(init cleanedEnd)]
    where 
        cleaned = (dropWhile (/= '{') $ head str)
        cleanedEnd = (dropWhileEnd (/= '}') $ last str)



extractStateName:: String -> String
extractStateName (x:xs) = cleanedEnd
    where
        cleanedStart = dropWhile (== '+') (dropWhile (== '*') xs)
        cleanedEnd = init (dropWhileEnd (/= '{') cleanedStart)

createStates:: [[String]] -> [StateTest]
createStates [] = []
createStates (x:xs) = initState x : createStates xs

-- Transition definition and its functions 
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
initTransition str = Transition (start) (name) (end) (text)
    where
        startAlt = keepUntil ':' $ tail str
        start = filter (/=' ') $ keepUntil '(' startAlt
        name = betweenBrackets startAlt
        end = filter (/=' ') $ dropUntil ')' startAlt
        text = tail $ dropWhile (/= ':') str

printTransitionText:: Transition -> IO()
printTransitionText transition = do
    Prelude.putStrLn $ getTransitionText transition

createTransitions:: [String] -> [Transition]
createTransitions [] = []
createTransitions (x:xs) = initTransition x : createTransitions xs

-- validation
validate:: [StateTest] -> [Transition] -> IO()
validate states [] = Prelude.putStrLn "Transitions valid"
validate states (x:xs) 
    | elem (getTransitionStart x) names && elem (getTransitionEnd x) names = validate states xs
    | otherwise = error "Invalid start or end state of transition"
    where 
        names = map getName states

hasStart:: [StateTest] -> IO()
hasStart states 
    | countStart states == 1 = Prelude.putStrLn "Start valid"
    | otherwise = error "Start states count not valid"

countStart:: [StateTest] -> Int
countStart [] = 0
countStart (x:xs) 
    | getType x == Start = 1 + countStart xs
    | otherwise = countStart xs

hasEnd:: [StateTest] -> IO()
hasEnd (x:xs) 
    | getType x == End = Prelude.putStrLn "End valid"
    | otherwise = hasEnd xs

-- util
firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

keepUntil:: Char -> [Char] -> [Char]
keepUntil ch [] = error "Error in definition"
keepUntil ch (x:xs) 
    | x == ch = []
    | otherwise = x : keepUntil ch xs

dropUntil:: Char -> [Char] -> [Char]
dropUntil ch [] = error "Error in definition"
dropUntil ch (x:xs) 
    | x == ch = xs
    | otherwise = dropUntil ch xs

betweenBrackets:: String -> String
betweenBrackets [] = []
betweenBrackets str
    | isInfixOf "(" str && isInfixOf ")" str = keepUntil ')' $ dropUntil '(' str
    | otherwise = error "Transition definition incorrect"

-- functions to read input
-- how many lines contain the state
findEnd:: [String] -> Int
findEnd [] = error "State Definition incorrect"
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


-- procedural function, interaction with user
getLineStr :: String -> [StateTest] -> [Transition] -> IO()
getLineStr origin states transitions = do
   line <- getLine
   let possible = map getTransitionName (filter (\n -> getTransitionStart n == origin) transitions)
   if elem line possible
       then do
           let currTransition = head (filter (\n -> getTransitionName n == line) transitions)
           printTransitionText currTransition
           let endStateName = getTransitionEnd currTransition
           let currState = head (filter (\n -> getName n == endStateName) states)
           printText currState
           case (getType currState) of
               End -> do exitSuccess
               _ -> getLineStr endStateName states transitions
       else print "Invalid input!" >> getLineStr origin states transitions

main :: IO ()
main = do
    -- (command:args) <- getArgs  
    -- print (typeOf (command:args))
    -- mapM_ Prelude.putStrLn (command:args)
    -- let path = (command:args) :: FilePath 
    -- if (command:args) == ""
    --     then Prelude.putStrLn "Please enter a path to the file" >> exitFailure
    --     else return()
    let path = "../../description2/invalid_syntax_error.machine"
    -- let path = "../../description2/car.machine"

    content <- Prelude.readFile path
    let linesOfFiles = lines content
    -- mapM_ Prelude.putStrLn linesOfFiles

    let states = findStart linesOfFiles
    -- Prelude.putStrLn $ show states

    let statesConv = createStates states
    Prelude.putStrLn $ show statesConv

    let transitions = findTransitions linesOfFiles
    -- Prelude.putStrLn $ show transitions

    let transConv = createTransitions transitions
    Prelude.putStrLn $ show transConv

    let names = map getName statesConv

    let boo1 =elem (getTransitionStart $ head transConv) names && elem (getTransitionEnd $ head transConv) names

    print $ show names
    print $ show boo1

    validate statesConv transConv
    hasStart statesConv
    hasEnd statesConv


    -- print $ show boo

    let start =  head $ filter (\n -> (getType n) == Start) statesConv 
    printText start

    getLineStr (getName start) statesConv transConv


-- TODO: check and assert that only one start exists
-- TODO: check and assert that an end exists