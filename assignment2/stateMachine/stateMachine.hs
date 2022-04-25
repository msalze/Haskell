import Data.ByteString (elemIndex, isPrefixOf, putStrLn)
import Data.List
import Data.Char ( isSpace, isDigit )
import qualified Control.Monad
import Data.Typeable
import System.Exit
import System.Environment
import System.Process
import Control.Time

-- State definition and its functions
data StateTest = StateTest String TypeOfState [String] Float -- name type text time
    deriving Show
data TypeOfState = Start | Middle | End
    deriving (Show, Eq)

getName:: StateTest -> String 
getName (StateTest name _ _ _) = name

getType:: StateTest -> TypeOfState
getType (StateTest _ t _ _) = t

getText:: StateTest -> [String]
getText (StateTest _ _ text _) = text

getTime:: StateTest -> Float
getTime (StateTest _ _ _ time) = time

printText:: StateTest -> IO()
printText state = do
    mapM_ Prelude.putStrLn $ getText state

-- creates a StateTest from 
initState:: [String] -> StateTest
initState str = StateTest (extractStateName $ head str) (findType $ head str) (findText str) (extractTime $ head str)

-- returns the type that the state definition has
findType:: String -> TypeOfState
findType (x:xs)
    | elem '*' relevant = Start
    | elem '+' relevant = End
    | otherwise = Middle
    where 
        relevant = init (dropWhileEnd (/= '{') xs)

-- extracts the text that should be printed
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
        cleanedStart = dropWhile (== '!') ( dropWhile (== '+') (dropWhile (== '*') (dropWhile (== '!') xs)))
        cleanedEnd = dropWhileEnd (isDigit) (init (dropWhileEnd (/= '{') cleanedStart))

extractTime:: String -> Float
extractTime (x:xs) 
    | isTimed = (read cleaned) / 1000 :: Float
    | otherwise = -1
    where
        isTimed = "!" `Data.List.isPrefixOf` (dropWhile (== '+') (dropWhile (== '*') xs))
        -- cleanedStart = dropWhile (/isDigit) xs
        cleaned = filter (`elem` ['0'..'9']) (init (dropWhileEnd (/= '{') xs))

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
validate states [] = Prelude.putStrLn ""
validate states (x:xs) 
    | elem (getTransitionStart x) names && elem (getTransitionEnd x) names = validate states xs
    | otherwise = error "Invalid start or end state of transition"
    where 
        names = map getName states

validateAuto:: [StateTest] -> [Transition] -> IO()
validateAuto [] transitions = Prelude.putStrLn ""
validateAuto (x:xs) transitions
    | getTime x == -1.0 = validateAuto xs transitions
    | onlyOne x transitions /= 1 = error "Invalid auto forward state" 
    | otherwise = validateAuto xs transitions

onlyOne:: StateTest -> [Transition] -> Int
onlyOne state [] = 0
onlyOne state (x:xs) 
    | (getTransitionStart x) == name = 1 + onlyOne state xs
    | otherwise = onlyOne state xs
    where 
        name = getName state

validateTiming:: [[String]] -> IO()
validateTiming [] = Prelude.putStrLn ""
validateTiming (x:xs) 
    | isTimed && cleaned == "" = error "Timing has to be set"
    | otherwise = validateTiming xs
    where
        relevant = init (dropWhileEnd (/= '{') $ tail $ head x)
        isTimed = elem '!' relevant
        cleaned = filter (`elem` ['0'..'9']) relevant


hasStart:: [StateTest] -> IO()
hasStart states 
    | countStart states == 1 = Prelude.putStrLn ""
    | otherwise = error "Start states count not valid"

countStart:: [StateTest] -> Int
countStart [] = 0
countStart (x:xs) 
    | getType x == Start = 1 + countStart xs
    | otherwise = countStart xs

hasEnd:: [StateTest] -> IO()
hasEnd (x:xs) 
    | getType x == End = Prelude.putStrLn ""
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
    | isInfixOf "(" str && isInfixOf ")" str = dropWhileEnd (==' ') $ dropWhile (==' ') (keepUntil ')' $ dropUntil '(' str)
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

-- find transition definitions in input
findTransitions:: [String] -> [String]
findTransitions [] = []
findTransitions (x:xs)
        | ">" `Data.List.isPrefixOf` (dropWhile (== ' ') x) = x : (findTransitions xs)
        | otherwise = findTransitions xs


-- procedural function, interaction with user
getLineStr :: StateTest -> [StateTest] -> [Transition] -> IO()
getLineStr origin states transitions = do
    if getTime origin == -1
        then do
            line <- getLine
            let possible = map getTransitionName (filter (\n -> getTransitionStart n == (getName origin)) transitions)
            let possibleWildcards = (filter (\n -> (init n) `Data.List.isPrefixOf` line) possible)
            if elem line possible
                then do
                    let currTransition = head (filter (\n -> getTransitionName n == line) transitions)
                    doTransition currTransition states transitions
            else
                if length possibleWildcards >= 1
                    then do 
                        let longestWildcard = longest possibleWildcards
                        let currTransition = head (filter (\n -> getTransitionName n == longestWildcard) transitions)
                        doTransition currTransition states transitions
                else print "Invalid input!" >> getLineStr origin states transitions
        else do
            delay $ getTime origin 
            let possible = (filter (\n -> getTransitionStart n == (getName origin)) transitions)
            doTransition (head possible) states transitions

doTransition :: Transition -> [StateTest] -> [Transition] -> IO()
doTransition currTrans states transitions =
    do
        callCommand "clear"
        printTransitionText currTrans
        let endStateName = getTransitionEnd currTrans
        let currState = head (filter (\n -> getName n == endStateName) states)
        printText currState
        case (getType currState) of
            End -> do exitSuccess
            _ -> getLineStr currState states transitions

timedTransition :: StateTest -> IO()
timedTransition state = do
    delay $ getTime state 


longest :: [String] -> String
longest xss = snd $ maximum $ [(length xs, xs) | xs <- xss]


main :: IO ()
main = do
    -- let path = "../../description3/doggo.machine"
    let path = "../../description3/rolling.machine"
    
    content <- Prelude.readFile path
    let linesOfFiles = lines content
    -- mapM_ Prelude.putStrLn linesOfFiles

    let states = findStart linesOfFiles
    -- Prelude.putStrLn $ show states

    validateTiming states

    let statesConv = createStates states
    Prelude.putStrLn $ show statesConv

    let transitions = findTransitions linesOfFiles
    -- Prelude.putStrLn $ show transitions

    let transConv = createTransitions transitions
    -- Prelude.putStrLn $ show transConv

    validate statesConv transConv
    validateAuto statesConv transConv
    hasStart statesConv
    hasEnd statesConv

    callCommand "clear"
    let start =  head $ filter (\n -> (getType n) == Start) statesConv 
    printText start

    getLineStr start statesConv transConv