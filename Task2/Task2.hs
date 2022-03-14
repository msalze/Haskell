import System.Exit
import Data.Char
import Data.Time.Calendar
import Data.Maybe
import Data.Typeable
import Text.Read

checkLeapYear :: Bool -> Integer -> Integer
checkLeapYear leap diff = if (leap && diff > 168) then diff -1 else diff -- 168 = 28 * 6, halbes Jahr vorbei, 168 nicht doppelt, da Programm zuerst abgebrochen wird

checkSpecialDay :: Bool -> Integer -> String
checkSpecialDay leap dayOfYear 
   | leap && dayOfYear == 168     = "\"Leap Day\""
   | leap && dayOfYear == 365     = "\"Year Day\""
   | not leap && dayOfYear == 364 = "\"Year Day\""
   | otherwise                    = ""

toMonth :: Int -> String
toMonth x = case x of
   0 -> "January"
   1 -> "February"
   2 -> "March"
   3 -> "April"
   4 -> "May"
   5 -> "June"
   6 -> "Sol"
   7 -> "July"
   8 -> "August"
   9 -> "September"
   10 -> "October"
   11 -> "November"
   12 -> "December"
   13 -> "Year Day"
   otherwise -> "error"

toWeekday :: Integer -> String
toWeekday x = case mod x 7 of
   0 -> "Sunday"
   1 -> "Monday"
   2 -> "Tuesday"
   3 -> "Wednesday"
   4 -> "Thursday"
   5 -> "Friday"
   6 -> "Saturday"
   otherwise -> "error"

beautifyDay :: Integer -> String
beautifyDay x = if x < 10 then "0" ++ show x else show x

getLineInt :: IO Int
getLineInt = do
   line <- getLine
   case (readMaybe line) of
      Just x -> return x 
      Nothing -> getLineInt

main = do
   putStrLn "Please enter three positive integer numbers (year month day) separated by one or more blank spaces or type quit."
   input1 <- getLine
   if input1 == "quit"
      then do exitSuccess
      else return ()

   test1 <- getLineInt
   test2 <- getLineInt

   let yearInt = (read input1 :: Integer)
   let month = test1
   let day = test2

   let inputDate = fromGregorianValid yearInt month day
   
   let prev = fromGregorian yearInt 1 1

   let diff = diffDays (fromJust inputDate) prev

   let leapYear = isLeapYear yearInt

   let specialDay = checkSpecialDay leapYear diff -- check with day number before changing for leap

   let dayOfYear = checkLeapYear leapYear diff

   if specialDay /= ""
      then do 
         putStrLn specialDay
         exitSuccess
      else return ()

   let month = fromInteger (div dayOfYear 28)

   let day = mod dayOfYear 28

   let result = input1 ++ (" " ++ (toMonth month)) ++ " " ++ beautifyDay (day + 1) ++ " (" ++ (toWeekday day) ++")"

   putStrLn result
