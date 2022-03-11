import System.Exit
import Data.Char
import Data.Time.Calendar
import Data.Maybe
import Data.Typeable

-- printMonth :: Int -> Int
-- printMonth days = 28
--    where 

checkLeapYear :: Bool -> Integer -> Integer
checkLeapYear leap diff = if (leap && diff > 168) then diff -1 else diff -- 168 = 28 * 6, halbes Jahr vorbei

checkSpecialDay :: Bool -> Integer -> String
-- checkSpecialDay leap dayOfYear = if (leap && dayOfYear == 168) then "Leap Day" else if (leap && dayOfYear == 365 || (not leap && dayOfYear == 364 ) then "Year Day"
checkSpecialDay leap dayOfYear 
   | leap && dayOfYear == 168     = "Leap Day"
   | leap && dayOfYear == 365     = "Year Day1"
   | not leap && dayOfYear == 364 = "Year Day2"
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

main = do
   putStrLn "Please enter three positive integer numbers (year month day) separated by one or more blank spaces or type quit."
   input1 <- getLine
   if input1 == "quit"
      then do exitSuccess
      else return ()
   input2 <- getLine -- todo: empty lines in between are allowed!
   input3 <- getLine

   let yearInt = (read input1 :: Integer)
   let month = (read input2 :: Int)
   let day = (read input3 :: Int)

   let inputDate = fromGregorianValid yearInt month day
   
   let prev = fromGregorian yearInt 1 1

   let diff = diffDays (fromJust inputDate) prev

   let leapYear = isLeapYear yearInt

   let specialDay = (checkSpecialDay leapYear diff) -- check with day number before changing for leap

   putStrLn specialDay

   let dayOfYear = checkLeapYear leapYear diff

   print dayOfYear 

   if specialDay /= ""
      then do 
         putStrLn specialDay
         exitSuccess
      else return ()

   let month = fromInteger (div dayOfYear 28)
   -- let month = div dayOfYear 28

   let day = mod dayOfYear 28

   print month
   print (toMonth month)
   print day
   putStrLn (toWeekday day)

   let result = input1 ++ (" " ++ (toMonth month)) ++ " " ++ show (day + 1) ++ " (" ++ (toWeekday day) ++")"

   putStrLn result
