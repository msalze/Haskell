import System.Process
import Text.Read (readMaybe)
import System.Posix

import Data.Typeable


main = do
    -- let result = readProcess "uname -a" [] []
    -- str <- result
    -- putStrLn str

    callCommand "uname -a"