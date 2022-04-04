import System.Process
import Text.Read (readMaybe)

main :: IO String
main = do
    -- let result = readProcess "/usr/bin/unam" [] "uname -a" 
    -- putStrLn "hello"
    -- let stri = readMaybe result
    -- putStrLn result
    readProcess "" [] "uname -a"


-- other :: IO ()
-- other = do
--   let stdin' = "uname -a"
--   (errCode, stdout', stderr') <- readProcessWithExitCode "echo" ["hi"] stdin'
--   putStrLn $ "stdout: " ++ stdout'
--   putStrLn $ "stderr: " ++ stderr'
--   putStrLn $ "errCode: " ++ show errCode

-- main :: IO ()
-- main = do
--   let stdin' = ""
--   (errCode, stdout', stderr') <- readProcessWithExitCode "echo" ["hi"] stdin'
--   putStrLn $ "stdout: " ++ stdout'
--   putStrLn $ "stderr: " ++ stderr'
--   putStrLn $ "errCode: " ++ show errCode