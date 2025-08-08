import System.Info
import Test.Tasty.Discover

main :: IO ()
main = do
  putStrLn $ "Current platform: " ++ os
  
  -- Test some expressions
  putStrLn $ "Test '!windows & !mingw32': " ++ show (evaluatePlatformExpression "!windows & !mingw32" os)
  putStrLn $ "Test 'darwin': " ++ show (evaluatePlatformExpression "darwin" os)
  putStrLn $ "Test 'linux': " ++ show (evaluatePlatformExpression "linux" os)
  putStrLn $ "Test 'unix': " ++ show (evaluatePlatformExpression "unix" os)
