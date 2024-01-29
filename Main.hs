module Main where
import Sexpr
import Eval
import Slib
import System.IO

readExpression :: Int -> IO String 
readExpression op = do 
  i <- getChar
  case i of 
    '('  -> (:) <$> pure '(' <*> readExpression (op + 1)
    ')'  -> (:) <$> pure ')' <*> readExpression (op - 1)
    '\n' -> if op <= 0 
      then return [] 
      else do
        let indent = take (op*2) $ repeat ' '
        putStr indent
        hFlush stdout
        (:) <$> pure ' ' <*> readExpression op
    c    -> (:) <$> pure c <*> readExpression op

repl :: Eval ()
repl = do 
  input <- doIO $ readExpression 0 
  let parsed = parseSExpr input 
  case parsed of 
    Just (expr, _) -> do 
      evalRes <- eval expr
      doIO $ putStrLn ("==> " ++ show evalRes)
    Nothing -> doIO $ putStrLn "Failed to parse expression"
  repl

main :: IO ()
main = do
  putStrLn "--------------------------------------"
  putStrLn "(entering  \n  (darkoiv-lisp-repl \n    (!!!welecome!!!)))"
  putStrLn "--------------------------------------\n"
  runEval repl standardlib
