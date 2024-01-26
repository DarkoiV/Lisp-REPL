module Main where
import Sexpr
import Eval
import Slib

repl :: Eval ()
repl = do 
  input <- doIO getLine 
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
