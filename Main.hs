module Main where
import Sexpr
import Eval

repl :: Eval ()
repl = do 
  input <- doIO getLine 
  repl

main :: IO ()
main = do
  putStrLn "--------------------------------------"
  putStrLn "(entering  \n  (darkoiv-lisp-repl \n    (!!!welecome!!!)))"
  putStrLn "--------------------------------------\n"
  startEval repl
  return ()
