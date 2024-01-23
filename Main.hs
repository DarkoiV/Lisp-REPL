module Main where
import Sexpr
import Eval
import Parser

repl :: EvalCtx -> IO EvalCtx
repl ctx = do 
  input <- getLine
  let expresion = runParser sexprP input 
  case expresion of 
    Nothing      -> putStrLn "Invalid expresion" >> repl ctx 
    Just (exp,_) -> 
      let (res, ctx') = runEval (eval exp) ctx
      in putStrLn (show res) >> repl ctx'

main :: IO ()
main = do
  putStrLn "--------------------------------------"
  putStrLn "(entering  \n  (darkoiv-lisp-repl \n    (!!!welecome!!!)))"
  putStrLn "--------------------------------------\n"
  repl defaultCtx
  return ()
