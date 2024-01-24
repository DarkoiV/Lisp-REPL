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
    Just (exp,_) -> doExpr ctx exp

doExpr :: EvalCtx -> SExpr -> IO EvalCtx
doExpr ctx exp = let (res, ctx') = runEval (eval exp) ctx in 
  case res of 
    Expr exp' -> putStrLn (" -> " ++ (show res)) >> doExpr ctx' exp'
    _         -> putStrLn (show res)             >> repl ctx
  

main :: IO ()
main = do
  putStrLn "--------------------------------------"
  putStrLn "(entering  \n  (darkoiv-lisp-repl \n    (!!!welecome!!!)))"
  putStrLn "--------------------------------------\n"
  repl defaultCtx
  return ()
