module Main where
import Sexpr
import Parser

repl :: IO ()
repl = do
  line <- getLine
  let expr = runParser sexprP line
  case expr of 
    Just res -> putStrLn $ show . eval $ fst res
    Nothing  -> putStrLn $ "Failed to parse input"
  repl

main :: IO ()
main = do
  putStrLn "(simple-lisp-repl)"
  repl
