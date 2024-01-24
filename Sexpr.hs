module Sexpr where
import Parser
import Control.Applicative
import Data.List (intercalate)

type Fun        = ([Val] -> Val)
type SymbolList = [(String, Val)]
data Val        = Nil 
                | Expr   [Val] 
                | Lambda Fun
                | Number Float 
                | Val    Bool
                | Str    String 
                | Symbol String
                | Err    String

instance Show Val where 
  show (Expr vals) = "(" ++ showVals ++ ")"
    where 
      showVals = intercalate " " $ map show vals
  show (Nil)        = "Nil"
  show (Lambda _)   = "λ"
  show (Number n)   = show n
  show (Str s)      = show s
  show (Symbol s)   = s
  show (Err s)      = "!! ERR: " ++ s ++ " !!"

valP :: Parser Val
valP = ignorewsP *> (sexpr <|> number <|> sliteral <|> symbol)
  where
    sexpr    = fmap Expr   sexprP
    number   = fmap Number numberP
    sliteral = fmap Str    stringliteralP
    symbol   = fmap Symbol tokenP

sexprP :: Parser [Val] 
sexprP = do
  charP '('
  ignorewsP
  vals <- many valP
  ignorewsP
  charP ')'
  return vals
