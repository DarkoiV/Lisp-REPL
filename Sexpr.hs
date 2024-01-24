module Sexpr where
import Parser
import Control.Applicative
import Data.List (intercalate)

type Fun        = ([Val] -> Val)
data SExpr      = SExpr [Val]
type SymbolList = [(String, Val)]
data Val        = Nil 
                | Expr   SExpr 
                | Lambda Fun
                | Macro  (SExpr -> SExpr)
                | Number Float 
                | Val    Bool
                | Str    String 
                | Symbol String
                | Err    String

instance Show SExpr where 
  show (SExpr vals) = "(" ++ showVals ++ ")"
    where 
      showVals = intercalate " " $ map show vals

instance Show Val where 
  show (Nil)        = "Nil"
  show (Expr sexpr) = show sexpr 
  show (Lambda _)   = "λ->"
  show (Macro _)    = "???"
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

sexprP :: Parser SExpr 
sexprP = do
  charP '('
  ignorewsP
  vals <- many valP
  ignorewsP
  charP ')'
  return (SExpr vals)
