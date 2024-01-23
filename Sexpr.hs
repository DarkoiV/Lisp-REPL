module Sexpr where
import Parser
import Control.Applicative

data Symbol     = Symbol String
data SExpr      = SExpr [Val]
type SymbolList = [(String, Val)]
data Val        = Nil 
                | Expr   SExpr 
                | Number Float 
                | Str    String 
                | Sym    Symbol
                | Err    String

instance Show Symbol where 
  show (Symbol s) = s

instance Show SExpr where 
  show (SExpr vals) = "(" ++ showVals ++ ")"
    where 
      showVals = concat $ map (\v -> " " ++ show v) vals

instance Show Val where 
  show (Nil)        = "Nil"
  show (Expr sexpr) = show sexpr 
  show (Number n)   = show n
  show (Str s)      = show s
  show (Sym s)      = show s
  show (Err s)      = "!! ERR: " ++ show s

symbolP :: SymbolList -> Parser Symbol
symbolP sl = pure Symbol <*> (foldr (<|>) empty $ map (stringP . fst) sl)

valP :: SymbolList -> Parser Val
valP sl = ignorewsP *> (sexpr <|> number <|> sliteral <|> symbol)
  where
    sexpr    = fmap Expr   (sexprP sl)
    number   = fmap Number numberP
    sliteral = fmap Str    stringliteralP
    symbol   = fmap Sym    (symbolP sl)

sexprP :: SymbolList -> Parser SExpr 
sexprP sl = do
  charP '('
  ignorewsP
  vals   <- many $ valP sl
  ignorewsP
  charP ')'
  return (SExpr vals)
