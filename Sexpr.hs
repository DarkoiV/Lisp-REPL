module Sexpr where
import Parser
import Control.Applicative
import Text.Read (readMaybe)

data Symbol = Add 
            | Sub
            | Mul 
            | Div
            | If
            | Symbol String

instance Show Symbol where 
  show (Add)      = "+"
  show (Sub)      = "-"
  show (Mul)      = "*"
  show (Div)      = "/"
  show (If)       = "if"
  show (Symbol s) = s

str2symbol :: String -> Symbol 
str2symbol "+"  = Add
str2symbol "-"  = Sub
str2symbol "*"  = Mul
str2symbol "/"  = Div
str2symbol "if" = If
str2symbol str  = Symbol str

data SExpr = SExpr Symbol [Val]

instance Show SExpr where 
  show (SExpr symbol vals) = "(" ++ show symbol ++ showVals ++ ")"
    where 
      showVals = concat $ map (\v -> " " ++ show v) vals

data Val = Nil | Expr SExpr | Number Float | Str String

instance Show Val where 
  show (Nil)        = "Nil"
  show (Expr sexpr) = show sexpr 
  show (Number n)   = show n
  show (Str s)      = show s

toNumber :: Val -> Maybe Float
toNumber (Number x) = Just x
toNumber (Expr e)   = toNumber $ eval e
toNumber (Str s)    = readMaybe s
toNumber _          = Nothing

fromNumber :: Maybe Float -> Val 
fromNumber (Nothing) = Nil
fromNumber (Just n)  = Number n

isTruthy :: Val -> Bool
isTruthy (Number 0) = False
isTruthy (Number _) = True
isTruthy (Nil)      = False
isTruthy (Expr e)   = isTruthy $ eval e
isTruthy _          = False

eval :: SExpr -> Val
eval (SExpr Add vals)   = fromNumber . (fmap sum) . sequence $ fmap toNumber vals  
eval (SExpr Sub vals)   = fromNumber $ fmap (foldl1 (-)) . sequence $ fmap toNumber vals
eval (SExpr Mul vals)   = fromNumber . (fmap product) . sequence $ fmap toNumber vals  
eval (SExpr Div vals)   = fromNumber $ fmap (foldl1 (/)) . sequence $ fmap toNumber vals

eval (SExpr If [i, t])    = eval $ SExpr If [i, t, Nil]
eval (SExpr If [i, t, e]) = if isTruthy i then t else e

eval _ = undefined

symbolP :: Parser Symbol
symbolP = fmap str2symbol $ stringP "+" 
      <|> stringP "-"
      <|> stringP "*"
      <|> stringP "/"
      <|> stringP "if"

valP :: Parser Val
valP = ignorewsP *> (sexpr <|> number <|> sliteral)
  where
    sexpr    = fmap Expr sexprP 
    number   = fmap Number numberP
    sliteral = fmap Str stringliteralP

sexprP :: Parser SExpr 
sexprP = do
  charP '('
  ignorewsP
  symbol <- symbolP 
  vals <- some valP 
  ignorewsP
  charP ')'
  return (SExpr symbol vals)
  

