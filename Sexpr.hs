module Sexpr where
import Parser
import Control.Applicative
import Text.Read (readMaybe)
import Data.List (find)

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

data SExpr      = SExpr Symbol [Val]
type SymbolList = [(String, Val)]
data Val        = Nil 
                | Expr SExpr 
                | Number Float 
                | Str String 
                | Sym Symbol

instance Show SExpr where 
  show (SExpr symbol vals) = "(" ++ show symbol ++ showVals ++ ")"
    where 
      showVals = concat $ map (\v -> " " ++ show v) vals

instance Show Val where 
  show (Nil)        = "Nil"
  show (Expr sexpr) = show sexpr 
  show (Number n)   = show n
  show (Str s)      = show s
  show (Sym s)      = show s

-- EVAL --------------------------------------------------  
data EvalCtx = EvalCtx { symbols :: SymbolList }
data Eval a  = Eval { runEval :: EvalCtx -> (a, EvalCtx) }

instance Functor Eval where 
  fmap f (Eval eval) = Eval $ \ctx -> 
    let (res, ctx') = eval ctx 
    in (f res, ctx')

instance Applicative Eval where
  pure x = Eval $ \ctx -> (x, ctx)
  (Eval ef) <*> (Eval ev) = Eval $ \ctx -> 
    let (f, ctx')  = ef ctx 
        (v, ctx'') = ev ctx' 
    in (f v, ctx'') 

instance Monad Eval where
  (Eval eval) >>= f = Eval $ \ctx -> 
    let (v, ctx') = eval ctx 
    in runEval (f v) ctx'

fromSymbol :: SymbolList -> String -> Val 
fromSymbol sl sym = let found = find (\(s,_) -> s == sym) sl in case found of
  Just spair -> snd spair
  Nothing    -> Nil

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

symbolP :: SymbolList -> Parser Symbol
symbolP sl = fmap str2symbol $ stringP "+" 
         <|> stringP "-"
         <|> stringP "*"
         <|> stringP "/"
         <|> stringP "if"

valP :: SymbolList -> Parser Val
valP sl = ignorewsP *> (sexpr <|> number <|> sliteral)
  where
    sexpr    = fmap Expr sexprP 
    number   = fmap Number numberP
    sliteral = fmap Str stringliteralP

sexprP :: Parser SExpr 
sexprP = do
  charP '('
  ignorewsP
  symbol <- symbolP [] 
  vals   <- some $ valP []
  ignorewsP
  charP ')'
  return (SExpr symbol vals)

eval :: SExpr -> Val
eval (SExpr Add vals)   = fromNumber . (fmap sum) . sequence $ fmap toNumber vals  
eval (SExpr Sub vals)   = fromNumber $ fmap (foldl1 (-)) . sequence $ fmap toNumber vals
eval (SExpr Mul vals)   = fromNumber . (fmap product) . sequence $ fmap toNumber vals  
eval (SExpr Div vals)   = fromNumber $ fmap (foldl1 (/)) . sequence $ fmap toNumber vals

eval (SExpr If [i, t])    = eval $ SExpr If [i, t, Nil]
eval (SExpr If [i, t, e]) = if isTruthy i then t else e

eval _ = undefined
