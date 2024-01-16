module Main where
import Parser
import Control.Applicative

data Symbol = Add 
            | Sub
            | Mul 
            | Div 
            | Symbol String

str2symbol :: String -> Symbol 
str2symbol "+" = Add
str2symbol "-" = Sub
str2symbol "*" = Mul
str2symbol "/" = Div
str2symbol str = Symbol str

data SExpr = SExpr Symbol [Val]
data Val = Nil | Expr SExpr | Number Float

toNumber :: Val -> Maybe Float
toNumber (Number x) = Just x
toNumber _          = Nothing

fromNumber :: Maybe Float -> Val 
fromNumber (Nothing) = Nil
fromNumber (Just n)  = Number n

eval :: SExpr -> Val
eval (SExpr Add vals) = fromNumber . (fmap sum) . sequence $ fmap toNumber vals  
eval (SExpr Sub vals) 
  | length vals == 2 = fromNumber $ fmap (foldr (-) 0) . sequence $ fmap toNumber vals
  | otherwise        = Nil
eval (SExpr Mul vals) = fromNumber . (fmap product) . sequence $ fmap toNumber vals  
eval (SExpr Div vals) 
  | length vals == 2 = fromNumber $ fmap (foldr (/) 0) . sequence $ fmap toNumber vals
  | otherwise        = Nil
eval _ = undefined

symbolP :: Parser Symbol
symbolP = fmap str2symbol $ stringP "Add" 
      <|> stringP "Sub"
      <|> stringP "Mul"
      <|> stringP "Div"

valP :: Parser Val
valP = sexpr <|> number <|> nil
  where
    sexpr  = fmap Expr sexprP 
    number = fmap Number (numberP) 
    nil    = pure Nil

sexprP :: Parser SExpr 
sexprP = do
  charP '('
  ignorewsP
  symbol <- symbolP 
  vals <- some valP 
  ignorewsP
  charP ')'
  return (SExpr symbol vals)
  

main :: IO ()
main = return ()
