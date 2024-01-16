module Main where
import Parser
import Control.Applicative

data Symbol = Add 
            | Sub
            | Mul 
            | Div 
            | Symbol String

str2symbol :: String -> Symbol 
str2symbol "Add" = Add
str2symbol str   = Symbol str

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

symbolP :: Parser Symbol
symbolP = fmap str2symbol $ stringP "Add" 
      <|> stringP "Sub"
      <|> stringP "Mul"
      <|> stringP "Div"

main :: IO ()
main = return ()
