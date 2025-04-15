-- json parsing in haskell --


import Control.Applicative (Alternative(..), optional)
import Data.List (intercalate)


data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: Integer, expo :: Integer }
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq)


instance Show JValue where
    show value = case value of
        JNull         -> "null"
        JBool   True  -> "true"
        JBool   False -> "false"
        JString s     -> show s
        JNumber i 0 0 -> show i
        JNumber i f 0 -> show i ++ "." ++ show f
        JNumber i 0 e -> show i ++ "e" ++ show e
        JNumber i f e -> show i ++ "." ++ show f ++ "e" ++ show e
        JArray  a     -> "[" ++ intercalate ", " (map show a) ++ "]"
        JObject o     -> "[" ++ intercalate ", " (map showKV o) ++ "]"
        where
            showKV (k, v) = show k ++ ": " ++ show v


escape :: Char -> Char
escape c = case c of
    '\\' -> '\\'
    '"'  -> '"'
    '/'  -> '/'
    'b'  -> '\b'
    'f'  -> '\f'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    _    ->  c


newtype Parser i o = Parser { parse :: i -> Maybe (i, o) }

-- lookAhead :: Parser String Char

-- jNull :: Parser String JValue

-- jBool :: Parser String JValue

-- jString :: Parser String JValue

-- jNumber :: Parser String JValue

-- jArray :: Parser String JValue

-- jObject :: Parser String JValue

-- jValue :: Parser String JValue


main :: IO ()
main = putStrLn "Parsing JSON from scratch in Haskell!"

