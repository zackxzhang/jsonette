-- json parsing in haskell --


import Control.Applicative (Alternative(..), optional)


data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: Integer, expo :: Integer }
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Show)

-- instance Show JValue where

newtype Parser i o = Parser { runParser :: i -> Maybe (i, o) }

-- lookahead :: Parser String Char

-- jNull :: Parser String JValue

-- jBool :: Parser String JValue

-- jString :: Parser String JValue

-- jNumber :: Parser String JValue

-- jArray :: Parser String JValue

-- jObject :: Parser String JValue

-- jValue :: Parser String JValue


main :: IO ()
main = putStrLn "Parsing JSON from scratch in Haskell!"

