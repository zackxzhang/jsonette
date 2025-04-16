-- json parsing in haskell --

{-# LANGUAGE LambdaCase #-}
module JSON where

import Control.Applicative (Alternative(..), optional)
import Data.Char (isDigit, isHexDigit, isSpace, digitToInt, chr, ord)
import Data.Functor (($>))
import Data.List (intercalate)


-- json
data JValue = JNull
            | JBool Bool
            | JString String
            | JNumber { int :: Integer, frac :: Integer, expo :: Integer }
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq)

instance Show JValue where
    show jvalue = case jvalue of
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


-- parser
newtype Parser i o = Parser { runParser :: i -> Maybe (i, o) }


---- functor: post-processing output
instance Functor (Parser i) where
    fmap fo p = Parser $ fmap (fmap fo) . runParser p

beginWith :: (a -> Bool) -> Parser [a] a
beginWith predicate = Parser $ \case
    (x:xs) | predicate x -> Just (xs, x)
    _                    -> Nothing

digit :: Parser String Int
digit = digitToInt <$> beginWith isDigit


---- applicative: pipelining head and tail
instance Applicative (Parser i) where
    pure x    = Parser $ pure . (, x)
    ph <*> pt = Parser $ \i -> case runParser ph i of
        Nothing        -> Nothing
        Just (rest, fh) -> fmap fh <$> runParser pt rest

char :: Char -> Parser String Char
char c = beginWith (==c)

string :: String -> Parser String String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

jNull :: Parser String JValue
jNull = string "null" $> JNull


---- alternative: switching between possibilities
instance Alternative (Parser i) where
    empty = Parser $ const empty
    p1 <|> p2 = Parser $ \i -> runParser p1 i <|> runParser p2 i

jBool :: Parser String JValue
jBool = string "true"  $> JBool True
    <|> string "false" $> JBool False


-- jString :: Parser String JValue

escape :: Char -> Maybe Char
escape c = case c of
    '\\' -> Just '\\'
    '"'  -> Just '"'
    '/'  -> Just '/'
    'b'  -> Just '\b'
    'f'  -> Just '\f'
    'n'  -> Just '\n'
    'r'  -> Just '\r'
    't'  -> Just '\t'
    _    -> Nothing

-- jNumber :: Parser String JValue

-- jArray :: Parser String JValue

-- jObject :: Parser String JValue

-- jValue :: Parser String JValue


main :: IO ()
main = do
    -- unit test
    print (runParser jNull "null")
    print (runParser jNull "dull")
    print (runParser jBool "true")
    print (runParser jBool "false")
    print (runParser jBool "truth")
    print (runParser jBool "falsehood")
    -- integration test
