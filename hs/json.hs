-- json parsing in haskell --
-- a top-down, combinator approach --


{-# LANGUAGE LambdaCase #-}
module JSON where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Char (isDigit, isHexDigit, isSpace, digitToInt, chr)
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


---- functor :: post-processing
instance Functor (Parser i) where
    fmap fo p = Parser $ fmap (fmap fo) . runParser p

satisfy :: (a -> Bool) -> Parser [a] a
satisfy predicate = Parser $ \case
    (x:xs) | predicate x -> Just (xs, x)
    _                    -> Nothing

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

hexDigit :: Parser String Int
hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> [Int] -> Integer
digitsToNumber base = foldl step 0
    where step number digit = number * fromIntegral base + fromIntegral digit


---- applicative :: pipelining
instance Applicative (Parser i) where
    pure x    = Parser $ pure . (, x)
    ph <*> pt = Parser $ \i -> case runParser ph i of
        Nothing        -> Nothing
        Just (rest, fh) -> fmap fh <$> runParser pt rest

char :: Char -> Parser String Char
char c = satisfy (==c)

string :: String -> Parser String String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

unicode :: Parser String Char
unicode = chr . fromIntegral . digitsToNumber 16
            <$> (string "\\u" *> replicateM 4 hexDigit)


---- alternative :: branching
instance Alternative (Parser i) where
    empty = Parser $ const empty
    p1 <|> p2 = Parser $ \i -> runParser p1 i <|> runParser p2 i

jNull :: Parser String JValue
jNull = string "null" $> JNull

jBool :: Parser String JValue
jBool = string "true"  $> JBool True
    <|> string "false" $> JBool False

jChar :: Parser String Char
jChar = string "\\\"" $> '"'
    <|> string "\\\\" $> '\\'
    <|> string "\\/"  $> '/'
    <|> string "\\b"  $> '\b'
    <|> string "\\f"  $> '\f'
    <|> string "\\n"  $> '\n'
    <|> string "\\r"  $> '\r'
    <|> string "\\t"  $> '\t'
    <|> unicode
    <|> satisfy (\c -> not (c == '\"' || c == '\\'))


---- monad :: sequencing
instance Monad (Parser i) where
    p >>= f = Parser $ \i -> case runParser p i of
        Nothing        -> Nothing
        Just (rest, o) -> runParser (f o) rest

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString')
    where
        jString' = do
            firstChar <- optional jChar
            case firstChar of
                Nothing -> "" <$ char '"'
                Just c  -> (c:) <$> jString'

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
    print (runParser unicode "\\u03A9")
    print (runParser jString "\"little\"")
    print (runParser jString "\"little")
    print (runParser jString "\"big\", 0.9")
    print (runParser jString "\"backslash is \\\\\"")
    -- integration test
