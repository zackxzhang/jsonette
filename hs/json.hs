-- json parsing in haskell --
-- a top-down, combinator approach --


{-# LANGUAGE LambdaCase, MultilineStrings #-}
module JSON where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (replicateM)
import Data.Char (isDigit, isHexDigit, isSpace, digitToInt, chr)
import Data.Functor (($>))
import Data.List (intercalate)


-- json structure
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
        JObject o     -> "{" ++ intercalate ", " (map showKV o) ++ "}"
        where
            showKV (k, v) = show k ++ ": " ++ show v


-- generic parser
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

digit' :: Parser String Int
digit' = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

hexDigit :: Parser String Int
hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> [Int] -> Integer
digitsToNumber base = foldl step 0
    where step number digit = number * fromIntegral base + fromIntegral digit


---- applicative :: pipelining
instance Applicative (Parser i) where
    pure x    = Parser $ \i -> Just (i, x)
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
            -- replicateM :: Applicative m => Int -> m a -> m [a]
            -- (*>) :: Applicative f => f a -> f b -> f b
            -- u *> v = (id <$ u) <*> v


---- alternative :: branching
instance Alternative (Parser i) where
    empty = Parser $ const empty
    p1 <|> p2 = Parser $ \i -> runParser p1 i <|> runParser p2 i

jNull :: Parser String JValue
jNull = string "null" $> JNull
    -- ($>) :: Functor f => f a -> b -> f b
    -- x $> y = y <$ x

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


---- monad :: conditioning
instance Monad (Parser i) where
    p >>= f = Parser $ \i -> case runParser p i of
        Nothing        -> Nothing
        Just (rest, o) -> runParser (f o) rest

jString :: Parser String JValue
jString = JString <$> (char '"' *> jString')
    where
        jString' = do
            -- optional :: Alternative f => f a -> f (Maybe a)
            -- optional p = Just <$> p <|> pure Nothing
            o <- optional jChar
            case o of
                -- (<$) :: Functor f => a -> f b -> f a
                -- (<$) = fmap . const
                Nothing -> ""   <$  char '"'
                Just c  -> (c:) <$> jString'


---- parser combinators for numbers
digits :: Parser String [Int]
digits = some digit
    -- some :: Alternative f => f a -> f [a]
    -- some v = (:) <$> v <*> many v
    -- many :: Alternative f => f a -> f [a]
    -- many v = some v <|> pure []

jUInt :: Parser String Integer
jUInt = (\d ds -> digitsToNumber 10 (d:ds)) <$> digit' <*> digits
    <|> fromIntegral <$> digit

sign :: Maybe Char -> Integer -> Integer
sign (Just '-') i = negate i
sign _          i = i

jSInt :: Parser String Integer
jSInt = sign <$> optional (char '-') <*> jUInt

jFrac :: Parser String Integer
jFrac = char '.' *> (fromIntegral . digitsToNumber 10 <$> digits)

jExp :: Parser String Integer
jExp = (char 'e' <|> char 'E')
    *> (sign <$> optional (char '+' <|> char '-') <*> jUInt)

jInt :: Parser String JValue
jInt = JNumber <$> jSInt <*> pure 0 <*> pure 0

jIntExp :: Parser String JValue
jIntExp = JNumber <$> jSInt <*> pure 0 <*> jExp

jIntFrac :: Parser String JValue
jIntFrac = JNumber <$> jSInt <*> jFrac <*> pure 0

jIntFracExp :: Parser String JValue
jIntFracExp = JNumber <$> jSInt <*> jFrac <*> jExp

jNumber :: Parser String JValue
jNumber = jIntFracExp <|> jIntExp <|> jIntFrac <|> jInt


---- parser combinators for arrays, objects, values
surroundedBy :: Parser String a -> Parser String b -> Parser String a
surroundedBy v w = w *> v <* w

separatedBy :: Parser i v -> Parser i s -> Parser i [v]
separatedBy v s =   (:) <$> v <*> many (s *> v) <|> pure []

spaces :: Parser String String
spaces = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t')

jArray :: Parser String JValue
jArray = JArray <$>
    (
        char '['
     *> jValue `separatedBy` char ',' `surroundedBy` spaces
     <* char ']'
    )

jObject :: Parser String JValue
jObject = JObject <$>
    (
        char '{'
     *> pair `separatedBy` char ',' `surroundedBy` spaces
     <* char '}'
    )
    where
        pair = (\ ~(JString s) j -> (s, j))
           <$> (jString `surroundedBy` spaces)
           <*  char ':'
           <*> jValue

jValue :: Parser String JValue
jValue = jValue' `surroundedBy` spaces
  where
    jValue' = jNull
          <|> jBool
          <|> jString
          <|> jNumber
          <|> jArray
          <|> jObject


-- json parser
parse :: String -> Maybe JValue
parse s = case runParser jValue s of
    Just ("", j) -> Just j
    _            -> Nothing


-- test
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
    print (runParser jNumber "0")
    print (runParser jNumber "5")
    print (runParser jNumber "5 1")
    print (runParser jNumber "+5")
    print (runParser jNumber "12.40")
    print (runParser jNumber "-3.2e5 ")
    print (runParser jArray "[null,  [ \"hello\" ,   {\"json\": 1.2e-3}  ] ] ")
    print (runParser jObject "{\"a\":1,\"bcd\":   {  \"xml\": [null, -2.3] }}")
    -- integration test
    print (parse "  -3.2e5 ")
    print (parse "[\"Mon\",\"Tue\",\"Wed\", \"Thu\",\"Fri\", \"Sat\",\"Sun\"]")
    print (parse """
        {
            \"employee\": {
                \"name\":       \"\\u03A9 Smith\",
                \"salary\":     56000,
                \"married\":    true
            }
        }
    """)
