{-# LANGUAGE LambdaCase #-}

module Parser where
    import Data.Char
    import Token
    import Control.Monad ( MonadPlus(..), ap, liftM )
    import Control.Applicative ( Alternative(empty, (<|>)) )


    newtype Parser  a  = Parser { parse :: String -> [(a, String)]}

    runParser :: Parser a -> String -> a
    runParser m s =
        case parse m s of
            [(res, [])] -> res
            [(_, rs)]   -> error "Parser did not consume entire stream"
            _           -> error "Parser Error"

    item :: Parser Char
    item = Parser $ \case
            []     -> []
            (c:cs) -> [(c,cs)]

    bind :: Parser a -> (a-> Parser b) -> Parser b
    bind p f = Parser $ \s -> concatMap (\(a,s') -> parse (f a) s') $ parse p s

    unit :: a -> Parser a
    unit a = Parser $ \s -> [(a,s)]

    instance Functor Parser where
        fmap = liftM

    instance Applicative Parser where
        pure    = unit
        (<*>)   = ap

    instance MonadPlus Parser where
        mzero = failure
        mplus = combine

    instance Alternative Parser where
        empty = mzero
        (<|>) = option

    instance Monad Parser where
        (>>=) = bind
        return = unit

    combine :: Parser a -> Parser a -> Parser a
    combine p q = Parser (\s -> parse p s ++ parse q s)

    failure :: Parser a
    failure = Parser (\cs -> [])

    option :: Parser a -> Parser a -> Parser a
    option  p q = Parser $ \s ->
        case parse p s of
            []     -> parse q s
            res    -> res

    -- | One or more.
    some :: Alternative f => f a -> f [a]
    some v = some_v
        where
            many_v = some_v <|> pure []
            some_v = (:) <$> v <*> many_v

    -- | Zero or more.
    many :: Alternative f =>f a -> f [a]
    many v = many_v
        where
            many_v = some_v <|> pure []
            some_v = (:) <$> v <*> many_v

    satisfy :: (Char -> Bool) -> Parser Char
    satisfy p = item `bind` \c ->
        if p c
        then unit c
        else (Parser (\cs -> []))

    oneOf :: [Char] -> Parser Char
    oneOf s = satisfy (flip elem s)

    chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
    chainl p op a = (p `chainl1` op) <|> return a

    chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
    p `chainl1` op = do {a <- p; rest a}
        where rest a = (do
                f <- op
                b <- p
                rest (f a b)) <|> return a

    char :: Char -> Parser Char
    char c = satisfy (c ==)

    natural :: Parser Integer
    natural = read <$> some (satisfy isDigit)

    string :: String -> Parser String
    string [] = return []
    string (c:cs) = do { char c; string cs; return (c:cs)}

    token :: Parser a -> Parser a
    token p = do { a <- p; spaces ; return a}

    reserved :: String -> Parser String
    reserved s = token (string s)

    spaces :: Parser String
    spaces = many $ oneOf " \n\r"

    digit :: Parser Char
    digit = satisfy isDigit

    number :: Parser Int
    number = do
        s <- string "-" <|> return []
        cs <- some digit
        return $ read (s ++ cs)

    between :: Char -> Parser a -> Char -> Parser a
    between s m e = do
        _ <- spaces
        char s
        _ <- spaces
        n <- m
        _ <- spaces
        char e
        _ <- spaces
        return n

    simpleChar' :: Parser Char
    simpleChar' = char '>' <|>
            char '<' <|>
            char '+' <|>
            char '-' <|>
            char '.' <|>
            char ','
    
    simpleChar :: Parser Char
    simpleChar = do
        _ <- spaces
        n <- simpleChar'
        _ <- spaces
        return n

    simpleOp :: Parser Token
    simpleOp = fmap build simpleChar
        where 
            build '>' = IncP
            build '<' = DecP
            build '+' = Plus
            build '-' = Minus
            build '.' = Put
            build ',' = Get
            build _   = error "Unknown character" -- this should never happen

    err :: Parser a
    err = error "Unknown character"

    loop :: Parser Token
    loop = Loop <$> between '[' program ']'

    program :: Parser Program
    program = spaces *> many operation <* spaces

    operation :: Parser Token
    operation = simpleOp <|> loop


