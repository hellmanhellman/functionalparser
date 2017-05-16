data Expr = Num Int | Var String | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
data Statement = Assignment String Expr

type Parser a = String -> Maybe (a, String)

infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs = case m cs of Nothing -> Nothing
                          Just (a,cs) -> if p a then Just (a,cs) else Nothing

infixl 6 #
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs = case m cs of
              Nothing -> Nothing
              Just(p, cs') -> case n cs' of
                                Nothing -> Nothing
                                Just(q, cs'') -> Just((p,q), cs'')

char :: Parser Char
char (c:cs) = Just (c,cs)
char [] = Nothing


lit :: Char -> Parser Char
lit c = char ? (==c)

semicolon :: Parser Char
semicolon = lit ';'

twochars :: Parser (Char, Char)
twochars = char # char

-- becomes ":=count+1:" -> (":=", "count+1;")
becomes :: Parser (Char,Char)
becomes = twochars ? (== (':','='))



main = print $ semicolon "; hej"
