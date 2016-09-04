{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (liftA2, some)
import Data.Monoid ((<>))
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Text (Parser)
import Text.PrettyPrint (Doc, (<+>))

import qualified Data.Text.IO
import qualified Text.PrettyPrint

main :: IO ()
main = do
    str <- Data.Text.IO.getContents
    print (parse (parseExpr <* spaces <* eof) "" str)

infixr <#>

(<#>) :: Parser Doc -> Parser Doc -> Parser Doc
x <#> y = do
    a <- x
    spaces
    b <- y
    return (a <+> b)

instance Monoid a => Monoid (ParsecT s u m a) where
    mempty = pure mempty

    mappend = liftA2 mappend

some' v = some_v
  where
    many_v = try some_v <|> pure []
    some_v = (fmap (:) v) <*> many_v

match :: String -> Parser Doc
match s = fmap Text.PrettyPrint.text (string s)

parseID :: Parser Doc
parseID = do
    c  <- letter <|> char '_'
    cs <- many (alphaNum <|> oneOf "_'-")
    return (Text.PrettyPrint.text (c:cs))

parseINT :: Parser Doc
parseINT = fmap Text.PrettyPrint.text (some digit)

parseFLOAT :: Parser Doc
parseFLOAT = do
    let alt0 =  fmap Text.PrettyPrint.char (oneOf "123456789")
            <>  fmap Text.PrettyPrint.text (many digit)
            <>  fmap Text.PrettyPrint.char (char '.')
            <>  fmap Text.PrettyPrint.text (many digit)
    let alt1 =  (fmap Text.PrettyPrint.char (char '0') <|> mempty)
            <>  fmap Text.PrettyPrint.char (char '.')
            <>  fmap Text.PrettyPrint.text (some digit)
    let alt2 =  fmap Text.PrettyPrint.char (oneOf "Ee")
            <>  (fmap Text.PrettyPrint.char (oneOf "+-") <|> mempty)
            <>  fmap Text.PrettyPrint.text (some digit)
    (alt0 <|> alt1) <> (alt2 <|> mempty)

parseFormals :: Parser Doc
parseFormals
    =   try parseFormals0
    <|> try parseFormals1
    <|> try parseFormals2
    <|>     parseFormals3

parseFormals0 :: Parser Doc
parseFormals0
    =   parseFormal
    <#> match ","
    <#> parseFormals

parseFormals1 :: Parser Doc
parseFormals1
    =   parseFormal

parseFormals2 :: Parser Doc
parseFormals2
    =   mempty

parseFormals3 :: Parser Doc
parseFormals3
    =   match "..."

parseFormal :: Parser Doc
parseFormal
    =   parseFormal0
    <|> parseFormal1

parseFormal0 :: Parser Doc
parseFormal0
    =   parseID

parseFormal1 :: Parser Doc
parseFormal1
    =   parseID
    <#> match "?"
    <#> parseExpr

parseBinds :: Parser Doc
parseBinds
    =   try parseBinds0
    <|> try parseBinds1
    <|> try parseBinds2
    <|>     parseBinds3

parseBinds0 :: Parser Doc
parseBinds0
    =   parseAttrPath
    <#> match "="
    <#> parseExpr
    <#> match ";"
    <#> parseBinds

parseBinds1 :: Parser Doc
parseBinds1
    =   match "inherit"
    <#> parseAttrs
    <#> match ";"
    <#> parseBinds

parseBinds2 :: Parser Doc
parseBinds2
    =   match "inherit"
    <#> match "("
    <#> parseExpr
    <#> match ")"
    <#> parseAttrs
    <#> match ";"
    <#> parseBinds

parseBinds3 :: Parser Doc
parseBinds3
    =   mempty

parseAttrPath :: Parser Doc
parseAttrPath
    =   try parseAttrPath0
    <|> try parseAttrPath1
    <|> try parseAttrPath2
    <|>     parseAttrPath3

parseAttrPath0 :: Parser Doc
parseAttrPath0
    =   parseAttr
    <#> match "."
    <#> parseAttrPath

parseAttrPath1 :: Parser Doc
parseAttrPath1
    =   parseStringAttr
    <#> match "."
    <#> parseAttrPath

parseAttrPath2 :: Parser Doc
parseAttrPath2
    =   parseAttr

parseAttrPath3 :: Parser Doc
parseAttrPath3
    =   parseStringAttr

parseAttrs :: Parser Doc
parseAttrs
    =   try parseAttrs0
    <|> try parseAttrs1
    <|>     parseAttrs2

parseAttrs0 :: Parser Doc
parseAttrs0
    =   parseAttr
    <#> parseAttrs

parseAttrs1 :: Parser Doc
parseAttrs1
    =   parseStringAttr
    <#> parseAttrs

parseAttrs2 :: Parser Doc
parseAttrs2
    =    mempty

parseAttr :: Parser Doc
parseAttr
    =   try parseAttr0
    <|>     parseAttr1

parseAttr0 :: Parser Doc
parseAttr0
    =   parseID

parseAttr1 :: Parser Doc
parseAttr1
    =   match "or"

parseStringAttr :: Parser Doc
parseStringAttr
    =   try parseStringAttr0
    <|>     parseStringAttr1

parseStringAttr0 :: Parser Doc
parseStringAttr0
    =   match "\""
    <>  parseStringParts
    <>  match "\""

parseStringAttr1 :: Parser Doc
parseStringAttr1
    =   match "${"
    <#> parseExpr
    <#> match "}"

parseStringParts :: Parser Doc
parseStringParts
    =   try parseStringParts0
    <|> try parseStringParts1
    <|>     parseStringParts2

parseStringParts0 :: Parser Doc
parseStringParts0
    =   parseSTR

parseStringParts1 :: Parser Doc
parseStringParts1
    =   parseStringPartsInterpolated

parseStringParts2 :: Parser Doc
parseStringParts2
    =   mempty

parseStringPartsInterpolated :: Parser Doc
parseStringPartsInterpolated
    =   try parseStringPartsInterpolated0
    <|> try parseStringPartsInterpolated1
    <|> try parseStringPartsInterpolated2
    <|> try parseStringPartsInterpolated3

parseStringPartsInterpolated0 :: Parser Doc
parseStringPartsInterpolated0
    =   parseSTR
    <>  parseStringPartsInterpolated

parseStringPartsInterpolated1 :: Parser Doc
parseStringPartsInterpolated1
    =   (   match "${"
        <#> parseExpr
        <#> match "}"
        )
    <>  parseStringPartsInterpolated

parseStringPartsInterpolated2 :: Parser Doc
parseStringPartsInterpolated2
    =   match "${"
    <#> parseExpr
    <#> match "}"

parseStringPartsInterpolated3 :: Parser Doc
parseStringPartsInterpolated3
    =   (   match "${"
        <#> parseExpr
        <#> match "}"
        )
    <>  parseSTR

parseSTR :: Parser Doc
parseSTR =
--  TODO: Handle other case
--      try parseSTR0 <|>
            parseSTR1

parseSTR1 :: Parser Doc
parseSTR1 = do
    let alt0 =  fmap Text.PrettyPrint.char (noneOf "$\"\\")
    let alt1 =  fmap Text.PrettyPrint.char (char '$')
            <>  fmap Text.PrettyPrint.char (noneOf "{\"\\")
    let alt2 =  fmap Text.PrettyPrint.char (char '\\')
            <>  fmap Text.PrettyPrint.char anyChar
    let alt3 =  fmap Text.PrettyPrint.char (char '$')
            <>  fmap Text.PrettyPrint.char (char '\\')
            <>  fmap Text.PrettyPrint.char anyChar
    fmap mconcat (some (try alt0 <|> try alt1 <|> try alt2 <|> alt3))

parseIND_STR :: Parser Doc
parseIND_STR
    =   try parseIND_STR0
    <|> try parseIND_STR1
    <|> try parseIND_STR2
    <|> try parseIND_STR3
    <|>     parseIND_STR4

parseIND_STR0 :: Parser Doc
parseIND_STR0 = do
    let alt0 =  fmap Text.PrettyPrint.char (noneOf "$'")
    let alt1 =  fmap Text.PrettyPrint.char (char '$')
            <>  fmap Text.PrettyPrint.char (noneOf "{'")
    let alt2 =  fmap Text.PrettyPrint.char (char '\'')
            <>  fmap Text.PrettyPrint.char (noneOf "'$")
    fmap mconcat (some' (try alt0 <|> try alt1 <|> alt2))

parseIND_STR1 :: Parser Doc
parseIND_STR1
    =   fmap Text.PrettyPrint.text (string "''$")

parseIND_STR2 :: Parser Doc
parseIND_STR2
    =   fmap Text.PrettyPrint.text (string "'''")

parseIND_STR3 :: Parser Doc
parseIND_STR3
    =   fmap Text.PrettyPrint.text (string "''.")

parseIND_STR4 :: Parser Doc
parseIND_STR4
    =   fmap Text.PrettyPrint.char (char '\'')

parseExpr :: Parser Doc
parseExpr = parseExprFunction

parseExprFunction :: Parser Doc
parseExprFunction
    =   try parseExprFunction0
    <|> try parseExprFunction1
    <|> try parseExprFunction2
    <|> try parseExprFunction3
    <|> try parseExprFunction4
    <|> try parseExprFunction5
    <|> try parseExprFunction6
    <|>     parseExprFunction7

parseExprFunction0 :: Parser Doc
parseExprFunction0 = do
    a <- parseID
    spaces
    b <- match ":"
    spaces
    c <- parseExprFunction
    return (a <+> b <+> c)

parseExprFunction1 :: Parser Doc
parseExprFunction1
    =   match "{"
    <#> parseFormals
    <#> match "}"
    <#> match ":"
    <#> parseExprFunction

parseExprFunction2 :: Parser Doc
parseExprFunction2
    =   match "{"
    <#> parseFormals
    <#> match "}"
    <#> match "@"
    <#> parseID
    <#> match ":"
    <#> parseExprFunction

parseExprFunction3 :: Parser Doc
parseExprFunction3
    =   parseID
    <#> match "@"
    <#> match "{"
    <#> parseFormals
    <#> match "}"
    <#> match ":"
    <#> parseExprFunction

parseExprFunction4 :: Parser Doc
parseExprFunction4
    =   match "assert"
    <#> parseExpr
    <#> match ";"
    <#> parseExprFunction

parseExprFunction5 :: Parser Doc
parseExprFunction5
    =   match "with"
    <#> parseExpr
    <#> match ";"
    <#> parseExprFunction

parseExprFunction6 :: Parser Doc
parseExprFunction6
    =   match "let"
    <#> parseBinds
    <#> match "in"
    <#> parseExprFunction

parseExprFunction7 :: Parser Doc
parseExprFunction7
    =   parseExprIf

parseExprIf :: Parser Doc
parseExprIf
    =   try parseExprIf0
    <|>     parseExprIf1

parseExprIf0 :: Parser Doc
parseExprIf0
    =   match "if"
    <#> parseExpr
    <#> match "then"
    <#> parseExpr
    <#> match "else"
    <#> parseExpr

parseExprIf1 :: Parser Doc
parseExprIf1
    =   parseExprOp

parseExprOp :: Parser Doc
parseExprOp = buildExpressionParser table (parseExprApp <* spaces)
  where
    table =
        [ [ Prefix (do a <- match "!"; spaces; return (a <+>))
          , Prefix (do a <- match "-"; spaces; return (a <+>))
          ]
        , [ Infix  (do a <- match "=="; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "!="; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "<"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "<="; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match ">"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match ">="; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "&&"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "!!"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "->"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "//"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "?"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "+"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "-"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "*"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "/"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          , Infix  (do a <- match "++"; spaces; return (\x y -> x <+> a <+> y)) AssocNone
          ]
        ]

parseExprApp :: Parser Doc
parseExprApp
    =   try parseExprApp0
    <|>     parseExprApp1

parseExprApp0 :: Parser Doc
parseExprApp0 = do
    a <- parseExprSelect
    _ <- space
    spaces
    b <- parseExprApp
    return (a <+> b)

parseExprApp1 :: Parser Doc
parseExprApp1
    =   parseExprSelect

parseExprSelect :: Parser Doc
parseExprSelect
    =   try parseExprSelect0
    <|> try parseExprSelect1
    <|> try parseExprSelect2
    <|>     parseExprSelect3

parseExprSelect0 :: Parser Doc
parseExprSelect0
    =   parseExprSimple
    <#> match "."
    <#> parseAttrPath

parseExprSelect1 :: Parser Doc
parseExprSelect1
    =   parseExprSimple
    <#> match "."
    <#> parseAttrPath
    <#> match "or"
    <#> parseExprSelect

parseExprSelect2 :: Parser Doc
parseExprSelect2
    =   parseExprSimple
    <#> match "or"

parseExprSelect3 :: Parser Doc
parseExprSelect3
    =   parseExprSimple

parseExprSimple :: Parser Doc
parseExprSimple
    =   try parseExprSimple0
    <|> try parseExprSimple1
    <|> try parseExprSimple2
    <|> try parseExprSimple3
    <|>     parseExprSimple4

parseExprSimple0 :: Parser Doc
parseExprSimple0
    = parseID

parseExprSimple1 :: Parser Doc
parseExprSimple1
    = parseINT

parseExprSimple2 :: Parser Doc
parseExprSimple2
    = parseFLOAT

parseExprSimple3 :: Parser Doc
parseExprSimple3
    =   fmap Text.PrettyPrint.char (char '"')
    <>  parseStringParts
    <>  fmap Text.PrettyPrint.char (char '"')

parseExprSimple4 :: Parser Doc
parseExprSimple4
    =   fmap Text.PrettyPrint.text (string "''")
    <>  fmap Text.PrettyPrint.text (many space)
    <>  parseIndStringParts
    <>  fmap Text.PrettyPrint.text (string "''")

parseIndStringParts :: Parser Doc
parseIndStringParts
    =   try parseIndStringParts0
    <|> try parseIndStringParts1
    <|>     parseIndStringParts2

parseIndStringParts0 :: Parser Doc
parseIndStringParts0
    =   parseIND_STR
    <>  parseIndStringParts

parseIndStringParts1 :: Parser Doc
parseIndStringParts1
    =   match "${"
    <#> parseExpr
    <#> match "}"
    <>  parseIndStringParts

parseIndStringParts2 :: Parser Doc
parseIndStringParts2
    =   mempty
