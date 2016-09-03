{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Data.Monoid ((<>))
import Data.Text (Text)
import Text.Earley
import Text.PrettyPrint (Doc, (<+>))

import qualified Data.Char
import qualified Data.Text
import qualified Data.Text.IO
import qualified Text.PrettyPrint

main :: IO ()
main = do
    str <- Data.Text.IO.getContents
    let x@(solutions, report) = fullParses (parser expr) str
    print x
    case solutions of
        [] -> fail "No valid parses!"
        _  -> return ()

infixr <#>

space :: Prod r Text Char Char
space = satisfy Data.Char.isSpace <?> "space"

spaces :: Prod r Text Char String
spaces = many space

char :: Char -> Prod r Text Char Char
char c = satisfy (== c) <?> ("'" <> Data.Text.singleton c <> "'")

string :: String -> Prod r Text Char String
string s = traverse char s <?> ("\"" <> Data.Text.pack s <> "\"")

anyChar :: Prod r Text Char Char
anyChar = satisfy (\_ -> True) <?> "any character"

letter :: Prod r Text Char Char
letter = satisfy Data.Char.isLetter <?> "letter"

alphaNum :: Prod r Text Char Char
alphaNum = satisfy Data.Char.isAlphaNum <?> "letter or digit"

digit :: Prod r Text Char Char
digit = satisfy Data.Char.isDigit <?> "digit"

oneOf :: String -> Prod r Text Char Char
oneOf cs = satisfy (`elem` cs) <?> ("one of " <> Data.Text.pack (show cs))

noneOf :: String -> Prod r Text Char Char
noneOf cs =
    satisfy (`notElem` cs) <?> ("not one of " <> Data.Text.pack (show cs))

(<#>) :: Prod r Text Char Doc -> Prod r Text Char Doc -> Prod r Text Char Doc
x <#> y = adapt <$> x <*> spaces <*> y
  where
    adapt a _ b = a <+> b

(<!>) :: (Applicative f, Monoid m) => f m -> f m -> f m
(<!>) = liftA2 mappend

match :: String -> Prod r Text Char Doc
match s = fmap Text.PrettyPrint.text (string s)

expr :: Grammar r (Prod r Text Char Doc)
expr = mdo
    parseID <- rule $ do
            fmap Text.PrettyPrint.char (letter <|> char '_')
        <!> fmap Text.PrettyPrint.text (many (alphaNum <|> oneOf "_'-"))

    parseINT <- rule $ do
            fmap Text.PrettyPrint.text (some digit)

    parseFLOAT <- rule $ do
        let alt0 =  fmap Text.PrettyPrint.char (oneOf "123456789")
                <!> fmap Text.PrettyPrint.text (many digit)
                <!> fmap Text.PrettyPrint.char (char '.')
                <!> fmap Text.PrettyPrint.text (many digit)
        let alt1 =  (fmap Text.PrettyPrint.char (char '0') <|> pure mempty)
                <!> fmap Text.PrettyPrint.char (char '.')
                <!> fmap Text.PrettyPrint.text (some digit)
        let alt2 =  fmap Text.PrettyPrint.char (oneOf "Ee")
                <!> (fmap Text.PrettyPrint.char (oneOf "+-") <|> pure mempty)
                <!> fmap Text.PrettyPrint.text (some digit)
        (alt0 <|> alt1) <!>(alt2 <|> pure mempty)

    parsePATH <- rule $ do
        let charSet = alphaNum <|> oneOf "._-+"
        let alt0 = fmap Text.PrettyPrint.text (many charSet)
        let alt1 = fmap Text.PrettyPrint.char (char '/') <!> alt0
        alt0 <!> fmap mconcat (some alt1)

    parseHPATH <- rule $ do
        let charSet = alphaNum <|> oneOf "._-+"
        let alt0 = fmap Text.PrettyPrint.text (some charSet)
        let alt1 = fmap Text.PrettyPrint.char (char '/') <!> alt0
        let alt2 = fmap mconcat (some alt1)
        fmap Text.PrettyPrint.char (char '~') <!> alt2

    parseSPATH <- rule $ do
        let charSet = alphaNum <|> oneOf "._-+"
        let alt0 = fmap Text.PrettyPrint.text (some charSet)
        let alt1 = fmap Text.PrettyPrint.char (char '/') <!> alt0
        let alt2 = fmap mconcat (some alt0)
        let alt3 = fmap mconcat (many alt1)
        let alt4 =  fmap Text.PrettyPrint.char (char '<')
                <!> alt2
                <!> alt3
                <!> fmap Text.PrettyPrint.char (char '>')
        alt4

    parseURI <- rule $ do
        let charSet0 = alphaNum <|> oneOf "+-."
        let charSet1 = alphaNum <|> oneOf "%/?:@&=+$,-_.!~*'"
        let alt0 = fmap Text.PrettyPrint.text (many charSet0)
        let alt1 = fmap Text.PrettyPrint.text (some charSet1)
        let alt2 =  fmap Text.PrettyPrint.char letter
                <!> alt0
                <!> fmap Text.PrettyPrint.char (char ':')
                <!> alt1
        alt2

    parseFormals <- rule $ do
            parseFormals0
        <|> parseFormals1
        <|> parseFormals2
        <|> parseFormals3

    parseFormals0 <- rule $ do
            parseFormal
        <#> match ","
        <#> parseFormals

    parseFormals1 <- rule $ do
            parseFormal

    parseFormals2 <- rule $ do
            pure mempty

    parseFormals3 <- rule $ do
            match "..."

    parseFormal <- rule $ do
            parseFormal0
        <|> parseFormal1

    parseFormal0 <- rule $ do
            parseID

    parseFormal1 <- rule $ do
            parseID
        <#> match "?"
        <#> parseExpr

    parseBinds <- rule $ do
            parseBinds0
        <|> parseBinds1
        <|> parseBinds2
        <|> parseBinds3

    parseBinds0 <- rule $ do
            parseBinds
        <#> parseAttrPath
        <#> match "="
        <#> parseExpr
        <#> match ";"

    parseBinds1 <- rule $ do
            parseBinds
        <#> match "inherit"
        <#> parseAttrs
        <#> match ";"

    parseBinds2 <- rule $ do
            parseBinds
        <#> match "inherit"
        <#> match "("
        <#> parseExpr
        <#> match ")"
        <#> parseAttrs
        <#> match ";"

    parseBinds3 <- rule $ do
            pure mempty

    parseAttrPath <- rule $ do
            parseAttrPath0
        <|> parseAttrPath1
        <|> parseAttrPath2
        <|> parseAttrPath3

    parseAttrPath0 <- rule $ do
            parseAttrPath
        <#> match "."
        <#> parseAttr

    parseAttrPath1 <- rule $ do
            parseAttrPath
        <#> match "."
        <#> parseStringAttr

    parseAttrPath2 <- rule $ do
            parseAttr

    parseAttrPath3 <- rule $ do
            parseStringAttr

    parseAttrs <- rule $ do
            parseAttrs0
        <|> parseAttrs1
        <|> parseAttrs2

    parseAttrs0 <- rule $ do
            parseAttrs
        <#> parseAttr

    parseAttrs1 <- rule $ do
            parseAttrs
        <#> parseStringAttr

    parseAttrs2 <- rule $ do
             pure mempty

    parseAttr <- rule $ do
            parseAttr0
        <|> parseAttr1

    parseAttr0 <- rule $ do
            parseID

    parseAttr1 <- rule $ do
            match "or"

    parseStringAttr <- rule $ do
            parseStringAttr0
        <|> parseStringAttr1

    parseStringAttr0 <- rule $ do
            match "\""
        <!> parseStringParts
        <!> match "\""

    parseStringAttr1 <- rule $ do
            match "${"
        <#> parseExpr
        <#> match "}"

    parseExprList <- rule $ do
            parseExprList0
        <#> parseExprList1

    parseExprList0 <- rule $ do
            parseExprList
        <#> parseExprSelect

    parseExprList1 <- rule $ do
            pure mempty

    parseStringParts <- rule $ do
            parseStringParts0
        <|> parseStringParts1
        <|> parseStringParts2

    parseStringParts0 <- rule $ do
            parseSTR

    parseStringParts1 <- rule $ do
            parseStringPartsInterpolated

    parseStringParts2 <- rule $ do
            pure mempty

    parseStringPartsInterpolated <- rule $ do
            parseStringPartsInterpolated0
        <|> parseStringPartsInterpolated1
        <|> parseStringPartsInterpolated2
        <|> parseStringPartsInterpolated3

    parseStringPartsInterpolated0 <- rule $ do
            parseStringPartsInterpolated
        <!> parseSTR

    parseStringPartsInterpolated1 <- rule $
            parseStringPartsInterpolated
        <!> (   match "${"
            <#> parseExpr
            <#> match "}"
            )

    parseStringPartsInterpolated2 <- rule $ do
            match "${"
        <#> parseExpr
        <#> match "}"

    parseStringPartsInterpolated3 <- rule $
            parseSTR
        <!> (   match "${"
            <#> parseExpr
            <#> match "}"
            )

    parseSTR <- rule $ do
            -- TODO: Fix
            parseSTR1

    parseSTR1 <- rule $ do
        let alt0 =  fmap Text.PrettyPrint.char (noneOf "$\"\\")
        let alt1 =  fmap Text.PrettyPrint.char (char '$')
                <!> fmap Text.PrettyPrint.char (noneOf "{\"\\")
        let alt2 =  fmap Text.PrettyPrint.char (char '\\')
                <!> fmap Text.PrettyPrint.char anyChar
        let alt3 =  fmap Text.PrettyPrint.char (char '$')
                <!> fmap Text.PrettyPrint.char (char '\\')
                <!> fmap Text.PrettyPrint.char anyChar
        fmap mconcat (some (alt0 <|> alt1 <|> alt2 <|> alt3))

    parseIND_STR <- rule $ do
            parseIND_STR0
        <|> parseIND_STR1
        <|> parseIND_STR2
        <|> parseIND_STR3
        <|> parseIND_STR4

    parseIND_STR0 <- rule $ do
        let alt0 =  fmap Text.PrettyPrint.char (noneOf "$'")
        let alt1 =  fmap Text.PrettyPrint.char (char '$')
                <!> fmap Text.PrettyPrint.char (noneOf "{'")
        let alt2 =  fmap Text.PrettyPrint.char (char '\'')
                <!> fmap Text.PrettyPrint.char (noneOf "'$")
        fmap mconcat (some (alt0 <|> alt1 <|> alt2))

    parseIND_STR1 <- rule $ do
            fmap Text.PrettyPrint.text (string "''$")

    parseIND_STR2 <- rule $ do
            fmap Text.PrettyPrint.text (string "'''")

    parseIND_STR3 <- rule $ do
            fmap Text.PrettyPrint.text (string "''.")

    parseIND_STR4 <- rule $ do
            fmap Text.PrettyPrint.char (char '\'')

    parseExpr <- rule $ do
            parseExprFunction

    parseExprFunction <- rule $ do
            parseExprFunction0
        <|> parseExprFunction1
        <|> parseExprFunction2
        <|> parseExprFunction3
        <|> parseExprFunction4
        <|> parseExprFunction5
        <|> parseExprFunction6
        <|> parseExprFunction7

    parseExprFunction0 <- rule $ do
        let adapt a _ b _ c = a <+> b <+> c
        adapt <$> parseID <*> spaces <*> match ":" <*> spaces <*> parseExprFunction

    parseExprFunction1 <- rule $ do
            match "{"
        <#> parseFormals
        <#> match "}"
        <#> match ":"
        <#> parseExprFunction

    parseExprFunction2 <- rule $ do
            match "{"
        <#> parseFormals
        <#> match "}"
        <#> match "@"
        <#> parseID
        <#> match ":"
        <#> parseExprFunction

    parseExprFunction3 <- rule $ do
            parseID
        <#> match "@"
        <#> match "{"
        <#> parseFormals
        <#> match "}"
        <#> match ":"
        <#> parseExprFunction

    parseExprFunction4 <- rule $ do
            match "assert"
        <#> parseExpr
        <#> match ";"
        <#> parseExprFunction

    parseExprFunction5 <- rule $ do
            match "with"
        <#> parseExpr
        <#> match ";"
        <#> parseExprFunction

    parseExprFunction6 <- rule $ do
            match "let"
        <#> parseBinds
        <#> match "in"
        <#> parseExprFunction

    parseExprFunction7 <- rule $ do
            parseExprIf

    parseExprIf <- rule $ do
            parseExprIf0
        <|> parseExprIf1

    parseExprIf0 <- rule $ do
            match "if"
        <#> parseExpr
        <#> match "then"
        <#> parseExpr
        <#> match "else"
        <#> parseExpr

    parseExprIf1 <- rule $ do
            parseExprOp

    parseExprOp <- rule $ do
            parseExprOp00
        <|> parseExprOp01
        <|> parseExprOp02
        <|> parseExprOp03
        <|> parseExprOp04
        <|> parseExprOp05
        <|> parseExprOp06
        <|> parseExprOp07
        <|> parseExprOp08
        <|> parseExprOp09
        <|> parseExprOp10
        <|> parseExprOp11
        <|> parseExprOp12
        <|> parseExprOp13
        <|> parseExprOp14
        <|> parseExprOp15
        <|> parseExprOp16
        <|> parseExprOp17

    parseExprOp00 <- rule $ do
            match "!"
        <#> parseExprOp

    parseExprOp01 <- rule $ do
            match "-"
        <#> parseExprOp

    parseExprOp02 <- rule $ do
            parseExprOp
        <#> match "=="
        <#> parseExprOp

    parseExprOp03 <- rule $ do
            parseExprOp
        <#> match "!="
        <#> parseExprOp

    parseExprOp04 <- rule $ do
            parseExprOp
        <#> match "<"
        <#> parseExprOp

    parseExprOp05 <- rule $ do
            parseExprOp
        <#> match "<="
        <#> parseExprOp

    parseExprOp06 <- rule $ do
            parseExprOp
        <#> match ">"
        <#> parseExprOp

    parseExprOp07 <- rule $ do
            parseExprOp
        <#> match ">="
        <#> parseExprOp

    parseExprOp08 <- rule $ do
            parseExprOp
        <#> match "&&"
        <#> parseExprOp

    parseExprOp09 <- rule $ do
            parseExprOp
        <#> match "||"
        <#> parseExprOp

    parseExprOp10 <- rule $ do
            parseExprOp
        <#> match "->"
        <#> parseExprOp

    parseExprOp11 <- rule $ do
            parseExprOp
        <#> match "//"
        <#> parseExprOp

    parseExprOp12 <- rule $ do
            parseExprOp
        <#> match "?"
        <#> parseExprOp

    parseExprOp13 <- rule $ do
            parseExprOp
        <#> match "+"
        <#> parseExprOp

    parseExprOp14 <- rule $ do
            parseExprOp
        <#> match "*"
        <#> parseExprOp

    parseExprOp15 <- rule $ do
            parseExprOp
        -- Slight hack so that `foo/bar/baz` is parsed as a path and not as
        -- "foo divided by bar divided by baz"
        <#> (space *> match "/" <* space)
        <#> parseExprOp

    parseExprOp16 <- rule $ do
            parseExprOp
        <#> match "++"
        <#> parseExprOp

    parseExprOp17 <- rule $ do
            parseExprApp

    parseExprApp <- rule $ do
            parseExprApp0
        <|> parseExprApp1

    parseExprApp0 <- rule $ do
        let adapt a _ _ b = a <+> b
        adapt <$> parseExprSelect <*> space <*> spaces <*> parseExprApp

    parseExprApp1 <- rule $ do
            parseExprSelect

    parseExprSelect <- rule $ do
            parseExprSelect0
        <|> parseExprSelect1
        <|> parseExprSelect2
        <|> parseExprSelect3

    parseExprSelect0 <- rule $ do
            parseExprSimple
        <#> match "."
        <#> parseAttrPath

    parseExprSelect1 <- rule $ do
            parseExprSimple
        <#> match "."
        <#> parseAttrPath
        <#> match "or"
        <#> parseExprSelect

    parseExprSelect2 <- rule $ do
            parseExprSimple
        <#> match "or"

    parseExprSelect3 <- rule $ do
            parseExprSimple

    parseExprSimple <- rule $ do
            parseExprSimple00
        <|> parseExprSimple01
        <|> parseExprSimple02
        <|> parseExprSimple03
        <|> parseExprSimple04
        <|> parseExprSimple05
        <|> parseExprSimple06
        <|> parseExprSimple07
        <|> parseExprSimple08
        <|> parseExprSimple09
        <|> parseExprSimple10
        <|> parseExprSimple11
        <|> parseExprSimple12

    parseExprSimple00 <- rule $ do
            parseID

    parseExprSimple01 <- rule $ do
            parseINT

    parseExprSimple02 <- rule $ do
            parseFLOAT

    parseExprSimple03 <- rule $ do
            fmap Text.PrettyPrint.char (char '"')
        <!> parseStringParts
        <!> fmap Text.PrettyPrint.char (char '"')

    parseExprSimple04 <- rule $ do
            fmap Text.PrettyPrint.text (string "''")
        <!> fmap Text.PrettyPrint.text (many space)
        <!> parseIndStringParts
        <!> fmap Text.PrettyPrint.text (string "''")

    parseExprSimple05 <- rule $ do
            parsePATH

    parseExprSimple06 <- rule $ do
            parseHPATH

    parseExprSimple07 <- rule $ do
            parseSPATH

    parseExprSimple08 <- rule $ do
            parseURI

    parseExprSimple09 <- rule $ do
            match "let"
        <#> match "{"
        <#> parseBinds
        <#> match "}"

    parseExprSimple10 <- rule $ do
            match "rec"
        <#> match "{"
        <#> parseBinds
        <#> match "}"

    parseExprSimple11 <- rule $ do
            match "{"
        <#> parseBinds
        <#> match "}"

    parseExprSimple12 <- rule $ do
            match "["
        <#> parseExprList
        <#> match "]"

    parseIndStringParts <- rule $ do
            parseIndStringParts0
        <|> parseIndStringParts1
        <|> parseIndStringParts2

    parseIndStringParts0 <- rule $ do
            parseIndStringParts
        <!> parseIND_STR

    parseIndStringParts1 <- rule $
            parseIndStringParts
        <!> (    match "${"
            <#> parseExpr
            <#> match "}"
            )

    parseIndStringParts2 <- rule $ do
            pure mempty

    return (spaces *> parseExpr <* spaces)
