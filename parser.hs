module Parser where

import Syntax
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Data.Char (isHexDigit)

lexer :: Token.TokenParser ()
-- reserve keywords
lexer = Token.makeTokenParser emptyDef
    { Token.reservedNames = [ "create", "add", "remove", "update", "convert"
                            , "assign", "contrastCheck", "css", "print"
                            , "shade", "tint", "complementary", "tertiary"
                            , "analogous", "triadic", "hex", "rgb", "hsl"]
                            , Token.commentLine = "//"
                            , Token.identLetter = alphaNum <|> char '-'
    }
-- reserve punctuation
symbol = Token.symbol lexer
parens = Token.parens lexer
brackets = Token.brackets lexer
comma = Token.comma lexer
identifier = Token.identifier lexer
reserved = Token.reserved lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

-- main
parseProgram :: Parser Program

parseProgram = whiteSpace >> many1 (parseStatement <* whiteSpace) <* eof

parseStatement :: Parser Statement
parseStatement = try parseCreate
             <|> try parseAdd
             <|> try parseRemove
             <|> try parseUpdate
             <|> try parseConvert
             <|> try parseAssign
             <|> try parseTransform
             <|> try parseContrastCheck
             <|> try parseCSS
             <|> try parsePrint

parseCreate :: Parser Statement
parseCreate = do
              reserved "create"
              name <- identifier
              entries <- brackets (parseEntry `sepBy` comma)
              return $ Create name entries

parseAdd :: Parser Statement
parseAdd = Add <$> (reserved "add" *> identifier) <*> parseColor

parseRemove :: Parser Statement
parseRemove = do
              reserved "remove"
              name <- identifier
              color <- optionMaybe parseColor
              return $ Remove name color

parseUpdate :: Parser Statement
parseUpdate = do
              reserved "update"
              name <- identifier
              target <- (Left <$> try parseColor) <|> (Right <$> identifier)
              newCol <- parseColor
              return $ Update name target newCol

parseConvert :: Parser Statement
parseConvert = do
               reserved "convert"
               m1 <- parseMode
               m2 <- parseMode
               target <- (Left <$> parseColor) <|> (Right <$> identifier)
               return $ Convert m1 m2 target


parseAssign :: Parser Statement
parseAssign = do
              reserved "assign"
              pName  <- identifier
              pRole  <- identifier
              target <- try parseComputed <|> parseDirect
              return $ Assign pName pRole target
            where
              parseDirect = Direct <$> parseColor
              parseComputed = do
                t <- parseTransformType
        -- color or target?
                val <- (Left <$> parseColor) <|> (Right <$> identifier)
                return $ Computed t val

parseTransform :: Parser Statement
parseTransform = do
                 t <- parseTransformType
                 target <- (Left <$> parseColor) <|> (Right <$> identifier)
                 return $ Transform t target

parseContrastCheck :: Parser Statement
parseContrastCheck = do
                     reserved "contrastCheck"
                     target <- try (ColorPair <$> parseColor <*> parseColor)
                            <|> (RolePair <$> identifier <*> identifier <*> identifier)
                     return $ ContrastCheck target

parseCSS :: Parser Statement
parseCSS = CSS <$> (reserved "css" *> identifier)

parsePrint :: Parser Statement
parsePrint = Print <$> (reserved "print" *> identifier)

-- helpers

parseEntry :: Parser Entry
parseEntry = try roleEntry <|> colorEntry
             where
                roleEntry = do
                    role <- identifier
                    symbol ":"
                    color <- parseColor
                    return $ RoleEntry role color
                colorEntry = ColorEntry <$> parseColor

parseColor :: Parser Color
parseColor = parseHex <|> parseRGB <|> parseHSL

parseHex :: Parser Color
parseHex = do
           symbol "#"
           h <- many1 (satisfy isHexDigit)
           return $ Hex h

parseRGB :: Parser Color
parseRGB = do
           reserved "rgb"
           (r, g, b) <- parseThreeInts
           return $ RGB r g b

parseHSL :: Parser Color
parseHSL = do
           reserved "hsl"
           (h, s, l) <- parseThreeInts
           return $ HSL h s l

parseThreeInts :: Parser (Int, Int, Int)
parseThreeInts = parens body <|> body
                 where body = do
                                v1 <- fromInteger <$> integer
                                optional comma
                                v2 <- fromInteger <$> integer
                                optional comma
                                v3 <- fromInteger <$> integer
                                return (v1, v2, v3)

parseMode :: Parser Mode
parseMode = (reserved "hex" >> return HexMode)
        <|> (reserved "rgb" >> return RGBMode)
        <|> (reserved "hsl" >> return HSLMode)

parseTransformType :: Parser Transform
parseTransformType = (reserved "shade" >> return Shade)
                 <|> (reserved "tint" >> return Tint)
                 <|> (reserved "complementary" >> return Complementary)
                 <|> (reserved "tertiary" >> return Tertiary)
                 <|> (reserved "analogous" >> return Analogous)
                 <|> (reserved "triadic" >> return Triadic)

-- helps run the parser
runSwatch :: String -> Either ParseError Program
runSwatch = parse parseProgram ""