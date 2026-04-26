{-
    course: CSCI3336
    name: Leslie A. Hurtado
    project: Swatch - Midterm Language Design Project

    Context Free Grammar:
    <Program> -> <statements>
    <statements> -> <statement> | <statement> <statements>
    <statement> ->
                    "create " <name> "[" <entries> "]"
                  | "add" <name> <color>
                  | "remove" <name> <color> | "remove" <name>
                  | "update" <name> <updateTarget> <color>
                  | "convert" <mode> <mode> <color> | "convert" <mode> <mode> <name>

                  | <transform> <target>

                  | "assign" <name> <role> <assignTarget>
                  | "contrastCheck" <color> <color> | "contrastCheck" <name> <role> <role>
                  | "css" <name>
                  | "print" <name>

    <transform> -> "shade" | "tint" | "complementary" | "tertiary" | "analogous" | "triadic"
    <target> -> <color> | <name>
    <updateTarget> -> <color | <role>
    <assignTarget> -> <color> | <transform> <target>
    <mode> -> "hex" | "rgb" | "hsl"

    <name> -> <identifier>
    <role> -> <identifier>
    <identifier> -> <letter> { <letter> | <digit> | "-" }
    
    <entries> -> <entry> | <entry> "," <entries>
    <entry> -> <color> | <role> ":" <color>
    <color> -> "rgb" <rgb> | "hsl" <hsl> | "#" <hex>

    <rgb> -> "(" <ℤ+> "," <ℤ+> "," <ℤ+> ")" | <ℤ+> "," <ℤ+> "," <ℤ+>
    <hsl> -> "(" <ℤ+> "," <ℤ+> "," <ℤ+> ")" | <ℤ+> "," <ℤ+> "," <ℤ+>

    <hex> -> <hexDigit>{3} | <hexDigit>{6}
    <hexDigit> -> [0-9] | [a-f] | [A-F]
-}             
import Syntax
import Parser (runSwatch)
import Text.Parsec (ParseError)

--Example 1 - Palette creation
{- 
    create Greyscale [Foreground: #000, #ccc, Background: #eee]
    add Greyscale #fff
    css Greyscale
-}
-- Haskell
example1 :: Program
example1 = [
    Create "Greyscale" [RoleEntry "Foreground" (Hex (Hex3 Zero Zero Zero)), ColorEntry (Hex (Hex3 UC UC UC)), RoleEntry "Background" (Hex (Hex3 Le Le Le))],
    Add "Greyscale" (Hex (Hex3 Lf Lf Lf)),
    CSS "Greyscale"
            ]

--Example 2 - Accessibility Aid
{-
    create Palette [Text: #eee, Paper: #aaa]
    contrastCheck Palette Text Paper
    shade #aaa
-}
-- Haskell
example2 :: Program
example2 = [
    Create "Palette" [RoleEntry "Text" (Hex (Hex3 UE UE Le)), RoleEntry "Paper" (Hex (Hex3 La UA UA))],
    ContrastCheck (RolePair "Palette" "Text" "Paper"),
    Transform Shade (Left (Hex (Hex3 La La La)))
            ]

--Example 3 Utilizing Parser
{-
    create pantone2026 [Cloud-Dancer: #F0EEE9, Nantucket-Breeze: #BCCFE7, Cosmic-Sky: #AAABC2, Alaskan-Blue: #FFFFFF, Regatta: #5479B2]
    update pantone2026 Alaskan-Blue #7BA7CE
    convert hex rgb pantone2026
    assign pantone2026 Alaskan-Complement complementary #5479B2
    print pantone2026
-}
-- Haskell
example3 :: String
example3 = unlines[
    "create pantone2026 [Cloud-Dancer: #F0EEE9, Nantucket-Breeze: #BCCFE7, Cosmic-Sky: #AAABC2, Alaskan-Blue: #FFFFFF, Regatta: #5479B2]",
    "update pantone2026 Alaskan-Blue #7BA7CE",
    "convert hex rgb pantone2026",
    "assign pantone2026 Alaskan-Complement complementary #5479B2",
    "print pantone2026"
                   ]

main :: IO ()
main = do
    putStrLn "--- Swatch Demo ---"

    putStrLn "\nExample 1 - Palette Creation:"
    mapM_ print example1
    
    putStrLn "\nExample 2 - Accessibility Aid:"
    mapM_ print example2

    putStrLn "\nExample 3 - Utilizing Parser:"
    case runSwatch example3 of
        Left err -> putStrLn $ "Parser Error: " ++ show err
        Right program -> mapM_ (putStrLn . show) program


{-
data Either a b = Left a | Right b

-}