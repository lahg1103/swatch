{-
    course: CSCI3336
    name: Leslie A. Hurtado
    project: Swatch - Midterm Language Design Project

    Context Free Grammar:
    <Program> -> <statements>
    <statements> -> <statement> | <statement> <statements>
    <statement> ->
                    "create palette" <name> "["<entries>"]"
                  | "add" <name> <color>
                  | "remove" <name> <color> | "remove" <name>
                  | "update" <name> <color>
                  | "convert" <mode> <mode> <color> <color>

                  | <transform> <target>

                  | "assign" <name> <role> <color>
                  | "contrastCheck" <color> <color> | "contrastCheck" <name> <role> <role>
                  | "css" <name>
                  | "print" <name>

    <transform> -> "shade" | "tint" | "complementary" | "tertiary" | "analogous" | "triadic"
    <target> -> <color> | <name>
    <mode> -> "hex" | "rgb" | "hsl"

    <name> -> <String>
    <role> -> <String>
    <String> -> <char> | <char> <String>
    
    <entries> -> <entry> | <entry> "," <entries>
    <entry> -> <color> | <role> ":" <color>
    <color> -> "rgb" <rgb> | "hsl" <hsl> | "#" <hex>

    <rgb> -> "(" <ℤ+> "," <ℤ+> "," <ℤ+> ")" | <ℤ+> "," <ℤ+> "," <ℤ+>
    <hsl> -> "(" <ℤ+> "," <ℤ+> "," <ℤ+> ")" | <ℤ+> "," <ℤ+> "," <ℤ+>

    <hex> -> <hexDigit><hexDigit><hexDigit> | <hexDigit><hexDigit><hexDigit><hexDigit><hexDigit><hexDigit>
    <hexDigit> -> [0-9] | [a-f] | [A-F]
-}                 
import Syntax

--Example 1 - Palette creation
{- 
    create Greyscale [Foreground: #000, #ccc, Background: #eee]
    add Greyscale #fff
    css Greyscale
-}
-- Haskell
example1 :: Program
example1 = [
    Create "Greyscale" [RoleEntry "Foreground" (Hex "000"), ColorEntry (Hex "ccc"), RoleEntry "Background" (Hex "eee")],
    Add "Greyscale" (Hex "fff"),
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
    Create "Palette" [RoleEntry "Text" (Hex "eee"), RoleEntry "Paper" (Hex "#aaa")],
    ContrastCheck (RolePair "Palette" "Text" "Paper"),
    Transform Shade (Left (Hex "#aaa"))
            ]

main :: IO ()
main = do
    putStrLn "--- Swatch Demo ---"

    putStrLn "\nExample 1 - Palette Creation:"
    mapM_ print example1
    
    putStrLn "\nExample 2 - Accessibility Aid:"
    mapM_ print example2