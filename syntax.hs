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
