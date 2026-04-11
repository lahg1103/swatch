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
module Syntax where
import Data.List (intercalate)


type Program = [Statement]

--types
type Name = String
type Role = String
type Value = Int

data Statement = Create Name [Entry]
                | Add Name Color
                | Remove Name (Maybe Color)
                | Update Name (Either Color Role) Color
                | Convert Mode Mode (Either Color Name)
                | Assign Name Role AssignTarget
                | Transform Transform (Either Color Name)
                | ContrastCheck ContrastTarget
                | CSS Name
                | Print Name

data Entry = ColorEntry Color | RoleEntry Role Color
data Color = Hex String | RGB Value Value Value | HSL Value Value Value

data Transform = Shade | Tint | Complementary | Tertiary | Analogous | Triadic
data Target = ColorTarget Color | NameTarget Name

data AssignTarget = Direct Color
                  | Computed Transform (Either Color Name) 

data ContrastTarget = ColorPair Color Color
                    | RolePair Name Role Role

data Mode = HexMode | RGBMode | HSLMode

instance Show Statement where
    show (Create n entries) = "create " ++ n ++ " [" ++ intercalate ", " (map show entries) ++ "]"
    show (Add n c) = "add " ++ n ++ " " ++ show c
    show (Remove n maybeC) = "remove " ++ n ++ maybe ( "" ) (\c -> " " ++ show c ) maybeC
    show (Update n t c) = "update " ++ n ++ " " ++ (either show id t) ++ " " ++ show c
    show (Convert m1 m2 t) = "convert " ++ show m1 ++ " " ++ show m2 ++ " " ++ (either show id t)
    show (Transform t target) = show t ++ " " ++ showTarget target
        where
            showTarget (Left color) = show color
            showTarget (Right name) = name
    show (Assign n r (Direct c)) = "assign " ++ n ++ " " ++ r ++ " " ++ show c
    show (Assign n r (Computed t target)) = "assign " ++ n ++ " " ++ r ++ " " ++ show t ++ " " ++ (either show id target)
    show (ContrastCheck ct) = "contrastCheck " ++ show ct
    show (CSS n) = "css " ++ n
    show (Print n) = "print " ++ n

instance Show Color where
    show (Hex s) = "#" ++ filter (/= '#') s
    show (RGB r g b) = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"
    show (HSL h s l) = "hsl(" ++ show h ++ "," ++ show s ++ "," ++ show l ++ ")"

instance Show Mode where
    show HexMode = "hex"
    show RGBMode = "rgb"
    show HSLMode = "hsl"

instance Show Transform where
    show Shade = "shade"
    show Tint = "tint"
    show Complementary = "complementary"
    show Tertiary = "tertiary"
    show Analogous = "analogous"
    show Triadic = "triadic"

instance Show Target where
    show (ColorTarget c) = show c
    show (NameTarget n) = n

instance Show ContrastTarget where
    show (ColorPair c1 c2) = show c1 ++ " " ++ show c2
    show (RolePair n r1 r2) = n ++ " " ++ r1 ++ " " ++ r2

instance Show Entry where
    show (ColorEntry c) = show c
    show (RoleEntry r c) = show r ++ ": " ++ show c
