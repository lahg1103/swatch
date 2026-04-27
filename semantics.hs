module Semantics where
import Syntax
import Debug.Trace (trace)
import Data.List (elemIndex)
import GHC.Exts.Heap (GenClosure(DoubleClosure))
evaluate :: Program -> Env
evaluate p = evaluateProg p []

evaluateProg :: [Statement] -> Env -> Env
evaluateProg [] env = env
evaluateProg (s : ss) env = let env' = evaluateStmt s env
                                in evaluateProg ss env'

evaluateStmt :: Statement -> Env -> Env
evaluateStmt (Create name entries) env = 
    case lookup name env of
        Just _ -> error $ "Palette '" ++ name ++ "' already exists."
        Nothing -> (name, entries) : env

evaluateStmt (Add name color) env =
    let entries = findPalette name env
        newEntries = entries ++ [ColorEntry color]
    in replacePalette name newEntries env

evaluateStmt (CSS name) env =
    let entries = findPalette name env
    in trace("/* CSS output for " ++ name ++ "*/\n" ++ show entries) env

-- helper
findPalette :: Name -> Env -> [Entry]
findPalette n env = case lookup n env of
    Nothing -> error $ "Palette '" ++ n ++ "' not found."
    Just entries -> entries

replacePalette :: Name -> [Entry] -> Env -> Env
replacePalette n newE [] = []
replacePalette n newE ((name, entries) : rest)
    | n == name = (name, newE) : rest
    | otherwise = (name, entries) : replacePalette n newE rest


--

--

--

-- math helpers

hexValueToInteger :: HexV -> Int
hexValueToInteger v = case v of
    Zero -> 0;
    One-> 1;
    Two -> 2;
    Three -> 3;
    Four -> 4;
    Five -> 5;
    Six -> 6;
    Seven -> 7;
    Eight -> 8;
    Nine -> 9;
    La -> 10; UA -> 10;
    Lb -> 11; UB -> 11;
    Lc -> 12; UC -> 12;
    Ld -> 13; UD -> 13;
    Le -> 14; UE -> 14;
    Lf -> 15; UF -> 15

valueToDouble :: Value -> Double
valueToDouble v = fromIntegral v / 255.0

doubleToValue :: Double -> Value
doubleToValue d = round (d * 255.0)

-- Takes (0-360, 0-100, 0-100) and creates valid HSLDouble
fromHSL :: Double -> Double -> Double -> HSLDouble
fromHSL h s l = HSLDouble (fmod h 360 ) (s / 100) (l / 100)

-- Takes (0-15) and creates a HexV
integerToHexValue :: Int -> HexV
integerToHexValue n = case n of
    0 -> Zero;
    1 -> One;
    2 -> Two;
    3 -> Three;
    4 -> Four;
    5 -> Five;
    6 -> Six;
    7 -> Seven;
    8 -> Eight;
    9 -> Nine;
    10 -> UA;
    11 -> UB;
    12 -> UC;
    13 -> UD;
    14 -> UE;
    15 -> UF;
    _ -> Zero -- fallback

--floating point modulo (because I just found out Haskell will only modulo when a value is an integeral)
fmod :: Double -> Double -> Double
fmod a b = a - b * fromIntegral (floor (a / b))



-- 

--

--

-- color math!

--convert a HexString (3 or 6 digits) to RGB values
hexToRGB :: HexString -> (Int, Int, Int)
hexToRGB (Hex3 r g b) = 
    let rVal = hexValueToInteger r * 17
        gVal = hexValueToInteger g * 17
        bVal = hexValueToInteger b * 17
    in (rVal, gVal, bVal)
hexToRGB (Hex6 r1 r2 g1 g2 b1 b2) = 
    let rVal = hexValueToInteger r1 * 16 + hexValueToInteger r2
        gVal = hexValueToInteger g1 * 16 + hexValueToInteger g2
        bVal = hexValueToInteger b1 * 16 + hexValueToInteger b2
    in (rVal, gVal, bVal)

--convert RGB values to HSL
rgbToHSL :: (Int, Int, Int) -> HSLDouble
rgbToHSL (rInt, gInt, bInt) = HSLDouble h s l
    where
        r = fromIntegral rInt / 255
        g = fromIntegral gInt / 255
        b = fromIntegral bInt / 255

        maxVal = max r (max g b)
        minVal = min r (min g b)
        delta = maxVal - minVal

        --lightness
        l = (maxVal + minVal) / 2

        --saturation
        s = if delta == 0 then 0
            else delta / (1 - abs (2 * l - 1))
        
        --hue
        h = fmod (if delta == 0 then 0
            else if maxVal == r then
                60 * fmod ((g - b) / delta) 6
            else if maxVal == g then 60 * (((b - r) / delta) + 2)
            else 60 * (((r - g) / delta) + 4)) 360

hslToRGB :: HSLDouble -> (Int, Int, Int)
hslToRGB (HSLDouble h s l) = (round ((r + m) * 255), round ((g + m) * 255), round ((b +m) * 255))
    where
        -- chroma
        c = (1 - abs (2 * l - 1)) * s
        x = c * (1 - abs ((fmod (h / 60) 2) - 1))
        -- lightness offset
        m = l - c / 2

        -- I'm using a formula I pulled from Google for all of these conversions, sorry!
        -- the hue is a circular color wheel from my understanding so the hue is actually in degrees from 0->360
        -- this is a step in converting that back to raw rgb values
        (r, g, b)
            | 0 <= h && h < 60 = (c, x, 0)
            | 60 <= h && h < 120 = (x, c, 0)
            | 120 <= h && h < 180 = (0, c, x)
            | 180 <= h && h < 240 = (0, x, c)
            | 240 <= h && h < 300 = (x, 0, c)
            | otherwise = (c, 0, x)

rgbToHex :: (Int, Int, Int) -> Color
rgbToHex (r, g, b) = Hex (Hex6 r1 r2 g1 g2 b1 b2)
    where
    -- breaks R G B values into two 4-bit nibbles (Result is 6 digit hex, I'm not bothering with 3 digit here)
        r1 = integerToHexValue (r `div` 16)
        r2 = integerToHexValue (r `mod` 16)
        g1 = integerToHexValue (g `div` 16)
        g2 = integerToHexValue (g `mod` 16)
        b1 = integerToHexValue (b `div` 16)
        b2 = integerToHexValue (b `mod` 16)

--

--

--

-- color math : use these functions



--everything to HSLDouble
colorToHSL :: Color -> HSLDouble
-- hsl 0-100 to HSLDouble
colorToHSL (HSL h s l) = fromHSL (fromIntegral h) (fromIntegral s) (fromIntegral l)

-- hex to HSLDouble using Hex -> RGB -> HSL o yea
colorToHSL (Hex h) = rgbToHSL (hexToRGB h)

-- rgb to HSLDouble (I want to avoid using the rgbToHSL when writing examples)
colorToHSL (RGB r g b) = rgbToHSL (r, g, b)


--everything to RGB
colorToRGB :: Color -> (Int, Int, Int)
-- rgb to rgb. Yes.
colorToRGB (RGB r g b) = (r, g, b)

-- hex
colorToRGB (Hex h) = hexToRGB h

-- this one is painful to look at, but I think this is the easiest way to standardize the input
colorToRGB (HSL h s l) = hslToRGB (colorToHSL (HSL h s l))

--everything to Hex
colorToHex :: Color -> Color
-- hex to hex (force 6 digit hex)
colorToHex (Hex h) = Hex h

-- rgb
colorToHex (RGB r g b) = rgbToHex (r, g, b)

-- hsl
colorToHex (HSL h s l) = rgbToHex (colorToRGB (HSL h s l))


--

--

--

-- color math : transformations

