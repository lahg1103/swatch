module Semantics where
import Syntax
import Debug.Trace (trace)
import Data.List (elemIndex)
import GHC.Exts.Heap (GenClosure(DoubleClosure))
import Data.Text.Array (new)
evaluate :: Program -> Env
evaluate p = evaluateProg p []

evaluateProg :: [Statement] -> Env -> Env
evaluateProg [] env = env
evaluateProg (s : ss) env = let env' = evaluateStmt s env
                                in evaluateProg ss env'


-- CRUD

evaluateStmt :: Statement -> Env -> Env
evaluateStmt (Create name entries) env = 
    case lookup name env of
        Just _ -> error $ "Palette '" ++ name ++ "' already exists."
        Nothing -> (name, entries) : env

evaluateStmt (Add name color) env =
    let entries = findPalette name env
        newEntries = entries ++ [ColorEntry color]
    in replacePalette name newEntries env

evaluateStmt (Update name target newColor) env =
    let entries = findPalette name env
        updatedEntries = map (updateEntry target newColor) entries
    in replacePalette name updatedEntries env
    where
        -- color
        updateEntry (Left oldColor) new (ColorEntry c)
            | c == oldColor = ColorEntry new
        -- role 
        updateEntry (Right targetRole) new (RoleEntry r c)
            | r == targetRole = RoleEntry r new
        -- if no match:
        updateEntry _ _ entry = entry

--remove whole palette
evaluateStmt (Remove name Nothing) env =
    filter (\(n, _)-> n /= name) env

--remove specific color
evaluateStmt (Remove name (Just target)) env =
    let entries = findPalette name env
        newEntries = filter (\entry -> case entry of
            ColorEntry c -> c /= target
            RoleEntry _ c -> c /= target) entries
    in replacePalette name newEntries env


-- color conversion

evaluateStmt (Convert _ RGBMode (Left color)) env =
    let (r, g, b) = colorToRGB color
        rgbColor = RGB r g b
    in trace (" Converted " ++ show color ++ " to RGB: " ++ show rgbColor ++ "*/") env

evaluateStmt (Convert _ HexMode (Left color)) env =
    let hexColor = colorToHex color
    in trace (" Converted " ++ show color ++ " to Hex: " ++ show hexColor ++ "*/") env

evaluateStmt (Convert _ mode target) env =
    case target of
        -- single color
        Left c -> 
            let converted = convertToMode mode c
            in trace ("Converted color: " ++ show converted ++ "\n") env
            
        -- entire palette
        Right name ->
            let entries = findPalette name env
                -- Map the conversion over every entry in the palette
                newEntries = map (\entry -> case entry of
                    ColorEntry c -> ColorEntry (convertToMode mode c)
                    RoleEntry r c -> RoleEntry r (convertToMode mode c)) entries
            in replacePalette name newEntries env

evaluateStmt (Print name) env =
    let entries = findPalette name env
        formatted = unlines $ map (\e -> " " ++ show e) entries
    in trace ("\n Palette: " ++ name ++ "\n" ++ formatted) env

evaluateStmt (CSS name) env =
    let entries = findPalette name env
        output = "CSS output for " ++ name ++ "\n" ++ 
                ":root {\n" ++ formatCSS name entries ++ "}"
    in trace output env


-- transform

evaluateStmt (Transform trans target) env = 
    let colors = resolveTarget target env
        results = applyTransform trans colors
    in trace (show trans ++ " result: " ++ show results ) env


-- contrast check

evaluateStmt (ContrastCheck target) env =
    let (c1, c2) = case target of 
            ColorPair a b -> (a, b)
            RolePair name r1 r2 ->
                let entries = findPalette name env
                    findRole r = head [c | RoleEntry role c <- entries, role == r]
                in (findRole r1, findRole r2)
        ratio = calculateContrast c1 c2
        status = if ratio > 4.5 then "PASS (AA)" else "FAIL"
    in trace ("Contrast Ratio for " ++ show c1 ++ " and " ++ show c2 ++ ": " ++ show (fromIntegral (round (ratio * 100)) / 100.0) ++ " : 1 [" ++ status ++ "]\n") env


-- assign

evaluateStmt (Assign name role target) env = 
    let entries = findPalette name env
        newColors = case target of
            Direct c -> [c]
            Computed trans t -> applyTransform trans (resolveTarget t env)
        
        -- create new entries to palette
        newEntries = case newColors of
            [single] -> [RoleEntry role single]
            multiple -> zipWith (\i c -> RoleEntry (role ++ "-" ++ show i) c) [1..] multiple
            
    in replacePalette name (entries ++ newEntries) env



--


--


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

formatCSS :: Name -> [Entry] -> String
formatCSS paletteName entries = concatMap formatEntry (zip [1..] entries)
    where
        formatEntry (i, ColorEntry c) =
            " --" ++ paletteName ++ "-" ++ show i ++ ": " ++ show c ++ ";\n"
        formatEntry (_, RoleEntry role c) =
            " --" ++ role ++ ": " ++ show c ++ ";\n"

resolveTarget :: Either Color Name -> Env -> [Color]
resolveTarget (Left c) _ = [c]
resolveTarget (Right name) env =
    map (\entry -> case entry of ColorEntry c -> c; RoleEntry _ c -> c) (findPalette name env)

applyTransform :: Transform -> [Color] -> [Color]
applyTransform trans colors = concatMap go colors
    where
        go c = case trans of
            Shade -> [hslDoubleToColor (colorToShade (colorToHSL c))]
            Tint -> [hslDoubleToColor (colorToTint (colorToHSL c))]
            Complementary -> [hslDoubleToColor (colorToComplementary (colorToHSL c))]
            Tertiary -> [hslDoubleToColor (colorToTertiary (colorToHSL c))]
            Analogous -> colorToAnalogous c
            Triadic -> colorToTriadic c

-- contrast check
-- formula from WCAG

calculateContrast :: Color -> Color -> Double
calculateContrast c1 c2 =
    let l1 = luminance c1
        l2 = luminance c2
    in (max l1 l2 + 0.05) / (min l1 l2 + 0.05)

-- apparently luminance used for the WCAG formula is different from the l in hsl.... :((
luminance :: Color -> Double
luminance c =
    let (r, g, b) = colorToRGB c
        [dr, dg, db] = map (\v -> fromIntegral v / 255) [r, g, b]
    in 0.2126 * dr + 0.7152 * dg + 0.0722 * db
        

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

-- Takes HSLDouble and creates a Color
hslDoubleToColor :: HSLDouble -> Color
hslDoubleToColor (HSLDouble h s l) =
    -- turn from decimal double to percent!
    HSL (round h) (round (s * 100)) (round (l * 100))

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


--convert a color to specific mode
convertToMode :: Mode -> Color -> Color
convertToMode HexMode c = colorToHex c
convertToMode RGBMode c = 
    let (r, g, b) = colorToRGB c
    in RGB r g b
convertToMode HSLMode c =
    hslDoubleToColor (colorToHSL c)


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

-- <transform> -> "shade" | "tint" | "complementary" | "tertiary" | "analogous" | "triadic"

-- shade - shades are darker versions of the color
colorToShade :: HSLDouble -> HSLDouble
colorToShade (HSLDouble h s l) =
        -- turn hsl lightness down by 10%, hue shift by -5 deg, saturation up by 8%
        -- modulus in case a color shifts past 0 deg or 360 deg
        HSLDouble ( fmod (h - 5) 360) (min 1.0 (s + 0.08)) (max 0.0 (l - 0.30))


-- tint - tints are lighter versions of the color
colorToTint :: HSLDouble -> HSLDouble
colorToTint (HSLDouble h s l) = 
        -- turn hsl lightness up by 10%, hue shift by 5 deg, saturation down by 8%
        HSLDouble (fmod (h + 5) 360) (max 0.0 (s - 0.08)) (min 1.0 (l + 0.30))


-- complementary - complementary colors are opposite on the color wheel
colorToComplementary :: HSLDouble -> HSLDouble
colorToComplementary (HSLDouble h s l) =
    -- spits out hex, so it's wrapper in colorToHex, but the math turns the hue 180 around, modulus in case it goes above 360, you know the drill
    HSLDouble (fmod (h + 180) 360) s l


-- tertiary - tertiary colors are one third of the way away on the color wheel,,, what?? between complementary and the color. that midpoint.
colorToTertiary :: HSLDouble -> HSLDouble
colorToTertiary (HSLDouble h s l) =
    -- spits out hex, so it's wrapper in colorToHex, but the math turns the hue 120 around, modulus in case it goes above 360, you know the drill
        HSLDouble (fmod (h + 60) 360) s l

-- analogous - returns a color list of original color, +30deg tint, and -30deg shade
colorToAnalogous :: Color -> [Color]
colorToAnalogous c = 
    let (HSLDouble h s l) = colorToHSL c
        c1 = c
        c2 = hslDoubleToColor (colorToTint (HSLDouble (fmod (h + 30) 360) s l ))
        c3 = hslDoubleToColor (colorToShade (HSLDouble (fmod (h - 30) 360) s l ))
    in [c1, c2, c3]


-- triadic - returns a color list of original color, +120 deg, +240 deg
colorToTriadic :: Color -> [Color]
colorToTriadic c =
    let (HSLDouble h s l) = colorToHSL c
        c1 = c
        c2 = hslDoubleToColor (HSLDouble (fmod (h + 120) 360) s l)
        c3 = hslDoubleToColor (HSLDouble (fmod (h + 240) 360) s l)
    in [c1, c2, c3]
