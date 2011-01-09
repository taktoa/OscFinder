module Parse where

decompCore :: Double -> Int -> (Double, Int)
decompCore x y
        | x >= 10.0                 = decompCore (x / 10.0) (y + 1)
        | x < 1.0                   = decompCore (x * 10.0) (y - 1)
        | otherwise                 = (x, y)

decomp x = decompCore x 0

recomp (x, y) = x * (10 ** y)

prefixParse :: Double -> (Int, String)
prefixParse x = (a, b)
        where
        c = (snd (decomp x))
        a = mod c 3
        b = case (c - a) of
                -12 -> "pico"
                -9 -> "nano"
                -6 -> "micro"
                -3 -> "milli"
                0 -> ""
                3 -> "kilo"
                6 -> "mega"

colorParseCore :: Char -> String
colorParseCore x = case x of
                    '0' -> "Black"
                    '1' -> "Brown"
                    '2' -> "Red"
                    '3' -> "Orange"
                    '4' -> "Yellow"
                    '5' -> "Green"
                    '6' -> "Blue"
                    '7' -> "Violet"
                    '8' -> "Gray"
                    '9' -> "White"

colorParse :: Double -> String
colorParse x = a ++ " " ++ b ++ " " ++ c
        where
        m = decomp x
        n = colorParseCore (head (show (snd m - 1)))
        o = round (10 * fst m)
        (p, q) = ((show o) !! 0, (show o) !! 1)
        (r, s) = (colorParseCore p, colorParseCore q)
        (a, b, c) = (r, s, n)