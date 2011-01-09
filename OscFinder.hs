import Data.Ord
import System.IO
import System.CPUTime
import Parse
import Optimize

phaseShift :: [Double] -> (Double, [Double])
phaseShift x = (1/(15.390597961942367 * head x * (x !! 1)), x)

main = do
        hSetBuffering stdout NoBuffering
        putStrLn "Welcome to OscFinder!"
        putStrLn "Please select an oscillator design"
        putStrLn "a) Phase-shift oscillator"
        putStr "Enter choice here [a]: "
        oscchoice <- getLine
        let calculate = case oscchoice of
                            "a" -> phaseShift
        let e24 = [10, 11, 12, 13, 15, 16, 18, 20, 22, 24, 27, 30, 33, 36, 39, 43, 47, 51, 56, 62, 68, 75, 82, 91]
        let e12 = [10, 12, 15, 18, 22, 27, 33, 39, 47, 56, 68, 82]
        let e6 = [10, 15, 22, 33, 47, 68]
        putStrLn "Please choose a precision level"
        putStrLn "a) E24 (5%, most common)"
        putStrLn "b) E12 (10%)"
        putStrLn "c) E6 (20%)"
        putStr "Enter choice here [a, b, c]: "
        precchoice <- getLine
        let (list, tol) = case precchoice of
                        "a" -> (e24, " Gold.")
                        "b" -> (e12, " Silver.")
                        "c" -> (e6, " Blank.")
                        otherwise -> ([],"")
        if list == [] then error "Invalid answer" else return ()
        putStrLn "For the following inputs, an input of x translates to 10^x"
        putStr "Please enter a lower bound for resistors: "
        alowerraw <- getLine
        let alower = 10 ** (read alowerraw::Double)
        putStr "Please enter an upper bound for resistors: "
        aupperraw <- getLine
        let aupper = 10 ** (read aupperraw::Double)
        putStr "Please enter a lower bound for capacitors: "
        blowerraw <- getLine
        let blower = 10 ** (read blowerraw::Double)
        putStr "Please enter an upper bound for capacitors: "
        bupperraw <- getLine
        let bupper = 10 ** (read bupperraw::Double)
        putStr "Please enter the desired frequency (Hz): "
        freqraw <- getLine
        let freq = read freqraw::Double
        a <- getCPUTime
        let results = findBest [(alower, aupper), (blower, bupper)] list phaseShift freq
        let result = head results
        let resultDiff = (fst result)
        let resultFreq = freq + resultDiff
        let resultErr = (resultDiff / freq) * 100.0
        let resultA = snd result !! 0
        let resultB = snd result !! 1
        let ax = round (fst (decomp resultA) * (fromIntegral 10 ^ fst (prefixParse resultA)))
        let ay = snd (prefixParse resultA)
        let bx = round ((fst (decomp resultB)) * (fromIntegral 10 ^ fst (prefixParse resultB)))
        let by = snd (prefixParse resultB)
        putStrLn ("Use a resistor of " ++ show ax ++ " " ++ ay ++ "ohms, which has a color code of: " ++ (colorParse resultA) ++ tol)
        putStrLn ("Use a capacitor of " ++ show bx ++ " " ++ by ++ "farads.")
        putStrLn ("The frequency of the resulting oscillator would be " ++ show (round resultFreq) ++ " Hz.")
        putStrLn ("This differs from the target frequency by " ++ show (round resultDiff) ++ " Hz, or " ++ take 5 (show resultErr) ++ "%.")
        b <- getCPUTime
        let c = fromIntegral (b - a)/1000000000000
        let cx = round ((fst (decomp c)) * (fromIntegral 10 ^ fst (prefixParse c)))
        let cy = snd (prefixParse c)
        putStrLn ("Took: " ++ show cx ++ " " ++ cy ++ "seconds.")