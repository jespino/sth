import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import qualified Data.List as L
import qualified STHLib


data Flags
    = Count
    | Mean
    | Stddev
    | Stderr
    | Sum
    | Var
    | Min
    | Q1
    | Median
    | Q3
    | Max
    | Help
    | Summary
    | Complete
    | Transpose
    deriving (Eq,Ord,Enum,Show,Bounded)

allStats = [Count, Mean, Stddev, Stderr, Sum, Var, Min, Q1, Median, Q3, Max]
summaryStats = [Min, Q1, Median, Q3, Max]

flags =
    [Option "nN" ["count"]    (NoArg Count)
        "Display the count"
    ,Option "m" ["mean", "avg"]    (NoArg Mean)
        "Display the mean"
    ,Option [] ["stddev", "sd"]    (NoArg Stddev)
        "Display the standard deviation"
    ,Option [] ["stderr", "se", "sem"]    (NoArg Stderr)
        "Display the standard error"
    ,Option "s" ["sum"]    (NoArg Sum)
        "Display the sumatory"
    ,Option [] ["var", "variance"]    (NoArg Var)
        "Display the variance"
    ,Option [] ["min"]    (NoArg Min)
        "Display the minimun"
    ,Option [] ["q1"]    (NoArg Q1)
        "Display the first quartile"
    ,Option [] ["median"]    (NoArg Median)
        "Display the mediam"
    ,Option [] ["q3"]    (NoArg Q3)
        "Display the third quartile"
    ,Option [] ["max"]    (NoArg Max)
        "Display the maximun"
    ,Option [] ["summary"]    (NoArg Summary)
        "Display the summary info"
    ,Option [] ["complete"]    (NoArg Complete)
        "Display the complete info"
    ,Option [] ["transpose-output", "tn"]    (NoArg Transpose)
        "Display the info in vertical mode"
    ,Option "h" ["help"]    (NoArg Help)
        "Print the help message"
    ]

showStats :: [Float] -> [Flags] -> [String]
showStats _ [] = []
showStats list (Count:xs) = ("Count: " ++ show (STHLib.count list)) : showStats list xs
showStats list (Mean:xs) = ("Mean: " ++ show (STHLib.mean list)) : showStats list xs
showStats list (Stddev:xs) = ("Stddev: " ++ show (STHLib.stddev list)) : showStats list xs
showStats list (Stderr:xs) = ("Stderr: " ++ show (STHLib.stderr list)) : showStats list xs
showStats list (Sum:xs) = ("Sum: " ++ show (STHLib.sum list)) : showStats list xs
showStats list (Var:xs) = ("Variance: " ++ show (STHLib.variance list)) : showStats list xs
showStats list (Min:xs) = ("Min: " ++ show (STHLib.min list)) : showStats list xs
showStats list (Q1:xs) = ("Q1: " ++ show (STHLib.q1 list)) : showStats list xs
showStats list (Median:xs) = ("Median: " ++ show (STHLib.median list)) : showStats list xs
showStats list (Q3:xs) = ("Q3: " ++ show (STHLib.q3 list)) : showStats list xs
showStats list (Max:xs) = ("Max: " ++ show (STHLib.max list)) : showStats list xs
showStats list (Transpose:xs) = showStats list xs

showStatsHeaders :: [Flags] -> String
showStatsHeaders [] = ""
showStatsHeaders (Count:xs) = printf "%10s" "Count" ++ showStatsHeaders xs
showStatsHeaders (Mean:xs) = printf "%10s" "Mean" ++ showStatsHeaders xs
showStatsHeaders (Stddev:xs) = printf "%10s" "Stddev" ++ showStatsHeaders xs
showStatsHeaders (Stderr:xs) = printf "%10s" "Stderr" ++ showStatsHeaders xs
showStatsHeaders (Sum:xs) = printf "%10s" "Sum" ++ showStatsHeaders xs
showStatsHeaders (Var:xs) = printf "%10s" "Variance" ++ showStatsHeaders xs
showStatsHeaders (Min:xs) = printf "%10s" "Min" ++ showStatsHeaders xs
showStatsHeaders (Q1:xs) = printf "%10s" "Q1" ++ showStatsHeaders xs
showStatsHeaders (Median:xs) = printf "%10s" "Median" ++ showStatsHeaders xs
showStatsHeaders (Q3:xs) = printf "%10s" "Q3" ++ showStatsHeaders xs
showStatsHeaders (Max:xs) = printf "%10s" "Max" ++ showStatsHeaders xs
showStatsHeaders (Transpose:xs) = showStatsHeaders xs

showHStats :: [Float] -> [Flags] -> String
showHStats _ [] = ""
showHStats list (Count:xs) = printf "%10f" (STHLib.count list) ++ showHStats list xs
showHStats list (Mean:xs) = printf "%10f" (STHLib.mean list) ++ showHStats list xs
showHStats list (Stddev:xs) = printf "%10f" (STHLib.stddev list) ++ showHStats list xs
showHStats list (Stderr:xs) = printf "%10f" (STHLib.stderr list) ++ showHStats list xs
showHStats list (Sum:xs) = printf "%10f" (STHLib.sum list) ++ showHStats list xs
showHStats list (Var:xs) = printf "%10f" (STHLib.variance list) ++ showHStats list xs
showHStats list (Min:xs) = printf "%10f" (STHLib.min list) ++ showHStats list xs
showHStats list (Q1:xs) = printf "%10f" (STHLib.q1 list) ++ showHStats list xs
showHStats list (Median:xs) = printf "%10f" (STHLib.median list) ++ showHStats list xs
showHStats list (Q3:xs) = printf "%10f" (STHLib.q3 list) ++ showHStats list xs
showHStats list (Max:xs) = printf "%10f" (STHLib.max list) ++ showHStats list xs
showHStats list (Transpose:xs) = showHStats list xs

contentToFloats :: String -> [Float]
contentToFloats = map read . lines

-- Mutable zone

populateArgs :: [Flags] -> [Flags]
populateArgs [] = []
populateArgs (Summary:xs) = summaryStats ++ populateArgs xs
populateArgs (Complete:xs) = allStats ++ populateArgs xs
populateArgs (x:xs) = x : populateArgs xs

parse argv = case getOpt Permute flags argv of
    (args, fs, []) -> do
        let files = if null fs then [] else fs

        if Help `elem` args
            then do hPutStrLn stderr usage
                    exitSuccess
            else return (L.nub (populateArgs args), files)
    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usage)
        exitWith (ExitFailure 1)
    where header = "Usage: sth [options] [files]"
          usage = usageInfo header flags


readFiles :: [FilePath] -> IO String
readFiles = fmap concat . mapM readFile

getData files = if null files
                    then getContents
                    else readFiles files

main = do
    (as, fs) <- getArgs >>= parse
    content <- getData fs

    if Transpose `elem` as
        then putStr $ unlines $ showStats (contentToFloats content) (if null [ x | x <- as, x /= Transpose] then allStats else as)
        else do putStrLn $ showStatsHeaders (if null as then allStats else as)
                putStrLn $ showHStats (contentToFloats content) (if null as then allStats else as)
