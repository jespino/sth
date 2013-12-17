import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
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
    deriving (Eq,Ord,Enum,Show,Bounded)

allStats = [Count, Mean, Stddev, Stderr, Sum, Var, Min, Q1, Median, Q3, Max]

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

contentToFloats :: String -> [Float]
contentToFloats = (map read) . lines

-- Mutable zone

parse argv = case getOpt Permute flags argv of
    (args, fs, []) -> do
        let files = if null fs then [] else fs
        if Help `elem` args
            then do hPutStrLn stderr (usageInfo header flags)
                    exitSuccess
            else return (L.nub args, files)
    (_, _, errs) -> do
        hPutStrLn stderr (concat errs ++ usageInfo header flags)
        exitWith (ExitFailure 1)
    where header = "Usage: sth [options] [files]"


readFiles :: [FilePath] -> IO String
readFiles = fmap concat . mapM readFile

getData files = if null files
                    then getContents
                    else readFiles files

main = do
    (as, fs) <- getArgs >>= parse
    content <- getData fs

    putStr $ unlines $ showStats (contentToFloats content) (if null as then allStats else as)
