import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import Text.Printf
import qualified Data.List as L
import qualified STHLib


data Flag
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
    deriving (Eq,Ord,Enum,Bounded)
instance Show Flag where
    show Count = "Count"
    show Mean = "Mean"
    show Stddev = "Stddev"
    show Stderr = "Stderr"
    show Sum = "Sum"
    show Var = "Variance"
    show Min = "Min"
    show Q1 = "Q1"
    show Median = "Median"
    show Q3 = "Q3"
    show Max = "Max"
    show Help = "Help"
    show Summary = "Summary"
    show Complete = "Complete"
    show Transpose = "Transpose"

data OutputFormat = Normal | Transposed deriving (Eq, Show)

data Stat = Stat Flag (Maybe Float)
instance Show Stat where
    show (Stat _ Nothing) = ""
    show (Stat f (Just x)) = show f ++ ": " ++ show x ++ "\n"

data Stats = Stats OutputFormat [Stat]
instance Show Stats where
    show (Stats Normal (Stat _ (Just v):[])) = show v ++ "\n"
    show (Stats Normal x) = unlines [headers x, values x]
    show (Stats Transposed x) = x >>= show

headers :: [Stat] -> String
headers [] = ""
headers (Stat f _:xs) = printf "%10s" (show f) ++ headers xs

values :: [Stat] -> String
values [] = ""
values (Stat _ (Just v):xs) = printf "%10f" v ++ values xs

allStats = [Count, Mean, Stddev, Stderr, Sum, Var, Min, Q1, Median, Q3, Max]
summaryStats = [Min, Q1, Median, Q3, Max]

flags = [
        Option "nN" ["count"] (NoArg Count) "Display the count",
        Option "m" ["mean", "avg"] (NoArg Mean) "Display the mean",
        Option [] ["stddev", "sd"] (NoArg Stddev) "Display the standard deviation",
        Option [] ["stderr", "se", "sem"] (NoArg Stderr) "Display the standard error",
        Option "s" ["sum"] (NoArg Sum) "Display the sumatory",
        Option [] ["var", "variance"] (NoArg Var) "Display the variance",
        Option [] ["min"] (NoArg Min) "Display the minimun",
        Option [] ["q1"] (NoArg Q1) "Display the first quartile",
        Option [] ["median"] (NoArg Median) "Display the mediam",
        Option [] ["q3"] (NoArg Q3) "Display the third quartile",
        Option [] ["max"] (NoArg Max) "Display the maximun",
        Option [] ["summary"] (NoArg Summary) "Display the summary info",
        Option [] ["complete"] (NoArg Complete) "Display the complete info",
        Option [] ["transpose-output", "tn"] (NoArg Transpose) "Display the info in vertical mode",
        Option "h" ["help"] (NoArg Help) "Print the help message"
    ]

generateStatList :: [Flag] -> [Float] -> [Stat]
generateStatList [] content = []
generateStatList (Count:xs) content = Stat Count (Just $ STHLib.count content) : generateStatList xs content
generateStatList (Mean:xs) content = Stat Mean (Just $ STHLib.mean content) : generateStatList xs content
generateStatList (Stddev:xs) content = Stat Stddev (Just $ STHLib.stddev content) : generateStatList xs content
generateStatList (Stderr:xs) content = Stat Stderr (Just $ STHLib.stderr content) : generateStatList xs content
generateStatList (Sum:xs) content = Stat Sum (Just $ STHLib.sum content) : generateStatList xs content
generateStatList (Var:xs) content = Stat Var (Just $ STHLib.variance content) : generateStatList xs content
generateStatList (Min:xs) content = Stat Min (Just $ STHLib.min content) : generateStatList xs content
generateStatList (Q1:xs) content = Stat Q1 (Just $ STHLib.q1 content) : generateStatList xs content
generateStatList (Median:xs) content = Stat Median (Just $ STHLib.median content) : generateStatList xs content
generateStatList (Q3:xs) content = Stat Q3 (Just $ STHLib.q3 content) : generateStatList xs content
generateStatList (Max:xs) content = Stat Max (Just $ STHLib.max content) : generateStatList xs content
generateStatList (_:xs) content = generateStatList xs content

contentToStats :: String -> [Flag] -> Stats
contentToStats content flags = if Transpose `elem` flags
                                  then Stats Transposed (generateStatList flags values)
                                  else Stats Normal (generateStatList flags values)
                               where values = map read $ lines content

populateArgs :: [Flag] -> [Flag]
populateArgs [] = []
populateArgs (Summary:xs) = summaryStats ++ populateArgs xs
populateArgs (Complete:xs) = allStats ++ populateArgs xs
populateArgs (x:xs) = x : populateArgs xs

-- Mutable zone

parse :: [String] -> IO ([Flag], [String])
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

getData :: [String] -> IO String
getData files = if null files
                    then getContents
                    else readFiles files

main = do
    (as, fs) <- getArgs >>= parse
    content <- getData fs

    putStr $ show $ contentToStats content (if null as then allStats else as)
