import System.IO
import qualified Data.List as L
import qualified STHLib

contentToFloats :: String -> [Float]
contentToFloats str = map (\x -> read x :: Float) $ lines str

main = do
    content <- getContents
    putStrLn $ show $ contentToFloats content
