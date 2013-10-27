import System.IO

contentToFloats :: String -> [Float]
contentToFloats str = map (\x -> read x :: Float) $ lines str

main = do
    content <- getContents
    putStrLn $ show $ contentToFloats content
