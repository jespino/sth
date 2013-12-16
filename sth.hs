import System.IO
import qualified Data.List as L

listMin::[Float] -> Float
listMin = foldl1 min

listQ1::[Float] -> Float
listQ1 list = sortedList!!((length sortedList) `div` 4)
              where sortedList = L.sort list

listMedian::[Float] -> Float
listMedian list = sortedList!!((length sortedList) `div` 2)
                  where sortedList = L.sort list


listQ3::[Float] -> Float
listQ3 list = sortedList!!(((length sortedList) `div` 4) * 3)
              where sortedList = L.sort list

listMax::[Float] -> Float
listMax = foldl1 max

listCount::[Float] -> Float
listCount = fromIntegral . length

listMean::[Float] -> Float
listMean list = (sum list) / (listCount list)

listStddev::[Float] -> Float
listStddev = sqrt . listVariance

listStderr::[Float] -> Float
listStderr list = listStddev list / (sqrt (listCount list))

listSum::[Float] -> Float
listSum = sum

listVariance::[Float] -> Float
listVariance list = (1 / (listCount list)) * (sum listWithoutMean)
                    where mean = listMean list
                          listWithoutMean = map (\x -> (x - mean)^2) list

contentToFloats :: String -> [Float]
contentToFloats str = map (\x -> read x :: Float) $ lines str

main = do
    content <- getContents
    putStrLn $ show $ contentToFloats content
