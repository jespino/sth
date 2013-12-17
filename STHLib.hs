module STHLib where

import qualified Data.List as L

min::[Float] -> Float
min = foldl1 Prelude.min

quartile:: Int -> [Float] -> Float
quartile n list= sortedList !! ((length list `div` 4) * n)
                  where sortedList = L.sort list

q1::[Float] -> Float
q1 = quartile 1

median::[Float] -> Float
median = quartile 2

q3::[Float] -> Float
q3 = quartile 3

max::[Float] -> Float
max = foldl1 Prelude.max

count::[Float] -> Float
count = fromIntegral . length

mean::[Float] -> Float
mean list = (Prelude.sum list) / (count list)

stddev::[Float] -> Float
stddev = sqrt . variance

stderr::[Float] -> Float
stderr list = stddev list / (sqrt (count list))

sum::[Float] -> Float
sum = Prelude.sum

variance::[Float] -> Float
variance list = (1 / (count list)) * (Prelude.sum listWithoutMean)
                    where listMean = mean list
                          listWithoutMean = map (\x -> (x - listMean)^2) list
