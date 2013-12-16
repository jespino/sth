module STHLib where

import qualified Data.List as L

min::[Float] -> Float
min = foldl1 Prelude.min

q1::[Float] -> Float
q1 list = sortedList!!((length sortedList) `div` 4)
              where sortedList = L.sort list

median::[Float] -> Float
median list = sortedList!!((length sortedList) `div` 2)
                  where sortedList = L.sort list


q3::[Float] -> Float
q3 list = sortedList!!(((length sortedList) `div` 4) * 3)
              where sortedList = L.sort list

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
