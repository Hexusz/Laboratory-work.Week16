module Main where

import Lib
import Data.Foldable (find)
import Data.Maybe
main :: IO ()

f1 :: (Num a, Ord a) => a -> a -> a -> a
f1 x y z = minimum [x,y,z]^2 + maximum [x,y,z]^2 --Вычисляет сумму квадратов двух наибольших чисел из набора

f2 :: (Num a, Enum a) => a -> a
f2 x = sum (map (^2)[1..x]) --возвращает сумму квадратов всех чисел в диапазоне 1 .. N

f3 :: (Num a, Ord a, Enum a) => a -> a
f3 n | n == 0 = 1 | n > 0 = product [1..n] --вычисляет факториал следующим образом fact n = 1 * 2 * 3 * n

f4 :: (Ord a, Fractional a, Enum a) => a -> a -> a
f4 n m | n>0 && m>0 && n > m = f3(n)/(f3(m)*f3(n-m)) --возвращает количество комбинаций объекта n взятых m раз

f5 :: Integral a => a -> a -> a
f5 x 0 = x
f5 x y | x > 0 && y > 0 = f5 y( x `mod` y) --возвращает наибольший общий делитель от x и y

f6 :: Int -> Bool
f6 1 = False
f6 x | x > 0 =  not(or(map (\y -> (x `mod` y)==0)  [2..x-1])) --Определение простых чисел

f7 :: Int -> Bool
f7 x | x >= 0 = sum(filter(\y -> (x `mod` y)==0)[1..x-1])==x --Определение совершенного числа

f8 :: Int -> Bool
f8 x | x >= 0 = sum(filter(\y -> (x `mod` y)==0)[1..x-1])>x --Определение избыточного числа

f9 :: Int -> Int -> Bool
f9 x z | x >= 0 && z >= 0 = sum(filter(\y -> (x `mod` y)==0)[1..x-1])==z && sum(filter(\y -> (z `mod` y)==0)[1..z-1])==x --Определение дружественных чисел

f10 :: Integral a => a -> a -> a -> a
f10 x y z = fromMaybe 0 (find (\a -> a `mod` x==0 && a `mod` y==0 && a `mod` z==0)[maximum[x,y,z],maximum[x,y,z]*2..]) --Наименьшее общего кратного для 3 чисел

main = do
  print "1 - First task"
  print  (f1 1 2 3)
  print "2 - Second task"
  print  (f2 3)
  print "3 - Third task"
  print  (f3 5)
  print "4 - Fourth task"
  print  (f4 4 2)
  print "5 - Fifth task"
  print  (f5 10 6)
  print "6 - Sixth task"
  print  (f6 479 )
  print "7 - Seventh task"
  print  (f7 6 )
  print "8 - Eighth task"
  print  (f8 12 )
  print "9 - Ninth task"
  print  (f9 220 284)
  print "10 - Tenth task"
  print  (f10 22 33 44)
  return()
