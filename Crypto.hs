module Crypto (caesar_encrypt, caesar_decrypt, vigenere_encrypt, vigenere_decrypt) where

import Data.Char

-- helping functions

aOrA :: Char -> Char
aOrA c 
 | c >= 'A' && c <= 'Z' = 'A' 
 | c >= 'a' && c <= 'z' = 'a'
 | otherwise = c


shiftLeftAndRigth :: [Char] -> Int -> Char -> [Char]
shiftLeftAndRigth [] n op = []
shiftLeftAndRigth xs n op
 | n < 0 = error "Não é permitido shift menor que 0"
 | op == '+' =  [chr (((ord c - ord (aOrA c) + n) `mod` 26) + ord (aOrA c)) | c <- xs]
 | op == '-' =  [chr (((ord c - ord (aOrA c) - n) `mod` 26) + ord (aOrA c)) | c <- xs]
 | otherwise = "Operação Inválida!"


-- Caesar Cipher

caesar_encrypt :: [Char] -> Int -> [Char]
caesar_encrypt xs n = shiftLeftAndRigth xs n '+'


caesar_decrypt :: [Char] -> Int -> [Char]
caesar_decrypt xs n = shiftLeftAndRigth xs n '-'


-- Vigenere Cipher

vigenere_encrypt :: [Char] -> [Char] -> [Char]
vigenere_encrypt [] key = []
vigenere_encrypt xs [] = []
vigenere_encrypt xs key = concat ([shiftLeftAndRigth [c] (ord k) '+' | (c,k)<- zip [c |c<-xs ] [k | k<-(take (length xs) (cycle key))]]) -- list comprehension com paralelismo ==  [x+y | x<-a | y<-b]


vigenere_decrypt :: [Char] -> [Char] -> [Char]
vigenere_decrypt [] key = []
vigenere_decrypt xs [] = []
vigenere_decrypt xs key = concat ([shiftLeftAndRigth [c] (ord k) '-' | (c,k)<- zip [c |c<-xs ] [k | k<-(take (length xs) (cycle key))]])


-- ADFGVX Cipher



