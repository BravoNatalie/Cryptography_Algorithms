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


-- Vignere Cipher

toNum :: [Char] -> [Int]
toNum [] = []
toNum xs = map ord xs


vignere_encrypt :: [Char] -> [Char] -> [Char]
vignere_encrypt [] key = []
vignere_encrypt xs [] = []
vignere_encrypt xs key = map chr (zipWith (+) (toNum xs) (toNum (take (length xs) (cycle key))) )

zipWith (+) "abc" (take 3 (cycle "a"))

vignere_encrypt :: [Char] -> [Char] -> [Char]
vignere_encrypt [] key = []
vignere_encrypt xs [] = []
vignere_encrypt xs key = map chr ([shiftLeftAndRigth xs (ord k) '+' | k<-(take (length xs) (cycle key))]) 

