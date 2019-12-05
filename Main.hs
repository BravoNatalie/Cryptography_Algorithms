
import Crypto

-- Main

main :: IO ()
main = do
  putStrLn "Select a mode:"
  putStrLn "1 - Encrypt"
  putStrLn "2 - Decrypt"
  mode <- getLine
  putStrLn "Select a cipher:"
  putStrLn "1 - Caesar"
  putStrLn "2 - Vigenere"
  putStrLn "3 - ADFGVX"
  opt <- getLine
  if opt == "1"
    then do caesar mode
    else do vigenere mode

caesar :: [Char] -> IO ()
caesar mode = do
  putStrLn "Enter the text:"
  text <- getLine
  putStrLn "Enter the shift value:"
  val <- getLine
  if mode == "1"
    then do
      putStrLn "[ENCRYPT]"
      putStrLn ("#: " ++ (caesar_encrypt text (read val :: Int)))
    else do
      putStrLn "[DECRYPT]"
      putStrLn ("#: " ++ (caesar_decrypt text (read val :: Int)))
  putStrLn " "
  putStrLn "-------------------------"
  putStrLn "Want to try again? "
  main

vigenere :: [Char] -> IO ()
vigenere mode = do
  putStrLn "Enter the text:"
  text <- getLine
  putStrLn "Enter the shift value:"
  val <- getLine
  if mode == "1"
    then do
      putStrLn "[ENCRYPT]"
      putStrLn ("#: " ++ (vigenere_encrypt text val))
    else do
      putStrLn "[DECRYPT]"
      putStrLn ("#: " ++ (vigenere_decrypt text val))
  putStrLn " "
  putStrLn "-------------------------"
  putStrLn "Want to try again? "
  main
