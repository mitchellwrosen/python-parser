{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)

import Lexer (lexToken, runAlex)

main :: IO ()
main = do
    -- accept input via args or stdin, for convenience
    contents <- getArgs >>= \case
                    []   -> getContents
                    args -> return (unwords args)
    putStrLn $ "contents = " ++ contents
    print (runAlex contents lexToken)
