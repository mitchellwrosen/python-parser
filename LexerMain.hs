{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import System.Environment (getArgs)

import Lexer (lexToken, runAlex)

main :: IO ()
main = do
    -- accept input via args, interactive mode, or stdin
    getArgs >>= \case
        []     -> getContents >>= mainOneoff
        ["-i"] -> mainInteractive
        args   -> mainOneoff (unwords args)

mainOneoff :: String -> IO ()
mainOneoff = print . runAlex lexToken

mainInteractive :: IO ()
mainInteractive = loop $ getLineGuard "exit" >>= lift . print . runAlex lexToken

-- Get string input, but fail if it matches the passed string.
getLineGuard :: String -> MaybeT IO String
getLineGuard sentinel = do
    line <- lift getLine
    when (line == sentinel)
        exit
    return line

exit :: MonadPlus m => m a
exit = mzero

loop :: MaybeT IO a -> IO ()
loop = void . runMaybeT . forever
