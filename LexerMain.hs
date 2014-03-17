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
mainInteractive = while (getLine != "exit") $ print . runAlex lexToken

(!=) :: (Monad m, Eq a) => m a -> a -> MaybeT m a
action != sentinel = do
    line <- lift action
    when (line == sentinel)
        mzero
    return line

-- Run a possibly failing action, use its result to run a second action.
-- Think of the first action as the loop "condition", and the second action
-- as the loop "body" that uses the result of the condition.
while :: (Functor m, Monad m) => MaybeT m a -> (a -> m b) -> m ()
while act1 act2 = void . runMaybeT . forever $ act1 >>= lift . act2
