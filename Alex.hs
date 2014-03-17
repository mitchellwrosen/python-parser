{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Alex where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Word           (Word8)

import LexerUtils

-- -----------------------------------------------------------------------------
-- The Lexeme type

data Lexeme = Lexeme
    { lexemePosn  :: AlexPosn
    , lexemeClass :: !LexemeClass
    , lexemeStr   :: String
    } deriving (Eq, Show)

data LexemeClass
    -- Indentation
    = LIndent | LDedent
    -- Identifiers
    | LIdent
    -- Keywords
    | LAnd    | LAs   | LAssert  | LBreak | LClass | LContinue | LDef    | LDelete | LElif  | LElse
    | LExcept | LExec | LFinally | LFor   | LFrom  | LGlobal   | LIf     | LImport | LIn    | LIs
    | LLambda | LNot  | LOr      | LPass  | LPrint | LRaise    | LReturn | LTry    | LWhile | LWith
    | LYield
    -- String Literals
    | LStringLiteral
    -- EOF
    | LEOF
    deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- The Alex monad

type StartCode = Int
type Column    = Int

data AlexState = AlexState
    { alexStateInput          :: AlexInput
    , alexStateStartCodeStack :: [StartCode]      -- the current stack of startcodes
    , alexStateIndentStack    :: [Column]         -- the current stack of indentation columns
    }

initialAlexState :: String -> AlexState
initialAlexState input = AlexState
    { alexStateInput          = initAlexInput input
    , alexStateStartCodeStack = [0] -- [bol,0]
    , alexStateIndentStack    = [0]
    }

-- Compile with -funbox-strict-fields for best results!

newtype Alex a = Alex { unAlex :: StateT AlexState (Either String) a }
               deriving (Functor, Applicative, Monad, MonadState AlexState)

runAlex :: Alex a -> String -> Either String a
runAlex (Alex s) = evalStateT s . initialAlexState

alexError :: String -> Alex a
alexError = Alex . StateT . const . Left

alexGetInput :: Alex AlexInput
alexGetInput = gets alexStateInput

alexSetInput :: AlexInput -> Alex ()
alexSetInput input = modify (\s -> s { alexStateInput = input })

alexGetStartCode :: Alex StartCode
alexGetStartCode = head <$> gets alexStateStartCodeStack

alexPopStartCode :: Alex StartCode
alexPopStartCode = do
    oldState <- get
    let (s:ss)   = alexStateStartCodeStack oldState
        newState = oldState { alexStateStartCodeStack = ss }
    put newState
    return s

alexPushStartCode :: StartCode -> Alex ()
alexPushStartCode n = modify (\s -> s { alexStateStartCodeStack = n : (alexStateStartCodeStack s) })

alexGetIndentation :: Alex Column
alexGetIndentation = head <$> gets alexStateIndentStack

-- -----------------------------------------------------------------------------
-- The input type

data AlexInput = AlexInput
    { alexInputPosn         :: AlexPosn  -- current position
    , alexInputPrevChar     :: !Char     -- previous char
    , alexInputPendingBytes :: [Word8]   -- pending bytes on current char
    , alexInputString       :: String    -- current input string
    }

initAlexInput :: String -> AlexInput
initAlexInput input = AlexInput
    { alexInputPosn         = alexStartPos
    , alexInputPrevChar     = '\n'
    , alexInputPendingBytes = []
    , alexInputString       = input
    }

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (AlexInput p c _ s) = AlexInput p c [] s

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput p c (b:bs) s)     = Just (b, AlexInput p c bs s)
alexGetByte (AlexInput p c []     [])    = Nothing
alexGetByte (AlexInput p _ []     (c:s)) = p' `seq`  Just (b, AlexInput p'  c  bs  s)
  where
    p' :: AlexPosn
    p' = alexMovePos p c

    (b:bs) = utf8Encode c

-- -----------------------------------------------------------------------------
-- Token positions

-- `AlexPosn' records the location of a token in the input text.
data AlexPosn = AlexPosn
    { alexPosnAddress :: !Int  -- Number of characters preceding the token
    , alexPosnLine    :: !Int  -- Line number
    , alexPosnCol     :: !Int  -- Column number
    } deriving (Eq, Show)

-- The position of the start of the file.
alexStartPos :: AlexPosn
alexStartPos = AlexPosn 0 1 1

-- Calculates the new position after traversing a given character. Assumes eight
-- character tab stops. TODO: Is this an okay assumption?
alexMovePos :: AlexPosn -> Char -> AlexPosn
alexMovePos (AlexPosn a l c) '\t' = AlexPosn (a+1)  l     (((l+7) `div` 8)*8+1)
alexMovePos (AlexPosn a l c) '\n' = AlexPosn (a+1) (l+1)   1
alexMovePos (AlexPosn a l c) _    = AlexPosn (a+1)  l     (l+1)
