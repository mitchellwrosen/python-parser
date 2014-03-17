{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Alex where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Word           (Word8)

import LexerUtils

-- -----------------------------------------------------------------------------
-- Token positions

-- `AlexPosn' records the location of a token in the input text.
data AlexPosn = AlexPosn
    { _alexPosnAddress :: !Int  -- Number of characters preceding the token
    , _alexPosnLine    :: !Int  -- Line number
    , _alexPosnCol     :: !Int  -- Column number
    } deriving (Eq, Show)
makeLenses ''AlexPosn

-- The position of the start of the file.
initAlexPosn :: AlexPosn
initAlexPosn = AlexPosn 0 1 1

-- Calculates the new position after traversing a given character. Assumes eight
-- character tab stops. TODO: Is this an okay assumption?
alexMovePos :: Char -> AlexPosn -> AlexPosn
alexMovePos '\t' = (alexPosnAddress %~ (+1))                          . (alexPosnCol %~ (\c -> ((c+7) `div` 8)*8+1))
alexMovePos '\n' = (alexPosnAddress %~ (+1)) . (alexPosnLine %~ (+1)) . (alexPosnCol .~ 1)
alexMovePos _    = (alexPosnAddress %~ (+1))                          . (alexPosnCol %~ (+1))

-- -----------------------------------------------------------------------------
-- The input type

data AlexInput = AlexInput
    { _alexInputPosn         :: AlexPosn  -- current position
    , _alexInputPrevChar     :: !Char     -- previous char
    , _alexInputPendingBytes :: [Word8]   -- pending bytes on current char
    , _alexInputString       :: String    -- current input string
    }
makeLenses ''AlexInput

initAlexInput :: String -> AlexInput
initAlexInput input = AlexInput
    { _alexInputPosn         = initAlexPosn
    , _alexInputPrevChar     = '\n'
    , _alexInputPendingBytes = []
    , _alexInputString       = input
    }

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes = alexInputPendingBytes .~ []

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput p c (b:bs) s)     = Just (b, AlexInput p c bs s)
alexGetByte (AlexInput p c []     [])    = Nothing
alexGetByte (AlexInput p _ []     (c:s)) = p' `seq`  Just (b, AlexInput p'  c  bs  s)
  where
    p' :: AlexPosn
    p' = alexMovePos c p

    (b:bs) = utf8Encode c

-- -----------------------------------------------------------------------------
-- The Lexeme type

data LexemeClass
    -- Newline/Indentation
    = LNewline | LIndent | LDedent
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

data Lexeme = Lexeme
    { _lexemePosn  :: AlexPosn
    , _lexemeClass :: !LexemeClass
    , _lexemeStr   :: String
    } deriving (Eq, Show)
makeLenses ''Lexeme

-- -----------------------------------------------------------------------------
-- The Alex monad

type StartCode = Int
type Column    = Int

data AlexState = AlexState
    { _alexStateInput          :: AlexInput
    , _alexStateStartCode      :: StartCode  -- the current startcode
    , _alexStateIndentStack    :: [Column]   -- the current stack of indentation columns
    , _alexStateParens         :: Int        -- the current number of unmatched (
    , _alexStateBrackets       :: Int        -- the current number of unmatched [
    , _alexStateBraces         :: Int        -- the current number of unmatched {
    }
makeLenses ''AlexState

initialAlexState :: String -> AlexState
initialAlexState input = AlexState
    { _alexStateInput          = initAlexInput input
    , _alexStateStartCode      = 0
    , _alexStateIndentStack    = [0]
    , _alexStateParens         = 0
    , _alexStateBrackets       = 0
    , _alexStateBraces         = 0
    }

-- Compile with -funbox-strict-fields for best results!

newtype Alex a = Alex { unAlex :: StateT AlexState (Either String) a }
               deriving (Functor, Applicative, Monad, MonadState AlexState)

runAlex :: Alex a -> String -> Either String a
runAlex (Alex s) = evalStateT s . initialAlexState

alexError :: String -> Alex a
alexError = Alex . StateT . const . Left

alexGetInput :: Alex AlexInput
alexGetInput = use alexStateInput

alexSetInput :: AlexInput -> Alex ()
alexSetInput = assign alexStateInput

alexGetStartCode :: Alex StartCode
alexGetStartCode = use alexStateStartCode

alexSetStartCode :: StartCode -> Alex ()
alexSetStartCode = assign alexStateStartCode

alexGetIndentation :: Alex (Maybe Column)
alexGetIndentation = safeHead <$> use alexStateIndentStack

alexPushIndentation :: Column -> Alex ()
alexPushIndentation col = alexStateIndentStack %= (col:)

alexPopIndentation :: Alex (Maybe Column)
alexPopIndentation = do
    top <- alexGetIndentation
    alexStateIndentStack %= tail
    return top

alexGetParens, alexGetBrackets, alexGetBraces :: Alex Int
alexGetParens   = use alexStateParens
alexGetBrackets = use alexStateBrackets
alexGetBraces   = use alexStateBraces

-- Expressions in parenthesis, square brackets, or curly braces can be split
-- over more than one physical line without using backslashes.
--
-- See http://docs.python.org/2/reference/lexical_analysis.html#implicit-line-joining
isImplicitlyJoining :: Alex Bool
isImplicitlyJoining = any (> 0) <$> sequence [alexGetParens, alexGetBrackets, alexGetBraces]

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
