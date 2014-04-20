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
-- The Token type

data Token
    = TNewline       AlexPosn
    | TIndent        AlexPosn
    | TDedent        AlexPosn
    | TIdent         AlexPosn String
    | TAnd           AlexPosn
    | TAs            AlexPosn
    | TAssert        AlexPosn
    | TBreak         AlexPosn
    | TClass         AlexPosn
    | TContinue      AlexPosn
    | TDef           AlexPosn
    | TDelete        AlexPosn
    | TElif          AlexPosn
    | TElse          AlexPosn
    | TExcept        AlexPosn
    | TExec          AlexPosn
    | TFinally       AlexPosn
    | TFor           AlexPosn
    | TFrom          AlexPosn
    | TGlobal        AlexPosn
    | TIf            AlexPosn
    | TImport        AlexPosn
    | TIn            AlexPosn
    | TIs            AlexPosn
    | TLambda        AlexPosn
    | TNot           AlexPosn
    | TOr            AlexPosn
    | TPass          AlexPosn
    | TPrint         AlexPosn
    | TRaise         AlexPosn
    | TReturn        AlexPosn
    | TTry           AlexPosn
    | TWhile         AlexPosn
    | TWith          AlexPosn
    | TYield         AlexPosn
    | TStringLiteral AlexPosn String
    | TInteger       AlexPosn Integer
    | TLongInteger   AlexPosn Integer
    | TFloat         AlexPosn Double
    | TImaginary     AlexPosn Double
    | TPlus          AlexPosn
    | TMinus         AlexPosn
    | TMult          AlexPosn
    | TPow           AlexPosn
    | TFloatDiv      AlexPosn
    | TIntDiv        AlexPosn
    | TMod           AlexPosn
    | TShiftL        AlexPosn
    | TShiftR        AlexPosn
    | TBinAnd        AlexPosn
    | TBinOr         AlexPosn
    | TXor           AlexPosn
    | TComplement    AlexPosn
    | TLT            AlexPosn
    | TGT            AlexPosn
    | TLTE           AlexPosn
    | TGTE           AlexPosn
    | TEQ            AlexPosn
    | TNEQ           AlexPosn
    | TOpenParen     AlexPosn
    | TOpenBrace     AlexPosn
    | TOpenBracket   AlexPosn
    | TCloseParen    AlexPosn
    | TCloseBrace    AlexPosn
    | TCloseBracket  AlexPosn
    | TAt            AlexPosn
    | TComma         AlexPosn
    | TColon         AlexPosn
    | TDot           AlexPosn
    | TBacktick      AlexPosn
    | TEquals        AlexPosn
    | TSemicolon     AlexPosn
    | TPlusEq        AlexPosn
    | TMinusEq       AlexPosn
    | TMultEq        AlexPosn
    | TFloatDivEq    AlexPosn
    | TIntDivEq      AlexPosn
    | TModEq         AlexPosn
    | TBinAndEq      AlexPosn
    | TBinOrEq       AlexPosn
    | TXorEq         AlexPosn
    | TShiftREq      AlexPosn
    | TShiftLEq      AlexPosn
    | TPowEq         AlexPosn
    | TEOF
    deriving (Eq, Show)

-- Short-hand show instance for quick visual inspection.
tokenShow :: Token -> String
tokenShow (TNewline _)           = "<\\n>"
tokenShow (TIndent _)            = "<indent>"
tokenShow (TDedent _)            = "<dedent>"
tokenShow (TIdent _ str)         = "<var '" ++ str ++ "'>"
tokenShow (TAnd _)               = "and"
tokenShow (TAs _)                = "as"
tokenShow (TAssert _)            = "assert"
tokenShow (TBreak _)             = "break"
tokenShow (TClass _)             = "class"
tokenShow (TContinue _)          = "continue"
tokenShow (TDef _)               = "def"
tokenShow (TDelete _)            = "delete"
tokenShow (TElif _)              = "elif"
tokenShow (TElse _)              = "else"
tokenShow (TExcept _)            = "except"
tokenShow (TExec _)              = "exec"
tokenShow (TFinally _)           = "finally"
tokenShow (TFor _)               = "for"
tokenShow (TFrom _)              = "from"
tokenShow (TGlobal _)            = "global"
tokenShow (TIf _)                = "if"
tokenShow (TImport _)            = "import"
tokenShow (TIn _)                = "in"
tokenShow (TIs _)                = "is"
tokenShow (TLambda _)            = "lambda"
tokenShow (TNot _)               = "not"
tokenShow (TOr _)                = "or"
tokenShow (TPass _)              = "pass"
tokenShow (TPrint _)             = "print"
tokenShow (TRaise _)             = "raise"
tokenShow (TReturn _)            = "return"
tokenShow (TTry _)               = "try"
tokenShow (TWhile _)             = "while"
tokenShow (TWith _)              = "with"
tokenShow (TYield _)             = "yield"
tokenShow (TStringLiteral _ str) = "<str '" ++ str ++ "'>"
tokenShow (TInteger _ n)         = "<int '" ++ show n ++ "'>"
tokenShow (TLongInteger _ n)     = "<long '" ++ show n ++ "'>"
tokenShow (TFloat _ n)           = "<float '" ++ show n ++ "'>"
tokenShow (TImaginary _ n)       = "<imag '" ++ show n ++ "'>"
tokenShow (TPlus _)              = "+"
tokenShow (TMinus _)             = "-"
tokenShow (TMult _)              = "*"
tokenShow (TPow _)               = "**"
tokenShow (TFloatDiv _)          = "/"
tokenShow (TIntDiv _)            = "//"
tokenShow (TMod _)               = "%"
tokenShow (TShiftL _)            = "<<"
tokenShow (TShiftR _)            = ">>"
tokenShow (TBinAnd _)            = "&"
tokenShow (TBinOr _)             = "|"
tokenShow (TXor _)               = "^"
tokenShow (TComplement _)        = "~"
tokenShow (TLT _)                = "<"
tokenShow (TGT _)                = ">"
tokenShow (TLTE _)               = "<="
tokenShow (TGTE _)               = ">="
tokenShow (TEQ _)                = "=="
tokenShow (TNEQ _)               = "!="
tokenShow (TOpenParen _)         = "("
tokenShow (TOpenBrace _)         = "["
tokenShow (TOpenBracket _)       = "{"
tokenShow (TCloseParen _)        = ")"
tokenShow (TCloseBrace _)        = "]"
tokenShow (TCloseBracket _)      = "}"
tokenShow (TAt _)                = "@"
tokenShow (TComma _)             = ","
tokenShow (TColon _)             = ":"
tokenShow (TDot _)               = "."
tokenShow (TBacktick _)          = "`"
tokenShow (TEquals _)            = "="
tokenShow (TSemicolon _)         = ";"
tokenShow (TPlusEq _)            = "+="
tokenShow (TMinusEq _)           = "-="
tokenShow (TMultEq _)            = "*="
tokenShow (TFloatDivEq _)        = "/="
tokenShow (TIntDivEq _)          = "//="
tokenShow (TModEq _)             = "%="
tokenShow (TBinAndEq _)          = "&="
tokenShow (TBinOrEq _)           = "|="
tokenShow (TXorEq _)             = "^="
tokenShow (TShiftREq _)          = ">>="
tokenShow (TShiftLEq _)          = "<<="
tokenShow (TPowEq _)             = "**="
tokenShow TEOF                   = "<EOF>"

-- -----------------------------------------------------------------------------
-- The Alex monad

type StartCode = Int
type Column    = Int

data AlexState = AlexState
    { _alexStateInput          :: AlexInput
    , _alexStateStartCode      :: StartCode  -- the current startcode
    , _alexStateIndentStack    :: [Column]   -- the current stack of indentation columns
    , _alexStateParens         :: Int        -- the current number of unmatched (, [, and {
    }
makeLenses ''AlexState

initialAlexState :: String -> AlexState
initialAlexState input = AlexState
    { _alexStateInput          = initAlexInput input
    , _alexStateStartCode      = 1 -- bol, but can't call it so (circluar import)
    , _alexStateIndentStack    = [1] -- spec says 0, but this is a 1-based column
    , _alexStateParens         = 0
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

alexGetParens :: Alex Int
alexGetParens = use alexStateParens

alexOpenParen :: Alex ()
alexOpenParen = alexStateParens %= (+1)

alexCloseParen :: Alex ()
alexCloseParen = alexStateParens %= (subtract 1)

-- Expressions in parenthesis, square brackets, or curly braces can be split
-- over more than one physical line without using backslashes.
--
-- See http://docs.python.org/2/reference/lexical_analysis.html#implicit-line-joining
isImplicitlyJoining :: Alex Bool
isImplicitlyJoining = (> 0) <$> alexGetParens

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
