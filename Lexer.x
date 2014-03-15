{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PackageImports             #-}

module Lexer (Alex, runAlex, lexToken) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Bits           ((.&.), shiftR)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Word           (Word8)
import           Text.Printf         (printf)
}

$ident_first = [a-zA-Z_]    -- first character of an identifier
$ident_char  = [a-zA-Z0-9_] -- any character of an identifier

-- http://docs.python.org/2/reference/lexical_analysis.html#identifiers
@ident = $ident_first $ident_char*

-- http://docs.python.org/2/reference/lexical_analysis.html#string-literals
@escape_seq = \\ [. \n]

$short_string_char  = [^ \n \r \\] -- <any source character except "\" or newline or the quote>

@short_string_char1 = $short_string_char # '
@short_string_char2 = $short_string_char # \"

@short_string_item1 = @short_string_char1 | @escape_seq
@short_string_item2 = @short_string_char2 | @escape_seq

@short_string = ' @short_string_item1* ' | \" @short_string_item2* \"

@string_prefix     = r | u | ur | R  | U  | UR | Ur | uR | b | B | br | Br | bR | BR
@string_literal    = @string_prefix? @short_string -- | @long_string)

-- TODO: long strings
-- @long_string       = ''' @long_string_item* '''
--                    | \"\"\" @long_string_item* \"\"\"
-- @long_string_item  = @long_string_char | @escape_seq



tokens :-

   $white+                       { mkL LWhite   }
   -- "--".*                        { mkL LComment }
   -- $digit+                       { mkL LInt     }
   -- [\=\+\-\*\/\(\)']             { mkL LSym     }

   @ident                        { keywordOrIdentifier }
   @string_literal               { mkL LStringLiteral  }

{
-- Each right-hand side has type :: AlexAction Lexeme

keywordOrIdentifier :: AlexAction Lexeme
keywordOrIdentifier input@(AlexInput _ _ _ str) len = mkL cls input len
  where
    cls :: LexemeClass
    cls = maybe LIdent id $ M.lookup (take len str) keywords

-- http://docs.python.org/2/reference/lexical_analysis.html#keywords
keywords :: Map String LexemeClass
keywords = M.fromList
    [ ("and",      LAnd)
    , ("as",       LAs)
    , ("assert",   LAssert)
    , ("break",    LBreak)
    , ("class",    LClass)
    , ("continue", LContinue)
    , ("def",      LDef)
    , ("del",      LDelete)
    , ("elif",     LElif)
    , ("else",     LElse)
    , ("except",   LExcept)
    , ("exec",     LExec)
    , ("finally",  LFinally)
    , ("for",      LFor)
    , ("from",     LFrom)
    , ("global",   LGlobal)
    , ("if",       LIf)
    , ("import",   LImport)
    , ("in",       LIn)
    , ("is",       LIs)
    , ("lambda",   LLambda)
    , ("not",      LNot)
    , ("or",       LOr)
    , ("pass",     LPass)
    , ("print",    LPrint)
    , ("raise",    LRaise)
    , ("return",   LReturn)
    , ("try",      LTry)
    , ("while",    LWhile)
    , ("with",     LWith)
    , ("yield",    LYield)
    ]

data Lexeme = Lexeme
    { lexemePosn  :: AlexPosn
    , lexemeClass :: !LexemeClass
    , lexemeStr   :: String
    } deriving (Eq, Show)

data LexemeClass
    = LWhite
    | LComment
    | LSym
    | LIdent
    | LInt
    | LErr
    -- Keywords
    | LAnd
    | LAs
    | LAssert
    | LBreak
    | LClass
    | LContinue
    | LDef
    | LDelete
    | LElif
    | LElse
    | LExcept
    | LExec
    | LFinally
    | LFor
    | LFrom
    | LGlobal
    | LIf
    | LImport
    | LIn
    | LIs
    | LLambda
    | LNot
    | LOr
    | LPass
    | LPrint
    | LRaise
    | LReturn
    | LTry
    | LWhile
    | LWith
    | LYield
    -- String Literals
    | LStringLiteral
    -- EOF
    | LEOF
    deriving (Eq, Show)

mkL :: LexemeClass -> AlexAction Lexeme
mkL c (AlexInput p _ _ str) len = return (Lexeme p c (take len str))

lexToken :: Alex [Lexeme]
lexToken = do
    lexeme@(Lexeme _ c _) <- alexMonadScan
    if c == LEOF
        then return []
        else (lexeme:) <$> lexToken

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
    go :: Int -> [Int]
    go oc
        | oc <= 0x7f   = [oc]
        | oc <= 0x7ff  = [ 0xc0 + (oc `shiftR` 6)
                         , 0x80 + oc .&. 0x3f
                         ]
        | oc <= 0xffff = [ 0xe0 + (oc `shiftR` 12)
                         , 0x80 + ((oc `shiftR` 6) Data.Bits..&. 0x3f)
                         , 0x80 + oc .&. 0x3f
                         ]
        | otherwise    = [ 0xf0 + (oc `shiftR` 18)
                         , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                         , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                         , 0x80 + oc .&. 0x3f
                         ]

type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type
data AlexInput = AlexInput
    { alexInputPosn         :: AlexPosn  -- current position
    , alexInputPrevChar     :: !Char      -- previous char
    , alexInputPendingBytes :: [Byte]    -- pending bytes on current char
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

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
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

-- -----------------------------------------------------------------------------
-- The Alex monad

type StartCode = Int

data AlexState = AlexState
    { alexStateInput          :: AlexInput
    , alexStateStartCodeStack :: [StartCode]      -- the current stack of startcodes
    }

initialAlexState :: String -> AlexState
initialAlexState input = AlexState
    { alexStateInput          = initAlexInput input
    , alexStateStartCodeStack = [0]
    }

-- Compile with -funbox-strict-fields for best results!

newtype Alex a = Alex { unAlex :: StateT AlexState (Either String) a }
               deriving (Functor, Applicative, Monad, MonadState AlexState)

runAlex :: String -> Alex a -> Either String a
runAlex input (Alex s) = evalStateT s (initialAlexState input)

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

alexMonadScan :: Alex Lexeme
alexMonadScan = do
    input     <- alexGetInput
    startCode <- alexGetStartCode
    case alexScan input startCode of
        AlexEOF                     -> onEOF
        AlexError input'            -> onError input'
        AlexSkip input' len         -> onSkip input' len
        AlexToken input' len action -> onToken input input' len action
  where
    onEOF :: Alex Lexeme
    onEOF = return (Lexeme undefined LEOF "")

    onError :: AlexInput -> Alex Lexeme
    onError (AlexInput (AlexPosn _ line col) ch _ _) = alexError $
        printf "lexical error at line %d, column %d: unexpected char %c" line col ch

    onSkip :: AlexInput -> Int -> Alex Lexeme
    onSkip input _ = do
        alexSetInput input
        alexMonadScan

    onToken :: AlexInput -> AlexInput -> Int -> AlexAction Lexeme -> Alex Lexeme
    onToken prevInput nextInput len action = do
        alexSetInput nextInput
        action (ignorePendingBytes prevInput) len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction a
    = AlexInput
   -> Int
   -> Alex a

-- Just ignore this token and scan another one
skip :: AlexAction Lexeme
skip _ _ = alexMonadScan

-- Ignore this token, but set the start code to a new value
begin :: StartCode -> AlexAction Lexeme
begin code _ _ = alexPushStartCode code >> alexMonadScan

-- Perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> StartCode -> AlexAction result
andBegin action code input len = alexPushStartCode code >> action input len

token :: (AlexInput -> Int -> a) -> AlexAction a
token t input len = return (t input len)
}
