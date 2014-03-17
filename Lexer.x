{
{-# LANGUAGE LambdaCase                 #-}

module Lexer (Alex, runAlex, lexToken) where

import           Control.Applicative ((<$>))
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Text.Printf         (printf)

import Alex
}

$any                = [. \n]                     -- any character
$eol                = [\n \r]                    -- any eol character

----------------------------------------------------------------------------------------------
-- Comments
----------------------------------------------------------------------------------------------
@comment            = \# ~$eol*                  -- # until end of line

----------------------------------------------------------------------------------------------
-- Identifiers (http://docs.python.org/2/reference/lexical_analysis.html#identifiers)
----------------------------------------------------------------------------------------------
$ident_char         = [a-zA-Z0-9_]               -- any character of an identifier
$ident_first        = [a-zA-Z_]                  -- first character of an identifier
@ident              = $ident_first $ident_char*

----------------------------------------------------------------------------------------------
-- String literals (http://docs.python.org/2/reference/lexical_analysis.html#string-literals)
----------------------------------------------------------------------------------------------
@escape_seq         = \\ $any -- TODO: should \CRLF be an escape seq?

$short_string_char  = $any # [\\ \n \r]          -- <any source character except "\" or newline or the quote>
@short_string_char1 = $short_string_char # '
@short_string_char2 = $short_string_char # \"
@short_string_item1 = @short_string_char1 | @escape_seq
@short_string_item2 = @short_string_char2 | @escape_seq
@short_string       = '  @short_string_item1* '
                    | \" @short_string_item2* \"

$long_string_char   = $any # \\  -- <any source character except "\">
@long_string_item   = $long_string_char | @escape_seq
@long_string        = '''    @long_string_item* '''
                    | \"\"\" @long_string_item* \"\"\"

@string_prefix      = r | u | ur | R  | U  | UR | Ur | uR | b | B | br | Br | bR | BR
@string_literal     = @string_prefix? (@short_string | @long_string)

----------------------------------------------------------------------------------------------
-- Tokens
----------------------------------------------------------------------------------------------

python :-

   -- Each right-hand side has type :: AlexAction Lexeme

   -- Normal lexing state
   <0> {

      @comment         { skip }
      $white+          { skip }

      @ident           { keywordOrIdentifier }
      @string_literal  { mkL LStringLiteral  }

   }

   -- At the beginning of a line
   <bol> {

      ()               { indentation }

   }

{

-- -----------------------------------------------------------------------------
-- Alex actions to produce Lexemes, or just useful combinators, etc.

type AlexAction a = AlexInput -> Int -> Alex a

mkL :: LexemeClass -> AlexAction Lexeme
mkL c (AlexInput p _ _ str) len = return (Lexeme p c (take len str))

keywordOrIdentifier :: AlexAction Lexeme
keywordOrIdentifier input@(AlexInput _ _ _ str) len = mkL cls input len
  where
    cls :: LexemeClass
    cls = maybe LIdent id $ M.lookup (take len str) keywords

-- http://docs.python.org/2/reference/lexical_analysis.html#keywords
keywords :: Map String LexemeClass
keywords = M.fromList
    [ ("and"    , LAnd    ), ("as"      , LAs      ), ("assert", LAssert), ("break" , LBreak )
    , ("class"  , LClass  ), ("continue", LContinue), ("def"   , LDef   ), ("del"   , LDelete)
    , ("elif"   , LElif   ), ("else"    , LElse    ), ("except", LExcept), ("exec"  , LExec  )
    , ("finally", LFinally), ("for"     , LFor     ), ("from"  , LFrom  ), ("global", LGlobal)
    , ("if"     , LIf     ), ("import"  , LImport  ), ("in"    , LIn    ), ("is"    , LIs    )
    , ("lambda" , LLambda ), ("not"     , LNot     ), ("or"    , LOr    ), ("pass"  , LPass  )
    , ("print"  , LPrint  ), ("raise"   , LRaise   ), ("return", LReturn), ("try"   , LTry   )
    , ("while"  , LWhile  ), ("with"    , LWith    ), ("yield" , LYield )
    ]

indentation :: AlexAction Lexeme
indentation (AlexInput (AlexPosn _ _ col) _ _ _) len = do
    prevCol <- alexGetIndentation
    undefined

-- Action to lex an entire file into a stream of tokens
lexToken :: Alex [Lexeme]
lexToken = do
    lexeme@(Lexeme _ c _) <- alexMonadScan
    if c == LEOF
        then return []
        else (lexeme:) <$> lexToken

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

-- The main entry point to the lexer interface.
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
}
