{
{-# LANGUAGE LambdaCase #-}

module Lexer (Alex, runAlex, lexToken) where

import           Control.Applicative ((<$>))
import           Control.Lens
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Text.Printf         (printf)

import Alex
}

$any                = [. \n]                     -- any character
$eol_char           = [\n \r]                    -- LF or CR
@eol_pattern        = \n | \r | \r \n            -- LF, CR, or CRLF
@line_join          = \\ @eol_pattern            -- a physical line that ends with a backslash
$white_no_nl        = $white # $eol_char         -- whitespace that is not a newline

----------------------------------------------------------------------------------------------
-- Comments

@comment            = \# ~$eol_char*              -- # until end of line

----------------------------------------------------------------------------------------------
-- Identifiers (http://docs.python.org/2/reference/lexical_analysis.html#identifiers)

$ident_char         = [a-zA-Z0-9_]               -- any character of an identifier
$ident_first        = [a-zA-Z_]                  -- first character of an identifier
@ident              = $ident_first $ident_char*

----------------------------------------------------------------------------------------------
-- String literals (http://docs.python.org/2/reference/lexical_analysis.html#string-literals)

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

python :-

   -- Each right-hand side has type :: AlexAction Lexeme

   <0,bol> {

      $white_no_nl+    { skip                }  -- whitespace is ignored
      @comment         { skip                }  -- comments are not tokens
      @line_join       { skip                }  -- line joins are ignored

   }

   -- Normal lexing state
   <0> {

      @eol_pattern     { eol                 }
      @ident           { keywordOrIdentifier }  -- ident could be a keyword
      @string_literal  { mkL LStringLiteral  }

   }

   -- At the beginning of a line
   <bol> {

      ()               { indentation True    }

   }

{

-- -----------------------------------------------------------------------------
-- Alex actions to produce Lexemes, or just useful combinators, etc.

type AlexAction a = AlexInput -> Int -> Alex a

mkL :: LexemeClass -> AlexAction Lexeme
mkL c (AlexInput p _ _ str) len = return (Lexeme p c (take len str))

-- Emit a token on consuming a newline. If implicitly joining lines, then
-- simply emit the next token. Otherwise, emit a NEWLINE and enter <bol>
-- state.
eol :: AlexAction Lexeme
eol input len = do
    joining <- isImplicitlyJoining
    if joining
        then alexMonadScan
        else emitNewlineToken `withCode` bol
  where
    emitNewlineToken :: Alex Lexeme
    emitNewlineToken = mkL LNewline input len

keywordOrIdentifier :: AlexAction Lexeme
keywordOrIdentifier input@(AlexInput _ _ _ str) len = mkL cls input len
  where
    cls :: LexemeClass
    cls = maybe LIdent id $ M.lookup (take len str) keywords

-- See http://docs.python.org/2/reference/lexical_analysis.html#keywords
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

-- At BOL (or BOF) - apply indentation rules to possibly emit INDENT or DEDENT
-- tokens.
--
-- See http://docs.python.org/2/reference/lexical_analysis.html#indentation
indentation :: Bool               -- True for BOL, False for BOF
            -> AlexAction Lexeme
indentation isBOL input len = alexGetIndentation >>= maybe (alexError "Inconsistent dedent") indentation'
  where
    indentation' :: Column -> Alex Lexeme
    indentation' prevCol = do
      let curCol = input ^. alexInputPosn . alexPosnCol
      case compare curCol prevCol of
          -- Same indentation: do nothing. Return to <0> state and lex.
          EQ -> alexMonadScan `withCode` 0

          -- More indentation: push the column on the indentation stack, and
          -- generate one INDENT token. Return to <0> state.
          GT -> do
              alexPushIndentation curCol
              emitIndentToken `withCode` 0

          -- Less indentation: it must be one of the numbers occurring on the
          -- stack; all numbers on the stack that are larger are popped off, and
          -- for each number popped off a DEDENT token is generated.
          --
          -- Leave the lexer in <bol> state so that it can generate as many
          -- DEDENT tokens as are necessary.
          LT -> do
              _ <- alexPopIndentation
              emitDedentToken

    emitIndentToken, emitDedentToken :: Alex Lexeme
    emitIndentToken = emitToken LIndent
    emitDedentToken = emitToken LDedent

    emitToken :: LexemeClass -> Alex Lexeme
    emitToken cls = mkL cls input len

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

-- Set a new start code and perform an action.
withCode :: Alex a -> StartCode -> Alex a
withCode action code = alexSetStartCode code >> action

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
