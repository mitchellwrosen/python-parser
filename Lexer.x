{
{-# LANGUAGE LambdaCase #-}

module Lexer (Alex, runAlex, lexToken) where

import           Control.Applicative ((<$), (<$>))
import           Control.Lens        ((^.))
import           Data.Char           (digitToInt)
import           Data.Digits         (unDigits)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Text.Parsec         ((<|>), char, digit, many, many1, oneOf, option, parse)
import           Text.Parsec.String  (Parser)
import           Text.Printf         (printf)

import Alex
}

$any                = [. \n]                     -- any character
$digit              = [0-9]

$eol_char           = [\n \r]                    -- LF or CR
@eol_pattern        = \n | \r | \r \n            -- LF, CR, or CRLF
@line_join          = \\ @eol_pattern            -- a physical line that ends with a backslash
$white_no_nl        = $white # $eol_char         -- whitespace that is not a newline

-- -----------------------------------------------------------------------------
-- Comments (http://docs.python.org/2/reference/lexical_analysis.html#comments)

@comment            = \# ~$eol_char*              -- # until end of line

--------------------------------------------------------------------------------
-- Identifiers (http://docs.python.org/2/reference/lexical_analysis.html#identifiers)

$lowercase          = [a-z]
$uppercase          = [A-Z]
$letter             = [$lowercase $uppercase]
$ident_first        = [$letter _]                -- first character of an identifier
$ident_char         = [$ident_first $digit]      -- any character of an identifier
@ident              = $ident_first $ident_char*

-- -----------------------------------------------------------------------------
-- String literals (http://docs.python.org/2/reference/lexical_analysis.html#string-literals)

@escape_seq         = \\ $any                    -- TODO: should \CRLF be an escape seq?

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

-- -----------------------------------------------------------------------------
-- Integer literals (http://docs.python.org/2/reference/lexical_analysis.html#integer-and-long-integer-literals)

$non_zero_digit     = [1-9]
$oct_digit          = [0-7]
$hex_digit          = [0-9a-fA-F]
$bin_digit          = [01]

@bin_integer        = 0 (b|B) $bin_digit+
@hex_integer        = 0 (x|X) $hex_digit+
@oct_integer        = 0 (o|O)? $oct_digit+
@dec_integer        = $non_zero_digit $digit* | 0

@bin_long           = @bin_integer (l|L)
@hex_long           = @hex_integer (l|L)
@oct_long           = @oct_integer (l|L)
@dec_long           = @dec_integer (l|L)

-- -----------------------------------------------------------------------------
-- Float literals (http://docs.python.org/2/reference/lexical_analysis.html#floating-point-literals)

@exponent           = (e|E) (\+|\-)? $digit+
@point_float        = $digit* \. $digit+ | $digit+ \.
@exponent_float     = ($digit+ | @point_float) @exponent
@float              = @point_float | @exponent_float

-- -----------------------------------------------------------------------------
-- Imaginary literals (http://docs.python.org/2/reference/lexical_analysis.html#imaginary-literals)

@imaginary          = (@float | $digit+) (j|J)

-- -----------------------------------------------------------------------------
-- Tokens

python :-

   -- Each right-hand side has type :: AlexAction Token

   <0,bol> {

      $white_no_nl+   { skip                     }  -- whitespace is ignored
      @comment        { skip                     }  -- comments are not tokens
      @line_join      { skip                     }  -- line joins are ignored

   }

   -- Normal lexing state
   <0> {

      @eol_pattern    { eol                      }

      -- Identifiers
      @ident          { keywordOrIdentifier      }  -- ident could be a keyword

      -- Literals
      @string_literal { token' TStringLiteral id                            id           } -- TODO: shouldn't really be id
      @dec_integer    { token' TInteger       read                          id           }
      @oct_integer    { token' TInteger       (readBase 8 . dropO . drop 1) id           }
      @hex_integer    { token' TInteger       (readBase 16 . drop 2)        id           }
      @bin_integer    { token' TInteger       (readBase 2 . drop 2)         id           }
      @dec_long       { token' TLongInteger   read                          (subtract 1) }
      @oct_long       { token' TLongInteger   (readBase 8 . dropO . drop 1) (subtract 1) }
      @hex_long       { token' TLongInteger   (readBase 16 . drop 2)        (subtract 1) }
      @bin_long       { token' TLongInteger   (readBase 2 . drop 2)         (subtract 1) }
      @float          { token' TFloat         parseFloat                    id           }
      @imaginary      { token' TImaginary     parseFloat                    (subtract 1) }

      -- Operators
      "+"             { token TPlus                                                      }
      "-"             { token TMinus                                                     }
      "*"             { token TMult                                                      }
      "**"            { token TPow                                                       }
      "/"             { token TFloatDiv                                                  }
      "//"            { token TIntDiv                                                    }
      "%"             { token TMod                                                       }
      "<<"            { token TShiftL                                                    }
      ">>"            { token TShiftR                                                    }
      "&"             { token TBinAnd                                                    }
      "|"             { token TBinOr                                                     }
      "^"             { token TXor                                                       }
      "~"             { token TComplement                                                }
      "<"             { token TLT                                                        }
      ">"             { token TGT                                                        }
      "<="            { token TLTE                                                       }
      ">="            { token TGTE                                                       }
      "=="            { token TEQ                                                        }
      "!="            { token TNEQ                                                       }
      "<>"            { token TNEQ                                                       }

      -- Delimiters
      "("             { openParen  TOpenParen                                            }
      "["             { openParen  TOpenBrace                                            }
      "{"             { openParen  TOpenBracket                                          }
      ")"             { closeParen TCloseParen                                           }
      "]"             { closeParen TCloseBrace                                           }
      "}"             { closeParen TCloseBracket                                         }
      "@"             { token TAt                                                        }
      ","             { token TComma                                                     }
      ":"             { token TColon                                                     }
      "."             { token TDot                                                       }
      "`"             { token TBacktick                                                  }
      "="             { token TEquals                                                    }
      ";"             { token TSemicolon                                                 }
      "+="            { token TPlusEq                                                    }
      "-="            { token TMinusEq                                                   }
      "*="            { token TMultEq                                                    }
      "/="            { token TFloatDivEq                                                }
      "//="           { token TIntDivEq                                                  }
      "%="            { token TModEq                                                     }
      "&="            { token TBinAndEq                                                  }
      "|="            { token TBinOrEq                                                   }
      "^="            { token TXorEq                                                     }
      ">>="           { token TShiftREq                                                  }
      "<<="           { token TShiftLEq                                                  }
      "**="           { token TPowEq                                                     }

   }

   -- At the beginning of a line
   <bol> {

      @eol_pattern     { skip                     }  -- don't emit newline
      ()               { indentation              }

   }

{

-- -----------------------------------------------------------------------------
-- Alex actions to produce Tokens, or just useful combinators, etc.

type AlexAction a = AlexInput -> Int -> Alex a

-- Construct a token.
token :: (AlexPosn -> Token) -> AlexAction Token
token f (AlexInput p _ _ _) _ = return (f p)

-- Construct a token, applying a function to the input string, and modifying the length taken.
token' :: (AlexPosn -> a -> Token) -> (String -> a) -> (Int -> Int) -> AlexAction Token
token' f g h (AlexInput p _ _ str) len = return $ f p (g $ take (h len) str)

dropO :: String -> String
dropO ('o':xs) = xs
dropO ('O':xs) = xs
dropO xs       = xs

readBase :: Integral a => a -> String -> a
readBase base = unDigits base . map (fromIntegral . digitToInt)

-- We can be a little sloppy here because the lexer already succeeded.
parseFloat :: String -> Double
parseFloat s = let Right n = parse float "" s in n
  where
    float :: Parser Double
    float = do
        number   <- read <$> many digit
        fraction <- option 0 floatFraction
        f        <- option id floatExpFunc
        return (f $ number + fraction)

    floatFraction :: Parser Double
    floatFraction = do
        _ <- char '.'
        many digit >>= \case
            "" -> return 0
            n  -> return (read ("0." ++ n))

    floatExpFunc :: Parser (Double -> Double)
    floatExpFunc = do
        _ <- oneOf "eE"
        f <- option id ((id <$ char '+') <|> (negate <$ char '-'))
        n <- read <$> many1 digit
        return (* (10 ** f n))

-- Emit a token on consuming a newline. If implicitly joining lines, then
-- simply emit the next token. Otherwise, emit a NEWLINE and enter <bol>
-- state.
eol :: AlexAction Token
eol input len = ifM isImplicitlyJoining alexMonadScan (emitNewlineToken `withCode` bol)
  where
    emitNewlineToken :: Alex Token
    emitNewlineToken = token TNewline input len

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb x y = mb >>= \b -> if b then x else y

keywordOrIdentifier :: AlexAction Token
keywordOrIdentifier input@(AlexInput p _ _ str) len =
   case M.lookup ident keywords of
      Nothing -> return (TIdent p ident)
      Just f  -> return (f p)
  where
    ident :: String
    ident = take len str

-- See http://docs.python.org/2/reference/lexical_analysis.html#keywords
keywords :: Map String (AlexPosn -> Token)
keywords = M.fromList
    [ ("and"    , TAnd    ), ("as"      , TAs      ), ("assert", TAssert), ("break" , TBreak )
    , ("class"  , TClass  ), ("continue", TContinue), ("def"   , TDef   ), ("del"   , TDelete)
    , ("elif"   , TElif   ), ("else"    , TElse    ), ("except", TExcept), ("exec"  , TExec  )
    , ("finally", TFinally), ("for"     , TFor     ), ("from"  , TFrom  ), ("global", TGlobal)
    , ("if"     , TIf     ), ("import"  , TImport  ), ("in"    , TIn    ), ("is"    , TIs    )
    , ("lambda" , TLambda ), ("not"     , TNot     ), ("or"    , TOr    ), ("pass"  , TPass  )
    , ("print"  , TPrint  ), ("raise"   , TRaise   ), ("return", TReturn), ("try"   , TTry   )
    , ("while"  , TWhile  ), ("with"    , TWith    ), ("yield" , TYield )
    ]

-- At beginning-of-line, not inside implicitly joined lines. Apply indentation
-- rules to possibly emit INDENT or DEDENT tokens.
--
-- See http://docs.python.org/2/reference/lexical_analysis.html#indentation
indentation :: AlexAction Token
indentation input len = alexGetIndentation >>= maybe (alexError "Inconsistent dedent") indentation'
  where
    indentation' :: Column -> Alex Token
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

    emitIndentToken, emitDedentToken :: Alex Token
    emitIndentToken = emitToken TIndent
    emitDedentToken = emitToken TDedent

    emitToken :: (AlexPosn -> Token) -> Alex Token
    emitToken f = token f input len

openParen :: (AlexPosn -> Token) -> AlexAction Token
openParen f input len = alexOpenParen >> token f input len

closeParen :: (AlexPosn -> Token) -> AlexAction Token
closeParen f input len = alexCloseParen >> token f input len

-- Action to lex an entire file into a stream of tokens
lexToken :: Alex [Token]
lexToken = do
    alexMonadScan >>= \case
      TEOF -> return []
      t    -> (t:) <$> lexToken

-- Just ignore this token and scan another one
skip :: AlexAction Token
skip _ _ = alexMonadScan

-- Set a new start code and perform an action.
withCode :: Alex a -> StartCode -> Alex a
withCode action code = alexSetStartCode code >> action

--token :: (AlexInput -> Int -> a) -> AlexAction a
--token t input len = return (t input len)

-- The main entry point to the lexer interface.
alexMonadScan :: Alex Token
alexMonadScan = do
    input     <- alexGetInput
    startCode <- alexGetStartCode
    case alexScan input startCode of
        AlexEOF                     -> onEOF
        AlexError input'            -> onError input'
        AlexSkip input' len         -> onSkip input' len
        AlexToken input' len action -> onToken input input' len action
  where
    onEOF :: Alex Token
    onEOF = return TEOF

    onError :: AlexInput -> Alex Token
    onError (AlexInput (AlexPosn _ line col) ch _ _) = alexError $
        printf "lexical error at line %d, column %d: unexpected char after '%c'" line col ch

    onSkip :: AlexInput -> Int -> Alex Token
    onSkip input _ = do
        alexSetInput input
        alexMonadScan

    onToken :: AlexInput -> AlexInput -> Int -> AlexAction Token -> Alex Token
    onToken prevInput nextInput len action = do
        alexSetInput nextInput
        action (ignorePendingBytes prevInput) len

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan >>=)

}
