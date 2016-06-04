
module Text.Parsing.Parser.Language
    ( haskellDef
    , haskell
    , emptyDef
    , haskellStyle
    , javaStyle
    )
      where

import Prelude

import Control.Alt ((<|>))
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (LanguageDef, TokenParser, GenLanguageDef(..), unGenLanguageDef, makeTokenParser, alphaNum, letter)

-----------------------------------------------------------
-- Styles: haskellStyle, javaStyle
-----------------------------------------------------------

-- | This is a minimal token definition for Haskell style languages. It
-- | defines the style of comments, valid identifiers and case
-- | sensitivity. It does not define any reserved words or operators.
haskellStyle :: LanguageDef
haskellStyle = LanguageDef (unGenLanguageDef emptyDef)
                { commentStart    = "{-"
                , commentEnd      = "-}"
                , commentLine     = "--"
                , nestedComments  = true
                , identStart      = letter
                , identLetter     = alphaNum <|> oneOf ['_', '\'']
                , opStart         = op'
                , opLetter        = op'
                , reservedOpNames = []
                , reservedNames   = []
                , caseSensitive   = true
                }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']

-- | This is a minimal token definition for Java style languages. It
-- | defines the style of comments, valid identifiers and case
-- | sensitivity. It does not define any reserved words or operators.
javaStyle  :: LanguageDef
javaStyle   = LanguageDef (unGenLanguageDef emptyDef)
                { commentStart    = "/*"
                , commentEnd      = "*/"
                , commentLine     = "//"
                , nestedComments  = true
                , identStart      = letter
                , identLetter     = alphaNum <|> oneOf ['_', '\'']
                , reservedNames   = []
                , reservedOpNames = []
                , caseSensitive   = false
                }

-----------------------------------------------------------
-- minimal language definition
--------------------------------------------------------

-- | This is the most minimal token definition. It is recommended to use
-- | this definition as the basis for other definitions. `emptyDef` has
-- | no reserved names or operators, is case sensitive and doesn't accept
-- | comments, identifiers or operators.
emptyDef :: LanguageDef
emptyDef = LanguageDef
            { commentStart:    ""
            , commentEnd:      ""
            , commentLine:     ""
            , nestedComments:  true
            , identStart:      letter <|> char '_'
            , identLetter:     alphaNum <|> oneOf ['_', '\'']
            , opStart:         op'
            , opLetter:        op'
            , reservedOpNames: []
            , reservedNames:   []
            , caseSensitive:   true
            }
  where
    op' :: forall m . (Monad m) => ParserT String m Char
    op' = oneOf [':', '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']

-- -----------------------------------------------------------
-- -- Haskell
-- -----------------------------------------------------------

-- | A lexer for the haskell language.
haskell :: TokenParser
haskell = makeTokenParser haskellDef

-- | The language definition for the Haskell language.
haskellDef  :: LanguageDef
haskellDef   =
    case haskell98Def of
        (LanguageDef def) -> LanguageDef def
                { identLetter    = def.identLetter <|> char '#'
                , reservedNames  = def.reservedNames <>
                                   ["foreign","import","export","primitive"
                                   ,"_ccall_","_casm_"
                                   ,"forall"
                                   ]
                }

-- | The language definition for the language Haskell98.
haskell98Def :: LanguageDef
haskell98Def = LanguageDef (unGenLanguageDef haskellStyle)
                { reservedOpNames = ["::","..","=","\\","|","<-","->","@","~","=>"]
                , reservedNames   = [ "let","in","case","of","if","then","else"
                                    , "data","type"
                                    , "class","default","deriving","do","import"
                                    , "infix","infixl","infixr","instance","module"
                                    , "newtype","where"
                                    , "primitive"
                                    -- "as","qualified","hiding"
                                    ]
                }
