module Parse ( ParseEnv(..), Parse, program, decls, decl, expr
             , parseEnvFromList
             , Assoc(..))
where

import Control.Applicative hiding (many, (<|>))
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (haskellStyle)
import qualified Text.Parsec.Token as Lex

import Lang

type Prec = Int
type Fixity = (Prec, Assoc)

instance Show Assoc where
    show AssocLeft = "AssocLeft"
    show AssocRight = "AssocRight"
    show AssocNone = "AssocNone"

newtype ParseEnv = ParseEnv { symbolFixity :: Map Name Fixity }
    deriving Show

parseEnvFromList :: [(Name, Prec, Assoc)] -> ParseEnv
parseEnvFromList l = ParseEnv $ Map.fromList [(n,(p,a)) | (n,p,a) <- l]

instance Monoid ParseEnv where
    -- left-biased
    mappend m1 m2 = ParseEnv (Map.union (symbolFixity m1) (symbolFixity m2))
    mempty = ParseEnv Map.empty

type Parse a = Parsec String () a

program :: ParseEnv -> Parse [Decl]
program env = whiteSpace >> decls env <* eof

decls :: ParseEnv -> Parse [Decl]
decls env = decls' [] env

decls' accum env = option (reverse accum) $ do
                     d <- decl env
                     case d of
                       Right x -> decls' (x:accum) env
                       Left extension -> decls' accum (mappend extension env)

-- decl ::= "print" exp
--        | "let" name "=" exp
--        | "def" name "(" names ")" "=" exp
--        | "def" name op name "=" exp
--        | "infix" prec op
--        | "infixl" prec op
--        | "infixr" prec op
decl env = choice [reserved x >> y | (x,y) <- table]
    where
      exp = expr env
      table = [ ("print", Right . Print <$> exp)
              , ("let", Right <$> (Let <$> identifier <*> (equals >> exp)))
              , ("def", Right <$> ((pDef =<< identifier) <*> (equals >> exp)))
              , ("infixl", pInfix AssocLeft)
              , ("infixr", pInfix AssocRight)
              , ("infix", pInfix AssocNone) ]
      pDef x = choice [ Def x <$> parens (commaSep identifier)
                      , Def <$> operator <*> ((\y-> [x,y]) <$> identifier) ]
      pInfix assoc = Left <$> (munge <$> (fromInteger <$> natural) <*> operator)
          where munge prec name = parseEnvFromList [(name, prec, assoc)]

assoc = choice [ AssocLeft <$ reserved "L"
               , AssocRight <$ reserved "R"
               , return AssocNone ]

declPrint env = do reserved "print"; Print <$> exp env

-- Parsing expressions
expr :: ParseEnv -> Parse Exp
expr env = exp
    where
      exp = buildExpressionParser (makeOperatorTable env) term
      -- term: an expression w/o infix operators
      term = foldl App <$> head <*> many argList
      head = atom <|> parens exp
      argList = parens (commaSep exp)

atom = var <|> literal
var = Var <$> identifier
literal = Lit <$> integer

makeOperatorTable :: ParseEnv -> OperatorTable String () Identity Exp
makeOperatorTable env = [map makeOperator ops
                         | (_, ops) <- Map.toDescList precMap]
    where
      precMap = Map.fromListWith (++)
                [(prec, [(name, assoc)]) | (name, (prec, assoc)) <- fixities]
      fixities = Map.toList (symbolFixity env)
      makeOperator (opName, assoc) = Infix p assoc
          where p = f <$ reservedOp opName
                f x y = App (Var opName) [x,y]

-- Our token parser
lang = haskellStyle {
         Lex.reservedNames = words "def let print infix infixl infixr"
       , Lex.reservedOpNames = words "="
       }
lexer = Lex.makeTokenParser lang

lexeme = Lex.lexeme lexer
parens = Lex.parens lexer
identifier = Lex.identifier lexer
operator = Lex.operator lexer
reserved = Lex.reserved lexer
reservedOp = Lex.reservedOp lexer
natural = Lex.natural lexer
integer = Lex.integer lexer
whiteSpace = Lex.whiteSpace lexer
semi = Lex.semi lexer
semiSep = Lex.semiSep lexer
semiSep1 = Lex.semiSep1 lexer
commaSep = Lex.commaSep lexer
commaSep1 = Lex.commaSep1 lexer
equals = reservedOp "="
