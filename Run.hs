module Main where

import System.Exit (exitFailure)

import Lang
import qualified Parse
import Parse (Assoc(..))

import Text.Parsec

builtinEnv = [ oper "+" (+)
             , oper "*" (*)
             , oper "-" (-)
             , oper "/" div
             , oper "^" (^) ]
oper name func = (name, Fn f)
    where f [Num x, Num y] = Num (func x y)
          f [x,y] = error "applied operator to non-number"
          f _ = error "wrong number of arguments"

parseEnv = Parse.parseEnvFromList
           [ ("+", 6, AssocLeft)
           , ("-", 6, AssocLeft)
           , ("*", 7, AssocLeft)
           , ("/", 7, AssocLeft)
           , ("^", 8, AssocRight) ]

main = do s <- getContents
          prog <- case parse (Parse.program parseEnv) "stdin" s of
                    Left err -> do print err; exitFailure
                    Right x -> return x
          run builtinEnv prog
