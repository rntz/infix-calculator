module Lang where

import Control.Applicative
import Data.Maybe (fromMaybe)

type Name = String

data Exp = Lit Integer
         | Var Name
         | App Exp [Exp]
           deriving Show

data Decl = Print Exp
          | Let Name Exp
          | Def Name [Name] Exp
            deriving Show

data Val = Num Integer
         | Fn ([Val] -> Val)

type Env = [(Name, Val)]

instance Show Val where
    show (Num i) = show i
    show Fn{} = "fn"

call :: Val -> [Val] -> Val
call (Fn f) xs = f xs
call _ _ = error "called a non-function"

eval :: Env -> Exp -> Val
eval env (Lit i) = Num i
eval env (Var n) = fromMaybe err $ lookup n env
    where err = error $ "unbound variable: " ++ n
eval env (App f xs) = call (eval env f) (map (eval env) xs)

runDecl :: Env -> Decl -> IO Env
runDecl env (Print e) = env <$ print (eval env e)
runDecl env (Let name exp) = return $ (name, eval env exp) : env
runDecl env (Def name params body) = return $ (name, Fn func) : env
    where func xs
              | length xs /= length params = error "wrong number of arguments"
          func xs = eval (zip params xs ++ env) body

run :: Env -> [Decl] -> IO Env
run e [] = return e
run e (d:ds) = do e' <- runDecl e d
                  run e' ds
