-- Expression

type Variable = String
type Lambda = (Variable, Term)

data Term = Var Variable
          | Abs Lambda
          | App Term Term

instance Show Term where
  show (Var v) = v
  show (Abs (v, t)) = "(λ" ++ v ++ "." ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ ") (" ++ show t2 ++ ")"

-- Evaluation (CEK machine)

type State   = (Term, Env, Kont)
data Closure = Clo Lambda Env
type Env     = [(Variable, Closure)]
data Kont    = Mt                      -- done
             | Ar (Term, Env, Kont)    -- evaluate Term in Env
             | Fn (Lambda, Env, Kont)  -- apply Lambda in Env

lookupEnv :: Variable -> Env -> Closure
lookupEnv v [] = error $ "symbol not found: " ++ v
lookupEnv v e  = let p = head e
                     t = tail e
                 in if v == fst p then snd p else lookupEnv v t

step :: State -> State
step (Var v, e, k) = let Clo lambda e' = lookupEnv v e in
                       (Abs lambda, e', k)
step (App t1 t2, e, k) = (t1, e, Ar (t2, e, k))
step (Abs lambda, e, Ar (arg, e', k)) = (arg, e', Fn (lambda, e, k))
step (Abs lambda, e, Fn ((v, b), e', k)) = (b, (v, Clo lambda e):e', k)

isFinal :: State -> Bool
isFinal (Abs _, _, Mt) = True
isFinal _              = False

eval :: (State -> State) -> (State -> Bool) -> State -> State
eval fn_step fn_isFinal s = if fn_isFinal s then s
                            else eval fn_step fn_isFinal (fn_step s)

-- main

-- program: (λx.λk.k x) (λy.y) (λz.z) = λy.y
prog :: Term
prog = App (App (Abs ("x", Abs ("k", App (Var "k") (Var "x")))) (Abs ("y", Var "y"))) (Abs ("z", Var "z"))

main :: IO()
main = let (t, _, _) = eval step isFinal (prog, [], Mt) in print t
