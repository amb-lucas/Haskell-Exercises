data Prop = Const Bool |
            Var Char |
            Not Prop |
            And Prop Prop |
            Or Prop Prop |
            Implies Prop Prop |
            Iff Prop Prop
            deriving (Read)

vars :: Prop -> [Char]
vars (Const a)      = []
vars (Var a)        = [a]
vars (Not a)        = (vars a)
vars (And a b)      = (vars a)++(vars b)
vars (Or a b)       = (vars a)++(vars b)
vars (Implies a b)  = (vars a)++(vars b)
vars (Iff a b)      = (vars a)++(vars b)

main = do
  n <- readLn
  print $ vars (n :: Prop)
