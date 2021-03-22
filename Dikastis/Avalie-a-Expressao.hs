
type Subst = [(Char, Bool)]

data Prop = Const Bool |
            Var Char |
            Not Prop |
            And Prop Prop |
            Or Prop Prop |
            Implies Prop Prop |
            Iff Prop Prop
            deriving (Read)

eval :: Subst -> Prop -> Bool
eval subs (Const b) = b

eval subs (Var c)
  | c == ch   = val
  | otherwise = eval as (Var c)
  where (ch, val) = head subs
        as        = tail subs

eval subs (Not p)
  | val == True = False
  | otherwise   = True
  where val = eval subs p

eval subs (And p1 p2) = v1 && v2
  where v1 = eval subs p1
        v2 = eval subs p2

eval subs (Or p1 p2)  = v1 || v2
  where v1 = eval subs p1
        v2 = eval subs p2

eval subs (Implies p1 p2)
  | v1 == True && v2 == False = False
  | otherwise                 = True
  where v1 = eval subs p1
        v2 = eval subs p2

eval subs (Iff p1 p2) = (v1 == v2)
  where v1 = eval subs p1
        v2 = eval subs p2
        
main = do
  n <- readLn
  x <- readLn
  print $ eval (n :: Subst) (x :: Prop)
