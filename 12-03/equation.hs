
calcPlusRoot (a,b,c) = (-b + sqrt(b*b-4*a*c))/2*a
calcMinusRoot (a,b,c) = (-b - sqrt(b*b-4*a*c))/2*a

roots (a,b,c)
  | b*b > 4*a*c  = [calcPlusRoot(a,b,c), calcMinusRoot(a,b,c)]
  | b*b == 4*a*c = [-b/(2*a)]
  | otherwise    = []
  

