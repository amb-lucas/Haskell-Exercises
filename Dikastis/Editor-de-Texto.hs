data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

editText :: String -> [Cmd] -> String
editText st cmds = editText' 0 st cmds

editText' :: Int -> String -> [Cmd] -> String
editText' _ st [] = st
editText' curPos st ((Cursor n):as)     = editText' (curPos + n) st as

editText' curPos st ((Backspace n): as) = editText' (curPos - n) newSt as
  where newSt = fstr ++ tstr
        (a, tstr)     = (splitAt curPos st)
        (fstr, sstr)  = (splitAt (curPos - n) st)

editText' curPos st ((Delete n):as)      = editText' curPos newSt as
  where newSt         = fstr ++ tstr
        (a, tstr)     = (splitAt (curPos + n) st)
        (fstr, sstr)  = (splitAt curPos st)
  
editText' curPos st ((Insert st2):as)    = editText' curPos newSt as
  where newSt = fstr ++ st2 ++ sstr
        (fstr, sstr) = (splitAt curPos st)

main = do
       a <- getLine
       b <- getLine
       let result = editText a (read b)
       print result
