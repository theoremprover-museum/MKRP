MKRP: (mkrp::cg-dump t '((mkrp::clauses all)))
   ACTUAL CLAUSES:

      R.=: All X:ANY   + =(x x)
      A0: All X,Y,Z:ANY   + =(plus(plus(z y) x) plus(z plus(y x)))
      A1: All X:ANY   + =(plus(0 x) x)
      A2: All X:ANY   + =(plus(minus(x) x) 0)
      T4.C3: All X,Y,Z:ANY   + =(z y)  + =(x y)  + =(z x)
      P2: All X,Y:ANY   + =(y plus(minus(x) plus(x y)))
      P6: + =(minus(0) 0)
      P9: All X:ANY   + =(x minus(minus(x)))
      P10: All X:ANY   + =(x plus(x 0))
      P11: All X:ANY   + =(plus(x minus(x)) 0)
      P12: All X,Y:ANY   + =(y plus(x plus(minus(x) y)))
      P18: All X,Y:ANY   + =(minus(plus(y x)) plus(minus(x) minus(y)))
      P19: All X:ANY   + =(c_1 minus(x))  + =(0 x)
      P20: All X:ANY   + =(plus(c_1 x) 0)  + =(0 x)
      P21: All X,Y:ANY   + =(y plus(x plus(c_1 y)))  + =(0 x)
      P24: All X,Y:ANY   + =(plus(y plus(c_1 x)) y)  + =(0 x)
      P24.C2: All X,Y:ANY   + =(plus(y plus(c_1 x)) y)  + =(0 x)
      P44: All X,Y:ANY   - =(minus(y) 0)  + =(x plus(y x))
      P44.C2: All X,Y:ANY   - =(minus(y) 0)  + =(x plus(y x))
      P46: All X,Y:ANY   - =(y 0)  + =(x plus(y x))
      P47: All X:ANY   + =(minus(x) 0)  - =(x 0)
      R4: - =(c_1 0)
      P48: All X:ANY   - =(minus(x) 0)  + =(0 x)
      R5: + =(c_1 minus(c_1))
      P49: All X:ANY   + =(c_1 x)  + =(0 x)
      P50: All X:ANY   + =(plus(x c_1) 0)  + =(0 minus(x))
      P51: All X:ANY   + =(0 plus(minus(x) c_1))  + =(0 x)
      P52: All X,Y:ANY   + =(plus(c_1 y) plus(x y))  + =(0 x)
      P53: All X,Y:ANY   + =(0 plus(c_1 y))  + =(plus(x y) x)
      P53.C2: All X,Y:ANY   + =(0 plus(c_1 y))  + =(plus(x y) x)
      P54: All X,Y:ANY   + =(plus(c_1 y) 0)  + =(x plus(y x))
      P54.C2: All X,Y:ANY   + =(plus(c_1 y) 0)  + =(x plus(y x))
      P55: All X,Y:ANY   + =(plus(y c_1) 0)  + =(x plus(y x))
      P55.C2: All X,Y:ANY   + =(plus(y c_1) 0)  + =(x plus(y x))
      P56: All X,Y:ANY   + =(c_1 minus(y))  + =(x plus(y x))
      P56.C2: All X,Y:ANY   + =(c_1 minus(y))  + =(x plus(y x))
      P57: + =(plus(c_1 c_1) 0)
      P58: All X:ANY   + =(x plus(c_1 plus(c_1 x)))
      P59: All X,Y:ANY   + =(plus(y x) x)  + =(c_1 y)
      P59.C2: All X,Y:ANY   + =(plus(y x) x)  + =(c_1 y)
      P60: All X,Y:ANY   + =(y plus(y x))  + =(c_1 x)
      P67: All X,Y:ANY   + =(y minus(x))  + =(c_1 plus(x y))
      P67.C2: All X,Y:ANY   + =(y minus(x))  + =(c_1 plus(x y))
      P69: All X:ANY   + =(c_1 x)  - =(c_1 minus(x))
      P71: All X,Y:ANY   + =(y x)  + =(y plus(x c_1))

NIL
You are being asked to enter a command or form.

These are the possible command names starting with "cop":
  Copy File
  Copy Microcode
  Copy Output History Into Editor
  Copy World

MKRP: :Copy Output History Into Editor