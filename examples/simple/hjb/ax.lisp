;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x,y,z ( LEQ(x y) and LEQ(y z) )  impl LEQ(x z))
(all x LEQ(x x))
(all x,y ( LEQ(x y)  and LEQ(y x) )  eqv  ( not NEQ(x y) ))
(all x,y NEQ(x y)  impl NEQ(y x))
(all x not NEQ(x x))
(all x,y EQ(x y)  eqv  ( not NEQ(x y)))
(all x,y LESS(x y)  eqv  ( LEQ(x y) and NEQ(x y)))

(eqv-eq eqv ((all x,y,z EQ(x y) and EQ(y z) impl EQ(x z))
             and (all x,y EQ(x y) impl EQ(y x))
             and (all x EQ(x x))))

(prop-less eqv ((all x,y,z LESS(x y) and LESS(y z) impl LESS(x z))
                and (all x not LESS(x x))))
