;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)

(ex x,y,z O(x) and O(y) and O(z)
    and not x = y and not x = z 
    and not y = z)

(ex x,y H(x) and H(y)
    and nOt x = y 
    and all w H(w) impl (w = x or w = y))

(all x O(x) impl ex y H(y) and In(x y))

(all x H(x) impl all y,z O(y) and O(z) and IN(y x) and In(z x) impl y = z)

