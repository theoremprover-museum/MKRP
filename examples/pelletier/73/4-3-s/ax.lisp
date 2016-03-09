;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* Axioms *)

(sort o,h:any)
(type in (o h))

(ex x,y,z,w:O
    not x = y and not x = z and not x = w
    and not y = z and not y = w and not z = w)

(ex x,y,z:H
    nOt x = y and nOt x = z and nOt y = z
    and all w:H w = x or w = y or w = z)

(all x:O ex y:H  In(x y))

(all x:H all y,z:O IN(y x) and In(z x) impl y = z)

