;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x a(x x) = x)
(all x o(x x) = x)
(all x,y a(x o(x y)) = x)
(all x,y o(x a(x y)) = x)
(all x,y a(x y) = a(y x))
(all x,y o(x y) = o(y x))
(all x,y,z a(x a(y z)) = a(a(x y) z))
(all x,y,z o(x o(y z)) = o(o(x y) z))

(all x o(0 x) = x)
(all x a(1 x) = x)
(all x a(0 x) = 0)
(all x o(1 x) = 1)

(* Modular)
(all x,y,z a(x z) = x impl a(z o(x y)) = o(x a(y z)))

(* Complement)
(all x,y c(x y) impl a(x y) = 0)
(all x,y c(x y) impl o(x y) = 1)
(all x,y a(x y) = 0 and o(x y) = 1 impl c(x y))

(all x,y uc(x y) impl c(x y))
(all x,y,z uc(x y) and c(x z) impl z = y)
(all x,y c(x y) impl uc(x y) or c(x f(x y)))
(all x,y c(x y) and f(x y) = y impl uc(x y))



(* Theorem *)

(all x,y,u,v c(x o(u v)) and c(y a(u v)) impl c(u o(x a(y v))))
