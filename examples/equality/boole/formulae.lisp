;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Commutativity *)
(all x,y o(x y) = o(y x))
(all x,y a(x y) = a(y x))

(* Distributivity)
(all x,y,z a(x o(y z)) = o(a(x y) a(x z)))
(all x,y,z o(x a(y z)) = a(o(x y) o(x z)))

(* Neutral elements *)
(all x o(x 0) = x)
(all x a(x 1) = x)

(* Complement *)
(all x a(x n(x)) = 0)
(all x o(x n(x)) = 1)





(* Theorems *)
(* Associativity *)
(all x,y,z a(x a(y z)) = a(a(x y) z))
(all x,y,z o(x o(y z)) = o(o(x y) z))

(* Idempotence *)
(all x a(x x) = x)
(all x o(x x) = x)

(* Absorption *)
(all x,y o(x a(x y)) = x)
(all x,y a(x o(x y)) = x)

(* De Morgan *)
(all x,y a(n(x) n(y)) = n(o(x y)))
(all x,y o(n(x) n(y)) = n(a(x y)))