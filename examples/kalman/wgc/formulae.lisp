;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


(* A(u x y z) means that the state where the boat, wolf, goat, and cabbages are respectively on banks u, x, y, z is an achievable)
(* state)
(*)
(* w and e are unequal)
(ne(w e))
(ne(e w))
(* all originally on west bank)
(A(w w w w))
(* goat across or back)
(all x,y A(w x w y) eqv A(e x e y))
(* wolf across)
(all x,y ne(x y) and A(w w x y) impl A(e e x y))
(* wolf back)
(all x,y ne(x y) and A(e e x y) impl A(w w x y))
(* cabbages across)
(all x,y ne(x y) and a(w x y w) impl A(e x y e))
(* cabbages back)
(all x,y ne(x y) and A(e x y e) impl A(w x y w))
(* boat across or back)
(all x,y,z ne(x y) and ne(y z) impl (A(w x y z) eqv A(e x y z)))




(* all transported to east bank)
(A(e e e e))

