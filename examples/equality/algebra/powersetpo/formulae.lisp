;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* Axioms *)
(* Powersets *)
(in(empty Powerset))
(all a ex b in(b Powerset) and (in (a b) impl not(ex x in(x b) and not (x = b))))
(all a,b in(a Powerset) and in(b Powerset) impl in(union(a b) Powerset))

(* Union *)
(all a,b,c in(a c) and in(a b) impl in(a union(b c)))

(* Subset *)
(all a,b,c in(a b) and subset(b c) impl in(a c))

(* Set equality *)
(all a,b Subset(a b) and subset(b a) impl a = b)


(* Theorem *)
(* subset is a partial order on a powerset *)

((all a,b in(a Powerset) and in(b Powerset) impl (subset(a b) and subset(b a) impl a = b))
 and (all a,b,c in(a Powerset) and in(b Powerset) and in(c Powerset) impl
	  (subset(a b) and subset(b c) impl subset(a c)))
 and (all a in(a Powerset) impl subset(a a)))