;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MKRP -*-
(sort set :any)
(sort element :any)
(type intersection (set set):set)
(type member(element set))
(type setequal (set set))
(all a,b :set setequal(a b) eqv (all x:element member(x a) eqv member (x b)))
(all a,b :set all x:element member(x intersection(a b)) eqv (member (x a) and member(x b)))

(all a,b:set setequal (intersection (a b) intersection (b a)))