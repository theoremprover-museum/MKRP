;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MKRP -*-
(all g,h parallel(g h) eqv (all s senkrecht(s g) impl senkrecht(s h)))
(symmetric (senkrecht))
(transitivvp eqv (all g,h,i parallel (g h) and parallel(h i) impl parallel (g i)))


(transitivvp)