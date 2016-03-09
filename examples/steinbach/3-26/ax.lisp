;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x a (c (x)) = c (b (b (x))))
(all x b (a (x)) = a (c (c (x))))
(all x c (b (x)) = b (a (a (x))))
(all x u (a (x)) = e)
(all x v (b (x)) = e)
(all x w (c (x)) = e)