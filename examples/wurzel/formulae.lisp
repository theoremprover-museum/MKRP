;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MKRP; Base: 10 -*-

(all x,y,z K+(x y z) eqv K+(y x z))
(all x,y,z K*(x y z) eqv K*(y x z))

(all x,y,z,u,v,w K+(x y z) and K+(z u v) impl ( K+(y u w) eqv K+(x w v)))

(all x K*(x eins x))
(all x NULL(x) or (Ex y K*(x y eins)))
(all x Ex y,z K+(x y z) and Null (z))

; theorem

(all x,y,z,w K*(x x z) and k*(y y z) impl x = y or (k+(x y w) and Null(w)))