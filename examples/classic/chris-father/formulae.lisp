;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(sort a,b :any)
(type d : a)

(all x:(f(a)) (ex z:(g(x)) e(z x)))

(e (d a-c))


(e (d b-c))




(E(Peter human))
(E(Paul human))
(E(Mary human))
(E(Paul fath(mary)))
(E(Peter Fath(Paul)))
(all x,y:human E(x Anc(y)) impl L(x y))
(all y:human (all x:(fath(y)) E(x Anc(y))))
(all y:human (all z:(fath(y)) (all x:(Anc(z)) E(x Anc(y)))))


(L(Peter Mary))