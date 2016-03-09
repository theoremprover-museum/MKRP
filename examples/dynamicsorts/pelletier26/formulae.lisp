;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


;((ex x e(x P)) eqv (ex x e(x Q)))
((e(a P) eqv e(b Q)))
(all x:P (all y:Q (R(x) eqv S(y))))
(not (((all x:P R(x)) and (not S(c) and e(c Q))) or ((not R(d) and e(d P)) and (all x:Q S(x)))))

;((all x:P R(x)) eqv (all x:Q S(x)))
