;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: MKRP -*-

(((ex x all y
      ((ex u all v s(x u) eqv s(x v)) eqv ((ex u s(u x)) eqv (all v s(v x))))
      eqv
      ((all u all v s(u y) eqv s(v y)) eqv ((ex u s(y u)) eqv (all v s(y v)))))
  eqv
  (ex x
      ((ex u all v r(u x) eqv r(v x)) eqv (all u all v r(x u) eqv r(x v)))
      eqv
      (all y (all u all v s(y u) eqv s(y v)) eqv ((ex u s(u y)) eqv (all v s(v y))))))
 
 eqv
 
 ((ex x all y
      ((ex u all v r(x u) eqv r(x v)) eqv ((ex u r(u x)) eqv (all v r(v x))))
      eqv
      ((all u all v r(u y) eqv r(v y)) eqv ((ex u r(y u)) eqv (all v r(y v)))))
  eqv
  (ex x
      ((ex u all v s(x u) eqv s(x v)) eqv (all u all v s(u x) eqv s(v x)))
      eqv
      (all y (all u all v r(y u) eqv r(y v)) eqv ((ex u r(u y)) eqv (all v r(v y)))))))