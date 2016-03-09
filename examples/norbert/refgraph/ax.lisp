;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x not Q(x) or p(x a) or p(x b))
(all x not Q(x) or not p(x a) or p(b x))
(all x not Q(x) or not p(x b) or p(a x))
(all x not Q(x) or not p(b x) or not p(a x))

(q(a) or q(b))