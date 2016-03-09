;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(* sorts *)
(sort I,ITO,IXITO:ANY)
(type a[IXITO](IXITO I I))
(type a[ITO](ITO I))

(* Definition of Composition *)
(type comp(IXITO IXITO):IXITO)
(all rho:IXITO all sigma:IXITO (all x:I all y:I
				    (ex z:I a[IXITO](rho x z) and a[IXITO](sigma z y))
                               eqv
			       a[IXITO](comp(rho sigma) x y)))


(* Theorem *)
(all rhO:IXITO all sigma:IXITO all tau:IXITO all x:I all y:I a[IXITO](comp(comp(rho sigma) tau) x y)
     eqv a[IXITO](comp(rho comp(sigma tau)) x y))