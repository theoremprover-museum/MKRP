;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x,y R(null x y) impl x = y)
(all x R(x x x))
(all x,y,z R(x y z) impl (R(y x z) and R(x z y)))
(all x,y,z,u,v (R(x y z) and R(z u v)) impl (ex w R(x u w) and R(w y v)))

(all world,a,b tt(world rimpl(a b)) eqv (all x,y tt(x a) and R(world x y) impl tt(y b)))

(all a tt(null rimpl(a a)))

(all a,b,c (ALL X |,| Y
		((ALL Z |,| U ((TT (Z A) AND R (X Z U)) IMPL (TT (U B))) AND R (null X Y)) IMPL
		 (ALL V |,| W
		      ((ALL X1 |,| X2 ((TT (X1 B) AND R (V X1 X2)) IMPL (TT (X2 C))) AND R (Y V W)) IMPL
		       (ALL X3 |,| X4 ((TT (X3 A) AND R (W X3 X4)) IMPL (TT (X4 C)))))))))

(not (all y (not(r(a e y)) or not(r(c y f))) and r(a c d) and r(d e f)))

(not (all y (not(r(a c d)) or not(r(a d y)) or not(r(y c d))) and r(a c d)))

(not (all v,w,vs,ws (ex g,h (not(r(c e v)) or not(r(v w f)) or not(r(a g h)) or not(r(c e vs)) or not(r(vs ws f)))
			and (not(r(c e v)) or r(w g h) or not(r(v w f)))
			and r(a c d)
			and r(d e f))))

(not (all v,w ((not(r(a e v))
		   or not(r(v g h))
	       or (not(r(a c w)) or not(r(w g h)))
	       and r(a c d)
	       and r(d e f)
	       and r(f g h))))

((not (((all v,w,vs,ws (not(r(c e v)) or not(r(v w f)) or not(r(a g h)) or not(r(c e vs)) or not(r(vs ws f)))
	  and (not r(c e v) or r(w g h) or ((not(r(v w f)))))
	  and r(a c d)
	  and r(d e f))))))