;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-
(* typisierte Version)
(* Axiome)
(* Typenvereinbarungen)
(sort real,funktion,nnreal:any)
(sort posreal:nnreal)
(type funplus(funktion funktion):funktion)
(type apply(funktion real):real)
(type nnklgleich(nnreal nnreal))
(type abs(real):nnreal)
(type plus(real real):real)
(type nnplus(nnreal nnreal):nnreal)
(type minus(real real):real)
(type limes(funktion real real))
(type f,g:funktion)

(* Hilfsaxiome)
(* nnklgleich Transitivitaet)
(all x,y,z:nnreal nnklgleich(x y) and nnklgleich(y z) impl nnklgleich(x z))

(* Archimedische Eigenschaft von R)
(all x:nnreal ex y:nnreal nnklgleich(nnplus(y y) x))

(* Transitivitaet von plus bzgl. nnklgleich)
(all x,y,z,u:nnreal nnklgleich(x y) and nnklgleich(z u) impl nnklgleich(nnplus(x z) nnplus(y u)))

(* Vertraeglichkeit von plus und minus)
(all a,b,c,d:real minus(plus(a b) plus(c d)) := plus(minus(a c) minus(b d)))

(* Dreiecksungleichung)
(all x,y:real nnklgleich(abs(plus(x y)) nnplus(abs(x) abs(y))))

(* Definition der Summenfunktion)
(all x:real
     apply(funplus(f g) x)    =    plus (apply(f x) apply(g x)))


(* Definition von Grenzwerten)
(all f:funktion all x0,a:real
     limes(f x0 a)
     eqv
     (all epsilon:posreal all x:real ex delta:posreal
	  nnklgleich(abs(minus(x x0)) delta)
	  impl
	  nnklgleich(abs(minus(apply(f x) a)) epsilon)))


(* Theorem)
(* Die Grenzwert einer Summe ist die Summe der Grenzwerte, wenn diese existieren)
(all a,b,x0:real (limes(f x0 a) and limes(g x0 b) impl limes(funplus(f g) x0 plus(a b))))