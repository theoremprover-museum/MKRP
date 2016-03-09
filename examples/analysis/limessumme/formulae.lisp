;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MKRP; Base: 10 -*-
(* typisierte Version)
(* Axiome)
(* Typenvereinbarungen)
(sort real,funktion:any)
(sort posreal:any)
(type half(posreal):posreal)
(type funplus(funktion funktion):funktion)
(type apply(funktion real):real)
(type kleinergleich(real real))
(type poskleinergleich(posreal posreal))
(type abs(real):posreal)
(type plus(real real):real)
(type posplus(posreal posreal):posreal)
(type minus(real real):real)
(type limes(funktion real real))
;(type minimum(posreal posreal):posreal)
(type f,g:funktion)

(* Hilfsaxiome)
(* kleinergleich Transitivitaet und Vertraeglichkeit mit minimum)
(all x,y,z:posreal poskleinergleich(x y) and poskleinergleich(y z) impl poskleinergleich(x z))
;(all x,y:posreal poskleinergleich(minimum(x y) x) and poskleinergleich(minimum(x y) y))
(* Archimedische Eigenschaft von R)
(all x:posreal ex y:posreal poskleinergleich(posplus(y y) x))
(* Transitivitaet von plus bzgl. kleinergleich)
(all x,y,z,u:posreal poskleinergleich(x y) and poskleinergleich(z u) impl poskleinergleich(posplus(x z) posplus(y u)))
(* Vertraeglichkeit von plus und minus)
(all a,b,c,d:real minus(plus(a b) plus(c d)) := plus(minus(a c) minus(b d)))
(* Dreiecksungleichung)
(all x,y:real poskleinergleich(abs(plus(x y)) posplus(abs(x) abs(y))))

(* Definition der Summenfunktion)
(all x:real
     apply(funplus(f g) x)    =    plus (apply(f x) apply(g x)))


(* Definition von Grenzwerten)
(all f:funktion all x0,a:real
     limes(f x0 a)
     eqv
     (all epsilon:posreal all x:real ex delta:posreal
	  poskleinergleich(abs(minus(x x0)) delta)
	  impl
	  poskleinergleich(abs(minus(apply(f x) a)) epsilon)))


(* Theorem)
(* Die Grenzwert einer Summe ist die Summe der Grenzwerte, wenn diese existieren)
(all a,b,x0:real (limes(f x0 a) and limes(g x0 b) impl limes(funplus(f g) x0 plus(a b))))