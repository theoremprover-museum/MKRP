;;; -*- Package: MKRP; Mode: LISP; Syntax: Common-lisp -*-

(ac(minimum))
(ac(maximum))


(all x,y min(x y minimum(x y)))
(all x,y max(x y maximum(x y)))

(all x,y ex z min(x y z) eqv min(x y minimum(x y)))
(all x,y ex z max(x y z) eqv max(x y maximum(x y)))

(all x equal(x x))
(all x,y equal(x y) impl equal(y x))
(all x,y,z equal(x y) and equal(y z) impl equal(x z))


(all x max(one x one))
(all x max(x x x))
(all x max(zero x x))
(all x min(zero x zero))
(all x min(x x x))
(all x min(one x x))
(all x,y,z not min(x y z) or max(x z x))
(all x,y,z not max(x y z) or min(x z x))
(all u,v,w,x,y,z not min(u v u) or not max(u w x) or not min(w v y) or not min(v x z) or max(u y z))
(all u,v,w,x,y,z not min(u v u) or not max(u w x) or not min(w v y) or not max(u y z) or min(v x z))
(all u,v,w,x,y,z not min(u v v) or not max(w v x) or not min(u w y) or not min(u x z) or max(v y z))
(all u,v,w,x,y,z not min(u v v) or not max(w v x) or not min(u w y) or not max(v y z) or min(u x z))

(all x max(one x one))
(all x max(x x x))
(all x max(zero x x))
(all x min(zero x zero))
(all x min(x x x))
(all x min(one x x))
(all x,y max(x minimum(x y) x))
(all x,y min(x maximum(x y) x))
(all x,y,z min(x z x) impl max(x minimum(y z) minimum(z maximum(x y))))
(all x,y,z min(x z x) impl min(z maximum(x y) maximum(x minimum(y z))))
(all x,y,z min(x z z) impl max(z minimum(x y) minimum(x maximum(y z))))
(all x,y,z min(x z z) impl min(x maximum(y z) maximum(z minimum(x y))))

(all x equal(one maximum(one x)))
(all x equal(x maximum(x x)))
(all x equal(x maximum(zero x)))
(all x equal(zero minimum(zero x)))
(all x equal(x minimum(x x)))
(all x equal(x minimum(one x)))
(all x,y equal(x maximum(x minimum(x y))))
(all x,y equal(x minimum(x maximum(x y))))
(all x,y,z equal(x minimum(x z)) impl equal(minimum(z maximum(x y)) maximum(x minimum(y z))))
(all x,y,z equal(z minimum(x z)) impl equal(maximum(z minimum(x y)) minimum(x maximum(y z))))


(min(a b c) and max(c d one) and min(b d e) and min(a e zero) and max(b a2 b2) and max(a b2 one) and max(a b c2) and min(a2 c2 zero) and min(d a d2) and max(a2 d2 e2) and min(d b a3) and max(a2 a3 b3) impl min(b3 e2 a2))

(max(minimum(a b) d one) and min(a minimum(b d) zero) and max(a maximum(b a2) one) and min(a2 maximum(a b) zero) impl min(maximum(a2 minimum(d b)) maximum(a2 minimum(d a)) a2))

(equal(one maximum(minimum(a b) d)) and equal(zero minimum(a minimum(b d))) and equal(one maximum(a maximum(b a2))) and equal(zero minimum(a2 maximum(a b))) impl equal(a2 minimum(maximum(a2 minimum(d b)) maximum(a2 minimum(d a)))))