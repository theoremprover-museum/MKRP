;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-


Clause Set 2

(all x0,xp0,xp1 not e(x0) or not exp(f(x0 xp0)) or gt(f(x0 ap0) f(x0 xp1)) or not exp(f(x0 xp1)))
(all x0,xp0 not e(x0) or not exp(f(x0 xp0)) or s(f(x0 ap0)))
(all x0,xp0 not e(x0) or not exp(f(x0 xp0)) or ine(f(x0 ap0)))
(all x0 exp(f(x0 ap1)) or e(x0))
(all x0,xp2,xp3 not s(f(x0 xp2)) or not ine(f(x0 xp2)) or ge(f(x0 ap2) f(x0 xp3)) or
     not s(f(x0 xp3)) or not ine(f(x0 xp3)) or e(x))
(all x0,xp2 not s(f(x0 xp2)) or not ine(f(x0 xp2)) or exp(f(x0 ap2)) or e(x0))
(all x1,xp4 not s(x1) or gt(f(x1 ap3) f(x1 xp4)) or not ine(f(x1 xp4)))
(all x1 not s(x1) or e(f(x1 ap3)))
(all x1 not s(x1) or exp(f(x1 ap3)))
(all x1,xp5,xp6 not e(f(x1 xp5)) or not exp(f(x1 xp5)) or ge(f(x1 ap4) f(x1 xp6)) or not e(f(x1 xp6)) or not exp(f(x1 xp6))
     or s(x1))
(all x1,xp5 not e(f(x1 xp5)) or not exp(f(x1 xp5)) or ine(f(x1 ap4)) or s(x1))
(all x2 not s(x2) or not bs(x2) or os(f(x2 af0)))
(all x3 not os(x3) or s(f(x3 ap5)))
(all x3 not os(x3) or bs(f(x3 ap5)))
(all x4 not s(x4) or not ini(x4) or osa(f(x4 af1)))
(all x5 not osa(x5) or s(f(x5 ap6)))
(all x5 not osa(x5) or ini(f(x5 ap6)))
(all x6 not s(x6) or not ine(x6) or r(x6))
(all x7 not bc(x7) or ose(f(x7 af2)))
(all x8 not ose(x8) or bc(f(x8 ap7)))
(all x9 bs(x9) eqv ps(x9))
(all x10 bc(x10) eqv pc(x10))

(not 
  ((pc(a0) or ose(a1)) and
   (all xf0 not ose(f(a0 xf0)) or ose(a1)) and
   (all xp7 pc(a0) or not pc(f(a1 xp7))) and
   (all xf0,xp7 not ose(f(a0 xf0)) or not pc(f(a1 xp7)))))



Clause Set 1

(all x22,xp17,xp18 not e(x22) or not exp(f(x22 xp17)) or gt(f(x22 ap16) f(x22 xp18)) or not exp(f(x22 xp18)))
(all x22,xp17 not e(x22) or not exp(f(x22 xp17)) or s(f(x22 ap16)))
(all x22,xp17 not e(x22) or not exp(f(x22 xp17)) or ine(f(x22 ap16)))
(all x22 exp(f(x22 ap17)) or e(x22))
(all x22,xp19,xp20 not s(f(x22 xp19)) or not ine(f(x22 xp19)) or ge(f(x22 ap18) f(x22 xp20)) or
     not s(f(x22 xp20)) or not ine(f(x22 xp20)) or e(x22))
(all x22,xp19 not s(f(x22 xp19)) or not ine(f(x22 xp19)) or exp(f(x22 ap18)) or e(x22))
(all x23,xp21 not s(x23) or gt(f(x23 ap19) f(x23 xp21)) or not ine(f(x23 xp21)))
(all x23 not s(x23) or e(f(x23 ap19)))
(all x23 not s(x23) or exp(f(x23 ap19)))
(all x23,xp22,xp23 not e(f(x23 xp22)) or not exp(f(x23 xp22)) or ge(f(x23 ap20) f(x23 xp23)) or
     not e(f(x23 xp23)) or not exp(f(x23 xp23)) or s(x23))
(all x23,xp22 not e(f(x23 xp22)) or not exp(f(x23 xp22)) or ine(f(x23 ap4)) or s(x23))
(all x24 not s(x24) or not bs(x24) or os(f(x24 af6)))
(all x25 not os(x25) or s(f(x25 ap21)))
(all x25 not os(x25) or bs(f(x25 ap21)))
(all x26 not s(x26) or not ini(x26) or osa(f(x26 af7)))
(all x27 not osa(x27) or s(f(x27 ap22)))
(all x27 not osa(x27) or ini(f(x27 ap22)))
(all x28 not s(x28) or not ine(x28) or r(x28))
(all x29 not bc(x29) or ose(f(x29 af2)))
(all x30 not ose(x30) or bc(f(x30 ap7)))
(all x31 bs(x31) eqv ps(x31))
(all x32 bc(x32) eqv pc(x32))




 
(not
  ((s(a6) or os(a7)) and
   (ps(a6) or os(a7)) and
   (all xf3 not os(f(a6 xf3)) or os(a7)) and
   (all xp24 s(a6) or not s(f(a7 xp24)) or not ps(f(a7 xp24))) and
   (all xp24 ps(a6) or not s(f(a7 xp24)) or not ps(f(a7 xp24))) and
   (all xf3,xp24 not os(f(a6 xf3)) or not s(f(a7 xp24)) or not ps(f(a7 xp24)))))
