;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(e(mon mo)
  and e(tue tu) 
  and e(wed we)
  and e(thu th)
  and e(fri fr)
  and e(sat sa)
  and e(sun su))

(all x:mo e(x ll))
(all x:tu e(x ll))
(all x:we e(x ll))

(all x:th not e(x ll))
(all x:fr not e(x ll))
(all x:sa not e(x ll))
(all x:su not e(x ll))

(all x:mo not e(x lu))
(all x:tu not e(x lu))
(all x:we not e(x lu))

(all x:th e(x lu))
(all x:fr e(x lu))
(all x:sa e(x lu))

(all x:su not e(x lu))

(all x:mo e(x d))
(all x:tu e(x d))
(all x:we e(x d))
(all x:th e(x d))
(all x:fr e(x d))
(all x:sa e(x d))
(all x:su e(x d))

(all x:ll e(x d))
(all x:lu e(x d))

(all x:mo e(yester(x) su))
(all x:tu e(yester(x) mo))
(all x:we e(yester(x) tu))
(all x:th e(yester(x) we))
(all x:fr e(yester(x) th))
(all x:sa e(yester(x) fr))
(all x:su e(yester(x) sa))

(all x,y:d (e(x ll) or not lies(lion x y) or e(y ll)))
(all x:d (all y:ll (e(x ll) or lies(lion x y))))
(all x,y:ll (not lies(lion x y)))
(all y:d (all x:ll (lies(lion x y) or e(y ll))))

(all x,y:d (e(x lu) or not lies(unicorn x y) or e(y lu)))
(all x:d (all y:lu (e(x lu) or lies(unicorn x y))))
(all x,y:lu (not lies(unicorn x y)))
(all y:d (all x:lu (lies(unicorn x y) or e(y lu))))



(ex x:d (lies(lion x yester(x)) and lies(unicorn x yester(x))))