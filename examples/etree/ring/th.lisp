;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(all x *(*(*(+(x 1) *(+(x -(1)) +(x -(1)))) +(x 1)) +(x -(1)))
     = 
     +(*(x *(x *(x *(x x))))
	+(-(*(x *(x x)))
	   +(-(*(x *(x *(x x))))
	      +(*(x x)
		 +(*(x *(x *(x x)))
		    +(-(*(x x))
		       +(-(*(x *(x x)))
			  +(x
			     +(-(*(x *(x *(x x))))
				+(*(x x)
				   +(*(x *(x x))
				      +(-(x) +(-(*(x *(x x))) +(x +(*(x x) -(1)))))))))))))))))