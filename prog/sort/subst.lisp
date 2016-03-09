;;; -*- Mode: LISP; Package: MARKGRAF-KARL; Syntax: Common-Lisp;  -*-

(in-package "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

;EDITED : 09-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : Eine gerade Zahl von Argumenten in der Reihenfolge: Variable1 Term1 Variable2 Term2 ... VariableN TermN
;EFFECT : keiner
;VALUE  : die Substitution die durch die Eingabe spezifiziert ist, wenn man diese wie folgt liest:
;         Variable1 <- Term1, Variable2 <- Term2, ... VariableN <- TermN

(defun subst-create (&rest variable.term.list) 
  (if (subst-substitution.p variable.term.list)
      (copy-list variable.term.list)                             ; this is not a bug, this is a feature...
      (error "subst-create - Arguments are no correct substitutions ~S" variable.term.list)))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution - eine Substitution
;EFFECT : keiner
;VALUE  : T, wenn substitution eine Substitution ist und NIL sonst.

(defun subst-substitution.p (substitution)
  (or (null substitution)
      (and (evenp (length substitution))
           (subst-every #'(lambda(subst)
			    (and (dt-variable.is (car subst))
				 (or (dt-term_c.term.is (cadr subst))
				     (dt-variable.is (cadr subst))
				     (dt-constant.is (cadr subst)))))
			substitution))))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : predicate    - eine Funktion, die als Eingabe (<variable> <term>) erhaelt
;         substitution - eine Substitution 
;EFFECT : keiner
;VALUE  : T, wenn das Praedikat predicate fuer alle Substitutionen der Form (<variable> <term>) gilt


(defun subst-every (predicate substitution)
  (if (subst-empty.p substitution)
      t
      (and (funcall predicate (firstn substitution 2))
	   (subst-every predicate (cddr substitution)))))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : predicate    - eine Funktion, die als Eingabe (<variable> <term>) erhaelt
;         substitution - eine Substitution 
;EFFECT : keiner
;VALUE  : den ersten Nicht-NIL-Wert, den das Praedikat predicate liefert oder NIL, falls keiner existiert

(defun subst-some (predicate substitution)
  (if (subst-empty.p substitution)
      nil
      (or (funcall predicate (firstn substitution 2))
	  (subst-some predicate (cddr substitution)))))



;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : msp.function - eine Funktion, die als Eingabe (<variable> <term>) erhaelt
;         substitution - eine Substitution 
;EFFECT : keiner
;VALUE  : eine Liste mit den Werten der Map-Funktion map.function, die sukzessive auf die einzelnen Substitutionen
;         (<variable> <term>) angewendet wurde

(defun subst-map (map.function substitution)
  (if (subst-empty.p substitution)
      nil
      (cons (funcall map.function (firstn substitution 2))
	    (subst-map map.function (cddr substitution)))))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution - eine Substitution
;EFFECT : keiner
;VALUE  : T, wenn die Substitution leer ist und NIL sonst

(defun subst-empty.p(substitution)
  (null substitution))



;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : variable     - eine Variable
;         term         - ein Term
;         substitution - eine Substitution
;EFFECT : keiner
;VALUE  : die Substitution, die aus substituion entsteht, wenn man diese um die Substitution variable <- term erweitert

(defun subst-substitution.extend (variable term substitution)
  (cons variable (cons term substitution)))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : variable     - eine Variable
;         substitution - eine Substitution
;EFFECT : keiner
;VALUE  : den Substitutions-Term, der zu der Variablen variable gehoert, falls diese im Domain der Substitution substitution
;         enthalten ist und NIL sonst

(defun subst-variable.substitution.term (variable substitution)
  (cadr (subst-some #'(lambda(subst)
			(when (dt-term_equal variable (car subst))
			  subst))
		    substitution)))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : variable     - eine Variable
;         substitution - eine Substitution
;EFFECT : keiner
;VALUE  : der Term, der laut Substitution fuer die Variable variable gesetzt werden soll oder die Variable variable selber,
;         falls diese nicht im Domain der Substitution substitution enthalten ist
                        
(defun subst=apply.to.variable (substitution variable)
  (let ((subst.term (subst-variable.substitution.term variable substitution)))
    (if subst.term	     
	subst.term
	variable)))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution - eine Substitution
;         term         - ein Term
;EFFECT : keiner
;VALUE  : der Term, der ensteht, wenn die Substitution substitution auf den Term term angewendet wird

(defun subst-apply.to.term (substitution term)
  (cond ((dt-variable.is term) (subst=apply.to.variable substitution term))
	((dt-constant.is term) term)
	((dt-term_c.term.is term)
	 (dt-term_create (dt-term_topsymbol term) (subst=apply.to.term substitution (dt-term_arguments term))))
	(t (error "subst-apply.to.variable - Term ist kein Term ~S"term))))
                                        

;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution - eine Substitution
;         term.list    - ein Liste von Termen
;EFFECT : keiner
;VALUE  : eine Liste von Termen, die ensteht, wenn die Substitution substitution auf die Terme der term.list angewendet wird
          
(defun subst=apply.to.term (substitution term.list)
  (mapcar #'(lambda (sub.term)
	      (cond ((dt-variable.is sub.term)    (subst=apply.to.variable substitution sub.term))
		    ((dt-constant.is sub.term)    sub.term)
		    ((dt-term_c.term.is sub.term) (subst-apply.to.term substitution sub.term))
		    (t (error "subst=apply.to.term - Term-Liste ist fehlerhaft ~S"term.list))))
	  term.list))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution.1 , substitution.2 - zwei Substitution
;EFFECT : keiner
;VALUE  : die Substitution, die entsteht, wenn substitution.1 auf substitution.2 angewendet wird

(defun subst-apply.to.substitution(substitution.1 substitution.2)
  (if (subst-empty.p substitution.2)
      substitution.2
      (subst-substitution.extend (first substitution.2)
				 (subst-apply.to.term substitution.1 (cadr substitution.2))
				 (subst-apply.to.substitution substitution.1 (cddr substitution.2)))))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution.1 substitution.2 - zwei Substitutionen
;EFFECT : keiner 
;VALUE  : die Substitution, die ensteht, wenn substitution.1 o substitution.2 gebildet wird

(defun subst-composition (substitution.1 substitution.2)
  (let* ((applied.substitution  (subst-apply.to.substitution substitution.1 substitution.2))
         (substitution.1.domain (subst-substitution.domain substitution.1))
         (substitution.2.domain (subst-substitution.domain substitution.2))
         (domain.difference     (set-difference substitution.1.domain substitution.2.domain :test 'dt-term_equal)))
    (if (null domain.difference)
	applied.substitution
	(progn
	  (mapc #'(lambda(variable)
		    (setq applied.substitution
			  (subst-substitution.extend variable
						     (subst-variable.substitution.term variable substitution.1)
						     applied.substitution)))
		domain.difference)
	  applied.substitution))))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution - eine Substitution
;EFFECT : keiner
;VALUE  : Der Domain der Substitution substitution

(defun subst-substitution.domain (substitution)
  (subst-map #'car substitution))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution - eine Substitution
;EFFECT : keiner
;VALUE  : Der Range(CoDomain) der Substitution substitution

(defun subst-range (substitution)
  (subst-map #'second substitution))




;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : variable.list - eine Liste von Variablen
;EFFECT : keiner 
;VALUE  : eine Substitution, die die Variablen von variable.list, durch "frische" Variablen substituiert

(defun subst-variables.rename (variable.list)
  (mapcan #'(lambda(subst.var)
	      (list subst.var (dt-variable.create (dt-variable.sort subst.var))))
	  variable.list))



;EDITED : 11-JUN-1991 15:35
;AUTHORS: KASPER
;INPUT  : substitution - eine Substitution
;EFFECT : die Substitution substitution wird auf den Bildschirm geschrieben
;VALUE  : undefiniert

(defun subst-print (substitution)
  (if (subst-empty.p substitution)
      nil
      (progn
	(format t "~%")
	(subst-print.term (car substitution) t)
	(format t "  <-  ")
	(subst-print.term (cadr substitution) t)
	(subst-print (cddr substitution)))))



(defun subst-print.term (term &optional (stream t))
  (cond ((null term) nil)
	((and (dt-variable.is term) 
	      (numberp (dt-variable.sort term)))
	 (format stream "~(~A~)_~(~A~)" (dt-pname term) (dt-pname (dt-variable.sort term))))
	((dt-variable.is term)(format stream "~(~A~)" (dt-pname term)))
	((dt-constant.is term) (format stream "~(~A~)" (dt-pname term)))
	((dt-term_c.term.is term) (progn
				    (format stream "~(~A~)("  (dt-pname (dt-term_topsymbol term)))
				    (mapl #'(lambda(rest.args)
					      (if (null (rest rest.args))
						  (subst-print.term (first rest.args))
						  (progn
						    (subst-print.term (first rest.args))
						    (format stream " "))))
					  (dt-term_arguments term))
				    (format stream ")")))
	(t (error "subst-print.term - Term ist kein Term"))))