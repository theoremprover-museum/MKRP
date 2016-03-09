;;; -*- Package: MARKGRAF-KARL; Mode: LISP; Syntax: Common-Lisp -*-

(in-package "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(proclaim '(declaration edited authors input effect value remarks))


(defvar upr*trace.print nil)


(defvar upr*term.element.of.sort.node.used.residues nil)


;-------------------------------------------------------------------------------------------------------
;--------------------------- THE MKRP UNIFICATION INTERFACE --------------------------------------------

(defvar sort_max.unification.rule.steps 40)

(defun upr=unification.maximum.number.of.applied.rules ()
  (opt-get.option sort_max.unification.rule.steps))


	  
(defvar sort_max.unification.tree.depth 100)

(defun upr=unification.maximum.tree.depth ()
  (opt-get.option sort_max.unification.tree.depth))



(defvar sort_max.unification.tree.open.nodes 20)

(defun upr=unification.maximum.open.nodes ()
  (opt-get.option sort_max.unification.tree.open.nodes))



(defvar sort_unifier.stop.number 100)

(defun upr=unification.n.unifier.stop.number ()
 (opt-get.option sort_unifier.stop.number))


(defvar sort_show.variable.sorts t)




(defun upr-match.substitution (subst variables)
  (declare (edited  "28-JAN-1992 18:28")
	   (authors KASPER)
	   (input   "substitution - eine MKRP-Substitution")
	   (effect  "keiner")
	   (value   "die Substitution, falls diese wohlsortiert ist; NIL sonst")
	   (ignore  variables))
  (when  (subst-every #'(lambda (subst)
			  (and
			    (upr=term.element.of.sort.node.p (first (subst-range subst))
							     (uds-variable.sort.node (first (subst-substitution.domain subst))))
			    (not upr*term.element.of.sort.node.used.residues)


			    #|  (upr=variable.term.element.of.sort.node.residues
				    (first (subst-range subst))
				    (uds-variable.sort.node (first (subst-substitution.domain subst))))) |#
		))
		      subst)
    (list subst)))



(defun upr-unify.substitution (substitution)
  (declare (edited  "28-JAN-1992 18:18")
	   (authors KASPER)
	   (input   "substitution- eine MKRP-Substitution")
	   (effect  "keiner")
	   (value   "eine Liste von  Paaren mit den gefunden Unifikatoren und ihren Residuen" ))
  ;(subst-print substitution)
  ;(terpri)
  (let* ((start.unification.state  (upr=substitution.unification.state.create substitution))
	 (result.unification.state (upr-unification.state.solve start.unification.state))) 
    (cond ((upr=unification.state.rule.fail.p      result.unification.state) nil)
	  ((upr=unification.state.control.fail.p   result.unification.state) (upr=unify.to.end result.unification.state))
          ((upr=unification.state.unifiers.found.p result.unification.state) (upr=unify.to.end result.unification.state))
	  (t (mapcar #'(lambda(mgu)
			 (cons (uds-mgu.substitution mgu) (uds-mgu.residuum.literals mgu)))
		     (uds-unification.state.mgus result.unification.state))))))



(defun upr=substitution.unification.state.create (substitution)
  (declare (edited  "28-JAN-1992 18:20")
	   (authors KASPER)
	   (input   "substitution - eine MKRP-Substitution")
	   (effect  "keiner")
	   (value   "ein Unifikationszustand mit einem Unifikationsproblem, dessen Gleichungen aus der Substitution gebildet"
		    "werden"))
  (uds-unification.state.create
    :problems (list (uds-unification.problem.create :set       (upr=substitution.to.equations substitution)
						    :variables (upr=substitution.variables substitution)))
    :number.of.open.nodes 1
    :tree.depth           1))



(defun upr=substitution.to.equations (substitution)
  (declare (edited  "28-JAN-1992 18:24")
	   (authors KASPER)
	   (input   "substitution - eine MKRP-Substitution")
	   (effect  "keiner")
	   (value   "eine Liste von Gleichungen, die aus der Substitution entanden sind"))
  (subst-map #'(lambda(variable.substitution)
		 (uds-equation.create (first (subst-substitution.domain variable.substitution))
				      (first (subst-range variable.substitution))))
	     substitution))



(defun upr=substitution.variables (substitution)
  (declare (edited  "30-JAN-1992 14:46")
	   (authors KASPER)
	   (input   "substitution - eine MKRP-Substitution")
	   (effect  "keiner")
	   (value   "Liste aller Variablen, die im Domain und Codomain der Substitution vorkommen"))
  (remove-duplicates (union (subst-substitution.domain substitution)
			    (apply 'append (mapcar #'(lambda(term)
						       (dt-term.variables term))
						   (subst-range substitution)))
			    :test 'dt-term_equal)
		     :test 'dt-term_equal))


(defun upr=unify.to.end (unification.state)
  (upr=unification.rules.set  (remove-if #'uds-unification.rule.sorted.p upr*unification.rules))
  (prog1                                                            
    (upr=unify.unsorted unification.state)
    (upr=unification.rules.init)
    (upr=unification.rules.set (upr=unification.rules))))



(defun upr=unify.unsorted (unification.state)
  (cond ((upr=unification.state.rule.fail.p  unification.state) nil)
	((upr=unification.state.solved.p     unification.state)
	 (mapcar #'(lambda(mgu)
		     (cons (uds-mgu.substitution mgu) (upr=mgu.residuum.literals.create mgu)))
		 (uds-unification.state.mgus unification.state)))	
	(t (upr=unify.unsorted (upr=unification.state.unification.step.perform unification.state)))))


(defun upr=mgu.residuum.literals.create (mgu)
  (declare (edited  "19-JUN-1992 15:05")
	   (authors KASPER)
	   (input   )
	   (effect  )
	   (value   ))
  (subst-map #'(lambda(subst)
		 (ds-lit.create '- (dt-predicate.element) (list (first (subst-range subst))
								(dt-variable.sort (first (subst-substitution.domain subst))))))
	     (uds-mgu.substitution mgu)))
  
;---------------------------------- END OF THE MKRP UNIFICATION INTERFACE ---------------------------
;----------------------------------------------------------------------------------------------------


(defun upr-unification.state.solve (unification.state)
  (declare (edited  "28-JAN-1992 17:38")
	   (authors KASPER)
	   (input   "unification.state - eine unification.state-Datensrtuktur")
	   (effect  "keiner")
	   (value   "das geloeste oder abbgebrochene Unifikations-Problem"))
;  (uds-unification.state.print unification.state)
  (cond ((upr=unification.state.control.fail.p   unification.state) (values unification.state :CONTROL.FAIL))
	((upr=unification.state.rule.fail.p      unification.state) (values unification.state :RULE.FAIL))
	((upr=unification.state.solved.p         unification.state) (values unification.state :UNIFICATION.SOLVED))
	((upr=unification.state.unifiers.found.p unification.state) (values unification.state :UNIFIERS.FOUND))
	(t (upr-unification.state.solve (upr=unification.state.unification.step.perform unification.state)))))


(defun upr=unification.state.control.fail.p (unification.state)
  (declare (edited  "28-JAN-1992 17:39")
	   (authors KASPER)
	   (input   "unification.state - eine unification.state-Datensrtuktur")
	   (effect  "keiner")
	   (value   "T, falls eine Restriktion verletzt wurde; NIL sonst"))
  (or (> (uds-unification.state.tree.depth unification.state)
	 (upr=unification.maximum.tree.depth))
      (> (uds-unification.state.number.of.open.nodes unification.state)
	 (upr=unification.maximum.open.nodes))
      (> (uds-unification.state.number.of.applied.rules unification.state)
	 (upr=unification.maximum.number.of.applied.rules))))



(defun upr=unification.state.rule.fail.p (unification.state)
  (declare (edited  "28-JAN-1992 17:41")
	   (authors KASPER)
	   (input   "unification.state - eine unification.state-Datensrtuktur")
	   (effect  "keiner")
	   (value   "T, falls die Unifikation nicht loesbar ist"))
  (and (null (uds-unification.state.problems unification.state))
       (null (uds-unification.state.mgus unification.state))))



(defun upr=unification.state.unifiers.found.p (unification.state)
  (declare (edited  "28-JAN-1992 17:42")
	   (authors KASPER)
	   (input   "unification.state - eine unification.state-Datensrtuktur")
	   (effect  "keiner")
	   (value   "T, falls eine bestimmte Anzahl (upr=unification.n.unifier.stop.number) viele gefunden wurde"))
  (= (upr=unification.n.unifier.stop.number)
     (length (uds-unification.state.mgus unification.state))))



(defun upr=unification.state.solved.p (unification.state)
  (declare (edited  "28-JAN-1992 17:43")
	   (authors KASPER)
           (input   "unification.state - eine unification.state-Datensrtuktur")
	   (effect  "keiner")
	   (value   "T, falls alle Unifikationsprobleme geloest sind"))
  (and (null (uds-unification.state.problems unification.state))
       (uds-unification.state.mgus unification.state)))




(defun upr=unification.state.unification.step.perform (unification.state)
  (declare (edited  "28-JAN-1992 17:54")
	   (authors KASPER)
	   (input   "unification.state - eine unification.state-Datenstruktur")
	   (effect  "keiner")
	   (value   "der Unifications-Zustand, nachdem ein Unifikationsschritt vollzogen wurde"))
  (let* ((next.unification.problem (upr=unification.state.next.unification.problem unification.state))
        (unification.object (upr=unification.problem.rules.applicable.p next.unification.problem (upr=fail.unification.rules))))
    (if (uds-unification.object.p unification.object)
	(progn
	  (when upr*trace.print 
	    (format t "~%===========Unifikation fail: ~%~S" (mapcar #'uds-unification.rule.name
								    (uds-unification.object.rules unification.object)))
	    (uds-equation.print (uds-unification.object.equation unification.object)))
	(upr=unification.state.fail.unification.step unification.state next.unification.problem))
	(let ((unification.object (upr=unification.problem.rules.applicable.p next.unification.problem
									      (upr=unsorted.unification.rules))))
	  (if (uds-unification.object.p unification.object)
	      (upr=unification.state.unsorted.unification.step unification.state unification.object)
	      (let ((unification.object (upr=unification.problem.every.rule.applicable.p next.unification.problem
										         (upr=non.confluent.unification.rules))))
		(if (uds-unification.object.p unification.object)
		    (upr=unification.state.non.confluent.unification.step unification.state unification.object)
		    (let ((unification.object (upr=unification.problem.rules.applicable.p next.unification.problem
											  (upr=sorted.unification.rules))))
		      (if (uds-unification.object.p unification.object)
			  (upr=unification.state.sorted.unification.step unification.state unification.object)
			  (upr=unification.state.insert.mgu unification.state next.unification.problem))))))))))



(defun upr=unification.state.fail.unification.step (unification.state next.unification.problem)
  (declare (edited  "29-JAN-1992 16:33")
	   (authors KASPER)
	   (input   "unification.state - eine unification.state-Datenstruktur"
		    "unification.problem - eine unification.problem-Datenstruktur")
	   (effect  "keiner")
	   (value   "der um das next.unification.problem reduzierte unification.state"))
  (let ((new.unification.problems (remove next.unification.problem (uds-unification.state.problems unification.state)
					  :test 'eq :count 1)))
    (uds-unification.state.create
      :problems      new.unification.problems 
      :tree.depth    (apply 'max (cons 1 (mapcar #'uds-unification.problem.depth new.unification.problems )))
      :number.of.open.nodes    (1- (uds-unification.state.number.of.open.nodes  unification.state))
      :number.of.applied.rules (1+ (uds-unification.state.number.of.applied.rules unification.state))
      :mgus                    (uds-unification.state.mgus unification.state))))



(defun upr=unification.state.unsorted.unification.step (unification.state unification.object)
  (declare (edited  "29-JAN-1992 16:44")
	   (authors KASPER)
	   (input   "unification.state  - eine unification.state-Datenstruktur"
		    "unification.object - eine unification.object-Datenstruktur")
	   (effect  "keiner")
	   (value   "der unification.state nach Anwendung einer Robinson-Unifikations-Regel"))
  (let* ((old.unification.problems (remove (uds-unification.object.unification.problem unification.object)
				 	   (uds-unification.state.problems unification.state) :test 'eq :count 1))
	 (new.unification.problems (upr=unification.object.unification.step unification.object))
	 (new.unification.state.unification.problems (append old.unification.problems new.unification.problems)))
    (uds-unification.state.create
      :problems   new.unification.state.unification.problems 
      :tree.depth (apply 'max (mapcar #'uds-unification.problem.depth new.unification.state.unification.problems))
      :number.of.open.nodes    (uds-unification.state.number.of.open.nodes  unification.state)
      :number.of.applied.rules (1+ (uds-unification.state.number.of.applied.rules unification.state))
      :mgus                    (uds-unification.state.mgus unification.state))))




(defun upr=unification.state.non.confluent.unification.step (unification.state unification.object)
  (declare (edited  "29-JAN-1992 16:58")
	   (authors KASPER)
	   (input   "unification.state  - eine unification.state-Datenstruktur"
		    "unification.object - eine unification.object-Datenstruktur")
	   (effect  "keiner")
	   (value   "der unification.state nach Anwendung der beiden Regeln 3 und 4 der Sorten-Unifikation"))
  (let* ((old.unification.problems (remove (uds-unification.object.unification.problem unification.object)
				 	   (uds-unification.state.problems unification.state) :test 'eq :count 1))
	 (new.unification.problems (upr=unification.object.unification.step unification.object))
	 (new.unification.state.unification.problems (append old.unification.problems new.unification.problems)))
    (uds-unification.state.create
      :problems   new.unification.state.unification.problems 
      :tree.depth (apply 'max (mapcar #'uds-unification.problem.depth new.unification.state.unification.problems))
      :number.of.open.nodes (length new.unification.state.unification.problems)
      :number.of.applied.rules (+ (length new.unification.state.unification.problems)
				  (uds-unification.state.number.of.applied.rules unification.state))
      :mgus                    (uds-unification.state.mgus unification.state))))



(defun upr=unification.state.sorted.unification.step (unification.state unification.object)
  (declare (edited  "29-JAN-1992 16:59")
	   (authors KASPER)
	   (input   "unification.state  - eine unification.state-Datenstruktur"
		    "unification.object - eine unification.object-Datenstruktur")
	   (effect  "keiner")
	   (value   "der unification.state nach Anwendung einer Regel der Sorten-Unifikation"))
  (let* ((old.unification.problems (remove (uds-unification.object.unification.problem unification.object)
				 	   (uds-unification.state.problems unification.state) :test 'eq :count 1))
	 (new.unification.problems (upr=unification.object.unification.step unification.object))
	 (new.unification.state.unification.problems (append old.unification.problems new.unification.problems)))
    (uds-unification.state.create
      :problems   new.unification.state.unification.problems 
      :tree.depth (apply 'max (mapcar #'uds-unification.problem.depth new.unification.state.unification.problems))
      :number.of.open.nodes (length new.unification.state.unification.problems)
      :number.of.applied.rules (+ (length new.unification.state.unification.problems)
				  (uds-unification.state.number.of.applied.rules unification.state))
      :mgus                    (uds-unification.state.mgus unification.state))))



(defun upr=unification.state.insert.mgu (unification.state solved.unification.problem)
  (declare (edited  "26-FEB-1992 14:03")
	   (authors KASPER)
	   (input   "unification.state - ein Unifikationsstatus"
		    "solved.unification.problem - ein Unifikationsproblem")
	   (effect  "keiner")
	   (value   "ein Unifikationsstatus ohne das solved.unification.problem und mit einem enstprechenden mgu"))
  (let ((new.unification.problems (remove solved.unification.problem (uds-unification.state.problems unification.state))))
    (uds-unification.state.create
      :problems new.unification.problems 
      :tree.depth (apply 'max (cons 1 (mapcar #'uds-unification.problem.depth new.unification.problems)))
      :number.of.open.nodes (length new.unification.problems )
      :number.of.applied.rules (uds-unification.state.number.of.applied.rules unification.state)
      :mgus (cons (uds-mgu.create (apply 'subst-create (mapcan #'(lambda(equation)
								   (list (uds-equation.left.term equation)
									 (uds-equation.right.term equation)))
							       (uds-unification.problem.set solved.unification.problem)))
				  (uds-unification.problem.residuum.literals solved.unification.problem))
		  (uds-unification.state.mgus unification.state)))))
  



(defun upr=unification.object.unification.step (unification.object)
  (declare (edited  "29-JAN-1992 17:08")
	   (authors KASPER)
	   (input   "unification.object - eine unification.object-Datenstruktur")
	   (effect  "keiner")
	   (value   "eine Liste von Unifikationsproblemen, die ensteht, wenn man die Regel aus dem unification.object auf"
		    "die Gleichung im unification.object des unification.problems im unification.object anwendet"))
  (when upr*trace.print
    (format t "~3%Regel(n): ")
    (mapc #'(lambda(rule)
	      (format t "~A "(uds-unification.rule.name rule)))
	  (uds-unification.object.rules unification.object))
    (format t "~%Gleichung: ")
    (uds-equation.print (uds-unification.object.equation unification.object))
    (uds-unification.problem.print (uds-unification.object.unification.problem unification.object))
						;(break)
    (format t "~%Liste der neuen Unifikationsprobleme:~%"))
  (mapcan #'(lambda(rule)
	      (let ((p
		      (upr=unification.problem.equation.unification.rule.apply
			(uds-unification.object.unification.problem unification.object)
			(uds-unification.object.equation unification.object)
			rule)))
			(when upr*trace.print
			  (mapc #'uds-unification.problem.print p))
		p))
	  (uds-unification.object.rules unification.object)))


(defun upr=unification.state.next.unification.problem (unification.state)
  (declare (edited  "28-JAN-1992 18:05")
	   (authors KASPER)
	   (input   "unification.state - eine unification.state-Datenstruktur")
	   (effect  "keiner")
	   (value   "das als naechtes zu behandelnde Unifikationsproblem")
	   (remarks "In diese Funktion sollte bei Bedarf ein entsprechende Unifikations-Problem-Auswahlheuristik"
		    "eingesetzt werden."))
  (first (uds-unification.state.problems unification.state)))



(defun upr=unification.problem.rules.applicable.p (unification.problem rules)
  (declare (edited  "28-JAN-1992 18:06")
	   (authors KASPER)
	   (input   "unification.problem - eine unification.problem Datenstruktur"
		    "rules               - eine Liste von Unifikationsregel-Datenstrukturen")
	   (effect  "keiner")
	   (value   "ein Unifikationsobjekt, falls eine Regel aus rules auf das unification.problem anwendbar ist; NIL sonst"))
  (some #'(lambda(rule)
	    (some #'(lambda(equation)
		      (when (upr=equation.unification.rule.applicable.p unification.problem equation rule)
			(uds-unification.object.create unification.problem equation (list rule))))
		  (uds-unification.problem.set unification.problem)))
	rules))



(defun upr=unification.problem.every.rule.applicable.p (unification.problem rules)
  (declare (edited  "26-FEB-1992 15:49")
	   (authors KASPER)
	   (input   "unification.problem - eine Unifikations-Problem-Datenstruktur"
		    "rules               - eine Liste von Unifikations-Regel-Datenstrukturen")
	   (effect  "keiner")
	   (value   "T, wenn alle Regeln in der Liste rules auf eine Gleichung des unification.problem anwendbar sind"))
  (some #'(lambda(equation)
	    (when  (and rules
			(every #'(lambda(rule)
				   (upr=equation.unification.rule.applicable.p unification.problem equation rule))
			       rules))
	      (uds-unification.object.create unification.problem equation rules)))
	(uds-unification.problem.set unification.problem)))



(defun upr=equation.unification.rule.applicable.p (unification.problem equation rule)
  (declare (edited  "28-JAN-1992 18:08")
	   (authors KASPER)
	   (input   "equation - eine Gleichungs-Datenstruktur"
		    "rule     - eine Unifikations- Regel-Datenstruktur")
	   (effect  "keiner")
	   (value   "T, falls die Regel rule auf die Gleichung equation anwendbar ist; NIL sonst"))
  (let ((function (uds-unification.rule.apply.predicate rule)))
    (if function
	(funcall function unification.problem equation)
	(error "upr=equation.unification.rule.applicable.p - Die Regel ~A hat kein Apply-Praedikat"
	       (uds-unification.rule.name rule)))))



(defun upr=unification.problem.equation.unification.rule.apply (unification.problem equation rule)
  (declare (edited  "31-JAN-1992 15:11")
	   (authors KASPER)
	   (input   "unification.problem - ein Unifikations-Problem"
		    "equation            - eine Gleichung"
		    "rule                - eine Unifikations-Regel")
	   (effect  "keiner")
	   (value   "Die Unifikations-Regel rule wird auf die Gleichung equation im Unifikations-Problem unification.problem"
		    "angewendet"))
  (let ((function (uds-unification.rule.applicator rule)))
    (if function
	(funcall function unification.problem equation)
	(error "upr=unification.problem.equation.unification.rule.apply - Die Regel ~A hat keine Apply-Funktion"
	       (uds-unification.rule.name rule)))))


;----------------------------------------------------------------------------------------------
;------------------------- THE UNIFICATION RULES ITSSELF --------------------------------------

(defvar upr*unification.rules nil)

(defun upr=unification.rules ()
  upr*unification.rules)



(defvar upr*unsorted.unification.rules)

(defun upr=unsorted.unification.rules ()
   upr*unsorted.unification.rules)



(defvar upr*fail.unification.rules)

(defun upr=fail.unification.rules ()
  upr*fail.unification.rules)




(defvar upr*non.confluent.unification.rules nil)

(defun upr=non.confluent.unification.rules ()
  upr*non.confluent.unification.rules)


(defvar upr*sorted.unification.rules nil)

(defun upr=sorted.unification.rules ()
  upr*sorted.unification.rules)



(defvar upr*occur.check)

(defvar upr*clash )

(defvar upr*subst )

(defvar upr*term.right )

(defvar upr*var.eliminate)

(defvar upr*equal.tops)


(defvar upr*sub.sort )

(defvar upr*max.sort )

(defvar upr*unification.rule.8 )

(defvar upr*unification.rule.4 )

(defvar upr*omega.fail )

(defvar upr*no.common.sub.sort.fail )

(defvar upr*no.sub.sort.l.epsilon.term.fail )

(defvar upr*sort.empty.fail )


(defun upr=unification.rules.init ()
  (declare (edited  "17-FEB-1992 15:15")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Liste aller dem System bekannten Unifikationsregeln werden initialisiert")
	   (value   "undefiniert"))
  (setf upr*unification.rules (list upr*occur.check upr*clash upr*subst upr*term.right upr*var.eliminate upr*equal.tops
				    upr*sub.sort upr*max.sort upr*unification.rule.8 upr*unification.rule.4 upr*omega.fail
				    upr*no.common.sub.sort.fail upr*no.sub.sort.l.epsilon.term.fail upr*sort.empty.fail)))



(defun upr-init ()
  (declare (edited  "17-FEB-1992 15:25")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "der Modul UPR wird initialisiert - insbesondere werden alle Regel defaaultmaessig gesetzt")
	   (value   "undefiniert"))
  (upr=unification.rule.occur.check.init)
  (upr=unification.rule.clash.init)
  (upr=unification.rule.subst.init)
  (upr=unification.rule.term.right.init)
  (upr=unification.rule.var.eliminate.init)
  (upr=unification.rule.equal.tops.init)
  

  (upr=unification.rule.sub.sort.init)
  (upr=unification.rule.max.sort.init)
  (upr=unification.rule.8.init)
  (upr=unification.rule.4.init)
  (upr=unification.rule.omega.fail.init)
  (upr=unification.rule.no.common.sub.sort.fail.init)
  (upr=unification.rule.no.sub.sort.l.epsilon.term.fail.init)
  (upr=unification.rule.sort.empty.fail.init)

  (upr=unification.rules.init)
  (upr=unification.rules.set (upr=unification.rules)))



(defun upr=unification.rules.set (unification.rules)
  (declare (edited  "17-FEB-1992 15:20")
	   (authors KASPER)
	   (input   "unification.rules - eine Liste von Unifikations-Regeln")
	   (effect  "Die im Algorithmus zu benutzenden Regeln werden gesetzt")
	   (value   "undefiniert"))
  (setf upr*unsorted.unification.rules (remove-if #'(lambda(rule)
						      (or (uds-unification.rule.sorted.p rule)
						          (uds-unification.rule.fail.p rule)))
						  unification.rules))
  (setf upr*fail.unification.rules (remove-if #'(lambda(rule) (not (uds-unification.rule.fail.p rule))) unification.rules))
  (setf upr*non.confluent.unification.rules (remove-if #'uds-unification.rule.confluent.p unification.rules))
  (setf upr*sorted.unification.rules (remove-if #'(lambda(rule)
						    (or (not (uds-unification.rule.sorted.p rule))
							(uds-unification.rule.fail.p rule)))
						unification.rules)))



;--------------------------- UNSORTED UNIFICATION RULES ---------------------------------------

;--------------------------------- OCCUR CHECK ------------------------------------------------


(defun upr=unification.rule.occur.check.applicable.p (unification.problem equation)
  (declare (edited  "30-JAN-1992 14:31")
	   (authors KASPER)
	   (ignore  unification.problem)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die Occur-check-Regel auf die Gleichung equation angwendet werden kann; NIL sonst"))
  (and (dt-variable.is (uds-equation.left.term equation))
       (not (dt-term_equal (uds-equation.left.term equation) (uds-equation.right.term equation)))
       (dt-term_in (uds-equation.left.term equation) (uds-equation.right.term equation))))



(defun upr=unification.rule.occur.check.init ()
  (declare (edited  "31-JAN-1992 15:09")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Occur-Check-Regel wird erzeugt und ihrer Variablen zugewiesen")
	   (value   "undefiniert"))
  (setf upr*occur.check (uds-unification.rule.create :applicator      NIL
						     :apply.predicate 'upr=unification.rule.occur.check.applicable.p
						     :sorted.p        NIL
						     :fail.p          T
						     :confluent.p     T
						     :name            "Occur-Check-Regel")))


;------------------------------------------ CLASH ----------------------------------------------
      
(defun upr=unification.rule.clash.applicable.p (unification.problem equation)
  (declare (edited  "30-JAN-1992 14:33")
	   (authors KASPER)
	   (ignore  unification.problem)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die Clash-Regel auf die Gleichung equation angwendet werden kann; NIL sonst"))
  (and (or (dt-term_c.term.is (uds-equation.left.term equation))	;
	   (dt-constant.is (uds-equation.left.term equation)))
       (or  (dt-term_c.term.is (uds-equation.right.term equation))
	    (dt-constant.is (uds-equation.right.term equation)))
       (not (equal (dt-term_topsymbol (uds-equation.left.term equation))
		   (dt-term_topsymbol (uds-equation.right.term equation))))))
     



(defun upr=unification.rule.clash.init ()
  (declare (edited  "31-JAN-1992 15:25")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Clash-Regel wird erzeugt und ihrer Variablen zugewiesen.")
	   (value   "undefiniert"))
  (setf upr*clash (uds-unification.rule.create :applicator      NIL
					       :apply.predicate 'upr=unification.rule.clash.applicable.p
					       :sorted.p        NIL
					       :fail.p          T
					       :confluent.p     T
					       :name "Clash-Regel")))

 

;----------------------------------------- SUBST RULE -------------------------------------------------


(defun upr=unification.rule.subst.applicable.p (unification.problem equation)
  (declare (edited  "30-JAN-1992 14:34")
	   (authors KASPER)	   
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die Substitutions-Regel auf die Gleichung equation angwendet werden kann; NIL sonst"))
  (and (dt-variable.is (uds-equation.left.term equation))
       (not (dt-term_in (uds-equation.left.term equation) (uds-equation.right.term equation)))
       (member (uds-equation.left.term equation) (uds-unification.problem.variables unification.problem) :test 'dt-term_equal)))



(defun upr=unification.rule.subst.apply (unification.problem equation)
  (declare (edited  "30-JAN-1992 14:51")
	   (authors KASPER)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "das Unifikations-Problem nach Anwendung der Substitutions-Regel"))
  (let ((subst (subst-create (uds-equation.left.term equation) (uds-equation.right.term equation))))
    (list (uds-unification.problem.create
	    :set (cons equation
		       (mapcar #'(lambda(map.equation)			  
				   (uds-equation.create (subst-apply.to.term subst (uds-equation.left.term map.equation))
							(subst-apply.to.term subst (uds-equation.right.term map.equation))
							:locked (uds-equation.locked map.equation)))
			       (remove equation (uds-unification.problem.set unification.problem) :test 'eq :count 1)))
	    :variables (remove (uds-equation.left.term equation)
			       (uds-unification.problem.variables unification.problem) :test 'dt-term_equal :count 1)
	    :depth     (1+ (uds-unification.problem.depth unification.problem))
	    :residuum.literals (uds-unification.problem.residuum.literals unification.problem)))))




(defun upr=unification.rule.subst.init ()
  (declare (edited  "31-JAN-1992 15:25")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Substitutions-Regel wird erzeugt und ihrer Variablen zugewiesen")
	   (value   "undefiniert"))
  (setf upr*subst (uds-unification.rule.create :applicator      'upr=unification.rule.subst.apply
					       :apply.predicate 'upr=unification.rule.subst.applicable.p
					       :sorted.p        NIL
					       :fail.p          NIL
					       :confluent.p     T
					       :name            "Substitutions-Regel")))


;--------------------------------------- TERM RIGHT RULE -------------------------------------------


(defun upr=unification.rule.term.right.applicable.p (unification.problem equation)
  (declare (edited  "30-JAN-1992 14:58")
	   (authors KASPER)
	   (ignore  unification.problem)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die Term-Right-Regel auf die Gleichung angewendet werden kann; NIL sonst"))
  (and (not (dt-variable.is (uds-equation.left.term equation)))
       (dt-variable.is (uds-equation.right.term equation))))



(defun upr=unification.rule.term.right.apply (unification.problem equation)
  (declare (edited  "30-JAN-1992 15:02")
	   (authors KASPER)
	   (input   "equation            - eine Gleichung"
		    "unification.problem - ein Unifikationsproblem")
	   (effect  "keiner")
	   (value   "das Unifikations-Problem nach Anwendung der Term-Right-Rule"))
  (let ((new.equation (uds-equation.create (uds-equation.right.term equation) (uds-equation.left.term equation))))
    (list (uds-unification.problem.create
	    :set (cons new.equation (remove equation (uds-unification.problem.set unification.problem) :test 'eq :count 1))
	    :variables (uds-unification.problem.variables unification.problem)
	    :depth     (1+ (uds-unification.problem.depth unification.problem))
	    :residuum.literals (uds-unification.problem.residuum.literals unification.problem)))))



(defun upr=unification.rule.term.right.init ()
  (declare (edited  "31-JAN-1992 15:24")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Term-Right-Regel wird erzeugt und ihrer Variablen zugewiesen.")
	   (value   "undefiniert"))
  (setf upr*term.right (uds-unification.rule.create :applicator      'upr=unification.rule.term.right.apply
						    :apply.predicate 'upr=unification.rule.term.right.applicable.p 
						    :sorted.p        NIL
						    :fail.p          NIL
						    :confluent.p     T
						    :name            "Tausche-Term-auf-rechte-Seite-Regel")))



;------------------------------------ EQUAL VARIABLES ELIMINATE ----------------------------------


(defun upr=unification.rule.equal.variables.eliminate.applicable.p (unification.problem equation)
  (declare (edited  "30-JAN-1992 15:04")
	   (authors KASPER)
	   (ignore  unification.problem)
	   (input   "equation - eine Gleichung")
	   (effect  " keiner")
	   (value   "T, wenn die Variablen-Eliminations-Regel auf die Gleichung equation anwendbar ist."))
  (and (dt-variable.is (uds-equation.left.term equation))
       (dt-term_equal (uds-equation.left.term equation)(uds-equation.right.term equation))))



(defun upr=unification.rule.equal.variables.eliminate.apply (unification.problem equation)
  (declare (edited  "30-JAN-1992 15:05")
	   (authors KASPER)
	   (input   "equation            - eine Gleichung"
		    "unification.problem - ein Unifikations-Problem")
	   (effect  "keiner")
	   (value   "das Unifikationsproblem nach Anwendung der Equal-Variables-Eliminate-Regel"))
  (list (uds-unification.problem.create
	  :set       (remove equation (uds-unification.problem.set unification.problem) :test 'eq :count 1)
	  :variables (uds-unification.problem.variables unification.problem)
	  :depth     (1+ (uds-unification.problem.depth unification.problem))
	  :residuum.literals (uds-unification.problem.residuum.literals unification.problem))))



(defun upr=unification.rule.var.eliminate.init ()
  (declare (edited  "31-JAN-1992 15:18")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Variablen-Eliminations-Regel wrd erzeugt und ihrer Variablen zugewiesen.")
	   (value   "undefiniert"))
  (setf upr*var.eliminate
	(uds-unification.rule.create :applicator      'upr=unification.rule.equal.variables.eliminate.apply
	     		             :apply.predicate 'upr=unification.rule.equal.variables.eliminate.applicable.p
				     :sorted.p        NIL
				     :fail.p          NIL
				     :confluent.p     T
				     :name            "Gleiche-Variablen-Eliminations-Regel")))


;------------------------------------- EQUAL TOP SYMBOLS -----------------------------------------------------


(defun upr=unification.rule.equal.tops.applicable.p (unification.problem equation)
  (declare (edited  "30-JAN-1992 15:10")
	   (authors KASPER)
	   (ignore  unification.problem)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die Gleiche-Top-Symbole-Aufloes-Regel auf die Gleichung equation anwendbar ist; NIL sonst"))
  (and (or (dt-term_c.term.is (uds-equation.left.term equation))
	   (dt-constant.is    (uds-equation.left.term equation)))
       (equal (dt-term_topsymbol (uds-equation.left.term equation))
	      (dt-term_topsymbol (uds-equation.right.term equation)))))



(defun upr=unification.rule.equal.tops.apply (unification.problem equation)
  (declare (edited  "30-JAN-1992 15:11")
	   (authors KASPER)
	   (input   "equation - eine Gleichung"
		    "unification.problem - ein Unifikations-Problem")
	   (effect  "keiner")
	   (value   "Die Gleichung equation wird aus der Menge der Gleichungen des zu ihr gehoerenden Unifikationsproblems "
		    "geloescht. Es werden in Abhaengigkeit von der Anzahl der Argumente der Funktionen in der alten Gleichung"
		    "entsprechend neue Gleichungen in das Unifikationsproblem zu dem equation gehoerte eingefuegt."))
  (list (uds-unification.problem.create
	  :set       (append (mapcar #'(lambda(left.term.argument right.term.argument)
					 (uds-equation.create left.term.argument right.term.argument))
	                             (dt-term_arguments (uds-equation.left.term equation))
				     (dt-term_arguments (uds-equation.right.term equation)))
			     (remove equation (uds-unification.problem.set unification.problem) :test 'dt-term_equal :count 1))
	  :variables (uds-unification.problem.variables unification.problem)
	  :depth     (1+ (uds-unification.problem.depth unification.problem))
	  :residuum.literals (uds-unification.problem.residuum.literals unification.problem))))
  


(defun upr=unification.rule.equal.tops.init ()
  (declare (edited  "31-JAN-1992 15:23")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Gleiche-Top-Symbole-Aufloes-regel wird erzeugt und ihrer Variablen zugewiesen")
	   (value   "undefiniert"))
  (setf upr*equal.tops (uds-unification.rule.create :applicator      'upr=unification.rule.equal.tops.apply 
						    :apply.predicate 'upr=unification.rule.equal.tops.applicable.p
						    :sorted.p        NIL
						    :fail.p          NIL
						    :confluent.p     T
						    :name            "Gleiche-Top-Symbole-Aufloes-Regel")))


;-------------------------------------------------------------------------------------
;----------------------------- SORTED UNIFICATION RULES ------------------------------


;----------------------------- SUB SORT VARIABLE RULE --------------------------------

(defun upr=unification.rule.sub.sort.applicable.p (unification.problem equation)
  (declare (edited  "01-FEB-1992 17:52")
	   (authors KASPER)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die Sub-sort-variable-rule auf die Gleichung equation anwendbar ist; NIL sonst")
	   (ignore  unification.problem))
  (and (dt-variable.is (uds-equation.left.term equation))
       (dt-variable.is (uds-equation.right.term equation))
       (upr=term.element.of.sort.node.p (uds-equation.left.term equation)
					(uds-variable.sort.node (uds-equation.right.term equation)))
       (not (upr=term.element.of.sort.node.p (uds-equation.right.term equation)
					     (uds-variable.sort.node (uds-equation.left.term equation))))))



(defun upr=unification.rule.sub.sort.apply (unification.problem equation)  
  (declare (edited  "05-FEB-1992 17:29")
	   (authors KASPER)
	   (input   "equation - eine Gleichung")
	   (effect  "die Regel Sub-sort-variable-rule wird destruktiv auf die Gleichung equation angewendet")
	   (value   "das Unifikationsproblem nach Anwendung der Sub-Sort-Variable-Regel"))
  (list
    (uds-unification.problem.create
      :set (cons (uds-equation.create (uds-equation.right.term equation) (uds-equation.left.term equation))
		 (remove equation (uds-unification.problem.set unification.problem) :test 'eq :count 1))
      :variables (uds-unification.problem.variables unification.problem)
      :depth     (1+ (uds-unification.problem.depth unification.problem))
      :residuum.literals (append (uds-unification.problem.residuum.literals unification.problem)
				 (upr=variable.term.element.of.sort.node.residues
				   (uds-equation.left.term equation)
				   (uds-variable.sort.node (uds-equation.right.term equation)))))))
  


(defun upr=unification.rule.sub.sort.init ()
  (declare (edited  "01-FEB-1992 18:01")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "die variable-sub-sort.rule wird erzeugt und ihrer Variablen zugewiesen")
	   (value   " undefiniert"))
  (setf upr*sub.sort (uds-unification.rule.create :applicator      'upr=unification.rule.sub.sort.apply
						  :apply.predicate 'upr=unification.rule.sub.sort.applicable.p
						  :sorted.p        T
						  :fail.p          NIL
						  :confluent.p     T
						  :name "Variable-Untersorten-Regel")))




;---------------------------- MAX SORT VARIABLE RULE -----------------------------

(defun upr=unification.rule.max.sort.applicable.p (unification.problem equation)
  (declare (edited  "01-FEB-1992 18:04")
	   (authors KASPER)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die Max-sort-variable-rule auf die Gleichung equation anwendbar ist; NIL sonst")
	   (ignore  unification.problem))
  (let ((left.term  (uds-equation.left.term equation))
	(right.term (uds-equation.right.term equation)))
    (and (dt-variable.is left.term)
	 (dt-variable.is right.term)
	 (not (equal (dt-variable.sort left.term)  (dt-constant.omega)))
	 (not (equal (dt-variable.sort right.term) (dt-constant.omega)))
	 (not (upr=term.element.of.sort.node.p left.term  (uds-variable.sort.node right.term)))
	 (not (upr=term.element.of.sort.node.p right.term (uds-variable.sort.node left.term)))	
	 (upr=sort.nodes.maximal.full.sub.sort.nodes (uds-variable.sort.node left.term) (uds-variable.sort.node right.term)))))




(defun upr=unification.rule.max.sort.apply (unification.problem equation)
  (declare (edited  "01-FEB-1992 18:08")
	   (authors KASPER)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "Liste der veraenderten Unifikationsprobleme nach Anwendung der Regel Max-sort-rule auf die Gleichung."))
  (let ((left.term  (uds-equation.left.term equation))
	(right.term (uds-equation.right.term equation)))
    (mapcar #'(lambda(tupel)
		(let ((new.variable (dt-variable.create (first (uds-sort.node.mkrp.names (uds-tupel.sort.node tupel))))))
		  (uds-unification.problem.create
		    :set (append (list (uds-equation.create left.term new.variable)
				       (uds-equation.create right.term new.variable))
				 (remove equation (uds-unification.problem.set unification.problem) :test 'eq :count 1))
		    :variables (cons new.variable (uds-unification.problem.variables unification.problem))
		    :depth     (1+ (uds-unification.problem.depth unification.problem))
		    :residuum.literals (append (uds-tupel.residuum.literals tupel)
					       (uds-unification.problem.residuum.literals unification.problem)))))
	    (upr=sort.nodes.maximal.full.sub.sort.nodes.residuum.literal.tupels (uds-variable.sort.node left.term)
										(uds-variable.sort.node right.term)))))



(defun upr=unification.rule.max.sort.init ()
  (declare (edited  "01-FEB-1992 18:18")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Regel Max-sort-variable-rule wird erzeugt und ihrer Variablen zugewiesen.")
	   (value   "undefiniert"))
  (setf upr*max.sort (uds-unification.rule.create :applicator      'upr=unification.rule.max.sort.apply
						  :apply.predicate 'upr=unification.rule.max.sort.applicable.p
						  :sorted.p         T
						  :fail.p           NIL
						  :confluent.p      NIL
						  :name            "Maximale Sorten-Regel")))



;------------------------------------------ RULE 8 -----------------------------------------


(defun upr=unification.rule.8.applicable.p (unification.problem equation)
  (declare (edited  "03-FEB-1992 16:00")
	   (authors KASPER)
	   (input   "equation            - eine Gleichung"
		    "unification.problem - ein Unifikations-Problem")
	   (ignore  unification.problem)
	   (effect  "keiner")
	   (value   "T, wenn Regel 8 des Unifikations-Algorithmus anwendbar ist"))
  (let ((left.term  (uds-equation.left.term equation))
	(right.term (uds-equation.right.term equation)))
    (and (not (uds-equation.locked.p equation))
         (dt-variable.is left.term)
	 (or (dt-constant.is right.term)
	     (dt-term_c.term.is right.term))
	 (not (equal (dt-variable.sort left.term) (dt-constant.omega)))
	 (not (dt-term_in left.term right.term))
	 (not (upr=term.element.of.sort.node.p right.term (uds-variable.sort.node left.term)))
	 (upr=sort.nodes.epsilon.literals.with.term.topsymbol
	   (upr=sort.node.full.sub.sort.nodes (uds-variable.sort.node left.term))
	   (dt-term_topsymbol right.term)))))



(defun upr=unification.rule.8.apply (unification.problem equation)
  (declare (edited  "03-FEB-1992 16:09")
	   (authors KASPER)
	   (input   "unification.problem - ein Unifikations-Problem"
		    "equation            - eine Gleichung")
	   (effect  "keiner")
	   (value   "Liste der Unifikations-Probleme, die netstehen, wenn man Regel 8 auf unification.problem anwendet"))
  (let* ((left.term  (uds-equation.left.term equation))
	 (right.term (uds-equation.right.term equation))
	 (tupels     (upr=sort.node.full.sub.sort.node.residuum.literal.tupels (uds-variable.sort.node left.term))))
    (mapcan #'(lambda(tupel)
		(mapcar #'(lambda(epsilon.literal)
			    (let* ((renaming.subst          (uds-epsilon.literal.renaming.substitution epsilon.literal))
			 	   (renamed.epsilon.literal (uds-epsilon.literal.subst.apply epsilon.literal renaming.subst)))
			      (uds-unification.problem.create
				:set (append (mapcar #'uds-equation.create
						     (dt-term_arguments right.term)
						     (dt-term_arguments (uds-epsilon.literal.term renamed.epsilon.literal)))
					     (list (uds-equation.create left.term right.term :locked t))
					     (remove equation (uds-unification.problem.set unification.problem) :count 1))
				:variables (union (uds-epsilon.literal.term.variables renamed.epsilon.literal)
						  (uds-unification.problem.variables unification.problem)
						  :test 'dt-term_equal)
				:depth (1+ (uds-unification.problem.depth unification.problem)) 
		                :residuum.literals (append
						     (uds-epsilon.literal.residuum.literals renamed.epsilon.literal)
						     (mapcar #'(lambda(mkrp.literal)
								 (uds-mkrp.literal.subst.apply mkrp.literal renaming.subst))
							     (append
							       (uds-unification.problem.residuum.literals unification.problem)
							       (uds-tupel.residuum.literals tupel)))))))
			(upr=tupel.epsilon.literals.with.term.topsymbol tupel (dt-term_topsymbol right.term))))
	    tupels)))



(defun upr=unification.rule.8.init ()
  (declare (edited  "03-FEB-1992 16:10")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Regel 8 wird ihrer Variablen zugewiesen")
	   (value   "undefiniert"))
  (setf upr*unification.rule.8 (uds-unification.rule.create :applicator      'upr=unification.rule.8.apply
				          		    :apply.predicate 'upr=unification.rule.8.applicable.p
							    :sorted.p        T
							    :fail.p          NIL
							    :confluent.p     T
							    :name            "Regel-8")))




;----------------------------------------- RULE 4 -----------------------------------------

(defun upr=unification.rule.4.applicable.p (unification.problem equation)
  (declare (edited  "14-FEB-1992 18:00")
	   (authors KASPER)
	   (input   "unification.problem - ein Unifikations-Problem"
		    "equation - ein Gleichung")
	   (effect  "keiner")
	   (value   "T, falls die Regel-4 auf die Gleichung equation anwendbar ist; NIL sonst")
	   (ignore  unification.problem))
  (let ((left.term  (uds-equation.left.term equation))
	(right.term (uds-equation.right.term equation)))
    (and (dt-variable.is left.term)
	 (dt-variable.is right.term)
	 (not (equal (dt-variable.sort left.term) (dt-constant.omega)))
	 (not (equal (dt-variable.sort right.term) (dt-constant.omega)))
	 (not (upr=term.element.of.sort.node.p right.term (uds-variable.sort.node left.term)))
	 (not (upr=term.element.of.sort.node.p left.term  (uds-variable.sort.node right.term)))
	 (intersection
	   (upr=sort.nodes.epsilon.non.variable.term.literals
	     (upr=sort.node.full.sub.sort.nodes (uds-variable.sort.node left.term)))
	   (upr=sort.nodes.epsilon.non.variable.term.literals
	     (upr=sort.node.full.sub.sort.nodes (uds-variable.sort.node right.term)))
	   :test 'equal
	   :key #'(lambda(epsilon.literal)
		    (dt-term_topsymbol (uds-epsilon.literal.term epsilon.literal)))))))



(defun upr=unification.rule.4.apply (unification.problem equation)
  (declare (edited  "14-FEB-1992 17:58")
	   (authors KASPER)
	   (input   "unification.problem - ein Unifikations-Problem"
		    "equation            - eine Gleichung")
	   (effect  "keiner")
	   (value   "Liste der Unifikations-Probleme, die ensteht, wenn man Regel-4 auf die Gleichung equation anwendet"))
  (let* ((left.term         (uds-equation.left.term equation))
	 (right.term        (uds-equation.right.term equation))
	 (left.term.tupels  (upr=sort.node.full.sub.sort.node.residuum.literal.tupels (uds-variable.sort.node left.term)))
	 (right.term.tupels (upr=sort.node.full.sub.sort.node.residuum.literal.tupels (uds-variable.sort.node right.term))))
    (mapcan #'(lambda(left.right.tupel)
		(let ((left.tupel (first left.right.tupel))
		      (right.tupel (second left.right.tupel)))
		  (mapcar #'(lambda(left.right.epsilon.literal)
			      (let* ((left.epsilon.literal (first left.right.epsilon.literal))
				     (right.epsilon.literal (second left.right.epsilon.literal))
				     (renaming.subst.left   (uds-epsilon.literal.renaming.substitution left.epsilon.literal))
				     (renaming.subst.right  (uds-epsilon.literal.renaming.substitution right.epsilon.literal))
				     (renamed.left.epsilon.literal (uds-epsilon.literal.subst.apply left.epsilon.literal
												    renaming.subst.left))
				     (renamed.right.epsilon.literal (uds-epsilon.literal.subst.apply right.epsilon.literal
												     renaming.subst.right))
				     (comb.subst (subst-apply.to.substitution renaming.subst.left renaming.subst.right)))

				(uds-unification.problem.create
				  :set (append
					 (remove equation (uds-unification.problem.set unification.problem) :test 'eq :count 1)
					 (list (uds-equation.create left.term
								    (uds-epsilon.literal.term renamed.left.epsilon.literal))
					       (uds-equation.create right.term
								    (uds-epsilon.literal.term renamed.right.epsilon.literal)))
					 (mapcar #'uds-equation.create
						 (dt-term_arguments (uds-epsilon.literal.term renamed.left.epsilon.literal))
						 (dt-term_arguments (uds-epsilon.literal.term renamed.right.epsilon.literal))))
				  :variables (union (union (uds-epsilon.literal.term.variables renamed.left.epsilon.literal)
							   (uds-epsilon.literal.term.variables renamed.right.epsilon.literal)
							   :test 'dt-term_equal)
						    (uds-unification.problem.variables unification.problem) :test 'dt-term_equal)
				  :depth (1+ (uds-unification.problem.depth unification.problem))
				  :residuum.literals (append (uds-epsilon.literal.residuum.literals renamed.left.epsilon.literal)
							    (uds-epsilon.literal.residuum.literals renamed.right.epsilon.literal)
							     (uds-unification.problem.residuum.literals unification.problem)
							     (mapcar #'(lambda(mkrp.literal)
									 (uds-mkrp.literal.subst.apply mkrp.literal comb.subst))
								     (union (uds-tupel.residuum.literals left.tupel)
									    (uds-tupel.residuum.literals right.tupel)
									    :test 'equal))))))
			  (delete-if-not #'upr=epsilon.literals.term.topsymbols.equal.p
				     (upr=n.sets.n.tupels.create
				       (list (upr=sort.node.epsilon.non.variable.term.literals (uds-tupel.sort.node left.tupel))
				             (upr=sort.node.epsilon.non.variable.term.literals (uds-tupel.sort.node right.tupel))))))))
	    (delete-duplicates (upr=n.sets.n.tupels.create (list left.term.tupels right.term.tupels))
			       :test #'upr=tupel.set.equal.p))))



(defun upr=tupel.set.equal.p(two.tupel.1 two.tupel.2)
  (and (eq (first two.tupel.1)
	   (second two.tupel.2))
       (eq (second two.tupel.1)
	   (first two.tupel.2))))


(defun upr=epsilon.literals.term.topsymbols.equal.p (epsilon.literals)
  (apply 'equal (mapcar #'(lambda(epsilon.literal)
			    (dt-term_topsymbol (uds-epsilon.literal.term epsilon.literal)))
			epsilon.literals)))



(defun upr=unification.rule.4.init ()
  (declare (edited  "14-FEB-1992 17:57")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Regel-4 wird initialisiert.")
	   (value   "undefiniert"))
  (setf upr*unification.rule.4 (uds-unification.rule.create :applicator      'upr=unification.rule.4.apply
							    :apply.predicate 'upr=unification.rule.4.applicable.p
							    :sorted.p        T
							    :fail.p          NIL
							    :confluent.p     NIL
							    :name            "Regel-4"))) 



;----------------------------------- OMEGA FAIL -----------------------------------


(defun upr=unification.rule.omega.fail.applicable.p (unification.problem equation)
  (declare (edited  "14-FEB-1992 17:56")
	   (authors KASPER)
	   (input   "unification.problem - ein Unifikationsproblem"
		    "equation            - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die Omega-Fail-Regel auf die Gleichung equation anwendbar ist; NIL sonst")
	   (ignore  unification.problem))
  (and (dt-variable.is (uds-equation.left.term equation))
       (equal (dt-variable.sort (uds-equation.left.term equation)) (dt-constant.omega))
       (not (upr=term.element.of.sort.omega.p (uds-equation.right.term equation)))))




(defun upr=unification.rule.omega.fail.init ()
  (declare (edited  "14-FEB-1992 17:55")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Omega-Fail-Regel wird initialisiert")
	   (value   "undefiniert"))
  (setf upr*omega.fail (uds-unification.rule.create :applicator      NIL
						    :apply.predicate 'upr=unification.rule.omega.fail.applicable.p 
						    :sorted.p         T
						    :fail.p           T
						    :confluent.p      T
						    :name            "Omega-Fail-Regel")))



;-------------------------------- NO COMMON SUB SORT FAIL ---------------------------------------

(defun upr=unification.rule.no.common.sub.sort.fail.applicable.p (unification.problem equation)
  (declare (edited  "20-MAR-1992 14:46")
	   (authors KASPER)
	   (input   "equation - eine Gleichung")
	   (ignore  unification.problem)
	   (effect  "keiner")
	   (value   "T, wenn die die sub-sort-fail-Regel auf die Gleichung anwendbar ist; NIL sonst"))
  (let ((left.term (uds-equation.left.term equation))
	(right.term (uds-equation.right.term equation)))
    (and (dt-variable.is left.term)
	 (dt-variable.is right.term)
	 (not (equal (dt-variable.sort left.term) (dt-constant.omega)))
	 (not (equal (dt-variable.sort right.term) (dt-constant.omega)))
	 (not (upr=term.element.of.sort.node.p right.term (uds-variable.sort.node left.term)))
	 (not (upr=term.element.of.sort.node.p left.term (uds-variable.sort.node right.term)))
	 (null (upr=sort.nodes.common.sub.sort.nodes (uds-variable.sort.node left.term) (uds-variable.sort.node right.term)))
	 (null (intersection
		 (upr=sort.nodes.epsilon.non.variable.term.literals
		   (upr=sort.node.full.sub.sort.nodes (uds-variable.sort.node left.term)))
		 (upr=sort.nodes.epsilon.non.variable.term.literals
		   (upr=sort.node.full.sub.sort.nodes (uds-variable.sort.node right.term)))
		 :test 'equal
		 :key #'(lambda(epsilon.literal)
			  (dt-term_topsymbol (uds-epsilon.literal.term epsilon.literal))))))))




(defun upr=unification.rule.no.common.sub.sort.fail.init ()
  (declare (edited  "14-FEB-1992 17:52")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Keine-gemeinsame-Untersorten-Fail-Regel wird initialisiert.")
	   (value   "undefiniert"))
  (setf upr*no.common.sub.sort.fail (uds-unification.rule.create
				      :applicator      NIL
				      :apply.predicate 'upr=unification.rule.no.common.sub.sort.fail.applicable.p
				      :sorted.p        T
				      :fail.p          T
				      :confluent.p     T
				      :name            "Keine-gemeinsame-Untersorten-Fail-Regel")))




;------------------------------ NO SUB SORT Lepsilon TERM ----------------------------------------------


(defun upr=unification.rule.no.sub.sort.l.epsilon.term.fail.applicable.p (unification.problem equation)
  (declare (edited  "14-FEB-1992 17:49")
	   (authors KASPER)
	   (input   "unification.problem - ein Unifikations-Problem"
		    "equation            - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, wenn die No-Sub-Sort-Lepsilon-Term-Fail-Regel anwenbar ist; NIL sonst")
	   (ignore  unification.problem))
  (let ((left.term  (uds-equation.left.term equation))
	(right.term (uds-equation.right.term equation)))
    (and (dt-variable.is left.term)
	 (or (dt-constant.is right.term)
	     (dt-term_c.term.is right.term))
	 (not (equal (dt-variable.sort left.term) (dt-constant.omega)))
	 (not (dt-term_in left.term right.term))
	 (not (upr=term.element.of.sort.node.p right.term (uds-variable.sort.node left.term)))
	 (not (some #'(lambda(epsilon.literal)
			(equal (dt-term_topsymbol (uds-epsilon.literal.term epsilon.literal))
			       (dt-term_topsymbol right.term)))
		   (upr=sort.nodes.epsilon.non.variable.term.literals
		     (upr=sort.node.full.sub.sort.nodes (uds-variable.sort.node left.term))))))))



(defun upr=unification.rule.no.sub.sort.l.epsilon.term.fail.init ()
  (declare (edited  "14-FEB-1992 17:54")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die No-Sub-Sort-Lepsilon-Term-Fail-Regel wird initialisiert.")
	   (value   "undefiniert"))
  (setf upr*no.sub.sort.l.epsilon.term.fail (uds-unification.rule.create
					      :applicator      NIL
					      :apply.predicate 'upr=unification.rule.no.sub.sort.l.epsilon.term.fail.applicable.p
					      :sorted.p        T
					      :fail.p          T
					      :confluent.p     T
					      :name            "No-Sub-Sort-Lepsilon-Term-Fail-Regel")))




;---------------------------------- EMPTY SORT FAIL --------------------------------------


(defun upr=unification.rule.sort.empty.fail.applicable.p (unification.problem equation)
  (declare (edited  "14-FEB-1992 17:46")
	   (authors KASPER)
	   (input   "unification.problem - ein Unifikations-Problem"
		    "equation            - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, falls die Sort-Empty-Fail-Regel anwendbar ist; NIL sonst")
	   (ignore  unification.problem))
  (and (dt-variable.is (uds-equation.left.term equation))
       (dt-variable.is (uds-equation.right.term equation))
       (or (uds-sort.node.empty.p (uds-variable.sort.node (uds-equation.left.term equation)))
	   (uds-sort.node.empty.p (uds-variable.sort.node (uds-equation.right.term equation))))))



(defun upr=unification.rule.sort.empty.fail.init ()
  (declare (edited  "14-FEB-1992 17:48")
	   (authors KASPER)
	   (input   "keiner")
	   (effect  "Die Sort-Empty-Fail-Regel wird initialisiert.")
	   (value   "undefiniert"))
  (setf upr*sort.empty.fail (uds-unification.rule.create :applicator      NIL
							 :apply.predicate 'upr=unification.rule.sort.empty.fail.applicable.p
							 :sorted.p        T
							 :fail.p          T
							 :confluent.p     T
							 :name            "Sort-Empty-Fail-Regel")))








;--------------------------------------------------------------------------------------
;----------------------------- TERM ELEMENT OF SORT------------------------------------


(defun upr=term.element.of.sort.node.p (term sort.node)
  (declare (edited  "05-FEB-1992 17:41")
	   (authors KASPER)
	   (input   "term - ein MKRP-Term"
		    "sort.node - eine Sort-Knoten-Datenstruktur")
	   (effect  "keiner")
	   (value   "T, wenn term Element der Sorte des Sort-Knotens sort.node ist; NIL sonst"))
  (setf upr*term.element.of.sort.node.used.residues nil)
  (cond ((and (dt-variable.is term)
	      (upr=sort.node.is.super.sort.node.p sort.node (uds-variable.sort.node term))
	      (uds-sort.node.full.p (uds-variable.sort.node term)))
	 (progn (setf upr*term.element.of.sort.node.used.residues
		      (upr=variable.term.element.of.sort.node.residues term sort.node))
		t))
	((and (dt-variable.is term)
	      (upr=sort.node.is.true.super.sort.node.p (uds-variable.sort.node term) sort.node)) nil)
        ((equal (first (uds-sort.node.mkrp.names sort.node)) (dt-constant.omega)) (upr=term.element.of.sort.omega.p term))
	(t (upr=term.element.of.non.omega.sort.node.p term sort.node))))


(defun upr=term.element.of.non.omega.sort.node.p (term sort.node)
  (let ((matcher.epsilon.literals (upr=sort.nodes.epsilon.literals.with.term.topsymbol
				    (upr=sort.node.full.sub.sort.nodes sort.node)
				    (dt-term_topsymbol term))))
    (when matcher.epsilon.literals
      (some #'(lambda(matcher)
		(subst-every #'(lambda(subst)		      
				 (upr=term.element.of.sort.node.p
				   (first (subst-range subst))
				   (uds-variable.sort.node (first (subst-substitution.domain subst)))))
			     matcher))
	    (upr=matchers.get term (mapcar #'uds-epsilon.literal.term matcher.epsilon.literals))))))



(defun upr=matchers.get (term matcher.terms)
  (if (null matcher.terms)
      nil
      (let ((matcher (upr=match.terms (list (first matcher.terms)) (list term) (subst-create))))
	(if (eq :fail matcher)
	    (upr=matchers.get term (rest matcher.terms))
	    (cons matcher (upr=matchers.get term (rest matcher.terms)))))))
	                                               



(defun upr=match.terms (terms.1 terms.2 subst)
  (let ((first.term.1 (first terms.1))
	(first.term.2 (first terms.2)))
    (cond ((eq subst :fail) :fail)
	  ((null terms.1) subst)
	  ((and (dt-constant.is first.term.1)
		(dt-term_equal first.term.1 first.term.2))
	   (upr=match.terms (cdr terms.1) (cdr terms.2) subst))
	  ((and (dt-variable.is first.term.1)
		(dt-variable.is first.term.2)
		(upr=sort.node.is.true.super.sort.node.p (uds-variable.sort.node first.term.2)
							 (uds-variable.sort.node first.term.1))) :fail)
	  ((and (dt-variable.is first.term.1)
		(not (member first.term.1 (subst-substitution.domain subst) :test 'dt-term_equal)))
	   (upr=match.terms (cdr terms.1) (cdr terms.2) (subst-substitution.extend first.term.1 first.term.2 subst)))
	  ((and (dt-variable.is first.term.1)
		(member first.term.1 (subst-substitution.domain subst) :test 'dt-term_equal)
		(dt-term_equal first.term.2 (subst-variable.substitution.term first.term.1 subst)))
	   (upr=match.terms (cdr terms.1) (cdr terms.2) subst))
	  ((and (dt-term_c.term.is first.term.1)
		(= (dt-term_topsymbol first.term.1)
		   (dt-term_topsymbol first.term.2)))
	   (upr=match.terms (cdr terms.1) (cdr terms.2)
			    (upr=match.terms (dt-term_arguments first.term.1) (dt-term_arguments first.term.2) subst)))
	  (t :fail))))


(defun upr=term.element.of.sort.omega.p (term)
  (every #'(lambda(variable)
	    (uds-sort.node.full.p (uds-variable.sort.node variable)))
	(dt-term.variables term)))


(defun upr=variable.term.element.of.sort.node.residues (term sort.node)
  (declare (edited  "16-OCT-1992 22:01")
	   (authors KASPER)
	   (input   "term - ein Term, der nur eine Variable sein darf!!!!"
		    "sort.node - ein Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste aller Residuum-Literale, die benutzt wurden, um festzustellen, ob term e Sort"))
  (let* ((term.subsort.node.residuum.literals (upr=sort.node.full.sub.sort.node.residuum.literal.tupels sort.node))
        (res (member (uds-variable.sort.node term) term.subsort.node.residuum.literals :test 'eq :key 'uds-tupel.sort.node)))
    (when res
      (uds-tupel.residuum.literals (first res)))))


;-----------------------------------------------------------------------------------------
;--------------------------- GETTING SUPER- SUB and MAX-SORTS ----------------------------


(defun upr=sort.node.full.sub.sort.nodes (sort.node)
  (declare (edited  "07-FEB-1992 16:08")
	   (authors KASPER)
	   (input   "sort.node - ein Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste aller Unter-Sorten-Knoten, die voll sind."))
  (delete-duplicates (nconc (when (uds-sort.node.full.p sort.node)
			      (list sort.node))
			    (mapcan #'upr=sort.node.full.sub.sort.nodes (uds-sort.node.adjacent.sort.nodes sort.node)))))



(defun upr=sort.node.full.super.sort.nodes (sort.node)
  (declare (edited  "07-FEB-1992 16:13")
	   (authors KASPER)
	   (input   "sort.node - ein Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste aller vollen Ober-Sorten-Knoten, die voll sind."))
  (delete-duplicates (nconc (when (uds-sort.node.full.p sort.node)
			      (list sort.node))
			    (mapcan #'upr=sort.node.full.super.sort.nodes
				    (mapcar #'uds-arc.source.node (uds-sort.node.arcs.in sort.node))))))



(defun upr=sort.node.true.full.super.sort.nodes (sort.node)
  (declare (edited  "14-FEB-1992 18:04")
	   (authors KASPER)
	   (input   "sort.node - ein Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste alle echten Ober-Sorten-Knoten von sort.node"))
  (delete sort.node (upr=sort.node.full.super.sort.nodes sort.node)))



(defun upr=sort.node.is.true.super.sort.node.p (super.sort.node sub.sort.node)
  (declare (edited  "17-FEB-1992 15:31")
	   (authors KASPER)
	   (input   "super.sort.node, sub.sort.node - zwei Sorten-Knoten")
	   (effect  "keiner")
	   (value   "T, wenn super.sort.node eine Ober-Sorte vob sub.sort.node ist; NIL sonst"))
  (member super.sort.node (upr=sort.node.true.full.super.sort.nodes sub.sort.node) :test 'eq))




(defun upr=sort.node.is.super.sort.node.p (super.sort.node sub.sort.node)
  (member super.sort.node (upr=sort.node.full.super.sort.nodes sub.sort.node) :test 'eq))

(defun upr=sort.nodes.maximal.full.sub.sort.nodes(sort.node.1 sort.node.2)
  (declare (edited  "14-FEB-1992 18:05")
	   (authors KASPER)
	   (input   "sort.node.1, sort.node.2 - zwei Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste der maximalen vollen Unter-Sorten-Knoten von sort.node.1 uns sort.node.2"))
  (let* ((sort.node.1.sub.sort.nodes (upr=sort.node.full.sub.sort.nodes sort.node.1))
	 (sort.node.2.sub.sort.nodes (upr=sort.node.full.sub.sort.nodes sort.node.2))
	 (common.sub.sort.nodes      (intersection sort.node.1.sub.sort.nodes sort.node.2.sub.sort.nodes :test 'equal)))
    (remove-if-not #'(lambda(sort.node)
		       (disjointp (upr=sort.node.true.full.super.sort.nodes sort.node) common.sub.sort.nodes :test 'eq))
		   common.sub.sort.nodes)))



(defun upr=sort.nodes.common.sub.sort.nodes (sort.node.1 sort.node.2)
  (declare (edited  "14-FEB-1992 16:14")
	   (authors KASPER)
	   (input   "sort.node.1, sort.node.2 - zwei Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste der gemeinsamen Unter-Sorten-Knoten von sort.node.1 sort.node.2"))
  (intersection (upr=sort.node.full.sub.sort.nodes sort.node.1)
		(upr=sort.node.full.sub.sort.nodes sort.node.2)
		:test 'eq))



;------------------------------------ WITH RESIDUES ----------------------------------------

(defun upr=sort.node.full.sub.sort.node.residuum.literal.tupels (sort.node)
  (declare (edited  "15-FEB-1992 21:20")
	   (authors KASPER)
	   (input   )
	   (effect  )
	   (value   ))
  (delete-if #'uds-sort.node.empty.p (upr=sort.node.sub.sort.node.residuum.literal.tupels sort.node) :key #'uds-tupel.sort.node))



(defun upr=sort.node.sub.sort.node.residuum.literal.tupels (sort.node)
  (declare (edited  "14-FEB-1992 18:07")
	   (authors KASPER)
	   (input   )
	   (effect  )
	   (value   ))
  (delete-duplicates (upr=arcs.sub.sort.node.residuum.tupels (list (uds-tupel.create :sort.node sort.node)) nil)
		     :test 'eq :key 'uds-tupel.sort.node))


(defun upr=arcs.sub.sort.node.residuum.tupels (open.tupels closed.tupels)
  (declare (edited  "14-FEB-1992 18:07")
	   (authors KASPER)
	   (input   )
	   (effect  )
	   (value   ))
  (if (null open.tupels)
      closed.tupels
      (upr=arcs.sub.sort.node.residuum.tupels
	  (mapcan #'upp=tupel.open.sub.tupels open.tupels)
	  (append open.tupels closed.tupels))))



(defun upp=tupel.open.sub.tupels (tupel)
  (declare (edited  "14-FEB-1992 18:08")
	   (authors KASPER)
	   (input   )
	   (effect  )
	   (value   ))
  (mapcar #'(lambda(arc)
	      (uds-tupel.create :sort.node         (uds-arc.target.node arc)
				:residuum.literals (append (uds-tupel.residuum.literals tupel)
							   (uds-arc.residuum.literals arc))))
	  (uds-sort.node.arcs.out (uds-tupel.sort.node tupel))))



(defun upr=sort.nodes.maximal.full.sub.sort.nodes.residuum.literal.tupels (sort.node.1 sort.node.2)
  (declare (edited  "17-FEB-1992 15:44")
	   (authors KASPER)
	   (input   "sort.node.1, sort.node.2 - zwei Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste aller Sorten-Knoten-Residuen-Tupel, deren Sorten maximale Unter-Sorten von sort.node.1 und sort.node.2"
		    "sind"))
  (let* ((sort.node.1.tupels (upr=sort.node.full.sub.sort.node.residuum.literal.tupels sort.node.1))
	 (sort.node.2.tupels (upr=sort.node.full.sub.sort.node.residuum.literal.tupels sort.node.2))
	 (common.sub.sort.node.tupels (intersection sort.node.1.tupels sort.node.2.tupels :key #'uds-tupel.sort.node :test 'eq)))
    (remove-if-not #'(lambda(tupel)
		       (disjointp (upr=sort.node.true.full.super.sort.nodes (uds-tupel.sort.node tupel))
				  (mapcar #'uds-tupel.sort.node common.sub.sort.node.tupels)
				  :test 'eq))
		   common.sub.sort.node.tupels)))



                                 

(defun upr=tupel.epsilon.literals.with.term.topsymbol (tupel topsymbol)
  (declare (edited  "14-FEB-1992 18:29")
	   (authors KASPER)
	   (input   "tupel - eine sort.node, residuum.literal Tupel-Struktur"
		    "topsymbol - ein Term-Topsymbol")
	   (effect  "keiner")
	   (value   "Liste aller Epsilon-Literale des von sort.node, deren Topsymbol gleich Topsymbol ist"))
  (remove-if-not #'(lambda(epsilon.literal)
		    (equal (dt-term_topsymbol (uds-epsilon.literal.term epsilon.literal)) topsymbol))
		(uds-sort.node.epsilon.literals (uds-tupel.sort.node tupel))))



;---------------------------- SORT NODE L-EPSILON OPERATIONS --------------------------------

(defun upr=sort.node.epsilon.non.variable.term.literals (sort.node)
  (declare (edited  "14-FEB-1992 18:17")
	   (authors KASPER)
	   (input   "sort.node - ein Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste alle Nicht-Variable-Term-Epsilon-Literale der Sorte sort.node"))
  (remove-if #'(lambda(epsilon.literal)
		 (dt-variable.is (uds-epsilon.literal.term epsilon.literal)))
	     (uds-sort.node.epsilon.literals sort.node)))


(defun upr=sort.nodes.epsilon.non.variable.term.literals (sort.nodes)
  (declare (edited  "14-FEB-1992 18:18")
	   (authors KASPER)
	   (input   "sort.node - ein Sorten-Knoten")
	   (effect  "keiner")
	   (value   "Liste alle Nicht-Variable-Term-Epsilon-Literale der Sorte sort.node"))
  (apply 'append (mapcar #'upr=sort.node.epsilon.non.variable.term.literals sort.nodes)))




(defun upr=sort.node.epsilon.literals.with.term.topsymbol (sort.node topsymbol)
  (declare (edited  "15-FEB-1992 20:46")
	   (authors KASPER)
	   (input   "sort.nodes - eine Liste von Sorten-Knoten"
		    "topsymbol  - ein Term-Topymbol")
	   (effect  "keiner")
	   (value   "Liste aller Epsilon-Literale des Sorten-Knoten, dessen Term-Topsymbol gleich topsymbol ist"))
  (remove-if-not #'(lambda (epsilon.literal)
		     (equal (dt-term_topsymbol (uds-epsilon.literal.term epsilon.literal)) topsymbol))
		 (uds-sort.node.epsilon.literals sort.node)))




(defun upr=sort.nodes.epsilon.literals.with.term.topsymbol (sort.nodes topsymbol)
  (declare (edited  "15-FEB-1992 20:45")
	   (authors KASPER)
	   (input   "sort.nodes - eine Liste von Sorten-Knoten"
		    "topsymbol  - ein Term-Topymbol")
	   (effect  "keiner")
	   (value   "Liste aller Epsilon-Literale der Sorten-Knoten, deren Term-Topsymbol gleich topsymbol ist"))
  (apply 'append (mapcar #'(lambda(sort.node)
			     (upr=sort.node.epsilon.literals.with.term.topsymbol sort.node topsymbol))
			 sort.nodes)))



;---------------------------------- SOME UTILITY FUNCTIONS -----------------------------------
      
(defun upr=n.sets.n.tupels.create (sets)
  (declare (edited  "14-FEB-1992 14:55")
	   (authors KASPER)
	   (input   "sets - eine Liste von Listen von Objekten (die Anzahl der Listen sei N)")
	   (effect  "keiner")
	   (value   "eine Liste aller moeglichen N-Tupel a fuer die gilt a1 aus Menge1, a2 aus Menge2, ... an aus MengeN"
		    "Bsp. (upr=n.sets.n.tupels.create ((1 2) (3 4))) -> ((1 3) (1 4) (2 3) (2 4))"
		    "     (upr=n.sets.n.tupels.create ((1 2) (3) (4 5)) -> ((1 3 4) (1 3 5) (2 3 4) (2 3 5))"))
  (if (null (rest sets))
      (mapcar #'list (first sets))
      (let ((tupels (upr=n.sets.n.tupels.create (rest sets))))
	(mapcan #'(lambda(element)
		    (mapcar #'(lambda(tupel)
				(cons element tupel))
			    tupels))
		(first sets)))))

