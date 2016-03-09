;;; -*- Package: MARKGRAF-KARL; Mode: LISP; Syntax: Common-Lisp -*-


(in-package "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(proclaim '(declaration edited authors input effect value remarks))



(defvar upp*sort.graph (uds-sort.graph.create))

(defmacro upp-sort.graph ()
  'upp*sort.graph)




(defvar upp*l.epsilon (uds-l.epsilon.create))

(defmacro upp-l.epsilon ()
  'upp*l.epsilon)


(defvar upp*mkrp.literals nil)

(defmacro upp=mkrp.literals ()
  'upp*mkrp.literals)



(defun upp=mkrp.literal.add (literal)
  (setf (upp=mkrp.literals) (cons literal (upp=mkrp.literals))))

(defun upp=mkrp.literal.delete (literal)
  (setf (upp=mkrp.literals) (delete literal (upp=mkrp.literals) :test 'equal)))

(defun upp-init ()
  (setf (upp-sort.graph) (uds-sort.graph.create))
  (setf (upp-l.epsilon) (uds-l.epsilon.create))
  (setf (uds=sort.node.alist) nil)
  (setf (upp=mkrp.literals) nil))


;----------------------------------------------------------------------------
;------------------- MKRP EPSILON-LITERAL INTERFACE -------------------------


(defun upp-epsilon.literals.insert (literals)
  (declare (edited  "05-FEB-1992 17:56")
	   (authors KASPER)
	   (input   "literals - eine Liste der in den Sorten-Graphen einzufuegenden Epsilon-Literale in MKRP-Rep.")
	   (effect  "Der Sorten-Graph und L-Epsilon werden destruktiv um die Epsilon-Literale literals erweitert.")
	   (value   "undefiniert"))
  (mapc #'(lambda(literal)
	    (when (upp=epsilon.literal.useful.p literal)
	      (let ((epsilon.literal (upp=epsilon.literal.create literal)))
		(upp=mkrp.literal.add literal)
		(upp=l.epsilon.literal.insert (upp-l.epsilon) epsilon.literal)
		(upp=sort.graph.epsilon.literal.insert (upp-sort.graph) epsilon.literal)
	      
		(upp=sort.graph.cycle.reduce (upp-sort.graph))
		(upp=sort.graph.mark.full.sorts (upp-sort.graph) (upp-l.epsilon)))))
	literals))



(defun upp-epsilon.literals.delete (literals)
  (declare (edited  "05-FEB-1992 17:58")
	   (authors KASPER)
	   (input   "literals - eine Liste der aus dem Sorten-Graphen zu loeschenden Epsilon-Literale in MKRP-Rep.")
	   (effect  "Der Sorten-Graph und L-Epsilon werden destruktiv um die Epsilon-Literale literals reduziert.")
	   (value   "undefiniert"))
  (mapc #'(lambda(literal)
	    (when (member literal (upp=mkrp.literals) :test 'equal)
	      (let ((epsilon.literal (upp=epsilon.literal.get literal)))
		(upp=mkrp.literal.delete literal)
		(upp=l.epsilon.literal.delete (upp-l.epsilon) epsilon.literal)
		(upp=sort.graph.epsilon.literal.delete (upp-sort.graph) epsilon.literal)
	      
		(upp=sort.graph.mark.full.sorts (upp-sort.graph) (upp-l.epsilon)))))
	literals))



(defun upp=epsilon.literal.useful.p (literal)
  (declare (edited  "11-JUN-1992 12:32")
	   (authors KASPER)
	   (input   "literal - eine Literal in MKRP-Form (<clause> <Literalno>)")
	   (effect  "keiner")
	   (value   "T, wenn das literal ein Epsilon-Literal ist, welches in die Menge L-Epsilon gehoert; NIL sonst"))
  (let ((clause (first literal)))
    (and (not (member clause (upp=mkrp.literals) :key 'first))
	 (every #'(lambda(index)
		    (and (equal (ds-lit.predicate (ds-clause.lit clause index))
				(dt-predicate.element))
			 (ds-sign.is.positive (ds-lit.sign (ds-clause.lit clause index)))))
		(upp=make.index.list 1 (ds-clause.nolit clause))))))


(defun upp=make.index.list (start end)
  (if (> start end)
      nil
      (cons start (upp=make.index.list (1+ start) end))))



(defun upp=epsilon.literal.get (clause.literal)
  (declare (edited  "07-FEB-1992 15:27")
	   (authors KASPER)
	   (input   "literal - ein E-Literal in MKRp-Rep. (<clause> <literal>)")
	   (effect  "keiner")
	   (value   "das E-Literal der internen Unifikations-Repraesentation"))
  (let* ((mkrp.literal (ds-clause.lit (first clause.literal) (second clause.literal)))
	 (sort         (second (ds-lit.termlist mkrp.literal)))
	 (el           nil))
    (some #'(lambda(epsilon.literal)
	      (when (equal (uds-epsilon.literal.mkrp.literal epsilon.literal)
			   mkrp.literal)
		(setq el epsilon.literal)
		epsilon.literal))
	  (uds-sort.node.epsilon.literals (uds-sort.get.sort.node sort)))
    (if el
	el
	(error "upp=epsilon.literal.get: Cannot find ~S" clause.literal))))



(defun upp=epsilon.literal.create (clause.literal)
  (declare (edited  "05-FEB-1992 18:19")
	   (authors KASPER)
	   (input   "clause.literal - eine Liste der Form (<clause.address> {<literal.index>}*)")
	   (effect  "keiner")
	   (value   "Es wird eine eigene Repraesentation des E-Literlas erzeugt und zurueckgeliefert"))
  (uds-epsilon.literal.create
    :mkrp.literal      (ds-clause.lit (first clause.literal) (second clause.literal))
    :residuum.literals (upp=clause.literal.residues clause.literal)))



(defun upp=clause.literal.residues (clause.literal)
  (declare (edited  "06-FEB-1992 15:08")
	   (authors KASPER)
	   (input   "clause.literal - eine Liste der Form (<clause.address> {<literal.index>}*)")
	   (effect  "keiner")
	   (value   "Liste der potentiellen Residuen-Literale"))
  (let ((clause (first clause.literal))
	(literal (second clause.literal))
	(residues nil))				; Achtung: Falls mehrere E-Literale in der Klausel stehen, werden die restlichen
    (if (= (ds-clause.nolit clause) 1)		;ignoriert. Die Unifikations ist jedoch vollst.
	nil
	(do ((index 1 (1+ index)))
	    ((= index (1+ (ds-clause.nolit clause))) residues)
	  (when (/= index literal)
	    (setq residues (cons (ds-clause.lit clause index) residues)))))))



;--------------------- END OF MKRP EPSILON-LITERAL INTERFACE ------------------
;------------------------------------------------------------------------------


(defun upp=l.epsilon.literal.insert (l.epsilon epsilon.literal)
  (declare (edited  "05-FEB-1992 18:32")
	   (authors KASPER)
	   (input   "l.epsilon - eine L-Epsilon-Datenstruktur"
		    "epsilon.literal - ein Epsilon-Literal")
	   (effect  "das Epsilon-Literal wird in das L-Epsilon eingefuegt")
	   (value   "undefiniert"))
  (cond ((dt-variable.is (uds-epsilon.literal.term epsilon.literal))
	 (setf (uds-l.epsilon.variable.term.literals l.epsilon)
	       (cons epsilon.literal (uds-l.epsilon.variable.term.literals l.epsilon))))
	((null (dt-term.variables (uds-epsilon.literal.term epsilon.literal)))
	 (setf (uds-l.epsilon.ground.term.literals l.epsilon)
	       (cons epsilon.literal (uds-l.epsilon.ground.term.literals l.epsilon))))
	(t (setf (uds-l.epsilon.non.ground.term.literals l.epsilon)
		 (cons epsilon.literal (uds-l.epsilon.non.ground.term.literals l.epsilon))))))



(defun upp=l.epsilon.literal.delete (l.epsilon epsilon.literal)
  (declare (edited  "05-FEB-1992 18:32")
	   (authors KASPER)
	   (input   "l.epsilon - eine L-Epsilon-Datenstruktur"
		    "epsilon.literal - ein Epsilon-Literal")
	   (effect  "das Epsilon-Literal wird aus L-Epsilon geloescht")
	   (value   "undefiniert"))
  (cond ((dt-variable.is (uds-epsilon.literal.term epsilon.literal))
	 (setf (uds-l.epsilon.variable.term.literals l.epsilon)
	       (delete epsilon.literal (uds-l.epsilon.variable.term.literals l.epsilon) :test 'eq :count 1)))
	((null (dt-term.variables (uds-epsilon.literal.term epsilon.literal)))
	 (setf (uds-l.epsilon.ground.term.literals l.epsilon)
	       (delete epsilon.literal (uds-l.epsilon.ground.term.literals l.epsilon) :test 'eq :count 1)))
	(t (setf (uds-l.epsilon.non.ground.term.literals l.epsilon)
		 (delete epsilon.literal (uds-l.epsilon.non.ground.term.literals l.epsilon) :test 'eq :count 1)))))



(defun upp=sort.graph.epsilon.literal.insert (sort.graph epsilon.literal)
  (declare (edited  "05-FEB-1992 18:40")
	   (authors KASPER)
	   (input   "sort.graph - ein Sorten-Graph"
		    "epsilon.literal - ein Epsilon-Literal")
	   (effect  "das Epsilon-Literal wird in den Sorten-Graphen einegfuegt"))
  (let ((sort.node (uds-epsilon.literal.sort.node epsilon.literal)))
    (if sort.node
	(setf (uds-sort.node.epsilon.literals sort.node) (cons epsilon.literal (uds-sort.node.epsilon.literals sort.node)))
	(let ((new.sort.node (uds-sort.node.create :mkrp.names (list (uds-epsilon.literal.mkrp.sort epsilon.literal))
						   :epsilon.literals (list epsilon.literal))))
	  (uds-sort.graph.sort.node.insert new.sort.node sort.graph)
	  (uds-sort.set.sort.node (uds-epsilon.literal.mkrp.sort epsilon.literal) new.sort.node)))
    
    (mapc #'(lambda(variable)
	      (unless (uds-sort.get.sort.node (dt-variable.sort variable))	      
		(let ((new.sort.node (uds-sort.node.create :mkrp.names (list (dt-variable.sort variable)))))
		  (uds-sort.graph.sort.node.insert new.sort.node sort.graph)
		  (uds-sort.set.sort.node (dt-variable.sort variable) new.sort.node))))
	  (dt-term.variables (uds-epsilon.literal.term epsilon.literal)))

    (when (dt-variable.is (uds-epsilon.literal.term epsilon.literal))
      (let ((variable.sort.node (uds-variable.sort.node (uds-epsilon.literal.term epsilon.literal)))
	    (super.sort.node    (uds-epsilon.literal.sort.node epsilon.literal)))
	(unless (or (intersection (uds-sort.node.arcs.out super.sort.node) (uds-sort.node.arcs.in variable.sort.node))
		    (eq variable.sort.node super.sort.node))
	  (uds-sort.graph.arc.insert super.sort.node variable.sort.node sort.graph
				     :residues (uds-epsilon.literal.residuum.literals epsilon.literal)))))))





(defun upp=sort.graph.epsilon.literal.delete (sort.graph epsilon.literal)
  (declare (edited  "05-FEB-1992 18:39")
	   (authors KASPER)
	   (input   "sort.graph - ein Sorten-Graph"
		    "epsilon.literal - ein Epsilon-Literal")
	   (effect  "das Epsilon-Literal wird aus dem Sorten-Graphen geloescht")
	   (value   "undifiniert"))
  (let ((sort.node (uds-epsilon.literal.sort.node epsilon.literal)))
    (setf (uds-sort.node.epsilon.literals sort.node)	  
	  (delete epsilon.literal (uds-sort.node.epsilon.literals sort.node) :test 'eq :count 1))
    (when (dt-variable.is (uds-epsilon.literal.term epsilon.literal))
      (let ((variable.sort.node (uds-variable.sort.node (uds-epsilon.literal.term epsilon.literal)))
	    (super.sort.node    (uds-epsilon.literal.sort.node epsilon.literal)))
	(when (intersection (uds-sort.node.arcs.out super.sort.node) (uds-sort.node.arcs.in variable.sort.node))
	  (uds-sort.graph.arc.delete (first (intersection (uds-sort.node.arcs.out super.sort.node)
							  (uds-sort.node.arcs.in variable.sort.node)))
				     sort.graph))))
    (when (not (some #'(lambda(e.literal)
			 (equal (uds-epsilon.literal.mkrp.sort e.literal)
				(uds-epsilon.literal.mkrp.sort epsilon.literal)))
		     (uds-sort.node.epsilon.literals (uds-epsilon.literal.sort.node epsilon.literal))))
      (setf (uds-sort.node.mkrp.names (uds-epsilon.literal.sort.node epsilon.literal))
	    (delete (uds-epsilon.literal.mkrp.sort epsilon.literal)
		    (uds-sort.node.mkrp.names (uds-epsilon.literal.sort.node epsilon.literal)))))
    (when (= (length (uds-sort.node.epsilon.literals sort.node)) 0)	
      (uds-sort.graph.sort.node.delete sort.node sort.graph)
      (uds-sort.node.alist.sort.node.delete sort.node))))


;------------------------ DETECTING AND ELIMINATING CYCLES IN THE SORT GRAPH ---------------------


(defun upp=sort.graph.cycle.reduce (graph)
  (declare (edited  "06-FEB-1992 15:11")
	   (authors KASPER)
	   (input   "graph - eine GRAPH-Datenstruktur")
	   (effect  "Durch Identifizieren von Knoten, die auf Zyklen liegen wird der Graph solange umstruktuiert,"
		    "bis der Graph zyklenfrei ist")
	   (value   "der zyklenfreie Graph"))
  (let ((cycle (upp=sort.graph.cycle.get graph)))
    (if (null cycle)
	graph
	(upp=sort.graph.cycle.reduce (upp=sort.graph.cycle.reduction.step graph cycle)))))



(defun upp=sort.graph.cycle.reduction.step (graph cycle)
  (declare (edited  "06-FEB-1992 15:13")
	   (authors KASPER)
	   (input   "graph - eine GRAPH-Datenstruktur"
                    "cycle - eine Liste von Knoten die einen Zyklus bilden")
	   (effect  "die ersten beiden Knoten des Zyklus werden identifiziert")
	   (value   "der um den zweiten Knoten des Zyklus reduzierte Graph"))
  (let ((first.node       (first cycle))
	(elimination.node (second cycle)))
    (when (eq first.node elimination.node)
      (uds-sort.graph.arc.delete (first (intersection (uds-sort.node.arcs.out first.node)
						      (uds-sort.node.arcs.in elimination.node)))
				 graph))
    (mapc #'(lambda(arc)						      ; erbe alle arcs die von dem zu 
	      (when (not (eq first.node (uds-arc.target.node arc)))	      ; loeschenden Knoten wegzeigen
		(uds-sort.graph.arc.insert first.node (uds-arc.target.node arc) graph
					   :residues (uds-arc.residuum.literals arc))))
	  (uds-sort.node.arcs.out elimination.node))
    (mapc #'(lambda(arc)						      ; erbe alle arcs die auf den zu 
	      (when (not (eq first.node (uds-arc.source.node arc)))	      ; loeschenden Knoten zeigen
		(uds-sort.graph.arc.insert (uds-arc.source.node arc) first.node graph
					   :residues (uds-arc.residuum.literals arc))))
	  (uds-sort.node.arcs.in elimination.node))
    (uds-sort.graph.sort.node.delete elimination.node graph)		      ; loescht Knoten und alle Kanten, die
                                                                              ; auf ihn und von ihm wegzeigen
    (setf (uds-sort.node.epsilon.literals first.node)
	  (append (uds-sort.node.epsilon.literals first.node) (uds-sort.node.epsilon.literals elimination.node)))
    (setf (uds-sort.node.mkrp.names first.node)
	  (append (uds-sort.node.mkrp.names first.node) (uds-sort.node.mkrp.names elimination.node)))
    (setf (uds-sort.node.full first.node)
	  (or (uds-sort.node.full.p first.node) (uds-sort.node.full.p elimination.node)))
    (mapc #'(lambda(mkrp.sort)
	      (uds-sort.set.sort.node mkrp.sort first.node))
	  (uds-sort.node.mkrp.names elimination.node))
    (upp=sort.graph.multiple.arcs.eliminate graph)
    graph))       




(defun upp=sort.graph.multiple.arcs.eliminate (graph)
  (declare (edited  "01-JUN-1992 15:25")
	   (authors KASPER)
	   (input   "graph - eine Graphenstruktur")
	   (effect  "aus dem graph werden alle Mehrfachkanten entfernt")
	   (value   "der schlichte Graph"))
  (mapc #'(lambda(arc)
	    (uds-sort.graph.arc.delete arc graph))
	(upp=sort.graph.redundant.arcs graph)))


(defun upp=sort.graph.redundant.arcs (graph)
  (delete nil (maplist #'(lambda(arc.list)
			   (when (member (first arc.list)
					 (rest arc.list) :test #'(lambda(arc.1 arc.2)
								   (and (eq (uds-arc.source.node arc.1)
									    (uds-arc.source.node arc.2))
									(eq (uds-arc.target.node arc.1)
									    (uds-arc.target.node arc.2)))))
			     (first arc.list)))
		       (uds-sort.graph.arcs graph))))



(defun upp=sort.graph.cycle.get(graph)
  (declare (edited  "06-FEB-1992 15:50")
	   (authors KASPER)
	   (input   "graph - eine Graph-Datenstruktur")
	   (effect  "keiner")
	   (value   "eine Liste von Knoten, die einen Zyklus in dem Graph bilden; oder NIL, falls der Graph zyklenfrei ist"))
  (some #'(lambda(node)
	    (upp=node.cycle.get (list node)))
	(uds-sort.graph.sort.nodes graph)))



(defun upp=node.cycle.get (path)
  (declare (edited  "06-FEB-1992 15:57")
	   (authors KASPER)
	   (input   "path - eine Liste mit einem Knoten")
	   (effect  "keiner")
	   (value   "eine Liste von Knoten, die einen Zyklus bilden, oder NIL falls der erste Knoten von path nicht"
		    "auf einem Zyklus liegt."))
  (some #'(lambda(arc)
	    (if (member (uds-arc.target.node arc) path)
		path
		(upp=node.cycle.get (cons (uds-arc.target.node arc) path))))
	(uds-sort.node.arcs.out (first path))))




;-------------------- DETECTING AND SETTING EMPTY SORTS IN THE SORT-HIERARCHY -------------------------


(defun upp=sort.graph.mark.full.sorts (sort.graph l.epsilon)
  (declare (edited  "06-FEB-1992 17:06")
	   (authors KASPER)
	   (input   "sort.graph - der Sorten-Graph"
		    "l.epsilon  - die L-Epsilon-Datenstruktur")
	   (effect  "Alle Sorten des Sorten-Graphen, die voll sind werden auf voll gesetzt")
	   (value   "undefiniert"))
  (mapc #'(lambda(sort.node)
	    (setf (uds-sort.node.full sort.node) nil)
	    (when (member (dt-constant.omega) (uds-sort.node.mkrp.names sort.node) :test 'equal)
	      (setf (uds-sort.node.full sort.node) t)))
	(uds-sort.graph.sort.nodes sort.graph))
  (upp=sort.graph.mark.full.sorts.with.ground.term.literals (uds-l.epsilon.ground.term.literals l.epsilon)))



(defun upp=sort.graph.mark.full.sorts.with.ground.term.literals (l.epsilon.ground.term.literals)
  (declare (edited  "06-FEB-1992 17:11")
	   (authors KASPER)
	   (input   "l.epsilon.ground.term.literals - eine Liste von Epsilon-Literalen, deren Term ein Grund-Term ist")
	   (effect  "Alle Sorten des Sorten-Graphen, die Grund-Terme enthalten, werden auf voll gesetzt")
	   (value   "undefiniert"))
  (if (null l.epsilon.ground.term.literals)
      (upp=sort.graph.mark.full.sorts.with.non.ground.term.literals nil (uds-l.epsilon.non.ground.term.literals (upp-l.epsilon)))
      (progn (upp=sort.graph.propagate.full.sort (first l.epsilon.ground.term.literals))
             (upp=sort.graph.mark.full.sorts.with.ground.term.literals (rest l.epsilon.ground.term.literals)))))


       
(defun  upp=sort.graph.mark.full.sorts.with.non.ground.term.literals (unknown non.ground.term.literals)
  (declare (edited  "06-FEB-1992 17:14")
	   (authors KASPER)
	   (input   "unknown - eine leere Liste"
		    " non.ground.term.literals -eine Liste von Epsilon-Literalen deren Term kein Grund-Terme ist")
	   (effect  "Alle Sorten des Sorten-Graphen, die voll sind werden auf voll gesetzt")
	   (value   "undefiniert"))
  (cond ((null non.ground.term.literals) t)
	((upp=sort.propagatable.p (first non.ground.term.literals))
	 (progn
	   (upp=sort.graph.propagate.full.sort (first non.ground.term.literals))
	   (upp=sort.graph.mark.full.sorts.with.non.ground.term.literals
	     nil
	     (append unknown  (remove-if #'(lambda(epsilon.literal)
					     (uds-sort.node.full.p (uds-epsilon.literal.sort.node epsilon.literal)))
					 non.ground.term.literals)))))
	(t (upp=sort.graph.mark.full.sorts.with.non.ground.term.literals (cons (first non.ground.term.literals) unknown)
									 (rest non.ground.term.literals)))))




(defun upp=sort.graph.propagate.full.sort (epsilon.literal)
  (declare (edited  "06-FEB-1992 17:19")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal")
	   (effect  "Der Sorten-Knoten des Epsilon-Literals, sowie alle Obersorten dieses Sorten-Knotens werden auf voll gesetzt")
	   (value   "undefiniert"))
  (let ((sort.node (uds-epsilon.literal.sort.node epsilon.literal)))
    (setf (uds-sort.node.full sort.node) t)
    (upp=sort.graph.propagate sort.node)))



(defun upp=sort.graph.propagate (sort.node)
  (declare (edited  "06-FEB-1992 17:19")
	   (authors KASPER)
	   (input   "sort.node - ein Sorten-Knoten")
	   (effect  "Alle Obersorten des  Sorten-Knotens sort.node werden auf voll gesetzt")
	   (value   "undefiniert"))
  (mapc #'(lambda (arc)
	    (setf (uds-sort.node.full (uds-arc.source.node arc)) t)
	    (upp=sort.graph.propagate (uds-arc.source.node arc)))
        (uds-sort.node.arcs.in sort.node)))


	    
(defun upp=sort.propagatable.p (epsilon.literal)
  (declare (edited  "06-FEB-1992 17:28")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal")
	   (effect  "keiner")
	   (value   "T, wenn das Literl benutzt werden kann volle Sorten zu setzten; NIL sonst"))
  (every #'(lambda(variable)
	     (uds-sort.node.full.p (uds-variable.sort.node variable)))
	 (dt-term.variables (uds-epsilon.literal.term epsilon.literal))))



;-------------------------- TEST INTERFACE -------------------------------------



(defun upp-literal.transfer (lit.list)
  (mapcar #'(lambda(lit)
	      (list (ds-clause.create nil nil nil (list (ds-lit.create '+ (dt-predicate.element) lit))) 1))
	  lit.list))