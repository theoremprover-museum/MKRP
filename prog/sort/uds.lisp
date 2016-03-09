;;; -*- Package: MARKGRAF-KARL; Mode: LISP; Syntax: Common-Lisp -*-

(in-package "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(proclaim '(declaration edited authors input effect value remarks))


;------------------------------- EQUATION -----------------------------------------

(defstruct (equation (:type list)
		     (:constructor uds-equation.create(left.term right.term &key locked))
		     (:copier uds-equation.copy)
		     (:conc-name uds-equation.)
		     (:predicate uds-equation.p)
		     :named)
  
  "left.term  - Term on the left side of the equation
   right.term - Term on the right side of the equation
   locked     - T if equation can be used for a specific unification rule"

  left.term
  right.term
  (locked nil))




;---------------------------- UNIFICATION PROBLEM --------------------------------


(defstruct (unification.problem (:type list)
				(:constructor uds-unification.problem.create)
				(:conc-name uds-unification.problem.)
				(:predicate uds-unification.problem.p)
				:named)
  "set               - List of equations of the unification problem
   variables         - List of the different variables in the unification problem
   depth             - the depth of this problem in the unification tree
   residuum.literals - List of the residues belonging to this unification problem"

  set
  (variables nil)
  (depth 1)
  (residuum.literals nil))



;---------------------------- UNIFICATION STATE ----------------------------------



(defstruct (unification.state (:type list)
			      (:constructor uds-unification.state.create)
			      (:conc-name uds-unification.state.)
			      (:predicate uds-unification.state.p)
			      :named)
  "problems                - List of the unification problems that are still activ
   tree.depth              - Depth of the tree constructed by the unification
   number.of.open.nodes    - Number of leafs in the unification-tree (should be equal to (length problems)
   number.of.applied.rules - Number of applied rules during unification
   mgus                    - List of all most general unifiers found"

  problems
  (tree.depth 1)
  (number.of.open.nodes 1)
  (number.of.applied.rules 0)
  (mgus nil))


;---------------------------- MOST GENERAL UNIFIER ----------------------------------



(defstruct (mgu (:type list)
		(:constructor uds-mgu.create (substitution residuum.literals))
		(:conc-name uds-mgu.)
		(:predicate uds-mgu.p)
		:named)
  "substitution      - the unifier itsself
   rediduum.literals - the residuum literals of this mgu"

  substitution
  residuum.literals)


;------------------------------- UNIFICATION RULE -----------------------------------


(defstruct (unification.rule (:type list)
			     (:constructor uds-unification.rule.create)
			     (:conc-name uds-unification.rule.)
			     (:predicate uds-unification.rule.p)
			     (:copier nil)
			     :named)
  "applicator      - the function that applies the to an equation
   apply.predicate - the predicate that ist true if the is applicable to an equation
   sorted.p        - T iff the rule is an sorted unification rule;
   fail.p          - T iff the rule is an fail rule
   confluent.p     - T iff the rule is confluent
   priority        - an fixnum which determines the heuristic priority of this during unification
   name            - a string which determines the name of the rule"
  
  applicator
  apply.predicate
  sorted.p
  fail.p
  confluent.p
  priority
  name)


;------------------------------- UNIFICATION OBJECT ---------------------------------

(defstruct (unification.object (:type list)
			       (:constructor uds-unification.object.create (unification.problem equation rules))
			       (:conc-name uds-unification.object.)
			       (:predicate uds-unification.object.p)
			       (:copier nil)
			       :named)
  "unification.problem - ein Unifikations-Problem
   equation            - eine Gleichung aus dem Unifikations-Problem
   rules               - eine Liste von Unifktaions-Regeln, die auf die Gleichung equation anwendbar sind"
  
  unification.problem
  equation
  rules)

;------------------------------- EPSILON LITERAL -------------------------------------

(defstruct (epsilon (:type list)
		    (:constructor uds-epsilon.literal.create)
		    (:conc-name uds-epsilon.literal.)
		    (:predicate uds-epsilon.literal.p)
		    :named)
  

  mkrp.literal
  residuum.literals)


;------------------------------- L EPSILON SET ----------------------------------------


(defstruct (l.epsilon (:type list)
		      (:constructor uds-l.epsilon.create)
		      (:conc-name uds-l.epsilon.)
		      :named)
  "ground.term.literals     - a list of all epsilon-literals which term is a ground term
   non.ground.term.literals - a list of all epsilon-literals which term is not a ground term
   variable.term.literals   - a list of all epsilon.literals which term is a variable"
  
  
  ground.term.literals
  non.ground.term.literals
  variable.term.literals)




;------------------------------ SORT RESIDUE TUPEL  ------------------------------

(defstruct (tupel (:type list)
		  (:constructor uds-tupel.create)
		  (:conc-name uds-tupel.)
		  :named)
  
  
  
  sort.node
  residuum.literals)

  

;-------------------------------- SORT GRAPH --------------------------------------

(defstruct (sort.graph (:type vector)
		       (:constructor uds-sort.graph.create)
		       (:conc-name uds-sort.graph.)
		       (:predicate uds-sort.graph.p)
		       (:copier nil)
		       :named)

  "sort.nodes     : Menge des Sorten-Knoten des Graphen
   arcs           : Menge der gerichteten Kanten des Graphen"

  sort.nodes
  arcs)



;-------------------------- SORT NODE Datenstruktur ---------------------------

(defstruct (sort.node (:type vector)
		      (:constructor uds-sort.node.create )
		      (:conc-name uds-sort.node.)
		      (:predicate uds-sort.node.p)
		      (:copier nil)
		      :named)

  "arcs.in   : Liste der auf diesen Knoten gerichteten Kanten
   arcs.out  : Liste der von diesem Knoten wegzeigenden Kanten"


  arcs.in
  arcs.out
  epsilon.literals
  (full nil)
  (mkrp.names nil))
 


;-------------------------- ARC Datenstruktur --------------------------------

(defstruct (arc (:type vector)
                (:constructor uds=arc.create (source.node target.node residuum.literals)) 
                (:conc-name uds-arc.)
		(:predicate uds-arc.p)
		(:copier nil)
                :named)

  "source.node  : Startknoten der gerichteten Kante
   target.node  : Zielknoten der gerichteten Kante"

  source.node
  target.node
  residuum.literals)




;------------------------ SORT GRAPH OPERATIONS ----------------------------
;----------------------------- Destruktoren --------------------------------


(defun uds-sort.graph.arc.insert (source.node target.node graph &key (residues nil))
  (declare (edited  "29-JAN-1992 16:00")
	   (authors KASPER)
	   (input   "source.node, target.node - zwei Knoten-Datenstrukturen"
		    "graph                    - eine Graph-Datenstruktur")
	   (effect  "es wird eine Kante in den Graphen graph von source.node nach target.node eingefuegt")
	   (value   "der um die neue Kante erweiterte Graph"))
  (let ((arc (uds=arc.create source.node target.node residues)))
    (setf (uds-sort.node.arcs.out source.node) (cons arc (uds-sort.node.arcs.out source.node)))
    (setf (uds-sort.node.arcs.in  target.node) (cons arc (uds-sort.node.arcs.in target.node)))
    (setf (uds-sort.graph.sort.nodes graph)    (delete-duplicates (append (uds-arc.sort.nodes arc)
									  (uds-sort.graph.sort.nodes graph))
								  :test 'eq))
    (setf (uds-sort.graph.arcs graph)          (cons arc (uds-sort.graph.arcs graph)))
    graph))   
                                              


(defun uds-sort.graph.sort.node.insert (node graph)
  (declare (edited  "29-JAN-1992 16:10")
	   (authors KASPER)
	   (input   "node  - ein Knoten"
		    "graph - ein Graph")
	   (effect  "Der Sortenknoten node wird in den Sorten-Graphen eingefuegt.")
	   (value   "der um den neuen Knoten erweiterte Graph"))
  (setf (uds-sort.graph.sort.nodes graph) (adjoin node (uds-sort.graph.sort.nodes graph)))
  graph)



(defun uds-sort.graph.arc.delete(arc graph)
  (declare (edited  "29-JAN-1992 16:13")
	   (authors KASPER)
	   (input   "arc   - eine gerichtete Kante"
		    "graph - ein Graph")
	   (effect  "Die gerichtete Kante arc wird aus der Struktur des Graphen geloescht.")
	   (value   "der um die Kante arc reduzierte Graph"))
  (let ((source.node (uds-arc.source.node arc))
	(target.node (uds-arc.target.node arc)))
    (setf (uds-sort.node.arcs.out source.node) (remove arc (uds-sort.node.arcs.out source.node)))
    (setf (uds-sort.node.arcs.in  target.node) (remove arc (uds-sort.node.arcs.in target.node)))
    (setf (uds-sort.graph.arcs graph)          (remove arc (uds-sort.graph.arcs graph)))
    graph))
 



(defun uds-sort.graph.sort.node.delete(node graph)
  (declare (edited  "29-JAN-1992 16:16")
	   (authors KASPER)
	   (input   "node  - ein Knoten"
		    "graph - ein Graph")
	   (effect  "Der Knoten node wird aus der Struktur des Graphen geloescht.")
	   (value   "der um den Knoten node reduzierte Graph")
	   (remarks "wird ein Knoten aus einem Graphen geloescht, an dem noch Kanten"
		    "haengen, so werden diese Kanten auch alle geloescht"))
  (mapc #'(lambda(arc)	    
	    (uds-sort.graph.arc.delete arc graph))		  
	(append (uds-sort.node.arcs.in node) (uds-sort.node.arcs.out node)))
  (setf (uds-sort.graph.sort.nodes graph) (remove node (uds-sort.graph.sort.nodes graph)))
  graph)




;------------------------ Selektoren - Praedikate ---------------------------


(defun uds-nodes.adjacent.p(node.1 node.2)
  (declare (edited  "29-JAN-1992 16:18")
	   (authors KASPER)
	   (input   "node.1, node.2 - zwei Knoten")
	   (effect  "keiner")
	   (value   "die Kante, die beide Knoten verbindet, falls diese adjazent sind und NIL sonst"))
  (some #'(lambda (arc)
	    (first (member arc (uds-sort.node.arcs.in node.2))))
	(uds-sort.node.arcs.out node.1)))
                    


(defun uds-arc.sort.nodes(arc)
  (declare (edited  "29-JAN-1992 16:19")
	   (authors KASPER)
	   (input   "arc - eine gerichtete Kante")
	   (effect  "keiner")
	   (value   "das Tupel der beiden Knoten der gerichteten Kante"))
  (if (uds-arc.p arc)
      (list (uds-arc.source.node arc) (uds-arc.target.node arc))
      (error "uds-arc.nodes - Argument ist keine gerichtet Kante (arc): ~S"arc)))




(defun uds-sort.node.adjacent.sort.nodes(node)
  (declare (edited  "29-JAN-1992 16:21")
	   (authors KASPER)
	   (input   "node - ein Knoten")
	   (effect  "keiner")
	   (value   "eine Liste aller Knoten, die zu node adjazent sind"))
   (remove-duplicates (mapcar #'uds-arc.target.node (uds-sort.node.arcs.out node))))




(defun uds-sort.node.full.p (sort.node)
  (uds-sort.node.full sort.node))


(defun uds-sort.node.empty.p (sort.node)
  (not (uds-sort.node.full.p sort.node)))



(defun uds-sort.node.print(node &optional (stream t))
  (declare (edited  "08-FEB-1992 14:12")
	   (authors KASPER)
	   (input   "node             - ein Knoten"
                    " &optional stream - ein Strom")
	   (effect  "Alle Informatione, die an dem Knoten haengen, werden auf den stream geschrieben")
	   (value   "undefiniert"))
  (if (uds-sort.node.p node)
      (progn 
	(format stream "~%")
	(format stream "Knoten:~%")
	(format stream "-------~%")
	(format stream "Sorte(n): ")
	(mapc #'(lambda(mkrp.sort) (uds-print.term mkrp.sort)) (uds-sort.node.mkrp.names node))
	(terpri stream)
	(format stream "E-Literale:~2%")
	(mapc #'(lambda(literal)
                  (uds-epsilon.literal.print literal t stream)
		  (format stream "~%"))
	      (uds-sort.node.epsilon.literals node))
	(format stream "Sorte voll: ~S~2%" (uds-sort.node.full.p node)))
      (error "uds-node.print: Argument der Funktion ist kein Knoten (node) ~S" node)))



(defun uds-arc.print (arc &optional (stream t))
  (declare (edited  "08-FEB-1992 14:30")
	   (authors KASPER)
	   (input   "arc - eine Kante")
	   (effect  "alle Informationen, die an der Kante haengen, werden auf den stream geschrieben.")
	   (value   "undefiniert"))
  (if (uds-arc.p arc)
      (progn
	(format stream "----------------------------------------------------~%")
	(format stream "Kante:~%")
	(format stream "Start-Knoten:~%")
	(uds-sort.node.print (uds-arc.source.node arc))
	(format stream "Ziel-Knoten:~%")
	(uds-sort.node.print (uds-arc.target.node arc))
	(format stream "Residuen:~%")
	(mapc #'(lambda (mkrp.literal)
	      (uds-print.term (first (ds-lit.termlist mkrp.literal)) stream)
	      (format stream " Element ")
	      (uds-print.term (second (ds-lit.termlist mkrp.literal)) stream)
	      (format stream "~%"))
	  (uds-arc.residuum.literals arc)))
      (error "uds-arc.print: Argument der Funktion ist keine Kante (arc) ~S" arc)))



(defun uds-sort.graph.print (sort.graph &optional (stream t))
  (declare (edited  "08-FEB-1992 14:55")
	   (authors KASPER)
	   (input   "sort.graph - ein Sorten-Graph")
	   (effect  "Die Informationen des Sorten-Graphen werden auf den Stream geschrieben.")
	   (value   "undefiniert"))  
  (if (uds-sort.graph.p sort.graph)
      (progn
	(format stream "~%SORTEN-GRAPH:~%")
	(format stream "-------------~%")
	(format stream "SORTEN-KNOTEN:~%")
	(mapc #'uds-sort.node.print (uds-sort.graph.sort.nodes sort.graph))
	(format stream "KANTEN:~%")	       
	(mapc #'uds-arc.print (uds-sort.graph.arcs sort.graph))
	t)
      (error "uds-sort.graph.print: Argument der Funktion ist keine Sorten-Graph-Datenstruktur ~S" sort.graph)))



;--------------------------------------------------------------------------------------------
;---------------------- SORT SORT-NODE ALIST OPERATIONS -------------------------------------

(defvar uds*sort.node.alist nil)

(defmacro uds=sort.node.alist ()
  'uds*sort.node.alist)


(defun uds-sort.get.sort.node (sort)
  (declare (edited  "05-FEB-1992 17:52")
	   (authors KASPER)
	   (input   "sort - eine MKRP-Sorte")
	   (effect  "keiner")
	   (value   "den Sorten-Knoten des Sorten-Graphen dieser Sorte"))
  (let ((acons (assoc sort (uds=sort.node.alist))))
    (if acons
      (rest acons)
      (let ((node (uds-sort.node.create :full nil :mkrp.names (list sort))))
	(setf (uds=sort.node.alist) (acons sort node (uds=sort.node.alist)))
	(uds-sort.graph.sort.node.insert node (upp-sort.graph))
	node))))

(defun uds-sort.set.sort.node (sort sort.node)
  (declare (edited  "05-FEB-1992 17:53")
	   (authors KASPER)
	   (input   "sort - eine MKRP-Sorte"
		    "sort.node - ein Sorten-Knoten")
	   (effect  "setzt zu der MKRP-Sorte sort den korrespondierenden Sorten-Knoten sort.node")
	   (value   "undefiniert"))
  (if (assoc sort (uds=sort.node.alist))
      (progn
	(setf (uds=sort.node.alist) (delete-if #'(lambda(acons) (= sort (first acons))) (uds=sort.node.alist)))
	(setf (uds=sort.node.alist) (acons sort sort.node (uds=sort.node.alist))))
      (setf (uds=sort.node.alist) (acons sort sort.node (uds=sort.node.alist)))))



(defun uds-sort.node.get.sort (sort.node)
  (declare (edited  "05-FEB-1992 17:54")
	   (authors KASPER)
	   (input   "sort.node - eine Sorten-Knoten")
	   (effect  "keiner")
	   (value   "MKRP-Sorte dieses Sorten-Knotens"))
  (let ((acons (rassoc sort.node (uds=sort.node.alist))))
    (when acons
      (first acons))))


(defun uds-sort.node.alist.sort.node.delete (sort.node)
  (setf (uds=sort.node.alist) (delete-if #'(lambda(acons) (eq (rest acons) sort.node)) (uds=sort.node.alist) :count 1)))



;-------------------------------------------------------------------------------------
;---------------------------- EPSILON LITERAL OPERATIONS -----------------------------



(defun uds-epsilon.literal.term (epsilon.literal)
  (declare (edited  "06-FEB-1992 17:34")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal")
	   (effect  "keiner")
	   (value   "der Term des Literals"))
  (first (ds-lit.termlist (uds-epsilon.literal.mkrp.literal epsilon.literal))))



(defun uds-epsilon.literal.mkrp.sort (epsilon.literal)
  (declare (edited  "06-FEB-1992 17:37")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal")
	   (effect  "keiner")
	   (value   "die MKRP-Sorte des Literals"))
  (second (ds-lit.termlist (uds-epsilon.literal.mkrp.literal epsilon.literal))))



(defun uds-epsilon.literal.sort.node (epsilon.literal)
  (declare (edited  "06-FEB-1992 17:38")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal")
	   (effect  "keiner")
	   (value   "den Sorten-Knoten der Sorten des Epsilon-Literals"))
  (uds-sort.get.sort.node (uds-epsilon.literal.mkrp.sort epsilon.literal)))


(defun uds-epsilon.literal.term.variables (epsilon.literal)
  (declare (edited  "15-FEB-1992 20:47")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal")
	   (effect  "keiner")
	   (value   "Liste aller Variablen des Terms des Epsilon-Literals"))
  (dt-term.variables (uds-epsilon.literal.term epsilon.literal)))



(defun uds-epsilon.literal.renaming.substitution (epsilon.literal)
  (declare (edited  "15-FEB-1992 21:16")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal")
	   (effect  "keiner")
	   (value   "eine Substitution, die die Variables des Epsilon-Literal-Terms umbenennt"))
  (subst-variables.rename (uds-epsilon.literal.term.variables epsilon.literal)))



(defun uds-epsilon.literal.rename (epsilon.literal)
  (declare (edited  "06-FEB-1992 17:39")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal")
	   (effect  "keiner")
	   (value   "ein Epsilon-Literal dessen Variablen umbenannt wurden und auf deren Residuuen die Umbenennung auch durch-"
		    "gefuehrt wurde"))
  (let ((renaming.subst (subst-variables.rename (dt-term.variables (uds-epsilon.literal.term epsilon.literal)))))
    (uds-epsilon.literal.create
      :mkrp.literal (uds-mkrp.literal.subst.apply (uds-epsilon.literal.mkrp.literal epsilon.literal) renaming.subst)
      :residuum.literals (mapcar #'(lambda(mkrp.literal)
				     (uds-mkrp.literal.subst.apply mkrp.literal renaming.subst))
				 (uds-epsilon.literal.residuum.literals epsilon.literal)))))


(defun uds-epsilon.literal.subst.apply (epsilon.literal subst)
  (declare (edited  "14-FEB-1992 14:38")
	   (authors KASPER)
	   (input   "epsilon.literal - ein Epsilon-Literal"
		    "subst           - eine Substitution")
	   (effect  "keiner")
	   (value   "das umbenannte Epsilon-Literal"))
  (uds-epsilon.literal.create
    :mkrp.literal (uds-mkrp.literal.subst.apply (uds-epsilon.literal.mkrp.literal epsilon.literal) subst)
    :residuum.literals (mapcar #'(lambda(mkrp.literal)
				   (uds-mkrp.literal.subst.apply mkrp.literal subst))
			       (uds-epsilon.literal.residuum.literals epsilon.literal))))



(defun uds-mkrp.literal.subst.apply (mkrp.literal subst)
  (declare (edited  "14-FEB-1992 14:42")
	   (authors KASPER)
	   (input   "mkrp.literal - ein MKRP-Literal"
		    "subst        - eine MKRP-Substitution")
	   (effect  "keiner")
	   (value   "Das Literal nach Anwendung der Substitution subst"))
  (ds-lit.create (ds-lit.sign mkrp.literal)
		 (ds-lit.predicate mkrp.literal)
		 (mapcar #'(lambda (term)
			     (subst-apply.to.term subst term))
			 (ds-lit.termlist mkrp.literal))))



(defun uds-epsilon.literal.print (epsilon.literal &optional (residues nil) (stream t))
  (declare (edited  "08-FEB-1992 14:31")
	   (authors KASPER)
	   (input   "epsion.literal - ein Epsilon-Literal")
	   (effect  "Alle Informationen des Epsilon-Literals werden auf den stream geschrieben.")
	   (value   "undefiniert"))
  (format stream "Epsilon-Literal:~%")
  (format stream "----------------~%")
  (uds-print.term (uds-epsilon.literal.term epsilon.literal) stream)
  (format stream " Element ")
  (uds-print.term (uds-epsilon.literal.mkrp.sort epsilon.literal) stream)
  (when residues
    (format stream "~%Residuen:~%")
    (mapc #'(lambda (mkrp.literal)
	      (uds-mkrp.literal.print mkrp.literal)
	      (format stream "~%"))
	  (uds-epsilon.literal.residuum.literals epsilon.literal)))
  (format stream "~%"))


(defun uds-mkrp.literal.print (mkrp.literal &optional (stream t))
  (format stream "~A" (dt-pname (ds-lit.predicate mkrp.literal)))
  (format stream "(")
  (mapl #'(lambda(rest.args)
	    (if (null (rest rest.args))
		(uds-print.term (first rest.args))
		(progn
		  (uds-print.term (first rest.args))
		  (format stream " "))))
	(ds-lit.termlist mkrp.literal))
  (format stream ")"))


;-------------------------------------------------------------------------------------
;---------------------------- UNIFICATION PROBLEM OPERATIONS -------------------------

(defun uds-unification.problem.print (unification.problem &optional (stream t))
  (declare (edited  "26-FEB-1992 14:08")
	   (authors KASPER)
	   (input   "unification.problem - ein Unifikationsproblem")
	   (effect  "Die Informationen des Unifikationsproblems werden auf den stream geschrieben.")
	   (value   "undefiniert"))
  (format stream "~%UNIFIKATIONS-PROBELM:~%")
  (format stream "---------------------~%")
  (format stream "Gleichungen:~%")
  (mapc #'(lambda(equation)
	    (uds-equation.print equation)
	    (terpri stream))
	(uds-unification.problem.set unification.problem))
  (format stream "Variablen: ")
  (mapc #'(lambda(variable)
	    (uds-print.term variable stream)
	    (format stream "   "))
	(uds-unification.problem.variables unification.problem))
  (format stream "~%")
  (format stream "Baumtiefe: ~S~%" (uds-unification.problem.depth unification.problem))
  (format stream "Residuumliterale:~%")
  (mapc #'(lambda(epsilon.literal)
	    (uds-mkrp.literal.print epsilon.literal stream))
	(uds-unification.problem.residuum.literals unification.problem))
  t)



;-------------------------------------------------------------------------------------
;------------------------------ L EPSILON SET OPERATIONS -----------------------------


(defun uds-l.epsilon.print (l.epsilon &optional (stream t))
  (declare (edited  "25-FEB-1992 13:32")
	   (authors KASPER)
	   (input   "l.epsilon - eine L-Epsilon-Datenstruktur")
	   (effect  "Die Informationen von l.epsilon werden auf den stream geschrieben.")
	   (value   "undefiniert"))
  (format stream "~%EPSILON-GRUND-TERM-LITERALE:~%")
  (format stream "----------------------------~%")
  (mapc #'(lambda(epsilon.literal)
	    (uds-epsilon.literal.print epsilon.literal t stream))
	(uds-l.epsilon.ground.term.literals l.epsilon))
  (format stream "~%EPSILON-VARIABLEN-TERM-LITERALE:~%")
  (format stream "--------------------------------~%")
  (mapc #'(lambda(epsilon.literal)
	    (uds-epsilon.literal.print epsilon.literal t stream))
	(uds-l.epsilon.variable.term.literals l.epsilon))
  (format stream "~%EPSILON-NICHT-GRUND-TERM-LITERALE:~%")
  (format stream "----------------------------------~%")
  (mapc #'(lambda(epsilon.literal)
	    (uds-epsilon.literal.print epsilon.literal t stream))
	(uds-l.epsilon.non.ground.term.literals l.epsilon))
  t)



;-------------------------------------------------------------------------------------
;---------------------------------- EQUATION OPERATIONS ------------------------------

(defun uds-equation.locked.p (equation)
  (declare (edited  "15-FEB-1992 21:14")
	   (authors KASPER)
	   (input   "equation - eine Gleichung")
	   (effect  "keiner")
	   (value   "T, falls die Gleichung gesperrt ist; NIL sonst"))
  (uds-equation.locked equation))


(defun uds-equation.print (equation &optional (stream t))
  (declare (edited  "26-FEB-1992 14:06")
	   (authors KASPER)
	   (input   "equation - eine Gleichung")
	   (effect  "die Gleichung wird auf den stream geschrieben")
	   (value   "undefiniert"))
  (uds-print.term (uds-equation.left.term equation) stream)
  (format stream " = ")
  (uds-print.term (uds-equation.right.term equation) stream)
  (format stream "       Locked: ~S" (uds-equation.locked equation)))

;-------------------------------------------------------------------------------------
;------------------------------ UNIFICATION STATE OPERATIONS -------------------------

(defun uds-unification.state.print (unification.state &optional (stream t))
  (declare (edited  "26-FEB-1992 16:04")
	   (authors KASPER)
	   (input   "unification.state - eine Unifikationsstatus-Datenstruktur")
	   (effect  "die Information aus dem unification.state werden auf den stream geschrieben")
	   (value   "undefiniert"))
  (format stream "~2%UNIFKATIONS-STATUS:")
  (format stream "~%-------------------")
  (mapc #'(lambda(unification.problem)
	    (uds-unification.problem.print unification.problem stream))
	(uds-unification.state.problems unification.state))
  (format stream "~%MGUS:")
  (mapc #'uds-mgu.print (uds-unification.state.mgus unification.state))
  t)


;-------------------------------------------------------------------------------------
;--------------------------------- MGU OPERATIONS ------------------------------------

(defun uds-mgu.print(mgu)
  (format t "~%")
  (subst-print (uds-mgu.substitution mgu))
  (mapc #'uds-epsilon.literal.print (uds-mgu.residuum.literals mgu)))

;-------------------------------------------------------------------------------------
;----------------------------------- UTILITY FUNCTIONS -------------------------------

(defun uds-variable.sort.node (variable)
  (uds-sort.get.sort.node (dt-variable.sort variable)))


(defun uds-print.term (term &optional (stream t))
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
						  (uds-print.term (first rest.args))
						  (progn
						    (uds-print.term (first rest.args))
						    (format stream " "))))
					  (dt-term_arguments term))
				    (format stream ")")))))