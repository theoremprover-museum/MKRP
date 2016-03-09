;;; -*- package: MKRP; syntax: common-lisp; mode: lisp -*-

#| Copyright (C) 1991 AG Siekmann, 
                      Fachbereich Informatik, Universitaet des Saarlandes, 
                      Saarbruecken, Germany

This file is part of Markgraf Karl Refutation Procedure (MKRP).

MKRP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  

Everyone is granted permission to copy, modify and redistribute
MKRP, but only if the it is not used for military purposes or any
military research. It is also forbidden to use MKRP in nuclear plants
or nuclear research, and for verifying programs in military 
and nuclear research.  A copy of this license is
supposed to have been given to you along with MKRP so you
can know your rights and responsibilities.  
Among other things, the copyright notice
must be preserved on all copies.  |#

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(defvar red*forward nil)
(defvar red*graph.isolation t)

(DEFUN RED-REDUCE.DEDUCED.GRAPH.PARTIAL (REDUCE.CLAUSES)
						; edited: 29-jun-84 17:55:58
						; input:  'reduce.clauses' is the list of clauses to
						;         be reduced (newly inserted).
						; effect: reductions only modifying 'reduce.clauses'
						;         are executed.
						;         deduce options are used.
						; value:  ('empty' clause), if empty clause is
						;         inferred by reductions, nil else.
  (let ((red*forward t))
    (RED=CTL_REDUCE 'DEDUCED NIL NIL NIL NIL NIL NIL NIL NIL REDUCE.CLAUSES NIL (RDS-RULES 'DEDUCED.PARTIAL) NIL)))

(DEFUN RED-REDUCE.DEDUCED.GRAPH (REMOVE.LITERALS REMOVE.CLAUSES RECOLOUR.UNIFIERS REDUCE.CLAUSES REDUCE.LINKS)
						; edited: 29-jun-84 18:09:06
						; input:  'remove.literals' is a list of expressions
						;         (clause litno reason protocol_type .
						;         protocol_arguments), describing literals to
						;         be removed.
						;         'remove.clauses' is a list of dotted pairs
						;         (clause . reason), describing clauses to be
						;         removed from actual connection graph.
						;         'recolour.unifis' is a list of expressions
						;         (link unifier . reason), describing
						;         unifiers to be recoloured to removed ones
						;         (for example operation unifiers).
						;         'reduce.clauses' is the list of clauses to
						;         be reduced.
						;         'reduce.links' is the list of links to be
						;         reduced(not incident with 'reduce.clauses').
						; effect: possible reductions not done in
						;         'red-reduce.deduced.graph.partial' are
						;         executed.
						;         deduce options are used.
						; value:  ('empty' clause), if empty clause is
						;         inferred by reductions,
						;         ('failure' 'graph.collapsed'), if no clause
						;         is in the graph after reductions,
						;         nil else.
  (MAPC #'(LAMBDA (CLAUSE) (WHEN (DS-CLAUSE.IS CLAUSE)
			     (red=ctl_REMOVE.CLAUSE CLAUSE "rewrite rule" NIL)))
	RED*RW_RULES)
  (prog1 (COND
	   ((RED=CTL_REDUCE 'DEDUCED NIL REMOVE.CLAUSES REMOVE.LITERALS NIL NIL RECOLOUR.UNIFIERS NIL NIL
			    REDUCE.CLAUSES REDUCE.LINKS (RDS-RULES 'DEDUCED) (RDS-RULES 'DEDUCED.SUPPRESSED)))
	   ((NULL (CG-CLAUSES ALL)) (LIST 'FAILURE 'GRAPH.COLLAPSED)))
	 (nar-narrow (set-difference REDUCE.CLAUSES (cg-clauses removed)))))

(defun red=links (colours clauses)
  (mapcan #'(lambda (clause) (ds-clause.all.links colours clause)) clauses))

(DEFUN RED-REDUCE.GRAPH NIL
						; edited: 29-jun-84 18:24:59
						; input:  -
						; effect: the whole actual connection graph is
						;         reduced by all reduction rules.
						;         initial options are used.
						; value:  ('empty' clause), if empty clause is
						;         inferred by reductions,
						;         ('failure' 'graph.collapsed'), if no clause
						;         is in the graph after reduction,
						;         nil else.
						; remark: graph is in initial state, no links are
						;         removed, it is a total graph.
  (COND
    ((RED=CTL_REDUCE 'INITIAL NIL NIL NIL NIL NIL NIL NIL NIL (CG-CLAUSES ALL) NIL (RDS-RULES 'ALL) NIL))
    ((NULL (CG-CLAUSES ALL)) (LIST 'FAILURE 'GRAPH.COLLAPSED))))

(DEFUN RED-REDUCE.INITIAL.GRAPH.PARTIAL (REDUCE.CLAUSES REDUCE.LINKS)
						; edited: 29-jun-84 18:32:48
						; input:  'reduce.clauses' is the list of clauses to
						;         be reduced.
						;         'reduce.links' is the list of links to be
						;         reduced.
						;         all are newly inserted objects, links
						;         incident with 'reduce.clauses' may not be
						;         listed in 'reduce.links'.
						; effect: 'reduce.clauses' and 'reduce.links', as
						;         the effect of their insertion for the
						;         rest graph by reductions are perfofmed.
						;         initial options are used.
						; value:  ('empty' clause), if empty clause is
						;         inferred by reductions,
						;         else nil.
						; remark: graph is in initial state, no links are
						;         removed, it is a total graph.
  
  (mapc #'red=rw_add.completion reduce.clauses)
  (RED=CTL_REDUCE 'INITIAL NIL NIL NIL NIL NIL NIL NIL NIL REDUCE.CLAUSES REDUCE.LINKS
		  (RDS-RULES 'INITIAL.PARTIAL) NIL))

(DEFUN RED-REDUCE.RESORTED.GRAPH (REMOVE.CLAUSES REMOVE.LITERALS REMOVE.LINKS REPLACE.UNIFIERS)
						; edited: 25-sep-84 12:08:55
						; input:  'remove.clauses' is a list of dotted pairs
						;         (clause . reason) describing
						;         clauses to be removed.
						;         'remove.literals' is a list of expressions
						;         (clause litno reason protocol_type .
						;         protocol_arguments) describing literals
						;         to be removed.
						;         'remove.links' is a list of dotted pairs
						;         (link . reason) describing links to be
						;         removed.
						;         'replace.unis' is a list of expressions
						;         (link unifiers . reson ) describing links
						;         which unifiers will be replaced.
						; effect: 'remove.clauses', 'remove.literals', and
						;         'remove.links' will be removed.
						;         protocol info is given to the protocol
						;         module.
						;         snowball effect of removals for reduction
						;         is considered.
						;         initial options are used.
						; value:  ('empty' clause) if empty clause is
						;         inferred by reductions else nil.
  (RED=CTL_REDUCE 'INITIAL NIL REMOVE.CLAUSES REMOVE.LITERALS REMOVE.LINKS NIL NIL REPLACE.UNIFIERS NIL NIL
		  NIL (RDS-RULES 'ALL) NIL))

(DEFUN RED-REDUCE.INITIAL.GRAPH NIL
						; edited: 29-jun-84 17:30:42
						; input:  -
						; effect: removes rewrite rule clauses from the
						;         actual connection graph.
						;         purity and link reduction rules will be
						;         applied.
						;         isolated subgraphs are removed.
						;         initial options are used for the reductions.
						; value:  a list ('failure' 'graph.collapsed'), if
						;         all clauses are removed, else nil.
  (MAPC #'(LAMBDA (CLAUSE) (WHEN (DS-CLAUSE.IS CLAUSE)
			     (red=ctl_REMOVE.CLAUSE CLAUSE "rewrite rule" NIL)))
	RED*RW_RULES)     
  (case (opt-get.option er_compile)
    ((tree-interpreter always-tree-compile)
     (red=info_update.tree (red-info_p.link.rules (cg-clauses inserted))
			   (red-info_p.link.unfail.rules (cg-clauses inserted))
			   (cg-links '(p piw r) all)))
    (otherwise 
      (red=info_update (red-info_p.link.rules (cg-clauses inserted)) (red-info_p.link.rules t)
		       (red-info_p.link.unfail.rules (cg-clauses inserted)) (red-info_p.link.unfail.rules t)
		       (cg-links '(p piw r) all))))
  (RED=CTL_REDUCE 'INITIAL NIL NIL NIL NIL NIL NIL NIL NIL (CG-CLAUSES ALL) NIL (RDS-RULES 'INITIAL) NIL)
  (when red*graph.isolation (RED=APPLY_GRAPH.ISOLATION.TO.GRAPH))
  (prog1 (COND ((NULL (CG-CLAUSES ALL))
		(LIST 'FAILURE 'GRAPH.COLLAPSED)))
	 (nar-narrow (cg-clauses all))))

(DEFUN RED-REDUCE.RULE.GRAPH (REMOVE.CLAUSES RECOLOUR.UNIFIERS REDUCE.CLAUSES REDUCE.LINKS)
						; edited: 29-jun-84 17:38:07
						; input:  'remove.clauses' is a list of dotted pairs
						;         (clause . reason), describing clauses to be
						;         removed from actual connection graph.
						;         'recolour.unifis' is a list of expressions
						;         (link unifier . reason), describing unifiers
						;         to be recoloured to removed ones(for
						;         example operation unifiers).
						;         'reduce.clauses' is the list of clauses to
						;         be reduced.
						;         'reduce.links' is the list of links to be
						;         reduced(not incident with 'reduce.clauses').
						; effect: possible reductions to modify or remove
						;         'reduce.clauses', incident links, and
						;         'reduce.links' are performed.
						;         options are switched on, recheck off.
						;         a total graph is presupposed (all possibles
						;         links exist).
						; value:  ('empty' clause), if the empty clause is
						;         inferred by reductions, nil else.
  (RED=CTL_REDUCE 'NEUTRAL NIL REMOVE.CLAUSES NIL NIL NIL RECOLOUR.UNIFIERS NIL NIL REDUCE.CLAUSES
		  REDUCE.LINKS (RDS-RULES 'RULE) NIL))



(DEFUN RED-CLAUSE.SUBSUMPTION NIL
						; edited:  2-aug-84 16:13:42
						; input:  clause subsumption is performed for the
						;         actual connection graph.
						;         option is switched on t (no check of link
						;         condition).
						;         a total graph is presupposed (all possible
						;         links exist).
						; value:  nil.
  (RED=CTL_REDUCE 'NEUTRAL NIL NIL NIL NIL NIL NIL NIL NIL (CG-CLAUSES ALL) NIL (RDS-RULES 'SUBSUMPTION) NIL))

(DEFUN RED-REMOVE.CLAUSE (CLAUSE &OPTIONAL PREDICATE.UPDATE.FLAG REASON INFO)
						; edited:  2-aug-84 16:15:30
						; input:  same as 'cg-remove.clause'.
						; effect: same as 'cg-remove.clause'.
						;         recheck agendas are filled with removed
						;         links according to recheck-options and
						;         useful colours.
						; value:  same as 'cg-remove.clause'.
  (DECLARE (IGNORE PREDICATE.UPDATE.FLAG))
  (red=ctl_REMOVE.CLAUSE CLAUSE REASON INFO))



(DEFMACRO RED-CLAUSE.TAUTOLOGY.IS (CLAUSE)
						; edited:  2-aug-84 16:17:39
						; input:  'clause' is a clause.
						; effect: -
						; value:  not nil iff a ti-link incident with 'clause'
						;         exists, which has the nil-unifier.
  `(RED=CT_CANDIDATE.LINKS ,CLAUSE))





(DEFUN RED-RESET (STATE &optional options.only.flag)
						; edited:  2-aug-84 16:06:22
						; input:  'state' is one of the atoms
						;         'initial': initial options,
						;         'deduced': deduced options,
						;         'neutral': options switched on.
						; effect: the initial rule components (names of apply
						;         and insert functions) are set in the
						;         rule descriptor arrays.
						;         if state is initial the recheck information
						;         lists of rules are set to nil.
						;         the initial or deduced options are set in
						;         the option cells of the rule descriptors
						;         corresponding to 'state'.
						;         in 'neutral' case the options are fixed in
						;         the program of 'red=ctl_init'.
						; value:  undefined.
  (RED=RESET STATE options.only.flag))

(DEFUN RED-SAVE (FILE)
						; edited: 10-aug-84 00:37:00
						; input:  'file' is a file open for output or nil.
						; effect: if 'file' is not nil an expression is
						;         written on the file.
						;         evaluation of it reconstructs the current
						;         state of the reduction module.
						; value:  if 'file' is nil then the expression as
						;         described above, else nil.
  (PROG
    ((SAVE.EXPRESSION
       `(PROGN (C "Save expression of reduction.")
	       (setq red*rw_rules ',red*rw_rules
		     rds*rw_assign ',rds*rw_assign
		     rds*rw_variables ',rds*rw_variables
		     red*rw_rules.completion ',red*rw_rules.completion
		     red*rw_rules.unfailing ',red*rw_rules.unfailing
		     red*graph.isolation ',red*graph.isolation)      
	       ,@(MAPCAR
		   #'(LAMBDA (RECHECK.RULE)
		       (LIST 'RDS-RULE.PUT ''RECHECK.INFO (KWOTE RECHECK.RULE)
			     (KWOTE (RDS-RULE 'RECHECK.INFO RECHECK.RULE))))
		   (RDS-RULES 'RECHECK)))))
    (COND (FILE (PRIN1 SAVE.EXPRESSION FILE)) (T (RETURN SAVE.EXPRESSION)))))



(defun red-save.reset ()
  (mapc #'(lambda (rule)
	    (when (ds-clause.lit.getprop (first rule) (rest rule) 'red*rule)
	      (setf (rds-rw_rule.application (ds-clause.lit.getprop (first rule) (rest rule) 'red*rule)) nil)))
	red*rw_rules.completion))


(DEFUN RED-END NIL
						; edited: 10-aug-84 00:40:00
						; input:  -
						; effect: -
						; value:  -
						; remark: used to be advised with the final reduction
						;         statistic prints in 'red-trace'.
  NIL)

(DEFUN RED=RESET (STATE options.only.flag)
						; edited:  2-aug-84 16:06:22
						; input:  'state' is one of the atoms
						;         'initial': initial options,
						;         'deduced': deduced options,
						;         'neutral': options switched on.
						; effect: the initial rule components (names of apply
						;         and insert functions) are set in the
						;         rule descriptor arrays.
						;         if state is initial the recheck information
						;         lists of rules are set to nil.
						;         the initial or deduced options are set in
						;         the option cells of the rule descriptors
						;         corresponding to 'state'.
						;         in 'neutral' case the options are fixed in
						;         the program of 'red=ctl_init'.
						; value:  undefined.
  (unless options.only.flag
    (RDS-BINDING.STACK.RESET)
    (RDS-MARKS.DESTROY 10)
    (setq rds*rw_variables nil)
    (setq rds*rw_assign nil)
    (SETQ RED*RW_RULES NIL)
    (SETQ RED*RW_RULES.completion NIL)
    (SETQ RED*RW_RULES.unfailing NIL)
    (setq red*info_links.changed nil)
    (SETQ RED*SERVICE_RENAME.VARIABLES NIL)
    (MAPC #'(LAMBDA (COMP)
	      (LET ((RULE (CAR COMP)))
		(RDS-RULE.PUT 'APPLY.TO.CLAUSE.FUNCTION RULE (THIRD COMP))
		(RDS-RULE.PUT 'APPLY.TO.LINK.FUNCTION RULE (FOURTH COMP))
		(RDS-RULE.PUT 'INSERT.CLAUSE.FUNCTION RULE (SECOND COMP))
		(RDS-RULE.PUT 'APPLY.TO.RECHECK.FUNCTION RULE (FIFTH COMP))
		(COND ((EQL STATE 'INITIAL) (RDS-RULE.PUT 'RECHECK.INFO RULE NIL)))))
	  (RDS-RULES 'INITIAL.RULE.COMPONENTS)))
  (CASE STATE
    (INITIAL (MAPC #'(LAMBDA (OPTION) (RED=RESET_OPTIONS STATE OPTION)) (OPT-GET.LIST.AREA.OPTIONS 'RED.I)))
    (DEDUCED (MAPC #'(LAMBDA (OPTION) (RED=RESET_OPTIONS STATE OPTION)) (OPT-GET.LIST.AREA.OPTIONS 'RED.D)))
    (NEUTRAL)
    (OTHERWISE (ERROR "red-check - illegal state in red=reset_ : ~a" STATE))))

(DEFUN RED=RESET_PUT (RULE OPTION CLAUSE.FLAG LINK.FLAG RECHECK.FLAG)
						; edited:  2-aug-84 15:33:01
						; input:  'rule' is a reduction rule descriptor in
						;         '(red=ds:rules 'all)'.
						;         'option' is a value for an option of 'rule'.
						;         by the three flags the options are selected
						;         where value 'option' will be set.
						; effect: 'option' is set into the option cells of
						;         'rule', where flag is not nil.
						;         'clause.flag' can be a natural number for
						;         a rule with more than one values for the
						;         option (forward and backward subsumption
						;         are two values). then the number 'clause.-
						;         flag' gives the number of the option.
						;         flags are so used that insertion into
						;         agendas only is possible if an
						;         apply-to-objecttype function exists.
						; value:  undefined.
  (COND
    ((INTEGERP CLAUSE.FLAG)
     (PROG ((OLD.OPT (RDS-RULE 'CONDITION.OPTION RULE)))
	   (COND
	     ((NOT OLD.OPT)
	      (SETQ OLD.OPT (RDS-RULE.PUT 'CONDITION.OPTION RULE (LIST NIL NIL)))))
	   (RPLACA (NTHCDR (1- CLAUSE.FLAG) OLD.OPT) OPTION)
	   (COND ((EQUAL OLD.OPT (LIST NIL NIL)) (RDS-RULE.PUT 'CONDITION.OPTION RULE NIL)))))
    (CLAUSE.FLAG (RDS-RULE.PUT 'CONDITION.OPTION RULE OPTION)))
  (COND (LINK.FLAG (RDS-RULE.PUT 'LINK.OPTION RULE OPTION)))
  (COND (RECHECK.FLAG (RDS-RULE.PUT 'RECHECK.OPTION RULE OPTION))))

(DEFUN RED=RESET_OPTIONS (STATE OPTION)
						; edited:  2-aug-84 15:43:18
						; input:  'state' is one of the atoms 'initial' and
						;         'deduced'.
						;         'option' is a dotted pair (option_name .
						;         value_of_the_option).
						; effect: the value of the option is set into the
						;         option cells of the rule descriptors
						;         corresponding to the option name.
						;         flags for selection of the three different
						;         cells are used:
						;         condition: control of insertion of clauses.
						;         link: control of insertion of links.
						;         recheck: control of insertion and removal
						;           of links.
						;         if condition is nil the two other cells
						;         are set to nil too.
						; value:  undefined.
						; remark: error-break if option name or status have
						;         not allowed value.
  (CASE STATE
    (INITIAL
      (CASE (CAR OPTION)
        (RED.I_CLAUSE.MULTIPLE.LITERALS (RED=RESET_PUT 'RED*CLAUSE.MULTIPLE.LITERALS (CDR OPTION) T T NIL))
        (RED.I_CLAUSE.PURITY (RED=RESET_PUT 'RED*CLAUSE.PURITY (CDR OPTION) T NIL NIL))
        (RED.I_CLAUSE.TAUTOLOGY (RED=RESET_PUT 'RED*CLAUSE.TAUTOLOGY (CDR OPTION) T T NIL))
        (RED.I_CLAUSE.TAUTOLOGY.RECHECK (RED=RESET_PUT 'RED*CLAUSE.TAUTOLOGY (CDR OPTION) NIL NIL T))
        (RED.I_CLAUSE.SUBSUMPTION (RED=RESET_PUT 'RED*CLAUSE.SUBSUMPTION.OBJECT (CDR OPTION) 1 NIL NIL)
				  (RED=RESET_PUT 'RED*CLAUSE.SUBSUMPTION.OBJECT (CDR OPTION) 2 NIL NIL)
				  (RED=RESET_PUT 'RED*CLAUSE.SUBSUMPTION.SUBJECT (CDR OPTION) T T NIL))
        (RED.I_CLAUSE.SUBSUMPTION.RECHECK
          (RED=RESET_PUT 'RED*CLAUSE.SUBSUMPTION.SUBJECT (CDR OPTION) NIL NIL T))
        (RED.I_CLAUSE.REPL.FACTORING (RED=RESET_PUT 'RED*CLAUSE.REPL.FACTORING (CDR OPTION) T T NIL))
        (RED.I_CLAUSE.REPL.FACTORING.RECHECK
          (RED=RESET_PUT 'RED*CLAUSE.REPL.FACTORING (CDR OPTION) NIL NIL T))
        (RED.I_CLAUSE.REPL.RESOLUTION
          (RED=RESET_PUT 'RED*CLAUSE.REPL.RESOLUTION.SUBJECT (CDR OPTION) T T NIL)
          (RED=RESET_PUT 'RED*CLAUSE.REPL.RESOLUTION.OBJECT (CDR OPTION) T NIL NIL))
        (RED.I_CLAUSE.REPL.RESOLUTION.RECHECK
          (RED=RESET_PUT 'RED*CLAUSE.REPL.RESOLUTION.SUBJECT (CDR OPTION) NIL NIL T))
        (RED.I_CLAUSE.REWRITING (RED=RESET_PUT 'RED*CLAUSE.REWRITING (CDR OPTION) T NIL NIL))
        (RED.I_LINK.INCOMPATIBILITY (RED=RESET_PUT 'RED*LINK.INCOMPATIBILITY (CDR OPTION) T T NIL))
        (RED.I_LINK.TAUTOLOGY (RED=RESET_PUT 'RED*LINK.TAUTOLOGY (CDR OPTION) T T NIL))
        (RED.I_LINK.TAUTOLOGY.RECHECK (RED=RESET_PUT 'RED*LINK.TAUTOLOGY (CDR OPTION) NIL NIL T))
        (RED.I_LINK.SUBSUMPTION (RED=RESET_PUT 'RED*LINK.SUBSUMPTION.SUBJECT (CDR OPTION) T T NIL)
				(RED=RESET_PUT 'RED*LINK.SUBSUMPTION.OBJECT (CDR OPTION) T T NIL))
        (RED.I_LINK.SUBSUMPTION.RECHECK
          (RED=RESET_PUT 'RED*LINK.SUBSUMPTION.SUBJECT (CDR OPTION) NIL NIL T))
        (OTHERWISE (ERROR "red-check - illegal option in red=reset_options: : ~a" (CAR OPTION)))))
    (DEDUCED
      (CASE (CAR OPTION)
        (RED.D_CLAUSE.MULTIPLE.LITERALS (RED=RESET_PUT 'RED*CLAUSE.MULTIPLE.LITERALS (CDR OPTION) T T NIL))
        (RED.D_CLAUSE.PURITY (RED=RESET_PUT 'RED*CLAUSE.PURITY (CDR OPTION) T NIL NIL))
        (RED.D_CLAUSE.TAUTOLOGY (RED=RESET_PUT 'RED*CLAUSE.TAUTOLOGY (CDR OPTION) T T NIL))
        (RED.D_CLAUSE.TAUTOLOGY.RECHECK (RED=RESET_PUT 'RED*CLAUSE.TAUTOLOGY (CDR OPTION) NIL NIL T))
        (RED.D_CLAUSE.SUBSUMPTION.FORWARD
          (RED=RESET_PUT 'RED*CLAUSE.SUBSUMPTION.OBJECT (CDR OPTION) 1 NIL NIL))
        (RED.D_CLAUSE.SUBSUMPTION.BACKWARD
          (RED=RESET_PUT 'RED*CLAUSE.SUBSUMPTION.OBJECT (CDR OPTION) 2 NIL NIL)
          (RED=RESET_PUT 'RED*CLAUSE.SUBSUMPTION.SUBJECT (CDR OPTION) T T NIL))
        (RED.D_CLAUSE.SUBSUMPTION.RECHECK
          (RED=RESET_PUT 'RED*CLAUSE.SUBSUMPTION.SUBJECT (CDR OPTION) NIL NIL T))
        (RED.D_CLAUSE.REPL.FACTORING (RED=RESET_PUT 'RED*CLAUSE.REPL.FACTORING (CDR OPTION) T T NIL))
        (RED.D_CLAUSE.REPL.FACTORING.RECHECK
          (RED=RESET_PUT 'RED*CLAUSE.REPL.FACTORING (CDR OPTION) NIL NIL T))
        (RED.D_CLAUSE.REPL.RESOLUTION
          (RED=RESET_PUT 'RED*CLAUSE.REPL.RESOLUTION.OBJECT (CDR OPTION) T NIL NIL)
          (RED=RESET_PUT 'RED*CLAUSE.REPL.RESOLUTION.SUBJECT (CDR OPTION) T T NIL))
        (RED.D_CLAUSE.REPL.RESOLUTION.RECHECK
          (RED=RESET_PUT 'RED*CLAUSE.REPL.RESOLUTION.SUBJECT (CDR OPTION) NIL NIL T))
	(RED.D_CLAUSE.REWRITING (RED=RESET_PUT 'RED*CLAUSE.REWRITING (CDR OPTION) T NIL NIL))
        (RED.D_LINK.INCOMPATIBILITY (RED=RESET_PUT 'RED*LINK.INCOMPATIBILITY (CDR OPTION) T T NIL))
        (RED.D_LINK.TAUTOLOGY (RED=RESET_PUT 'RED*LINK.TAUTOLOGY (CDR OPTION) T T NIL))
        (RED.D_LINK.TAUTOLOGY.RECHECK (RED=RESET_PUT 'RED*LINK.TAUTOLOGY (CDR OPTION) NIL NIL T))
        (RED.D_LINK.SUBSUMPTION (RED=RESET_PUT 'RED*LINK.SUBSUMPTION.SUBJECT (CDR OPTION) T T NIL)
				(RED=RESET_PUT 'RED*LINK.SUBSUMPTION.OBJECT (CDR OPTION) T T NIL))
        (RED.D_LINK.SUBSUMPTION.RECHECK
          (RED=RESET_PUT 'RED*LINK.SUBSUMPTION.SUBJECT (CDR OPTION) NIL NIL T))
        (OTHERWISE (ERROR "red-check - illegal option in red=reset_options: : ~a" (CAR OPTION)))))
    (OTHERWISE (ERROR "red-check - illegal state in red=reset_options: : ~a" STATE))))

(DEFVAR RED*AGENDA NIL)

(DEFVAR RED*TEST NIL)

(DEFVAR RED*CHECK T)


(DEFUN RED=CTL_REDUCE
       (STATE REMOVE.RULES REMOVE.CLAUSES REMOVE.LITERALS REMOVE.LINKS REMOVE.UNIS RECOUL.UNIS REPLACE.UNIS
	INHIBIT.UNIS REDUCE.CLAUSES REDUCE.LINKS SWITCH.ON.RULES SUPPRESSED.RULES)
						; edited: 10-jul-84 11:32:56
						; input:  'state' is one of the atoms 'initial',
						;         'deduced', and 'neutral'.
						;         'remove.rules' is a list of dotted pairs
						;         (rule_clause . reason) describing rule
						;         clauses to be removed with their induced
						;         links from the graphs.
						;         'remove.clauses' is a list of dotted pairs
						;         (clause . reason) describing clauses to be
						;         removed from connection graph.
						;         'remove.literals' is a list of expressions
						;         (clause litno reason protocol_type .
						;         protocol_arguments) describing literals to
						;         be removed.
						;         'remove.links' is a list of dotted pairs
						;         (link . reason) describing links to be
						;         removed.
						;         'remove.unis' is a list of expressions
						;         (link unifier . reason) describing unifiers
						;         to be removed.
						;         'recoul.unis' is a list of expressions
						;         (link unifier . reason) describing unifiers
						;         to be recoloured.
						;         'replace.unis' is a list of expressions
						;         (link unifiers . reason) describing links
						;         which unifiers will be replaced.
						;         'inhibit.unis' is a list of expressions
						;         (link unifier . reason) describing
						;         unifiers to be inhibited.
						;         'reduce.clauses' is the list of newly
						;         inserted clauses to be reduced.
						;         'reduce.links' is the list of newly inserted
						;         links to be reduced (not incident with
						;         'remove.clauses').
						;         'switch.on.rules' and 'suppressed.rules'
						;         are lists of reduction rule descriptors
						;         (subsets of '(rds-rules 'all)').
						; effect: if 'predicate.update.flag' is not nil the
						;         predicate occurrence lists are updated for
						;         literal and clause removals.
						;         'state' selects the used options.
						;         'initial' for initial options if 'red-reset'
						;         is called for this state, 'deduced' for
						;         deduce options if 'red-reset' is called
						;         for this state, 'neutral' for options
						;         switched on (recheck off).
						;         'remove.clauses', 'remove.literals', and
						;         'recoul.unis' will be removed. effects of
						;         removals for reductions are considered.
						;         'inhibit.unis' will be inhibited.
						;         effects of insertions of 'reduce.clauses'
						;         and 'reduce.links' for reduction is
						;         considered.
						;         'switch.on.rules' is the subset of
						;         '(rds-rules 'all)' to be switched on.
						;         for 'reduce.clauses' and 'reduce.links'
						;         only 'switch.on.rules' without
						;         'suppressed.rules' are used for reductions.
						;         if removal by reductions are made all
						;         'switch.on.rules' are applied to modified
						;         situations.
						; value:  ('empty' clause) if the empty clause is
						;         inferred by reductions , else nil.
  (COND
    ((OR REDUCE.CLAUSES REMOVE.LITERALS RECOUL.UNIS INHIBIT.UNIS REMOVE.CLAUSES REDUCE.LINKS REMOVE.RULES
	 REPLACE.UNIS REMOVE.LINKS REMOVE.UNIS)
     (RED.SERVICE-RENAME.POINT)
     (RED=CTL_INIT SWITCH.ON.RULES STATE)
     (when (and (eq red*state 'deduced) (not red*forward))
       (mapc #'red=rw_add.completion reduce.clauses)  
       (case (opt-get.option er_compile)
	 ((tree-interpreter always-tree-compile)
	  (red=info_update.tree (red-info_p.link.rules reduce.clauses)
				(red-info_p.link.unfail.rules reduce.clauses)
				(red=links '(p piw r) reduce.clauses)))
	 (otherwise 
	   (red=info_update (red-info_p.link.rules reduce.clauses) (red-info_p.link.rules t)
			    (red-info_p.link.unfail.rules reduce.clauses) (red-info_p.link.unfail.rules t)
			    (red=links '(p piw r) reduce.clauses)))))
     (MAPC #'(LAMBDA (RECOLOUR.UNI)
	       (RED=CTL_AGENDA.UPDATE
		 (RED=CTL_REMOVE.UNIFIER (CAR RECOLOUR.UNI) (SECOND RECOLOUR.UNI) t (CDDR RECOLOUR.UNI) NIL) NIL NIL))
	   RECOUL.UNIS)
     (MAPC #'(LAMBDA (REMOVE.CLAUSE)
	       (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.CLAUSE (CAR REMOVE.CLAUSE) (CDR REMOVE.CLAUSE) NIL) NIL NIL))
	   REMOVE.CLAUSES)
     (MAPC #'(LAMBDA (REMOVE.LIT)
	       (RED=CTL_AGENDA.UPDATE
		 (RED=CTL_REMOVE.LITERAL (CAR REMOVE.LIT) (SECOND REMOVE.LIT) NIL (THIRD REMOVE.LIT) NIL) (CAR REMOVE.LIT)
		 (RDS-RULES 'SUCCESSOR.LITERAL.REMOVAL))
	       (PR-OPERATION (FOURTH REMOVE.LIT) (CDDDDR REMOVE.LIT)))
	   REMOVE.LITERALS)
     (MAPC #'(LAMBDA (REMOVE.LINK)
	       (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.LINK (CAR REMOVE.LINK) (CDR REMOVE.LINK) NIL) NIL NIL))
	   REMOVE.LINKS)
     (MAPC #'(LAMBDA (REMOVE.UNI)
	       (RED=CTL_AGENDA.UPDATE
		 (RED=CTL_REMOVE.UNIFIER (CAR REMOVE.UNI) (SECOND REMOVE.UNI) NIL (CDDR REMOVE.UNI) NIL) NIL NIL))
	   REMOVE.UNIS)
     (MAPC #'(LAMBDA (INHIBIT.UNI)
	       (CG-INHIBIT.UNIFIER (CAR INHIBIT.UNI) (SECOND INHIBIT.UNI) (CDDR INHIBIT.UNI) NIL))
	   INHIBIT.UNIS)
     (PROG
       ((CLAUSE.RULES
	  (COND (SUPPRESSED.RULES (SET-DIFFERENCE (RDS-RULES.SWITCHED.ON) SUPPRESSED.RULES))
		(T (RDS-RULES.SWITCHED.ON)))))
       (MAPC #'(LAMBDA (REDUCE.CLAUSE)
		 (RED=CTL_AGENDA.UPDATE NIL REDUCE.CLAUSE CLAUSE.RULES))
	     REDUCE.CLAUSES))
     (MAPC #'(LAMBDA (REDUCE.LINK)
	       (MAPC #'(LAMBDA (RULE) (RED=CTL_AGENDA.INSERT.LINK RULE REDUCE.LINK)) (RDS-RULES.SWITCHED.ON)))
	   REDUCE.LINKS)
     (PROG1 (RED=CTL_APPLY.RULES)
	    (RED.SERVICE-RENAME.DELETE)
	    (RED=CTL_UPDATE.RECHECK.INFO)))))

(DEFUN RED=CTL_APPLY.RULES NIL
						; edited:  2-aug-84 15:12:41
						; input:  -
						; effect: the rules with descriptors in
						;         'red*rules.switched.on' are applied to the
						;         entries in the three agendas of each rule,
						;         considering the snowball effect: new entries
						;         in the agendas due to application of
						;         rules.
						; value:  list ('empty' clause) if the empty clause
						;         is inferred by a reduction rule, nil else.
						; remark: control of reduction by three agendas:
						;         clause agenda: insertion of newly generated
						;           clauses, clauses where incident links are
						;           removed, and changed clauses.
						;         link agenda: insertion of newly generated
						;           links, where incident clauses are not new.
						;         recheck agenda: insertion of removed links
						;           with colour so that link condition can be
						;           fulfilled after removal for objects in
						;           the recheck information lists.
						;         for each agenda there exists a control field
						;         in the rule descriptor containing an option
						;         value.
						;         recheck information list is a component
						;         of rule descriptors for rules with link
						;         condition. if it is not fulfilled, the
						;         objects attached are inserted into this
						;         lists.
  (COND
    (RED*CHECK
     (LET ((RULE.AND.CLAUSE NIL)
	   (RULE.AND.RECHECK NIL)
	   (RULE.AND.LINK NIL)
	   (EMPTY NIL)
	   (RESULT NIL))
       (WHILE (NOT EMPTY)
						; invariant: empty clause is not inferred and last
						; pop from agendas has got an object.
	 (COND
	   (RULE.AND.CLAUSE
						; get next object and rule.
	    (SETQ RESULT (RED=CTL_APPLY.RULE.TO.CLAUSE (CAR RULE.AND.CLAUSE) (CDR RULE.AND.CLAUSE))))
	   (RULE.AND.LINK
	    (SETQ RESULT (RED=CTL_APPLY.RULE.TO.LINK (CAR RULE.AND.LINK) (CDR RULE.AND.LINK))))
	   (RULE.AND.RECHECK
	    (SETQ RESULT (RED=CTL_APPLY.RULE.TO.RECHECK (CAR RULE.AND.RECHECK) (CDR RULE.AND.RECHECK)))))
	 (WHEN (OR (NOT (OR (SETQ RULE.AND.CLAUSE (RED=CTL_AGENDA.POP.RULE.AND.CLAUSE))
			    (SETQ RULE.AND.LINK (RED=CTL_AGENDA.POP.RULE.AND.LINK))
			    (SETQ RULE.AND.RECHECK (RED=CTL_AGENDA.POP.RULE.AND.RECHECK))))
		   RESULT)
	   (SETQ EMPTY T)))
       RESULT))
    (RED*TEST
     (LET ((RULE.AND.CLAUSE NIL) (EMPTY NIL) (RESULT NIL))
       (WHILE (NOT EMPTY)
	 (COND (RULE.AND.CLAUSE (SETQ RESULT (FUNCALL (SECOND RULE.AND.CLAUSE) (CDDR RULE.AND.CLAUSE)))))
	 (COND
	   ((OR (NOT (OR (SETQ RULE.AND.CLAUSE (RED=CTL_AGENDA.POP.RULE.AND.CLAUSE)))) RESULT) (SETQ EMPTY T))))
       RESULT))
    (T
     (LET ((RESULT NIL))
       (WHILE (AND RED*AGENDA (NOT RESULT))
	 (SETQ RESULT (FUNCALL (CAAR RED*AGENDA) (CDAR RED*AGENDA)))
	 (SETQ RED*AGENDA (CDR RED*AGENDA)))
       RESULT))))

(defvar red*state nil)

(DEFUN RED=CTL_INIT (RULES.ON STATE)
						; edited: 29-jun-84 18:40:25
						; input:  'rules.on' is a list of reduction rules
						;         (subset of '(rds-rules 'all)').
						;         'state' is one of the atoms 'initial',
						;         'neutral', and 'deduced'.
						; effect: if 'state' is 'initial' or 'deduced' the
						;         option values stored by 'red-reset' in the
						;         option cells of rule descriptors are moved
						;         in the option control cells of rule
						;         descriptors.
						;         if 'state' is 'neutral', this control cells
						;         are set to t, for application to clauses
						;         and links, but to nil for recheck (no link
						;         condition is checked).
						;         the control list 'red*rules.switched.on'
						;         will become the list of all rules in
						;         'rules.on' in the order of '(rds-rules
						;         'all)'.
						;         the agendas of the rules switched on are set
						;         to nil.
						; value:  undefined.
  (PROG ((RULES (LIST NIL)))
	(setq red*state state)
	(MAPC #'(LAMBDA (RULE)
		  (COND
		    ((AND (MEMBER RULE RULES.ON) (OR (EQL STATE 'NEUTRAL) (RDS-RULE 'CONDITION.OPTION RULE)))
		     (QCONC1 RULES RULE)
		     (CASE STATE
		       ((DEDUCED INITIAL)
			(when (RDS-RULE.PUT 'CONDITION RULE (RDS-RULE 'CONDITION.OPTION RULE))
			  (RDS-RULE.PUT 'RECHECK RULE (RDS-RULE 'RECHECK.OPTION RULE))
			  (RDS-RULE.PUT 'LINK RULE (RDS-RULE 'LINK.OPTION RULE))))
		       (NEUTRAL (RDS-RULE.PUT 'CONDITION RULE T)
				(RDS-RULE.PUT 'RECHECK RULE NIL)
				(RDS-RULE.PUT 'LINK RULE T))
		       (OTHERWISE NIL))
		     (RDS-RULE.PUT 'CLAUSE.AGENDA RULE NIL)
		     (RDS-RULE.PUT 'LINK.AGENDA RULE NIL)
		     (RDS-RULE.PUT 'RECHECK.AGENDA RULE NIL))
		    (T (RDS-RULE.PUT 'CONDITION RULE NIL)
		       (RDS-RULE.PUT 'RECHECK RULE NIL)
		       (RDS-RULE.PUT 'LINK RULE NIL))))
	      (RDS-RULES 'ALL))
	(when (EQL STATE 'NEUTRAL)
	  (RDS-RULE.PUT 'CONDITION 'RED*CLAUSE.SUBSUMPTION.OBJECT (LIST T T)))
	(RDS-RULES.SWITCHED.ON.PUT (CAR RULES))))

(DEFUN RED=CTL_AGENDA.INSERT.CLAUSE (RULE CLAUSE)
						; edited: 27-jul-84 05:42:53
						; input:  'rule' is a reduction rule descriptor
						;         (member of '(rds-rules 'all)').
						;         'clause' is a clause.
						; effect: if rule with descriptor 'rule' is switched
						;         on (option control cell for clause
						;         reduction is not nil) the clause 'clause'
						;         will be inserted into the clause agenda
						;         of the rule with its clause insert function.
						; value:  undefined.
  (COND
    ((RDS-RULE 'CONDITION RULE)
     (COND
       (RED*CHECK
	(RDS-RULE.PUT 'CLAUSE.AGENDA RULE
			 (FUNCALL (RDS-RULE 'INSERT.CLAUSE.FUNCTION RULE) CLAUSE (RDS-RULE 'CLAUSE.AGENDA RULE))))
       (RED*TEST
	(RDS-RULE.PUT 'CLAUSE.AGENDA RULE
			 (ADJOIN (CONS (RDS-RULE 'APPLY.TO.CLAUSE.FUNCTION RULE) CLAUSE)
				 (RDS-RULE 'CLAUSE.AGENDA RULE) :TEST
				 #'EQUAL)))
       (T
	(SETQ RED*AGENDA
	      (ADJOIN (CONS (RDS-RULE 'APPLY.TO.CLAUSE.FUNCTION RULE) CLAUSE) RED*AGENDA :TEST #'EQUAL)))))))

(DEFUN RED=CTL_AGENDA.INSERT.LINK (RULE LINK)
						; edited: 27-jul-84 05:42:53
						; input:  'rule' is a reduction rule descriptor
						;         (member of '(rds-rules 'all)').
						;         'link' is a link.
						; effect: if rule with descriptor 'rule' is switched
						;         on (option control cell for link
						;         reduction is not nil) the link 'link'
						;         will be inserted into the link agenda
						;         of the rule with the insert function 'ins'.
						; value:  undefined.
  (when (RDS-RULE 'LINK RULE)
    (COND (RED*CHECK (RDS-RULE.PUT 'LINK.AGENDA RULE (INS LINK (RDS-RULE 'LINK.AGENDA RULE))))
	  (RED*TEST
	   (RDS-RULE.PUT 'CLAUSE.AGENDA RULE
			    (ADJOIN (CONS (RDS-RULE 'APPLY.TO.LINK.FUNCTION RULE) LINK)
				    (RDS-RULE 'CLAUSE.AGENDA RULE))))
	  (T (SETQ RED*AGENDA
		   (ADJOIN (CONS (RDS-RULE 'APPLY.TO.LINK.FUNCTION RULE) LINK) RED*AGENDA))))))

(DEFUN RED=CTL_AGENDA.INSERT.RECHECK (RULE RECHECK)
						; edited: 27-jul-84 05:42:53
						; input:  'rule' is a reduction rule descriptor
						;         (member of '(rds-rules 'all)').
						;         'recheck' is a link removed from
						;         connection graph but still in memory.
						; effect: if rule with descriptor 'rule' is switched
						;         on (option control cell for recheck
						;         reduction is t) the link 'recheck'
						;         will be inserted into the recheck agenda
						;         of the rule with the insert function 'ins'.
						; value:  undefined.
  (COND
    ((EQL T (RDS-RULE 'RECHECK RULE))
     (COND
       (RED*TEST
	(RDS-RULE.PUT 'CLAUSE.AGENDA RULE
			 (ADJOIN (CONS (RDS-RULE 'APPLY.TO.RECHECK.FUNCTION RULE) RECHECK) (RDS-RULE 'CLAUSE.AGENDA RULE)
				 :TEST #'EQUAL)))
       (RED*CHECK (RDS-RULE.PUT 'RECHECK.AGENDA RULE (INS RECHECK (RDS-RULE 'RECHECK.AGENDA RULE))))
       (T
	(SETQ RED*AGENDA
	      (ADJOIN (CONS (RDS-RULE 'APPLY.TO.RECHECK.FUNCTION RULE) RECHECK) RED*AGENDA :TEST #'EQUAL)))))))

(DEFUN RED=CTL_AGENDA.POP.RULE.AND.CLAUSE NIL
						; edited: 27-jul-84 05:51:31
						; input:  -
						; effect: in the clause agenda of the first rule in
						;         in the order of the descriptors in
						;         'red*rules.switched.on' with not empty
						;         clause agenda, the first clause in this
						;         agenda will be removed, if it exists.
						; value:  a dotted pair (rule_descriptor . clause)
						;         with descriptor and clause as described
						;         above, if they exist, else nil.
  (let (RULE.AND.CLAUSE AGENDA)
    (MEMBER-IF #'(LAMBDA (RULE)
		   (SETQ AGENDA (RDS-RULE 'CLAUSE.AGENDA RULE))
		   (when AGENDA
		     (RDS-RULE.PUT 'CLAUSE.AGENDA RULE (CDR AGENDA))
		     (SETQ RULE.AND.CLAUSE (CONS RULE (CAR AGENDA)))))
	       (RDS-RULES.SWITCHED.ON))
    RULE.AND.CLAUSE))

(DEFUN RED=CTL_AGENDA.POP.RULE.AND.LINK NIL
						; edited: 27-jul-84 05:51:31
						; input:  -
						; effect: in the link agenda of the first rule
						;         in the order of the descriptors in
						;         'red*rules.switched.on' with not empty
						;         link agenda, the first link in this
						;         agenda will be removed, if it exists.
						; value:  a dotted pair (rule_descriptor . link)
						;         with descriptor and link as described
						;         above, if they exist, else nil.
  (let (RULE.AND.LINK AGENDA)
    (MEMBER-IF #'(LAMBDA (RULE)
		   (SETQ AGENDA (RDS-RULE 'LINK.AGENDA RULE))
		   (COND (AGENDA (RDS-RULE.PUT 'LINK.AGENDA RULE (CDR AGENDA))
				 (SETQ RULE.AND.LINK (CONS RULE (CAR AGENDA))))))
	       (RDS-RULES.SWITCHED.ON))
    RULE.AND.LINK))

(DEFUN RED=CTL_AGENDA.POP.RULE.AND.RECHECK NIL
						; edited: 27-jul-84 05:59:49
						; input:  -
						; effect: in the recheck agenda of the first rule
						;         in the order of the descriptors in
						;         'red*rules.switched.on' with not empty
						;         recheck agenda, the first link in this
						;         agenda will be removed, if it exists.
						; value:  a dotted pair (rule_descriptor . link)
						;         with descriptor and link as described
						;         above, if they exist, else nil.
  (PROG (RULE.AND.RECHECK AGENDA)
	(MEMBER-IF
	  #'(LAMBDA (RULE) (SETQ AGENDA (RDS-RULE 'RECHECK.AGENDA RULE))
		    (COND
		      (AGENDA (RDS-RULE.PUT 'RECHECK.AGENDA RULE (CDR AGENDA))
			      (SETQ RULE.AND.RECHECK (CONS RULE (CAR AGENDA))))))
	  (RDS-RULES.SWITCHED.ON))
	(RETURN RULE.AND.RECHECK)))

(DEFUN RED=CTL_AGENDA.UPDATE (INCIDENT.CLAUSES REDUCE.CLAUSE REDUCE.RULES)
						; edited: 10-jul-84 13:10:27
						; input:  'incident.clauses' is a list of clauses.
						;         'reduce.clause' is a clause or nil.
						;         'reduce.rules' is a list of reduction
						;         rule descriptors (subset of
						;         '(rds-rules 'all)').
						; effect: implementation of the snowball effect
						;         without recheck of link conditions.
						;         'reduce.rules' will be applied to modified
						;         clause 'reduce.clause'.
						;         the several 'reduce.rules' sets can be
						;         found in the selectq of 'rds-rules'
						;         with the prefix 'successor'.
						;         'incident.clauses' are clauses incident
						;         with a removed r- or p-link.
						;         'successor.standard' rules are applied to
						;         them.
						; value:  undefined.
  (let ((RULES.CLAUSES (RDS-RULES 'SUCCESSOR.STANDARD)))
    (when REDUCE.CLAUSE
      (MAPC #'(LAMBDA (S.RULE) (RED=CTL_AGENDA.INSERT.CLAUSE S.RULE REDUCE.CLAUSE)) REDUCE.RULES))
    (MAPC #'(LAMBDA (S.RULE)
	      (MAPC #'(LAMBDA (REDUCE.CLAUSE) (RED=CTL_AGENDA.INSERT.CLAUSE S.RULE REDUCE.CLAUSE)) INCIDENT.CLAUSES))
	  RULES.CLAUSES)))

(DEFUN RED=CTL_AGENDAs.insert.links (links)
						; Edited:  28-AUG-1990 19:35
						; Authors: PRCKLN
						; Input:   
						; Effect:  
						; Value:   
  (MAPC #'(LAMBDA (RULE)
	    (MAPC #'(LAMBDA (link) (RED=CTL_AGENDA.INSERT.link RULE link)) links))
	'(red*link.subsumption.object red*link.tautology)))

(DEFUN RED=CTL_APPLY.RULE.TO.RECHECK (RULE RECHECK)
						; edited: 27-jul-84 04:22:54
						; input:  'rule' is a reduction rule descriptor
						;         (member of '(rds-rules 'all)').
						;         'recheck' is a link, removed from
						;         connection graph but not from memory.
						; effect: the apply to recheck function of rule
						;         with descriptor 'rule' will be applied to
						;         'recheck'.
						; value:  nil.
  (FUNCALL (OR (RDS-RULE 'APPLY.TO.RECHECK.FUNCTION RULE) #'IGNORE) RECHECK))

(DEFUN RED=CTL_APPLY.RULE.TO.CLAUSE (RULE CLAUSE)
						; edited: 27-jul-84 04:41:29
						; input:  'rule' is a reduction rule descriptor
						;         (member of '(rds-rules 'all)').
						;         'clause' is a clause.
						; effect: if clause 'clause' not has been removed
						;         from connection graph, the apply to
						;         clause function of rule with descriptor
						;         'rule' will be applied to clause 'clause'.
						; value:  list ('empty' clause) if the empty clause
						;         is inferred by literal removals, nil else.
  (UNLESS (MEMBER CLAUSE (CG-CLAUSES REMOVED))
    (FUNCALL (OR (RDS-RULE 'APPLY.TO.CLAUSE.FUNCTION RULE) #'IGNORE) CLAUSE)))

(DEFUN RED=CTL_APPLY.RULE.TO.LINK (RULE LINK)
						; edited: 27-jul-84 04:50:28
						; input:  'rule' is a reduction rule descriptor
						;         (member of '(rds-rules 'all)').
						;         'link' is a link.
						; effect: if link 'link' not has been removed
						;         from connection graph, the apply to
						;         link function of rule with descriptor
						;         'rule' will be applied to link 'link'.
						; value:  list ('empty' clause) if the empty clause
						;         is inferred by literal removals, nil else.
  (unless (MEMBER LINK (CG-LINKS (DS-LINK.COLOUR LINK) REMOVED))
     (FUNCALL (OR (RDS-RULE 'APPLY.TO.LINK.FUNCTION RULE) 'NIL) LINK))) 


(DEFUN RED=CTL_REMOVE.CLAUSE (CLAUSE REASON INFO)
						; edited: 27-jul-84 05:12:12
						; input:  same as 'cg-remove.clause' but no
						;         'predicate.update.flag'. for it the common
						;         variable is used.
						; effect: same as 'cg-remove.clause' and link
						;         condition relevant links incident with
						;         clause 'clause' will be inserted into
						;         recheck agendas for rules with link
						;         condition, if recheck option is switched on
						;         total.
						; value:  same as 'cg-remove.clause'.
  (MAPC #'(LAMBDA (RECHECK.RULE)
	    (COND
	      ((EQL T (RDS-RULE 'RECHECK RECHECK.RULE))
	       (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
		 (MAPC #'(LAMBDA (RECHECK.LINKS)
			   (MAPC #'(LAMBDA (RECHECK.LINK) (RED=CTL_AGENDA.INSERT.RECHECK RECHECK.RULE RECHECK.LINK))
				 RECHECK.LINKS))
		       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RECHECK.CLAUSE) CLAUSE (1+ RPTN)))))))
	(RDS-RULES 'RECHECK))
  (when (eq (ds-clause.parents clause) 'theorem) (setq red*graph.isolation nil))
  (case (opt-get.option er_compile)
    ((tree-interpreter always-tree-compile)
     (ds-clause.do #'(lambda (litno)
		       (when (ds-clause.lit.getprop clause litno 'red*rule)
			 (red=rw_c.remove (ds-clause.lit.getprop clause litno 'red*rule))))
		   clause))
    (otherwise (setq red*rw_rules.completion (delete clause red*rw_rules.completion :key #'first))))
  (setq red*rw_rules (delete clause red*rw_rules))
  (setq red*rw_rules.unfailing (delete clause red*rw_rules.unfailing :key #'first))
  (CG-REMOVE.CLAUSE CLAUSE REASON INFO))

(DEFUN RED=CTL_REMOVE.LITERAL (CLAUSE LITNO SIBLINGLITNO REASON INFO)
						      ; edited: 27-jul-84 05:12:12
						      ; input:  same as 'cg-remove.literal' but no
						      ;         'predicate.update.flag'. for it the common
						      ;         variable is used.
						      ; effect: same as 'cg-remove.literal' and link
						      ;         condition relevant links incident with
						      ;         literal to be removed will
						      ;         be inserted into
						      ;         recheck agendas for rules with link
						      ;         condition, if recheck option is switched on
						      ;         total.
						      ; value:  same as 'cg-remove.literal'.
  (declare (special red*insert.application))
  (MAPC #'(LAMBDA (RECHECK.RULE)
	    (when (EQL T (RDS-RULE 'RECHECK RECHECK.RULE))
	      (MAPC #'(LAMBDA (RECHECK.LINKS)
			(MAPC #'(LAMBDA (RECHECK.LINK) (RED=CTL_AGENDA.INSERT.RECHECK RECHECK.RULE RECHECK.LINK))
			      RECHECK.LINKS))
		    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RECHECK.CLAUSE) CLAUSE LITNO))))
	(RDS-RULES 'RECHECK))
  (setq red*insert.application t)
  (ds-clause.do #'(lambda (litno) 
		    (cond ((ds-clause.lit.rewrite.rule clause litno)
			   (case (opt-get.option er_compile)
			     (tree-interpreter (red=rw_c.remove (ds-clause.lit.getprop clause litno 'red*rule)))
			     (always-tree-compile (red=rw_c.c.remove (ds-clause.lit.getprop clause litno 'red*rule)))
			     (otherwise (setq red*rw_rules.completion (delete clause red*rw_rules.completion :key #'first)))))
			  ((ds-clause.lit.is.unfailing clause litno)
			   (setq red*rw_rules.unfailing (delete clause red*rw_rules.unfailing :key #'first)))))
		clause)
  (let* ((max (ds-clause.lit.is.max clause litno))
	 (res (CG-REMOVE.LITERAL CLAUSE LITNO SIBLINGLITNO REASON INFO)))
    (when (and (opt-is.kz.completion) (> (ds-clause.nolit clause) 0))
      (when max (cons-construct.links (list clause) nil))
      (red=rw_add.completion clause)
      (mapc #'(lambda (link) (red=info_link.result.replace link (op-paramodulate link (first (ds-link.unifiers link)) nil t) t))
	    (ds-clause.all.links 'p clause))
      (mapc #'(lambda (link) (red=info_link.result.replace link (op-paramodulate link (first (ds-link.unifiers link)) nil t) t))
	    (ds-clause.all.links 'piw clause))
      (mapc #'(lambda (link) (red=info_link.result.replace link (op-resolve link (first (ds-link.unifiers link)) nil t) t))
	    (ds-clause.all.links 'r clause))
      (case (opt-get.option er_compile)
	((tree-interpreter always-tree-compile)
	 (red=info_update.tree (red-info_p.link.rules (list clause))
			       (red-info_p.link.unfail.rules (list clause))
			       nil))
	(otherwise 
	  (red=info_update (red-info_p.link.rules (list clause)) (red-info_p.link.rules t)
			   (red-info_p.link.unfail.rules (list clause)) (red-info_p.link.unfail.rules t)
			   nil))))    
    (when (and max (not (opt-is.kz.completion)) (> (ds-clause.nolit clause) 0))
      (mapc #'(lambda (plink) (cg-remove.link plink 'PARENTLITREMOVAL (CONS CLAUSE LITNO)))
	    (ds-clause.all.links 'p clause))
      (cons-construct.links (list clause) '(p)))
    res))


(defun red=ctl_reconstruct (rules literals)
  (cons-Reconstruct.links rules literals)
  (MAPC #'(LAMBDA (CLAUSE)
	    (let ((clause (first clause)))
	      (ds-clause.do #'(lambda (litno1)
				(mapc #'(lambda (link)
					  (red=info_link.result.replace
					    link (op-paramodulate link (first (ds-link.unifiers link)) nil t) t))
				      (ds-clause.links 'p clause litno1))
				(mapc #'(lambda (link)
					  (red=info_link.result.replace
					    link (op-paramodulate link (first (ds-link.unifiers link)) nil t) t))
				      (ds-clause.links 'piw clause litno1))
				(mapc #'(lambda (link)
					  (red=info_link.result.replace
					    link (op-resolve link (first (ds-link.unifiers link)) nil t) t))
				      (ds-clause.links 'r clause litno1))
				(cond ((ds-clause.lit.rewrite.rule clause litno1)
				       (case (opt-get.option er_compile)
					 (tree-interpreter (when (ds-clause.lit.getprop clause litno1 'red*rule)
							     (red=rw_c.remove (ds-clause.lit.getprop clause litno1 'red*rule))))
					 (always-tree-compile (when (ds-clause.lit.getprop clause litno1 'red*rule)
								(red=rw_c.c.remove (ds-clause.lit.getprop clause litno1 'red*rule))))
					 (otherwise (setq red*rw_rules.completion
							  (delete clause red*rw_rules.completion :key #'first))))
				       (ds-clause.lit.putprop clause litno1 'red*rule nil))
				      ((ds-clause.lit.is.unfailing clause litno1)
				       (ds-clause.lit.putprop clause litno1 'red*unfail nil)
				       (setq red*rw_rules.unfailing (delete clause red*rw_rules.unfailing :key #'first)))))
			    clause)
	      (red=rw_add.completion clause)
	      (mapc #'(lambda (colour)
			(mapc #'(lambda (link)
				  (case (opt-get.option er_compile)
				    ((tree-interpreter always-tree-compile)
				     (red=info_update.tree (red-info_p.link.rules (list clause))
							   (red-info_p.link.unfail.rules (list clause))
							   (list link)))
				    (otherwise 
				      (red=info_update nil (red-info_p.link.rules t)
						       nil (red-info_p.link.unfail.rules t)
						       (list link)))))
			      (ds-clause.all.links colour clause)))
		    '(p piw r))))
	LITERALS))


(DEFUN RED=CTL_replace.LITERAL (CLAUSE LITNO sign predicate NEW.TERMLIST REASON INFO)
						; edited: 27-jul-84 05:12:12
						; input:  same as 'cg-remove.literal' but no
						;         'predicate.update.flag'. for it the common
						;         variable is used.
						; effect: same as 'cg-remove.literal' and link
						;         condition relevant links incident with
						;         literal to be removed will
						;         be inserted into
						;         recheck agendas for rules with link
						;         condition, if recheck option is switched on
						;         total.
						; value:  same as 'cg-replace.literal'.
  (PROG1 (CG-REPLACE.LITERAL CLAUSE LITno sign predicate NEW.TERMLIST reason info)
	 (ds-clause.put.passive.positions clause litno nil)))

(DEFUN RED=CTL_REMOVE.LINK (LINK REASON INFO)
						; edited: 27-jul-84 05:17:34
						; input:  same as 'cg-remove.link'.
						; effect: same as 'cg-remove.link' and link
						;         of the unifier is inserted into recheck
						;         agendas of rules with link condition, if
						;         it has a link condition relevant colour and
						;         recheck option is switched on total.
						; value:  same as 'cg-remove.link'.
  (MAPC #'(LAMBDA (RECHECK.RULE)
	    (COND
	      ((AND (EQL T (RDS-RULE 'RECHECK RECHECK.RULE))
		    (MEMBER (DS-LINK.COLOUR LINK) (RDS-LINK.COLOURS 'RECHECK.LINK)))
	       (RED=CTL_AGENDA.INSERT.RECHECK RECHECK.RULE LINK))))
	(RDS-RULES 'RECHECK))
  (CG-REMOVE.LINK LINK REASON INFO))

(DEFUN RED=CTL_REMOVE.UNIFIER (LINK UNIFIER &OPTIONAL RECOLOUR.FLAG REASON INFO)
						; edited: 27-jul-84 05:17:34
						; input:  same as 'cg-remove.unifier'.
						; effect: same as 'cg-remove.unifier' and link
						;         of the unifier is inserted into recheck
						;         agendas of rules with link condition, if
						;         it has a link condition relevant colour,
						;         recheck option is switched on total.
						; value:  same as 'cg-remove.unifier'.
  (when (ds-link.is link)
    (MAPC #'(LAMBDA (RECHECK.RULE)
	      (COND
		((AND (EQL T (RDS-RULE 'RECHECK RECHECK.RULE))
		      (MEMBER (DS-LINK.COLOUR LINK) (RDS-LINK.COLOURS 'RECHECK.LINK)))
		 (RED=CTL_AGENDA.INSERT.RECHECK RECHECK.RULE LINK))))
	  (RDS-RULES 'RECHECK))
    (CG-REMOVE.UNIFIER LINK UNIFIER RECOLOUR.FLAG REASON INFO)))


(DEFUN RED=CTL_UPDATE.RECHECK.INFO NIL
						; edited: 10-jul-84 12:56:34
						; input:  -
						; effect: recheck information lists of rules with
						;         link condition check are updated: entries
						;         of clauses or links, which are not in the
						;         connection graph or in the memory will be
						;         removed.
						; value:  undefined.
  (RED=CT_UPDATE.RECHECK.INFO)
  (RED=CS_UPDATE.RECHECK.INFO)
  (RED=LT_UPDATE.RECHECK.INFO)
  (RED=LS_UPDATE.RECHECK.INFO))

;;; Handling of information stored in the links
;;; -------------------------------------------

(defun red=info_rec.p.link.literals (lits negpar rules unfail.rules depth)
						; Edited:  24-AUG-1990 18:42
						; Authors: PRCKLN
						; Input:   LITS is the result list f literals for LINK.
						;          NEGPAR is the negative parent of link if it is a demodulating P-Link.
						;          RULES is a list of RED=RW_RULEs.
						;          UNFAIL.RULES is a list of undirectable equations.
						;          DEPTH is the allowed term depth.
						; Effect:  Applies one of RULES and UNFAIL.RULES at one position in LITS.
						;          Updates PROTOCOL info of LINK if replacement took place.
						; Value:   True iff replacement took place.
  (if lits
      (or (red=info_rec.p.link.list (ds-lit.termlist (first lits)) negpar rules unfail.rules depth)
	  (red=info_rec.p.link.literals (rest lits) negpar rules unfail.rules depth))
      nil))

(defun red=info_rec.p.link.list (termlist negpar rules unfail.rules depth)
						; Edited:  10-JUN-1990 16:46
						; Authors: PRCKLN
						; Input:   A list of terms, a clause, a list of rules to be applied to
						;          TERMLIST, a list of not orientable equations, that also
						;          must be applied to TERMLIST and a positive integer DEPTH
						; Effect:  All rules und unfailing equations are applied to TERMLIST
						;          until no further application is possible.
						; Value:   True iff TERMLIST has changed.
  (let ((changed nil)
	(change t))
    (while change
      (setq change (or (some #'(lambda (rule)
				 (unless (eql negpar (rds-rw_rule.clause rule))
				   (red=rw_apply.to.termlist rule termlist nil nil depth)))
			     rules)
		       (some #'(lambda (rule)
				 (unless (eql negpar (rds-rw_rule.clause rule))
				   (red=rw_apply.to.termlist rule termlist t nil depth)))
			     unfail.rules))
	    changed (or changed change)))
    changed))

(defun red-info_p.link.rules (clauses)      
  (if (eq clauses t)
      (mapcar-not-nil #'(lambda (clause) (ds-clause.lit.getprop (first clause) (rest clause) 'red*rule))
		      red*rw_rules.completion)
      (mapcan #'(lambda (clause)
		  (let ((rules nil))
		    (ds-clause.do #'(lambda (litno) (when (ds-clause.lit.getprop clause litno 'red*rule)
						      (push (ds-clause.lit.getprop clause litno 'red*rule) rules)))
				  clause)
		    rules))
	      clauses)))

(defun red-info_p.link.unfail.rules (clauses)
						; Edited:  19-JAN-1990 20:59
						; Authors: PRCKLN
						; Input:   A set of clauses
						; Effect:  -
						; Value:   The list of unfailing rules of CLAUSES
						;          in the form (left . right)
  (if (eq clauses t)
      (mapcan #'(lambda (clause) (copy-list (ds-clause.lit.getprop (first clause) (rest clause) 'red*unfail)))
	      red*rw_rules.unfailing)
      (mapcan #'(lambda (clause)
		  (let ((rules nil))
		    (ds-clause.do #'(lambda (litno)
				      (when (ds-clause.lit.getprop clause litno 'red*unfail)
					(setq rules (append (ds-clause.lit.getprop clause litno 'red*unfail) rules))))
				  clause)
		    rules))
	      clauses)))

(defun red=info_rename.rule (rule)
  (rds-rw_rule.create :clause (first rule)
		      :left (uni-apply.substitution (ds-clause.renaming (rds-rw_rule.clause rule))
						    (rds-rw_rule.left rule) t)
		      :right (uni-apply.substitution (ds-clause.renaming (rds-rw_rule.clause  rule))
						     (rds-rw_rule.right rule) t)))


(defvar red*info_links.changed nil)


(defun red=info_lit.delete (lits)
						; Edited:  20-MAR-1990 21:12
						; Authors: PRCKLN
						; Input:   A list of literals and a literal 
						; Effect:  -
						; Value:   A list of literals where double and false lits are removed
  (red=info_lit.delete1 lits lits))



(defun red=info_lit.unify (tl1 tl2 vars)
  (some #'(lambda (uni)
	    (or (null uni)
		(and (not (opt-get.option sort_literals))
		     (uni-unifier.becomes.matcher uni vars t nil))))
	(uni-unify.termlists tl1 tl2)))

(defun red=info_lit.delete1 (lits all.lits)
						      ; Edited:  20-MAR-1990 21:12
						      ; Authors: PRCKLN
						      ; Input:   A list of literals and a literal 
						      ; Effect:  -
						      ; Value:   A list of literals where double and false lits are removed
  (if lits
      (let ((lit (first lits)))
	(if (or (some #'(lambda (clause)
			  (and (eql 1 (ds-clause.nolit clause))
			       (not (eq (ds-clause.sign clause 1) (ds-lit.sign lit)))
			       (eq (ds-clause.predicate clause 1) (ds-lit.predicate lit))
			       (or (red=info_lit.unify (ds-clause.termlist clause 1)
						       (ds-lit.termlist lit)
						       (ds-lits.vars (remove lit all.lits)))
				   (and (dt-predicate.is.symmetric (ds-lit.predicate lit))
					(red=info_lit.unify (ds-clause.termlist clause 1)
							    (reverse (ds-lit.termlist lit))
							    (ds-lits.vars (remove lit all.lits)))))))
		      (cg-clauses all))
		(some #'(lambda (lit2) (equal lit lit2))
		      (rest lits)))
	    (red=info_lit.delete1 (rest lits) all.lits)
	    (cons lit (red=info_lit.delete1 (rest lits) all.lits))))
      nil))


(defun red=info_link.result.replace (link res &optional variables.p)
  (mapc #'(lambda (var) (dt-variable.delete var)) (ds-link.result.variables link))
  (red=info_link.result.put link res variables.p))

(defun red=info_link.result.put (link res &optional variables.p)		
  (RED=CTL_AGENDAs.insert.links (list link))
  (when (or variables.p (null (ds-link.result link)))
    (ds-link.put.result.variables link (dt-termlist.variables (mapcan #'(lambda (lit) (copy-list (ds-lit.termlist lit))) res))))
  (ds-link.put.result link res)
  (ds-link.put.selection.info link (sel-weight res)))

(defun red=info_remove (links)
  (mapc #'(lambda (link)
	    (red=info_link.result.replace link
					  (case (ds-link.colour link)
					    (r (op-resolve link (first (ds-link.unifiers link)) nil t))
					    (p (op-paramodulate link (first (ds-link.unifiers link)) nil t)))
					  t))
	links))

(defun red=info_insert.result (links)
						; Edited:  24-AUG-1990 18:21  
						; Authors: PRCKLN
						; Input:   LINKS is a list of P, PIW, and R-links.
						; Effect:  Fills the result slot of the links.
						; Value:   Undefined.
  (mapc #'(lambda (link)
	    (let ((res (if (member (ds-link.colour link) '(p piw))
			   (op-paramodulate link (first (ds-link.unifiers link)) nil t)
			   (if (and (or (opt-is.kz.completion)
					(opt-is.with.residues)
					(eq (opt-get.option er_paramodulation) 'clause-graph))
				    (member (ds-link.colour link) '(r)))
			       (op-resolve link (first (ds-link.unifiers link)) nil t)))))
	      (when res
		(push link red*info_links.changed)
		(red=info_link.result.put link res t))))
	links))

(defun red=info_update (rules1 rules2 unfail.rules1 unfail.rules2 plinks)
						; Edited:  13-FEB-1990 22:38
						; Authors: PRCKLN
						; Input:   None
						; Effect:  The information in CG-LINK.RESULT is updated, i.e.
						;          all rewrite reduction rules are applied to the literal lists
						;          of all newly inserted links, the newly inserted reduction rules are
						;          applied to all p and piw links (if they hit, all
						;          rules must be applied to the result), and double and
						;          unit resolvable literals are removed from CG-LINK.RESULT.
						; Value:   Undefined
  
  (red=info_insert.result plinks)
  (when (opt-get.option er_cp.reduction)
    (when (or rules1 unfail.rules1)
      (unless (eq (opt-get.option er_cp.reduction) 'partial)
	(mapc #'(lambda (link)
		  (let* ((negpar (ds-link.negpar link))
			 (TERMLIST (ds-link.result link))
			 (changed.p (red=info_rec.p.link.literals termlist
								  (if (ds-link.demodulation.is link) negpar nil)
								  rules1 unfail.rules1 (opt-get.option str_term.depth))))
		    (when changed.p
		      (red=info_rec.p.link.literals termlist (if (ds-link.demodulation.is link) negpar nil)
						    rules2 unfail.rules2
						    (opt-get.option str_term.depth)))
		    (setq changed.p (or (red=info_reduce.cc link (ds-lit.termlist (first termlist))) changed.p))
		    (when (rest termlist)
		      (let ((new (red=info_lit.delete termlist))) 
			(setq changed.p (or changed.p (not (eq new termlist))))
			(setq termlist new)))
		    (when changed.p
		      (red=info_link.result.put link TERMLIST)
		      (push link red*info_links.changed))))
	      (cg-links '(p piw r) all))))
    (mapc #'(lambda (link)
	      (let* ((negpar (ds-link.negpar link))
		     (TERMLIST (ds-link.result link))
		     (changed.p (red=info_rec.p.link.literals termlist
							      (if (ds-link.demodulation.is link) negpar nil)
							      rules2 unfail.rules2
							      (opt-get.option str_term.depth))))
		(setq changed.p (or (red=info_reduce.cc link (ds-lit.termlist (first termlist))) changed.p))
		(when (rest termlist)
		  (let ((new (red=info_lit.delete termlist))) 
		    (setq changed.p (or changed.p (not (equal new termlist))))
		    (setq termlist new)))
		(when changed.p
		  (red=info_link.result.put link TERMLIST)
		  (push link red*info_links.changed))))
	  plinks)))

(defun red=info_reduce.cc (link termlist)
  (let (result)
    (when (and (eq (opt-get.option er_completion) 'constant-congruence)
	       (not (or (ord-greater (first termlist) (second termlist))
			(ord-greater (second termlist) (first termlist)))))
      (mapc #'(lambda (variable) (dt-putprop variable 'red*cc t))
	    (dt-termlist.variables termlist))
      (WHILE (if (SOME #'(LAMBDA (RULES FLAG)
			   (SOME #'(LAMBDA (RULE)
				     (WHEN (RED=RW_APPLY.TO.TERMLIST
					     RULE termlist FLAG nil nil link)
				       T))
				 RULES))
		       (list (red-info_p.link.rules t)
			     (red-info_p.link.unfail.rules t))
		       (LIST NIL T))
		 (setq result t)
		 nil))
      (mapc #'(lambda (variable) (dt-putprop variable 'red*cc nil))
	    (dt-termlist.variables termlist)))
    result))

(defun red=info_update.tree (rules1 unfail.rules1 plinks)
						      ; Edited:  13-FEB-1990 22:38
						      ; Authors: PRCKLN
						      ; Input:   Two list of rules. A list of P and PIW-links
						      ; Effect:  The information in DS-LINK.RESULT is updated, i.e.
						      ;          all rewrite reduction rules are applied to the literal lists
						      ;          of all newly inserted links, the newly inserted reduction rules are
						      ;          applied to all p and piw links (if they hit, all
						      ;          rules must be applied to the result), and double and
						      ;          unit resolvable literals are removed from DS-LINK.RESULT.
						      ; Value:   Undefined
  (red=info_insert.result plinks)
  (when (opt-get.option er_cp.reduction)
    (unless (eq (opt-get.option er_cp.reduction) 'partial)
      (when (or rules1 unfail.rules1)
	(mapc #'(lambda (link)
		  (let* ((negpar (ds-link.negpar link))
			 (TERMLIST (ds-link.result link))
			 (changed.p (red=info_rec.p.link.literals termlist
								  (if (ds-link.demodulation.is link) negpar nil)
								  rules1 unfail.rules1 (opt-get.option str_term.depth))))
		    (when changed.p (mapc #'(lambda (lit)
					      (if (eq (opt-get.option er_compile) 'always-tree-compile)
						  (red=rw_apply.to.termlist.c.c (ds-lit.termlist lit) nil nil nil)
						  (if (eq :depth
							  (red=rw_apply.to.termlist.c.i (ds-lit.termlist lit) nil nil nil nil nil nil nil
											(opt-get.option str_term.depth)))
						      t)))
					  termlist))
		    (when (rest termlist)
		      (let ((new (red=info_lit.delete termlist))) 
			(setq changed.p (or changed.p (not (eq new termlist))))
			(setq termlist new)))
		    (when changed.p
		      (red=info_link.result.put link TERMLIST)
		      (push link red*info_links.changed))))
	      (cg-links '(p piw r) all))))
    (mapc #'(lambda (link) 
	      (let* ((TERMLIST (ds-link.result link))
		     (changed.p (mapc #'(lambda (lit)
					  (if (eq (opt-get.option er_compile) 'always-tree-compile)
					      (red=rw_apply.to.termlist.c.c (ds-lit.termlist lit) nil nil nil)
					      (red=rw_apply.to.termlist.c.i (ds-lit.termlist lit) nil nil nil nil nil nil nil
									    (opt-get.option str_term.depth))))
				      termlist)))
		(when (rest termlist)
		  (let ((new (red=info_lit.delete termlist))) 
		    (setq changed.p (or changed.p (not (eq new termlist))))
		    (setq termlist new)))
		(when changed.p
		  (red=info_link.result.put link TERMLIST)
		  (push link red*info_links.changed))))
	  plinks)))

(DEFUN RED=APPLY_GRAPH.ISOLATION.TO.GRAPH NIL
						; edited: 10-jul-84 12:59:04
						; input:  -
						; effect: if there are theorems in the graph all
						;         clauses with no connection about chains
						;         of r- and p-links to a theorem will be
						;         removed.
						; value:  undefined.
  (when (and (RDS-RULE 'CONDITION 'RED*CLAUSE.purity)
	     (not (opt-is.completion)))
    (let ((THEOREMS NIL))
      (MAPC #'(LAMBDA (CLAUSE)
		(when (EQL (DS-CLAUSE.PARENTS CLAUSE) 'THEOREM)
		  (SETQ THEOREMS T)
		  (RED=GI_MARK.OBTAINABLE.CLAUSES CLAUSE)))
	    (CG-CLAUSES ALL))
      (when THEOREMS
	(MAPC #'(LAMBDA (CLAUSE)
		  (COND ((DT-GETPROP CLAUSE 'RED*GI_OBTAINABLE) (DT-REMPROP CLAUSE 'RED*GI_OBTAINABLE))
			(T (CG-REMOVE.CLAUSE CLAUSE "isolated subgraph" NIL))))
	      (CG-CLAUSES ALL))))))

(DEFUN RED=GI_MARK.OBTAINABLE.CLAUSES (CLAUSE)
						; edited: 10-jul-84 13:02:09
						; input:  a clause 'clause'.
						; effect: recursive marking of all by r- and p-links
						;         obtainable clauses.
						; value:  undefined.
  (COND
    ((NOT (DT-GETPROP CLAUSE 'RED*GI_OBTAINABLE)) (DT-PUTPROP CLAUSE 'RED*GI_OBTAINABLE CLAUSE)
     (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
       (MAPC #'(LAMBDA (OP.LINKS.EXT)
		 (MAPC #'(LAMBDA (OP.LINK.EXT)
			   (RED=GI_MARK.OBTAINABLE.CLAUSES (RED.SERVICE-OTHERPAR.EXTERNAL OP.LINK.EXT CLAUSE)))
		       OP.LINKS.EXT))
	     (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.EXTERNAL) CLAUSE (1+ RPTN)))))))

(DEFUN RED=APPLY_CLAUSE.MULTIPLE.LITERALS.TO.CLAUSE (CLAUSE)
						; edited: 27-mar-84 10:22:40
						; input:  a clause.
						; effect: multiple literals of clause 'clause' will be
						;         removed. they are detected by identical
						;         substitutions of si- and sid-links of this
						;         clause.
						;         the deleted literal was the positive parent
						;         of the link.
						;         information is given to protocol and
						;         connectiongraph module. agenda is updated.
						; value:  nil.
  (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
    (COND
      ((AND
         (MEMBER-IF
           #'(LAMBDA (NCI.LINKS)
               (MEMBER-IF #'(LAMBDA (NCI.LINK) (RED=CML_TO.DETECTION.LINK CLAUSE NCI.LINK)) NCI.LINKS))
           (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL) CLAUSE (1+ RPTN)))
         (< (1+ RPTN) (DS-CLAUSE.NOLIT CLAUSE)))
						; if a double literal is removed there can be deleted
						; more than one link from the current lists and the
						; inner 'some' can have as next one a removed link.
						; therefore it must be sure that the current literal
						; is considered a second time.
       (SETQ RPTN (1+ RPTN)))))
  NIL)

(DEFUN RED=APPLY_CLAUSE.MULTIPLE.LITERALS.TO.LINK (LINK)
						; edited: 27-mar-84 10:31:45
						; input:  a link.
						; effect: if link 'link' is a si- or sid-link with
						;         identical substitution it indicates double
						;         literals.
						;         its positive parentliteral will be removed.
						;         information is given to protocol and
						;         connectiongraph module. agenda is updated.
						; value:  nil.
  (COND
    ((MEMBER (DS-LINK.COLOUR LINK) (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL))
     (RED=CML_TO.DETECTION.LINK (DS-LINK.POSPAR LINK) LINK)))
  NIL)

(DEFUN RED=CML_TO.DETECTION.LINK (CLAUSE LINK)
						; Authors: MKRP
						; edited:  13-MAR-1992 19:32
						; input:   'link' is a si- or sid-link incident with
						;          clause 'clause'.
						; effect:  if link 'link' has the identical
						;          substitution it indicates double literals.
						;          its positive parentliteral will be removed.
						;          information is given to protocol and
						;          connectiongraph module. agenda is updated.
						; value:   not nil iff a literal has been removed.
  
  (when (MEMBER NIL (DS-LINK.UNIFIERS LINK))
    (let* ((POSLITNO (DS-LINK.POSLITNO LINK))
	   (NEGLITNO (DS-LINK.NEGLITNO LINK))
	   (pos.r (length (ds-clause.links 'r clause poslitno)))
	   (neg.r (length (ds-clause.links 'r clause neglitno)))
	   remove.litno remain.litno)
      (if (or (> neg.r pos.r) (ds-link.rule link))
	  (setq remove.litno poslitno remain.litno neglitno)
	  (setq remove.litno neglitno remain.litno poslitno))
      (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.LITERAL CLAUSE remove.LITNO remain.LITNO 'DOUBLE.LITERAL NIL) NIL NIL)
      (CG-CHANGE.QUEUE_APPEND (LIST 'DOUBLE.LITERAL (CONS (CONS CLAUSE remain.LITNO) (CONS CLAUSE remove.LITNO))))
      (PR-OPERATION 'DOUBLE.LITERAL CLAUSE remain.LITNO remove.LITNO (DS-LINK.RULE LINK))
      t)))

(DEFUN RED=APPLY_CLAUSE.PURITY.TO.CLAUSE (CLAUSE)
						; edited: 10-aug-84 00:50:00
						; input:  'clause' is a clause.
						; effect: 'clause' will be removed from connection
						;         graph if one of its literals has no r-
						;         or p-links.
						; value:  nil.
  (when (and (not (opt-is.kz.completion))
	     (or (< (length (DT-PREDICATE.POSITIVE.OCCURRENCES (first (dt-predicate.equalities)))) 2)
		 (do* ((nolit (DS=CLAUSE.GET 'NOLIT CLAUSE) (1- nolit))
		       (condition (and (not (dt-predicate.is.equality (ds-clause.predicate clause nolit)))
				       (not (if (ds-sign.is.positive (ds-clause.sign clause nolit))
						(DT-PREDICATE.negative.OCCURRENCES (ds-clause.predicate clause nolit))
						(DT-PREDICATE.POSITIVE.OCCURRENCES (ds-clause.predicate clause nolit)))))
				  (and (not (dt-predicate.is.equality (ds-clause.predicate clause nolit)))
				       (not (if (ds-sign.is.positive (ds-clause.sign clause nolit))
						(DT-PREDICATE.negative.OCCURRENCES (ds-clause.predicate clause nolit))
						(DT-PREDICATE.POSITIVE.OCCURRENCES (ds-clause.predicate clause nolit)))))))
		      ((or condition (= 1 nolit)) condition))
		 ;(eql t (RDS-RULE 'CONDITION.OPTION 'red*clause.purity))
		 )
	     (DS-CLAUSE.IS.PURE CLAUSE))
    (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.CLAUSE CLAUSE "purity" NIL) NIL NIL))
  NIL)

(DEFUN RED=APPLY_CLAUSE.TAUTOLOGY.TO.CLAUSE (CLAUSE)
						; edited: 29-may-84 00:56:21
						; input:  'clause' is a clause.
						;         same as function 'red=ct_attempt.to.remove',
						;         that is called with the option of the rule
						;         and the links, which make the tautology
						;         recognizable, i.e. ti-links with
						;         nil-unifier.
						; value:  nil.
						; remark: 'tautology' is defined in the comment of
						;         'red=ct_attempt.to.remove'.
  (RED=CT_ATTEMPT.TO.REMOVE CLAUSE (RDS-RULE 'CONDITION 'RED*CLAUSE.TAUTOLOGY)
			    (RED=CT_CANDIDATE.LINKS CLAUSE)))

(DEFUN RED=APPLY_CLAUSE.TAUTOLOGY.TO.LINK (LINK)
						; edited: 29-may-84 01:50:05
						; input:  a link 'link'.
						; effect: if colour of 'link' is a tautology
						;         recognizing colour (ti), it will be
						;         detected, wether the parent clause of 'link'
						;         is a tautology, if recheck-option is
						;         switched on partial or total.
						;         if colour is the colour of links, which are
						;         necessary to perform link condition (riw or
						;         r) and recheck-option is switched on total,
						;         the detection of a tautology, where link
						;         condition was not met up to now, is
						;         possible.
						;         link condition is described in
						;         'red=ct_link.condition'.
						;         the recognized tautologies are removed or
						;         their creator unifiers are inhibited due
						;         to link condition and option as explained
						;         in 'red=ct_attempt.to.remove'.
						; value:  nil.
						; remark: definition of 'tautology' in
						;         'red=ct_attempt.to.remove' too.
  (PROG
    ((CONDITION (RDS-RULE 'CONDITION 'RED*CLAUSE.TAUTOLOGY)) (RECHECK (RDS-RULE 'RECHECK 'RED*CLAUSE.TAUTOLOGY))
     (COLOUR (DS-LINK.COLOUR LINK)))
    (COND
      ((MEMBER COLOUR (RDS-LINK.COLOURS 'TAUTOLOGY.DETECTION.INTERNAL))
       (COND
	 ((AND RECHECK (MEMBER NIL (DS-LINK.UNIFIERS LINK)))
	  (RED=CT_ATTEMPT.TO.REMOVE (DS-LINK.POSPAR LINK) CONDITION (LIST LINK)))))
      ((AND (MEMBER COLOUR (RDS-LINK.COLOURS 'RESOLUTION.ALL)) (EQL RECHECK T))
       (PROG (POS.OTHERPARS NEG.OTHERPARS)
	     (MAPC #'(LAMBDA (R.LINKS)
		       (MAPC #'(LAMBDA (R.LINK)
				 (SETQ POS.OTHERPARS
				       (CONS (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK (DS-LINK.POSPAR LINK)) POS.OTHERPARS)))
			     R.LINKS))
		   (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'TAUTOLOGY.CONDITION.EXTERNAL) (DS-LINK.POSPAR LINK)
					     (DS-LINK.POSLITNO LINK)))
	     (MAPC #'(LAMBDA (R.LINKS)
		       (MAPC #'(LAMBDA (R.LINK)
				 (SETQ NEG.OTHERPARS
				       (CONS (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK (RED.SERVICE-LINK.NEGPAR R.LINK T))
					     NEG.OTHERPARS)))
			     R.LINKS))
		   (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'TAUTOLOGY.CONDITION.EXTERNAL)
					     (RED.SERVICE-LINK.NEGPAR LINK T) (DS-LINK.NEGLITNO LINK)))
	     (RED=CT_UPDATE.RECHECK.INFO)
	     (MAPC #'(LAMBDA (TAUTOLOGY.CLAUSE)
		       (COND
			 ((AND (MEMBER TAUTOLOGY.CLAUSE POS.OTHERPARS) (MEMBER TAUTOLOGY.CLAUSE NEG.OTHERPARS))
			  (RED=CT_ATTEMPT.TO.REMOVE TAUTOLOGY.CLAUSE CONDITION
						    (RED=CT_CANDIDATE.LINKS TAUTOLOGY.CLAUSE)))))
		   (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY)))))))

(DEFUN RED=APPLY_CLAUSE.TAUTOLOGY.TO.RECHECK (LINK)
						; edited: 29-may-84 01:20:55
						; input:  'link' is a link, removed from
						;         connectiongraph, but not deleted in memory.
						; effect: if 'link' is an external link with a colour,
						;         for which link condition is checked (only
						;         r), recheck information list is updated
						;         (removal of removed clauses) and clauses
						;         remaining in this list, incident with our
						;         link are rechecked, wether they are
						;         tautologies, complying with the link
						;         condition.
						; value:  nil.
						; remark: definition of 'tautology' in
						;         'red=ct_attempt.to.remove'.
  (RED=CT_UPDATE.RECHECK.INFO)
  (MAPC #'(LAMBDA (TAUTOLOGY.CLAUSE)
	    (COND
	      ((OR (EQL TAUTOLOGY.CLAUSE (DS-LINK.POSPAR LINK))
		   (AND (MEMBER (DS-LINK.COLOUR LINK) (RDS-LINK.COLOURS 'RESOLUTION))
			(EQL TAUTOLOGY.CLAUSE (DS-LINK.NEGPAR LINK))))
	       (RED=CT_ATTEMPT.TO.REMOVE TAUTOLOGY.CLAUSE (RDS-RULE 'CONDITION 'RED*CLAUSE.TAUTOLOGY)
					 (RED=CT_CANDIDATE.LINKS TAUTOLOGY.CLAUSE)))))
	(RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY))
  NIL)

(DEFUN RED=CT_ATTEMPT.TO.REMOVE (CLAUSE CONDITION DETECTION.LINKS)
						; edited: 29-may-84 01:03:01
						; input:  'clause' is a clause.
						;         'condition' is one of the atoms 't',
						;         'remove', 'inhibit', and 'remove-inhibit'.
						;         'detection.links' is a list of ti-links
						;         with nil-unifiers incident with 'clause'.
						; effect: if 'detection.links' is not nil, 'clause'
						;         will be removed depending on 'meaning of
						;         condition'.
						;         meaning of condition:
						;         ---------------------
						;         't': (1) is sufficient for removal.
						;         'remove': (1) and (2) must be fulfilled.
						;         'inhibit': (1) is met, creator unifier must
						;           exist, and clause is not in clauses
						;           changed (unifier will be inhibited).
						;         'remove-inhibit': (1) is fulfilled. if (2)
						;           fails, creator unifier must exist and
						;           'clause' is not changed (unifier will be
						;           inhibited).
						;         if clause is not removed, depending on one
						;         of the 4 cases, it will be inserted into
						;         recheck information list of tautology rule.
						;         meaning of tautology:
						;         ---------------------
						;         a clause c is a tautology iff
						;         (1) there exists a ti-link with identical
						;           substitution, incident with c.
						;         (2) link condition is fulfilled as described
						;           in 'red=ct_link.condition'.
						;
						;         snowball effect is considered as described
						;         in 'red=ctl_agenda.update'.
						; value:  undefined.
  (PROG (LINK.CONDITION CREATOR.UNIFIER)
	(COND
	  (DETECTION.LINKS
	   (COND
	     ((MEMBER CONDITION '(REMOVE REMOVE-INHIBIT))
	      (SETQ LINK.CONDITION
		    (MEMBER-IF
		      #'(LAMBDA (LINK)
			  (RED.LC-CLAUSE.TAUTOLOGY CLAUSE (DS-LINK.POSLITNO LINK) (DS-LINK.NEGLITNO LINK)))
		      DETECTION.LINKS))))
	   (CASE CONDITION
	     ((T) (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.CLAUSE CLAUSE 'TAUTOLOGY NIL) NIL NIL))
	     (REMOVE-INHIBIT
	       (COND
		 (LINK.CONDITION
		  (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.CLAUSE CLAUSE 'TAUTOLOGY NIL) NIL NIL))
		 ((SETQ CREATOR.UNIFIER (CG-CLAUSE_CREATOR.UNIFIER CLAUSE))
		  (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.CLAUSE CLAUSE 'TAUTOLOGY NIL) NIL NIL)
		  (CG-INHIBIT.UNIFIER (CAR CREATOR.UNIFIER) (CDR CREATOR.UNIFIER) 'TAUTOLOGY NIL))
		 (T
		  (RDS-RULE.PUT 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY
				   (INS CLAUSE (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY))))))
	     (REMOVE
	       (COND
		 (LINK.CONDITION
		  (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.CLAUSE CLAUSE 'TAUTOLOGY NIL) NIL NIL))
		 (T
		  (RDS-RULE.PUT 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY
				   (INS CLAUSE (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY))))))
	     (INHIBIT
	       (COND
		 ((SETQ CREATOR.UNIFIER (CG-CLAUSE_CREATOR.UNIFIER CLAUSE))
		  (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.CLAUSE CLAUSE 'TAUTOLOGY NIL) NIL NIL)
		  (CG-INHIBIT.UNIFIER (CAR CREATOR.UNIFIER) (CDR CREATOR.UNIFIER) 'TAUTOLOGY NIL))
		 (T
		  (RDS-RULE.PUT 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY
				   (INS CLAUSE (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY))))))
	     (OTHERWISE (ERROR "red-check - illegal condition in red=ct_attempt.to.remove: : ~a" CONDITION)))))))

(DEFUN RED=CT_CANDIDATE.LINKS (CLAUSE)
						; edited: 29-may-84 01:58:53
						; input:  'clause' is a clause.
						; effect: -
						; value:  list of all ti-links with nil-unifiers,
						;         incident with 'clause'.
  (let ((CANDIDATE.LINKS NIL))
    (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
      (MAPC #'(LAMBDA (I.LINKS)
		(MAPC #'(LAMBDA (I.LINK)
			  (COND
			    ((AND (< (RED.SERVICE-OTHERLITNO.INTERNAL I.LINK (1+ RPTN)) (1+ RPTN))
				  (RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS I.LINK)))
			     (SETQ CANDIDATE.LINKS (CONS I.LINK CANDIDATE.LINKS)))))
		      I.LINKS))
	    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'TAUTOLOGY.DETECTION.INTERNAL) CLAUSE (1+ RPTN))))
    CANDIDATE.LINKS))

(DEFUN RED=CT_UPDATE.RECHECK.INFO NIL
						; edited: 29-may-84 01:40:47
						; input:  -
						; effect: all clauses, that have been removed from
						;         connectiongraph or memory, also will be
						;         removed from the recheck information list
						;         of rule 'red*clause.tautology'.
						; value:  undefined.
  (RDS-RULE.PUT 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY
		   (DREMAP (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.TAUTOLOGY) NIL
			   #'(LAMBDA (TAUTOLOGY.CLAUSE) (SETQ TAUTOLOGY.CLAUSE (CAR TAUTOLOGY.CLAUSE))
				     (OR (NOT (DS-CLAUSE.IS TAUTOLOGY.CLAUSE)) (MEMBER TAUTOLOGY.CLAUSE (CG-CLAUSES REMOVED))))
			   NIL)))

(DEFUN RED=APPLY_CLAUSE.REPL.FACTORING.TO.CLAUSE (CLAUSE)
						; edited: 22-may-84 01:38:31
						; input:  'clause' is a clause.
						; effect: see 'red=crf_to.clause'.
						; value:  nil.
  (RED=CRF_TO.CLAUSE CLAUSE) NIL)

(DEFUN RED=APPLY_CLAUSE.REPL.FACTORING.TO.LINK (LINK)
						; edited: 22-may-84 01:45:17
						; input:  'link' is a link.
						; effect: if link has colour si or sid, it is checked,
						;         wether the factor of 'link' replacement-
						;         subsumes the parent clause.
						;         if subsumption is not possible and recheck
						;         option is switched on, the factors of all
						;         si- and sid-links are considered. the
						;         insertion of the si- or sid-link may lead
						;         to the occurrence of a double literal after
						;         factoring.
						;         if factoring has success, information is
						;         given to the protocol and connectiongraph
						;         module. snowball effect leads to insertion
						;         into agendas of other reduction rules.
						; value:  nil.
  (COND
    ((AND (MEMBER (DS-LINK.COLOUR LINK) (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL))
	  (NOT (RED=CRF_ATTEMPT.TO.FACTORIZE (DS-LINK.POSPAR LINK) LINK (DS-LINK.COLOUR LINK)))
	  (RDS-RULE 'RECHECK 'RED*CLAUSE.REPL.FACTORING))
     (RED=CRF_TO.CLAUSE (DS-LINK.POSPAR LINK))))
  NIL)

(DEFUN RED=CRF_TO.UNIFIER (LINK.OF.THE.UNIFIER UNIFIER CLAUSE CHANGE.LITNO UNCHANGE.LITNO MARKS)
						; edited: 22-may-84 01:56:31
						; input:  'unifier' is a unifier
						;         of the si- or sid-link
						;         'link.of.the.unifier'. the parent literals
						;         of this link are the literals of clause
						;         'clause' with numbers 'change.litno' and
						;         'unchange.litno'.
						;         'marks' is an array: !'marks'! = !'clause'!,
						;         denoting marks of the corresponding
						;         literals.
						; effect: if the clause created by application of
						;         matcher merged from binding and 'unifier'
						;         replacement-subsumes the parent
						;         clause, it
						;         will be replaced by the new clause. this is
						;         done by removing literals of 'clause'.
						;         else recursive call of this function with a
						;         next unifier to merge into the binding.
						;         reduction agendas are updated in success
						;         case and information is given to the
						;         protocol and connectiongraph module.
						; value:  not nil iff an operation took place.
  (let ((NEW.MERGE (UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) UNIFIER))
	EVERY.AFFECTED.REMOVABLE MATCHER.DOMAIN NOT.MOST.INNER.INCARNATION RES.CLAUSE DETECT.LINK INFO OTHERLITNO)
    (MEMBER-IF #'(LAMBDA (MATCHER)
		   (RDS-MARKS.RESET MARKS NIL)	; all affected literals exceptly 'change.litno' are
						; marked with t in the array 'marks', the others with
						; nil.
		   (SETQ EVERY.AFFECTED.REMOVABLE T)
		   (SETQ NOT.MOST.INNER.INCARNATION NIL)
		   (SETQ MATCHER.DOMAIN (UNI-UNIFIER.DOMAIN MATCHER NIL))
		   (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
		     (unless (EQL (1+ RPTN) CHANGE.LITNO)
		       (when (INTERSECTION (DS-CLAUSE.LIT.VARIABLES CLAUSE (1+ RPTN)) MATCHER.DOMAIN)
			 (RDS-MARK.PUT MARKS (1+ RPTN) T))))	; bindings are set.
		   (RDS-BINDING.PUSH MATCHER)
		   (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
		     (COND ((RDS-MARK MARKS (1+ RPTN))	; consideration of removability of all affected, i.e.
						; marked literals.
			    (COND ((AND (MEMBER (1+ RPTN) (DS-CLAUSE.POTENTIALLY.FALSE.LITNOS CLAUSE))
					(SETQ RES.CLAUSE (RED.SERVICE-FALSE.CLAUSE CLAUSE (1+ RPTN) (RDS-BINDING.TOP))))
						; literal is false. old mark is replaced by number of
						; clause with which this literal can be resolved
						; away.
				   (RDS-MARK.PUT MARKS (1+ RPTN) RES.CLAUSE))
				  ((MEMBER-IF #'(LAMBDA (NCI.LINKS)
						  (MEMBER-IF #'(LAMBDA (NCI.LINK)
								 (SETQ DETECT.LINK NCI.LINK)
								 (AND (NOT (RDS-MARK MARKS
											(SETQ OTHERLITNO
											      (RED.SERVICE-OTHERLITNO.INTERNAL
												NCI.LINK (1+ RPTN)))))
								      (OR (NOT (RED.SERVICE-LINK.ORIENTED NCI.LINK))
									  (EQL (1+ RPTN) (DS-LINK.POSLITNO NCI.LINK)))
								      (MEMBER-IF
									#'(LAMBDA (UNIFIER.TO.BE.NIL)
									    (COND
									      ((UNI-BINDING.IS.INSTANCE.OF UNIFIER.TO.BE.NIL)
						; literal is a double literal. old mark is replaced
						; by a dotted pair (number_of_remaining_literal .
						; rule_of_si-link_leading_to_detection_of_doubleness).
									       (RDS-MARK.PUT
										 MARKS (1+ RPTN)
										 (CONS OTHERLITNO (DS-LINK.RULE DETECT.LINK))))
									      ((RED=CRF_TO.UNIFIER NCI.LINK UNIFIER.TO.BE.NIL
												   CLAUSE
												   CHANGE.LITNO UNCHANGE.LITNO
												   MARKS)
									       (SETQ NOT.MOST.INNER.INCARNATION T))))
									(DS-LINK.UNIFIERS NCI.LINK))))
							     NCI.LINKS))
					      (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL)
									CLAUSE (1+ RPTN))))
				  (T		; no success. literal cannot be removed.
				   (SETQ EVERY.AFFECTED.REMOVABLE NIL) (SETQ RPTN 0))))))
		   (COND
		     ((AND EVERY.AFFECTED.REMOVABLE (NOT NOT.MOST.INNER.INCARNATION)) 
						; only in the most inner iteration removals are done.
		      (RDS-MARK.PUT MARKS CHANGE.LITNO (CONS UNCHANGE.LITNO (DS-LINK.RULE LINK.OF.THE.UNIFIER)))
						; constructing information for protocol module.
		      (SETQ INFO (RED=CRF_PROTOCOL.RECONSTRUCT CLAUSE CHANGE.LITNO UNCHANGE.LITNO MARKS))
						; reset of bindings.
		      (RDS-BINDING.RESET)	; information for induction is given to
						; connectiongraph module.
		      (RED=CRF_INSERT.ANCESTORS (CAR (FOURTH INFO)) (THIRD INFO))
		      (PR-OPERATION 'REPLACEMENT.OPERATION CLAUSE INFO)	; all affected literals can and will be removed. new
						; candidates for reduction rules are inserted into
						; agendas.
		      (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
			(when (RDS-MARK MARKS (1+ RPTN))
			  (RED=CTL_AGENDA.UPDATE
			    (RED=CTL_REMOVE.LITERAL CLAUSE (1+ RPTN)
						    (unless (DS-CLAUSE.IS (RDS-MARK MARKS (1+ RPTN)))
						      (first (RDS-MARK MARKS (1+ RPTN))))	;UNCHANGE.LITNO)
						    "replacement factoring" NIL)
			    CLAUSE (RDS-RULES 'SUCCESSOR.LITERAL.REMOVAL))
			  (dodown (litno rptn)
			    (when (and (rds-mark marks (1+ litno))
				       (consp (rds-mark marks (1+ litno)))
				       (> (first (rds-mark marks (1+ litno))) rptn))
			      (decf (first (rds-mark marks (1+ litno)))))
			    #|(mapl #'(lambda (rest) (when (> (first rest) rptn) (decf (first rest))))
				  (rds-mark marks (1+ litno)))|#)))
		      (SETQ NOT.MOST.INNER.INCARNATION T))
		     (T (RDS-BINDING.POP)))
		   (MEMBER LINK.OF.THE.UNIFIER (CG-LINKS (DS-LINK.COLOUR LINK.OF.THE.UNIFIER) REMOVED)))
	       (PROG2 (RDS-BINDING.PUSH NIL)
		      (MAPCAN #'(LAMBDA (UNI) (UNI-UNIFIER.BECOMES.MATCHER UNI (DS-CLAUSE.LIT.VARIABLES CLAUSE UNCHANGE.LITNO)))
			      NEW.MERGE)
		      (RDS-BINDING.POP)))
    (MEMBER LINK.OF.THE.UNIFIER (CG-LINKS (DS-LINK.COLOUR LINK.OF.THE.UNIFIER) REMOVED))))


(DEFUN RED=CRF_TO.CLAUSE (CLAUSE)
						; edited: 22-may-84 02:47:37
						; input:  'clause' is a clause.
						; effect: while there exists a sequence of
						;         si- and sid-links
						;         incident with 'clause', so that the factor
						;         replacement-subsumes
						;         'clause', 'clause' will be
						;         replaced by this factor, by removing
						;         literals of 'clause'.
						;         if operations are done, information is given
						;         to the protocol and connectiongraph module.
						;         snowballeffect of reduction rules is
						;         considered.
						; value:  nil.
  (WHILE
    (BLOCK CRF.TO.CLAUSE
      (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	(MAPC #'(LAMBDA (NCI.LINKS)
		  (MAPC #'(LAMBDA (NCI.LINK)
			    (COND
			      ((AND (< (1+ RPTN) (RED.SERVICE-OTHERLITNO.INTERNAL NCI.LINK (1+ RPTN)))
				    (RED=CRF_ATTEMPT.TO.FACTORIZE CLAUSE NCI.LINK (DS-LINK.COLOUR NCI.LINK)))
						; the inner loop can be terminated, next replacement
						; factoring for this clause is searched.
						; in the else case of cond the outer loop is
						; terminated, no replacement factoring can be done for
						; this clause now.
			       (RETURN-FROM CRF.TO.CLAUSE T))))
			NCI.LINKS))
	      (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.INTERNAL) CLAUSE (1+ RPTN)))))))

(DEFUN RED=CRF_ATTEMPT.TO.FACTORIZE (CLAUSE LINK COLOUR)
  (DECLARE (IGNORE COLOUR))
						; edited: 22-may-84 03:01:44
						; input:  'link' is a link with colour 'colour'
						;         incident with clause 'clause'. 'colour' is
						;         si or sid.
						; effect: if a factor of 'link' subsumes its parent
						;         clause, 'clause' will be replaced by this
						;         factor by removing literals of 'clause'.
						;         the information about performed operations
						;         is given to the protocol and connectiongraph
						;         module. snowball effect of reduction rules
						;         is considered.
						; value:  not nil iff there was an operation.
  (unless (ds-link.sort.inhibited link)
    (let ((MARKS (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT CLAUSE))))
      (PROG1
	(MEMBER-IF #'(LAMBDA (UNI)
						; attempt to use a matcher affecting a
						; parent literal of 'link'.
						; the negative literal is used only if link is not
						; oriented.
		       (OR (RED=CRF_TO.UNIFIER LINK UNI CLAUSE (DS-LINK.POSLITNO LINK) (DS-LINK.NEGLITNO LINK) MARKS)
			   (AND (NOT (RED.SERVICE-LINK.ORIENTED LINK))
				(RED=CRF_TO.UNIFIER LINK UNI CLAUSE (DS-LINK.NEGLITNO LINK) (DS-LINK.POSLITNO LINK) MARKS))))
		   (DS-LINK.UNIFIERS LINK))
	(RDS-MARKS.DESTROY 0)))))

(DEFUN RED=CRF_INSERT.ANCESTORS (MULTIPLES RESOLUTIONS)
						; edited: 10-aug-84 03:10:00
						; input:  'multiples' is a list of elements
						;         (literal_to_be_removed literal_to_remain
						;         rule) each literal being a dotted pair
						;         (clause . litno). 'resolutions' is a list
						;         of dotted pairs (literal_of_object_clause .
						;         other_literal).
						; effect: the list of elements (literal_to_be_removed
						;         . literal_to_remain) and
						;         (literal_of_clause . atom_'remove')
						;         is given to change queue of connectiongraph
						;         module.
						; value:  undefined.
  (CG-CHANGE.QUEUE_APPEND
    (CONS 'REPLACEMENT.FACTORING
	  (NCONC (MAPCAR #'(LAMBDA (DOUBLE) (CONS (CAR DOUBLE) (SECOND DOUBLE))) MULTIPLES)
		 (MAPCAR #'(LAMBDA (RESOLUTION) (CONS (CAR RESOLUTION) 'REMOVE)) RESOLUTIONS)))))

(DEFUN RED=CRF_PROTOCOL.RECONSTRUCT (CLAUSE CHANGE.LITNO UNCHANGE.LITNO MARKS)
						; edited: 22-may-84 02:29:09
						; input:  'unchange.litno' and 'change.litno' are
						;         literal numbers of clause 'clause'. 'marks'
						;         is an array with !'marks'! = !'clause'!,
						;         each cell being a label of a literal of
						;         clause 'clause'.
						; effect: -
						; value:  list (uni clauses resolutions doubles),
						;         'uni' being the current binding,
						;         'clauses' a list of all clauses used during
						;         replacement factoring,
						;         'resolutions' describing the literals of
						;         resolutions to remove false literals_ list
						;         of ((clause1 . litno1) . (clause2 .
						;         litno2))'
						;         'doubles' a list of constructs like the
						;         sample ((clause . remaining_literal_number)
						;         (clause . removed_litno) rule_of_link)
						;         denoting double literals.
  (DECLARE (IGNORE UNCHANGE.LITNO CHANGE.LITNO))
  (PROG ((CLAUSES NIL) (RESOLUTIONS NIL) (MULTIPLES NIL) MARK)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (PROGN (SETQ MARK (RDS-MARK MARKS (1+ RPTN)))
		 (COND
		   ((INTEGERP MARK)
						; if mark is a clause, literal is false.
		    (SETQ CLAUSES (CONS MARK CLAUSES))
		    (SETQ RESOLUTIONS (CONS (CONS (CONS CLAUSE (1+ RPTN)) (CONS MARK 1)) RESOLUTIONS)))
		   (MARK			; if mark is a dotted pair, it is a double literal.
		    (SETQ MULTIPLES
			  (CONS (LIST (CONS CLAUSE (CAR MARK)) (CONS CLAUSE (1+ RPTN)) (CDR MARK)) MULTIPLES))))))
	(RETURN (LIST (RDS-BINDING.TOP) (CONS CLAUSE CLAUSES) RESOLUTIONS (LIST MULTIPLES)))))

(DEFUN RED=APPLY_CLAUSE.SUBSUMPTION.SUBJECT.TO.CLAUSE (CLAUSE)
						; edited: 21-may-84 17:59:16
						; input:  a clause 'clause'.
						; effect: each clause subsumed by clause 'clause' will
						;         be handled depending on options.
						;         information about removed clauses is given
						;         to the connectiongraph module.
						; value:  nil.
						; remark: subsumption, forward and backward, also as
						;         the values of the option are described more
						;         detailed in the comment of the function
						;         'red=cs_attempt.to.subsume'.
						;         here (in the subject case, clause will
						;         subsume) only backward option is senseful,
						;         there exists no forward option.
						;         option is 't' in initial and 'remove' in
						;         deduced state. 'nil' has lead to a removal
						;         from rules switched on.
  (MAPC #'(LAMBDA (SUBSUMENT)
	    (RED=CS_ATTEMPT.TO.SUBSUME (LIST CLAUSE) SUBSUMENT
				       (RDS-RULE 'CONDITION 'RED*CLAUSE.SUBSUMPTION.SUBJECT)))
	(RED=CS_CANDIDATES.TO.BE.SUBSUMED CLAUSE))
  NIL)

(DEFUN RED=APPLY_CLAUSE.SUBSUMPTION.OBJECT.TO.CLAUSE (CLAUSE)
						; edited: 21-may-84 18:12:52
						; input:  a clause 'clause'.
						; effect: clause 'clause' will be considered depending
						;         on options, wether it is subsumed by another
						;         clause or it has a true literal.
						;         if clause 'clause' is removed, information
						;         is given to the connectiongraph module.
						; value:  nil.
						; remark: subsumption, forward and backward, also as
						;         the values of the option are described more
						;         detailed in the comment of the function
						;         'red=cs_attempt.to.subsume'.
						;         here (in the object case, clause is
						;         subsumed) the option for subsumption is a
						;         list (forward backward) where one can be
						;         'nil'.
						;         removal of clause with true literals only is
						;         possible, if forward option is set.
						;         in the initial state option list is ('t' 't'
						;         ).
  (let ((CONDITION (if (CG-CLAUSE_CREATOR.UNIFIER CLAUSE)
		       (FIRST  (RDS-RULE 'CONDITION 'RED*CLAUSE.SUBSUMPTION.OBJECT))
		       (SECOND (RDS-RULE 'CONDITION 'RED*CLAUSE.SUBSUMPTION.OBJECT)))))
    (COND ((NULL CONDITION))
	  ((RED=CS_SUBSUME.TRUE.LITERAL CLAUSE))
	  ((RED=CS_ATTEMPT.TO.SUBSUME (RED=CS_CANDIDATES.TO.BE.SUBSUMING CLAUSE) CLAUSE CONDITION)))
    nil))

(DEFUN RED=APPLY_CLAUSE.SUBSUMPTION.SUBJECT.TO.LINK (LINK)
						; edited: 21-may-84 18:24:17
						; input:  a link 'link', newly inserted into the
						;         connectiongraph at any place.
						; effect: only if colour of link is s, si, r, or riw.
						;         function is controlled by recheck- and
						;         forward//backward-option.
						;         insertion of s-links can lead to subsumption
						;         of one parent by the other one. this is
						;         checked if recheck-option is set to 't' or
						;         'partial' ('link' incident with clause to
						;         be removed).
						;         insertion of si-, r-, or riw-links can lead
						;         to compliance with the link condition
						;         described in 'red=cs_link.condition'. this
						;         is checked if recheck-option is set to 't'.
						; example:r-link insertion
						;         --------------  inserted  ----------
						;         !          ! !------------! !
						;         --------------   r-link   ----------
						;               !                    !
						;           subsumption              !
						;               v                    !old
						;         --------------             !r-link
						;         !          ! !-------------
						;         --------------
						;         now link condition can be fulfilled.
						; value:  nil.
						; remark: subsumption, forward and backward, also as
						;         the values of the forward//backward-option
						;         are described more detailed in
						;         'red=cs_attempt.to.subsume'.
						;         it is useful to speak of a subject case.
						;         therefore backward option for control of
						;         removal is used.
  (PROG ((RECHECK (RDS-RULE 'RECHECK 'RED*CLAUSE.SUBSUMPTION.SUBJECT)) (COLOUR (DS-LINK.COLOUR LINK)))
	(COND
	  ((MEMBER COLOUR (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL))
	   (COND
	     (RECHECK
	      (RED=CS_TO.SUBSUMPTION.LINK (DS-LINK.POSPAR LINK) (DS-LINK.NEGPAR LINK)
					  (RDS-RULE 'CONDITION 'RED*CLAUSE.SUBSUMPTION.SUBJECT)))))
	  ((AND (MEMBER COLOUR (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION)) (EQL RECHECK T))
	   (RED=CS_TO.LINK.CONDITION.LINK (RED.SERVICE-LINK.PARENTS LINK)
					  (RDS-RULE 'CONDITION 'RED*CLAUSE.SUBSUMPTION.SUBJECT))))))

(DEFUN RED=APPLY_CLAUSE.SUBSUMPTION.SUBJECT.TO.RECHECK (RECHECK)
						; edited: 21-may-84 18:43:39
						; input:  'recheck' is a link removed from the actual
						;         connectiongraph, but not from the memory.
						; effect: if removal of a link 'recheck' leads to
						;         compliance with the link condition,
						;         described in 'red=cs_link.condition' for
						;         any pair of clauses which can subsume one
						;         another, stored in recheck info list of
						;         rule 'red*clause.subsumption.subject', then
						;         subsumption induced by this pair of clauses
						;         is considered again.
						;         information about removed clauses is given
						;         to the connectiongraph module.
						; value:  nil.
						; remark: subsumption is explained in the function
						;         'red=cs_attempt.to.subsume'.
						;         here application of rule subsumption is
						;         controlled by the second, the recheck
						;         option. it must be 't' for recheck.
  (COND
    ((MEMBER (DS-LINK.COLOUR RECHECK) (RDS-LINK.COLOURS 'SLC.RECHECK.SUBSUMENT))
     ;; 'recheck' must have adequate colour.
     (PROG ((PARENTS (RED.SERVICE-LINK.PARENTS RECHECK))) (RED=CS_UPDATE.RECHECK.INFO)
	   ;; removal of clause pairs from recheck info list,
	   ;; where one or both have been removed from
	   ;; connectiongraph or memory.
	   (MAPC #'(LAMBDA (SUBSUMER.SUBSUMENT)
		     ;; searching all pairs where subsument is incident
		     ;; with our link 'recheck'.
		     (COND
		       ((MEMBER (CAR SUBSUMER.SUBSUMENT) PARENTS)
			(RED=CS_ATTEMPT.TO.SUBSUME (LIST (CDR SUBSUMER.SUBSUMENT))
						   (CAR SUBSUMER.SUBSUMENT)
						   (RDS-RULE 'CONDITION 'RED*CLAUSE.SUBSUMPTION.SUBJECT))
			;; handling all clause pairs attached.
			)))
		 (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.SUBSUMPTION.SUBJECT))))))

(DEFUN RED=CS_TO.SUBSUMPTION.LINK (CLAUSE1 CLAUSE2 CONDITION)
						; edited: 21-may-84 19:08:56
						; input:  'clause1' and 'clause2' are two clauses,
						;         the parent clauses of a newly inserted
						;         s-link.
						;         'condition' is one of the atoms 't',
						;         'remove', 'inhibit', and 'remove-inhibit'
						;         (the option for forward or backward
						;         subsumption, described in the function
						;         'red=cs_attempt.to.subsume').
						; effect: if one of the clauses subsumes the other
						;         one, it is handled as described in
						;         'red=cs_attempt.to.subsume'.
						; value:  nil.
  (PROG (LITERALS1 LITERALS2)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE1))
	  (MAPC #'(LAMBDA (NC.LINKS)
						; considering all s-links between the two clauses.
						; after 'rptq' 'literali' contains all literals of
						; 'clausei' with a s-link also incident with the
						; other clause.
						; for oriented s-links 'clausei' must be its positive
						; parent.
		    (MAPC #'(LAMBDA (NC.LINK)
			      (COND
				((EQL CLAUSE2 (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK CLAUSE1))
				 (COND
				   ((OR (EQL CLAUSE1 (DS-LINK.POSPAR NC.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK)))
				    (SETQ LITERALS1 (INS (1+ RPTN) LITERALS1))))
				 (COND
				   ((OR (EQL CLAUSE2 (DS-LINK.POSPAR NC.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK)))
				    (SETQ LITERALS2 (INS (RED.SERVICE-OTHERLITNO.EXTERNAL NC.LINK CLAUSE1) LITERALS2)))))))
			  NC.LINKS))
		(RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) CLAUSE1 (1+ RPTN))))
						; if 'literali' does not contain all literals of
						; 'clausei', no subsumption is possible (see
						; illustration in 'red=cs_attempt.to.subsume').
						(COND
						  ((EQL (LIST-LENGTH LITERALS1) (DS-CLAUSE.NOLIT CLAUSE1))
						   (COND
						     ((NOT (RED=CS_ATTEMPT.TO.SUBSUME (LIST CLAUSE1) CLAUSE2 CONDITION))
						; if 'red=cs_attempt.to.subsume' delivers not nil one
						; of the clauses is removed.
						      (COND
							((EQL (LIST-LENGTH LITERALS2) (DS-CLAUSE.NOLIT CLAUSE2))
							 (RED=CS_ATTEMPT.TO.SUBSUME (LIST CLAUSE2) CLAUSE1 CONDITION)))))))))

(DEFUN RED=CS_TO.LINK.CONDITION.LINK (PARENTS CONDITION)
						; edited: 21-may-84 19:28:59
						; input:  'parents' is a list of one or two clauses,
						;         parents of a newly inserted si-, r-, or
						;         riw-link.
						;         'condition' is one of the atoms 't',
						;         'remove', 'inhibit', and 'remove-inhibit'
						;         (the option for backward subsumption in
						;         subject case, described in the function
						;         'red=cs_attempt.to.subsume').
						; effect: if one of the clauses is a subsumer
						;         candidate in the recheck info list of rule
						;         (i.e. it is the first one in one of the
						;         pairs stored there), then subsumption for
						;         this pair is checked again according to
						;         'condition'.
						; value:  nil.
  (PROG ((CLAUSE.PAIRS NIL)) (RED=CS_UPDATE.RECHECK.INFO)
	;; removal of pairs, containing removed clauses,from
	;; recheck info list.
	(MAPC #'(LAMBDA (SUBSUMER.SUBSUMENT)
		  ;; searching all clause pairs where the subsumer
		  ;; candidate is incident with our inserted link.
		  (COND
		    ((MEMBER (CAR SUBSUMER.SUBSUMENT) PARENTS)
		     (RED=CS_ATTEMPT.TO.SUBSUME (LIST (CAR SUBSUMER.SUBSUMENT)) (CDR SUBSUMER.SUBSUMENT) CONDITION)
		     ;; recheck subsumption.
		     )))
	      (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.SUBSUMPTION.SUBJECT))
	(MAPC #'(LAMBDA (SUBSUMER.SUBSUMENT)
		  (RED=CS_ATTEMPT.TO.SUBSUME (LIST (CAR SUBSUMER.SUBSUMENT)) (CDR SUBSUMER.SUBSUMENT)
					     (RDS-RULE 'CONDITION 'RED*CLAUSE.SUBSUMPTION.SUBJECT)))
	      CLAUSE.PAIRS)))

(DEFUN RED=CS_ATTEMPT.TO.SUBSUME (SUBSUMERS SUBSUMENT CONDITION)
						; edited: 21-may-84 19:54:44
						; input:  'subsumers' is a list of clauses,
						;         'subsument' is a clause, 'condition' is one
						;         of the atoms 't', 'remove', 'inhibit', and
						;         'remove-inhibit'. each literal of
						;         'subsumers' has a s-link incident with a
						;         literal of 'subsument'. for oriented s-links
						;         'subsument' is the negative parent.
						; effect: meaning of 'condition':
						;         -----------------------
						;         't': if (1) - (3) of meaning of subsumption
						;           is fulfilled for a clause in 'subsumers'
						;           and 'subsument', 'subsument' will be
						;           removed from connectiongraph.
						;         'remove': if (1) - (4) is fulfilled,
						;           'subsument' will be removed.
						;         'inhibit': if (1) - (3) is fulfilled and
						;           we are in a forward case,
						;           'subsument' will be removed and creator
						;           unifier inhibited.
						;         'remove-inhibit': if (1) - (4) is fulfilled
						;           for a subsumer 'subsument' will be
						;           removed, but if (4) fails for all 'sub=
						;           sumers', and (1) - (3) are fulfilled for
						;           a subsumer and forward case is given, it
						;           will be handled as under 'inhibit'.
						;         meaning of subsumption:
						;         -----------------------
						;         a clause c2 is subsumed by a clause c1 iff:
						;         (1) for the literals exists an injective
						;           function f as illustrated:
						;               -----------------
						;               !   !   !   !   !       c1
						;               --+---+---+---+--
						;                 !   !   !___!___
						;             f   !   !___    !   !
						;                 v       v   v   v
						;           -------------------------
						;           !   !   !   !   !   !   !   c2
						;           -------------------------
						;         (2) between literals l in c1 and f(l) in c2
						;           exists a s-link. if this s-link is
						;           oriented, c2 must be its negative parent.
						;         (3) the merge of the unifiers of the s-links
						;           one for each pair (l, f(l)) is a matcher
						;           only modifying variables of clause c1.
						;         (4) link condition described in
						;           'red=cs_link.condition' is fulfilled.
						;         a weaker case complying only with (1) - (3)
						;         sometimes also will be included in the
						;         notion subsumption.
						;         meaning of forward and backward:
						;         --------------------------------
						;         we destinguish two cases of subsumption.
						;         if there exists the creator unifier of the
						;         subsumed clause and it has not been changed
						;         during reduction, it is a forward case,
						;         else a backward case.
						; value:  a list (indicator
						;         subsumer literals unifier).
						;         iff subsument has been removed.
						;         indicator in {red*cs_inhibit, red*cs_remove}
						;         describes type of removal
						;         according to 'condition', 'subsumer' is the
						;         subsuming clause in 'subsumers', 'literals'
						;         is the literal adjoining (1).
						;         'unifier' is the merge matcher (3) for
						;         subsumption.
  (unless (ds-clause.some #'(lambda (litno) (ds-clause.irreducible.is subsument litno)) subsument)
    (PROG (RELATION RECHECKS NEXT.RELATION)
	  ;; invariant:
	  ;; 'next.relation' is a legal
	  ;; value of the function iff for
	  ;; one of the considered 'subsumers' (1) - (3) are
	  ;; fulfilled, (4) is not met and 'condition' is
	  ;; 'inhibit'.
	  ;; or 'remove-inhibit'.
	  ;; 'subsumers' is not nil.
	  ;; there does not exist any considered subsumer where
	  ;; (4) is fulfilled, if 'condition' is 't', 'remove',
	  ;; or 'remove-inhibit'.
	  (WHILE SUBSUMERS
	    (SETQ NEXT.RELATION
		  (RED=CS_RELATION.INJECTIVE (DS-CLAUSE.NOLIT (CAR SUBSUMERS)) 1 NIL NIL (CAR SUBSUMERS) SUBSUMENT CONDITION))
	    (COND
	      ((NOT NEXT.RELATION)		; literals do not join or there is no matcher.
	       (SETQ SUBSUMERS (CDR SUBSUMERS)))
	      ((EQL (CAR NEXT.RELATION) 'RED*CS_REMOVE)	; clause can be removed, (4) is fulfilled, or
						; 'condition' is 't'.
	       (SETQ SUBSUMERS NIL)
	       (SETQ RELATION NEXT.RELATION))
	      ((EQL (CAR NEXT.RELATION) 'RED*CS_INHIBIT)	; inhibition is possible, but there will be searched
						; for a subsumer, so that removal without inhibition
						; can be made. inhibition possibility stored in
						; 'relation'. it can be rewritten
						; because only one inhibition case is interesting,
						; and in remove case loop is terminated.
	       (SETQ SUBSUMERS (CDR SUBSUMERS))
	       (SETQ RELATION NEXT.RELATION))
	      (T	;; if 'condition' is 'remove', 'remove-inhibit', or
	       ;; 'inhibit' (without creator unifier), clause pairs
	       ;; subsumer - subsument are inserted into recheck info
	       ;; list, if furthermore no subsumer is found.
	       (SETQ RECHECKS (CONS NEXT.RELATION RECHECKS)) (SETQ SUBSUMERS (CDR SUBSUMERS)))))
	  ;; inhibition if possible, else insertion into recheck
	  ;; info list, where clause pair waits for insertion or
	  ;; removal of links, so that it complies with (4).
	  (COND
	    ((EQL (CAR RELATION) 'RED*CS_INHIBIT)
	     ;; inhibition.
	     (PROG ((CREATOR.UNIFIER (CG-CLAUSE_CREATOR.UNIFIER SUBSUMENT)))
		   (CG-INHIBIT.UNIFIER (CAR CREATOR.UNIFIER) (CDR CREATOR.UNIFIER) 'SUBSUMPTION
				       (CONS (SECOND RELATION) (FOURTH RELATION)))
		   (RED=CTL_AGENDA.UPDATE
		     (RED=CTL_REMOVE.CLAUSE SUBSUMENT 'SUBSUMPTION (CONS (SECOND RELATION) (FOURTH RELATION))) NIL NIL)
		   (RED=CS_INSERT.ANCESTORS (SECOND RELATION) SUBSUMENT (THIRD RELATION))))
	    ((EQL (CAR RELATION) 'RED*CS_REMOVE)
	     ;; removal.
	     (RED=CTL_AGENDA.UPDATE
	       (RED=CTL_REMOVE.CLAUSE SUBSUMENT 'SUBSUMPTION (CONS (SECOND RELATION) (FOURTH RELATION))) NIL NIL)
	     (RED=CS_INSERT.ANCESTORS (SECOND RELATION) SUBSUMENT (THIRD RELATION)))
	    (RECHECKS	;; recheck insertion.
	     (RDS-RULE.PUT 'RECHECK.INFO 'RED*CLAUSE.SUBSUMPTION.SUBJECT
			      (UNION (MAPCAR #'(LAMBDA (RECHECK) (CONS SUBSUMENT (SECOND RECHECK))) RECHECKS)
				     (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.SUBSUMPTION.SUBJECT)))))
	  (RETURN RELATION))))

(DEFUN RED=CS_SUBSUME.TRUE.LITERAL (POT.TRUE.CLAUSE)
						; edited: 21-may-84 21:07:57
						; input:  'pot.true.clause' is a clause.
						; effect: if one of the potentially true literals of
						;         the clause is true (example: a = a), the
						;         clause will be removed. if one never can
						;         become true (example: a = b), its number
						;         will be removed from the list of
						;         potentially true literal numbers of clause
						;         'pot.true.clause'.
						; value:  not nil iff 'pot.true.clause' is removed.
  (MEMBER-IF
    #'(LAMBDA (POT.TRUE.LITNO)
        (CASE (RED.SERVICE-TRUE.LITERAL.IS POT.TRUE.CLAUSE POT.TRUE.LITNO (RDS-BINDING.TOP))
          (IMPOSSIBLE
            (DS-CLAUSE.PUT.POTENTIALLY.TRUE.LITNOS POT.TRUE.CLAUSE
						   (DELETE POT.TRUE.LITNO (DS-CLAUSE.POTENTIALLY.TRUE.LITNOS POT.TRUE.CLAUSE)))
            NIL)
          ((T) (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.CLAUSE POT.TRUE.CLAUSE "True clause" POT.TRUE.LITNO) NIL NIL)
	   T)
          ((NIL))
          (OTHERWISE (ERROR "red-check - illegal value in red=cs_subsume.true.literal: : ~a"
			    (RED.SERVICE-TRUE.LITERAL.IS POT.TRUE.CLAUSE POT.TRUE.LITNO)))))
    (DS-CLAUSE.POTENTIALLY.TRUE.LITNOS POT.TRUE.CLAUSE)))

(DEFUN RED=CS_INSERT.ANCESTORS (SUBSUMER SUBSUMENT LITERALS)
						; edited: 21-may-84 21:14:43
						; input:  'subsumer' and 'subsument' are two clauses.
						;         'literals' is a list (ln, ..., l1) of
						;         literal numbers of clause 'subsument', so
						;         that with literal function f of
						;         'red=cs_attempt.to.subsume': f(i) = li.
						; effect: appends the literal association as one block
						;         at the end of connectiongraph change queue.
						;         there the adjoinment is described by dotted
						;         pairs (('subsumer' . i) . ('subsument' .
						;         li)).
						; value:  undefined.
  (PROG ((LITNO (1+ (DS-CLAUSE.NOLIT SUBSUMER))))
	(CG-CHANGE.QUEUE_APPEND
	  (CONS 'SUBSUMPTION
		(MAPCAR
		  #'(LAMBDA (LITERAL) (SETQ LITNO (1- LITNO)) (CONS (CONS SUBSUMER LITNO) (CONS SUBSUMENT LITERAL)))
		  LITERALS)))))

(DEFUN RED=CS_UPDATE.RECHECK.INFO NIL
						; edited: 21-may-84 21:22:31
						; input:  -
						; effect: removes all pairs of clauses from recheck
						;         information list of rule
						;         'red*clause.subsumption.subject', where one
						;         or both clauses have been removed from
						;         connectiongraph or memory.
						; value:  undefined.
  (RDS-RULE.PUT 'RECHECK.INFO 'RED*CLAUSE.SUBSUMPTION.SUBJECT
		   (DREMAP (RDS-RULE 'RECHECK.INFO 'RED*CLAUSE.SUBSUMPTION.SUBJECT) NIL
			   #'(LAMBDA (CLAUSE.PAIR) (SETQ CLAUSE.PAIR (CAR CLAUSE.PAIR))
				     (OR (NOT (DS-CLAUSE.IS (CAR CLAUSE.PAIR))) (NOT (DS-CLAUSE.IS (CDR CLAUSE.PAIR)))
					 (MEMBER (CAR CLAUSE.PAIR) (CG-CLAUSES REMOVED))
					 (MEMBER (CDR CLAUSE.PAIR) (CG-CLAUSES REMOVED))))
			   NIL)))

(DEFUN RED=CS_CANDIDATES.TO.BE.SUBSUMING (SUBSUMENT.CANDIDATE)
						; edited: 21-may-84 21:26:49
						; input:  'subsument.candidate' is a clause.
						; effect: -
						; value:  list of all clauses in the current
						;         connectiongraph, so that for all literals
						;         of these clauses, there exists a s-link to
						;         clause 'subsument.candidate'. oriented
						;         s-links are only considered if they have
						;         'subsument.candidate' as negative parent.
						; remark: quick intersection algorithm with labeling.
  (let ((SUBSUMER.CANDIDATES NIL)
	(NOLIT (DS-CLAUSE.NOLIT SUBSUMENT.CANDIDATE))
	OTHERPAR)
    ;; mark clauses obtainable by s-links with literal
    ;; number lists.
    (DODOWN (RPTN NOLIT)
      (MAPC #'(LAMBDA (NC.LINKS)
		(MAPC #'(LAMBDA (S.LINK) (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL S.LINK SUBSUMENT.CANDIDATE))
				(COND
				  ((AND (NOT (> (DS-CLAUSE.NOLIT OTHERPAR) NOLIT))
					(OR (EQL SUBSUMENT.CANDIDATE (DS-LINK.NEGPAR S.LINK))
					    (NOT (RED.SERVICE-LINK.ORIENTED S.LINK))))
				   (DT-PUTPROP OTHERPAR 'RED*CS_CANDIDATES
					       (INS (DS-LINK.OTHERLITNO S.LINK SUBSUMENT.CANDIDATE (1+ RPTN))
						    (DT-GETPROP OTHERPAR 'RED*CS_CANDIDATES)))
				   (SETQ SUBSUMER.CANDIDATES (INSERT OTHERPAR SUBSUMER.CANDIDATES)))))
		      NC.LINKS))
	    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBSUMENT.CANDIDATE (1+ RPTN))))
    ;; remove marks from all candidates.
    ;; remove clauses from candidate list which do not
    ;; have s-links for all literals.
    (DREMAP SUBSUMER.CANDIDATES NIL
	    #'(LAMBDA (CANDT)
		(PROG1
		  (< (LIST-LENGTH (DT-GETPROP (CAR CANDT) 'RED*CS_CANDIDATES)) (DS-CLAUSE.NOLIT (CAR CANDT)))
		  (DT-REMPROP (CAR CANDT) 'RED*CS_CANDIDATES)))
	    NIL)))

(DEFUN RED=CS_CANDIDATES.TO.BE.SUBSUMED (SUBSUMER.CANDIDATE)
						; edited: 21-may-84 21:36:45
						; input:  'subsumer.candidate' is a clause.
						; effect: -
						; value:  list of all clauses in the connectiongraph,
						;         so that for every clause in this list and
						;         every literal of clause 'subsumer.clause',
						;         there exists a s-link incident with both.
						;         oriented  s-links are only considered, if
						;         they have 'subsumer.candidate' as positive
						;         parent.
						; remark: quick intersection algorithm with labeling.
  (let (CANDIDATES.TO.BE.SUBSUMED (NOLIT (DS-CLAUSE.NOLIT SUBSUMER.CANDIDATE)) OTHERPAR)
    ;; mark clauses obtainable by s-links with lists
    ;; of numbers of literals of
    ;; 'subsumer.candidate' literals.
    (DODOWN (RPTN NOLIT)
      (COND ((EVERY #'(LAMBDA (NC.LINKS)
			(MAPC #'(LAMBDA (NC.LINK)
				  (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK SUBSUMER.CANDIDATE))
				  (COND ((AND (NOT (< (DS-CLAUSE.NOLIT OTHERPAR) NOLIT))
					      (OR (EQL SUBSUMER.CANDIDATE (DS-LINK.POSPAR NC.LINK))
						  (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK))))
					 (DT-PUTPROP OTHERPAR 'RED*CS_CANDIDATES
						     (INS (1+ RPTN) (DT-GETPROP OTHERPAR 'RED*CS_CANDIDATES)))
					 (SETQ CANDIDATES.TO.BE.SUBSUMED (INS OTHERPAR CANDIDATES.TO.BE.SUBSUMED)))))
			      NC.LINKS)
			(NULL NC.LINKS))
		    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBSUMER.CANDIDATE
					      (1+ RPTN)))
	     ;; if a literal has no s-link, subsumption is
	     ;; impossible, all marks are removed, no candidates can
	     ;; be returned.
	     (MAPC #'(LAMBDA (NO.MORE.CANDIDATES) (DT-REMPROP NO.MORE.CANDIDATES 'RED*CS_CANDIDATES))
		   CANDIDATES.TO.BE.SUBSUMED)
	     (SETQ CANDIDATES.TO.BE.SUBSUMED NIL) (SETQ RPTN 0))))
    ;; remove marks from all candidates.
    ;; remove clauses from candidate list, which do not
    ;; have s-links for all literals of clause
    ;; 'subsumer.candidate'.
    (DREMAP CANDIDATES.TO.BE.SUBSUMED NIL
	    #'(LAMBDA (CANDIDATES.TAIL)
		(PROG1 (> NOLIT (LIST-LENGTH (DT-GETPROP (CAR CANDIDATES.TAIL) 'RED*CS_CANDIDATES)))
		       (DT-REMPROP (CAR CANDIDATES.TAIL) 'RED*CS_CANDIDATES)))
	    NIL)))

(DEFUN RED=CS_RELATION.MATCH (S.UNIS S.LITERALS SUBSUMER SUBSUMENT CONDITION)
						; edited: 21-may-84 23:10:04
						; input:  'subsumer' and 'subsument' are two clauses.
						;         's.unis' is a list of lists of unifiers
						;         (unis_nolit_of_subsumer, ..., unis_1).
						;         'literals' is a list of numbers of literals
						;         of clause 'subsument', no element appearing
						;         twice (l_nolit_of_subsumer, ..., l_1).
						;         unis_i are the unifiers of a s-link
						;         incident with literal i of clause 'subsumer'
						;         and literal l_i of clause 'subsument'.
						;         'literals' represents the function f.
						; effect: -
						; value:  a list (indicator, subsumer, literals,
						;         unifiers), if the merge of unifiers 's.unis'
						;         is a matcher, only modifying variables of
						;         clause 'subsumer'. 'indicator' is
						;         'red*cs_remove', if clause 'subsument' can
						;           be removed, without inhibition,
						;         'red*cs_inhibit', if for removal the
						;           inhibition is necessary,
						;         'red*cs_recheck', if no removal is possible,
						;           because link condition is met and no
						;           creator link exists or option so is set,
						;           that ihibition is not allowed ('remove').
						;         'literals' = 'literals', 'subsumer' =
						;         'subsumer', and 'unifiers' is a list of
						;         matchers obtained by merging 's.unis'.
  (let (MATCHER.MERGE SUBSUMPTION)
    (COND ((SETQ MATCHER.MERGE (UNI-MERGE.LIST.OF.MATCHERLISTS.fit.on.sorts S.UNIS (DS-CLAUSE.VARIABLES SUBSUMENT) NIL))
	   (CASE CONDITION
	     ((T) (SETQ SUBSUMPTION (LIST 'RED*CS_REMOVE SUBSUMER S.LITERALS MATCHER.MERGE)))
	     (INHIBIT
	       (COND ((CG-CLAUSE_CREATOR.UNIFIER SUBSUMENT)
		      (SETQ SUBSUMPTION (LIST 'RED*CS_INHIBIT SUBSUMER S.LITERALS MATCHER.MERGE)))
		     (T (SETQ SUBSUMPTION (LIST 'RED*CS_RECHECK SUBSUMER)))))
	     (REMOVE-INHIBIT
	       (COND ((RED.LC-CLAUSE.SUBSUMPTION S.LITERALS SUBSUMER SUBSUMENT MATCHER.MERGE)
		      (SETQ SUBSUMPTION (LIST 'RED*CS_REMOVE SUBSUMER S.LITERALS MATCHER.MERGE)))
		     ((CG-CLAUSE_CREATOR.UNIFIER SUBSUMENT)
		      (SETQ SUBSUMPTION (LIST 'RED*CS_INHIBIT SUBSUMER S.LITERALS MATCHER.MERGE)))
		     (T (SETQ SUBSUMPTION (LIST 'RED*CS_RECHECK SUBSUMER)))))
	     (REMOVE
	       (COND ((RED.LC-CLAUSE.SUBSUMPTION S.LITERALS SUBSUMER SUBSUMENT MATCHER.MERGE)
		      (SETQ SUBSUMPTION (LIST 'RED*CS_REMOVE SUBSUMER S.LITERALS MATCHER.MERGE)))
		     (T (SETQ SUBSUMPTION (LIST 'RED*CS_RECHECK SUBSUMER)))))
	     (OTHERWISE (ERROR "red-check - illegal condition in red=cs_attempt.to.subsume: : ~a" CONDITION)))))
    SUBSUMPTION))

(DEFUN RED=CS_RELATION.INJECTIVE (NOLIT LITNO S.UNIS LITERALS SUBSUMING SUBSUMED CONDITION)
						; edited: 21-may-84 22:41:09
						; input:  'subsumer' and 'subsument' are two clauses,
						;         'nolit' is the number of literals of clause
						;         'subsumer', 'litno' the number of one of
						;         its literals_ 1 <= 'litno' <= 'nolit'.
						;         's.unis' is a list of lists of unifiers.
						;         'literals' is a list of numbers of literals
						;         of clause 'subsument'.
						; invariant of recursion: 's.unis' =
						;         (unis_'nolit'-'litno', ..., unis_1), where
						;         'unis_i' are the unifiers of a s-link of the
						;         ith literal of 'subsumer' to a literal of
						;         'subsument'. for an oriented s-link
						;         'subsumer' must be the positive parent.
						;         'literals' (l_'nolit'-'litno', ..., l_1)
						;         represents the function f introduced in
						;         'red=cs_attempt.to.subsume', so that f(i) =
						;         l_i.
						; effect: -
						; value:  a list (indicator, subsumer, literals,
						;         unifiers), if the merge of unifiers 's.unis'
						;         for all literals of clause 'subsumer'
						;         (unis_'nolit', ..., unis_1) is a matcher
						;         only changing variables of 'subsumer'.
						;         'indicator' is one of the atoms
						;         'red*cs_remove', 'red*cs_inhibit', and
						;         'red*cs_recheck', depending on option,
						;         'subsumer' = 'subsumer', 'literal'
						;         represents f, so that f(i) = l_i. 'unifiers'
						;         is a list of matchers, obtained by the merge
						;         of 's.unis'.
  (PROG (RESULT OTHERLITNO NEXT.RESULT)
	(SETQ LITERALS (CONS NIL LITERALS))
	;; initialize literal and unifier for 'litno'th
	;; literal.
	(SETQ S.UNIS (CONS NIL S.UNIS))
	(MEMBER-IF
	  #'(LAMBDA (NC.LINKS)
	      (MEMBER-IF
		#'(LAMBDA (NC.LINK)
		    ;; link between the two clauses, injectivity of
		    ;; function f, and orientation of the link.
		    (when (AND (not (ds-link.sort.inhibited nc.link))
			       (EQL SUBSUMED (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK SUBSUMING))
			       #|(implies (ds-clause.lit.is.max
					  subsumed (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.EXTERNAL NC.LINK SUBSUMING)))
					(ds-clause.lit.is.max subsuming litno))|#
			       (NOT (MEMBER (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.EXTERNAL NC.LINK SUBSUMING)) (CDR LITERALS)))
			       (OR (EQL SUBSUMING (DS-LINK.POSPAR NC.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK))))
		      (RPLACA S.UNIS (DS-LINK.UNIFIERS NC.LINK))
		      (RPLACA LITERALS OTHERLITNO)
		      (SETQ NEXT.RESULT
			    (COND ((EQL NOLIT LITNO)
				   ;; ancre of recursion --> construction of result.
				   (RED=CS_RELATION.MATCH S.UNIS LITERALS SUBSUMING SUBSUMED CONDITION))
				  (T	;; recursive call.
				   (RED=CS_RELATION.INJECTIVE NOLIT (1+ LITNO) S.UNIS LITERALS SUBSUMING SUBSUMED
							      CONDITION))))
		      (COND
			((OR (NULL RESULT) (MEMBER (CAR NEXT.RESULT) '(RED*CS_REMOVE RED*CS_INHIBIT)))
			 ;; store information about combination of literal, that
			 ;; will be used for result up to now.
			 (SETQ RESULT NEXT.RESULT)))
		      ;; only if result leads to a removal without inhibition
		      ;; no more other combination of literals is considered.
		      (EQL (CAR RESULT) 'RED*CS_REMOVE)))
		NC.LINKS))
	  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBSUMING LITNO))
	(RETURN RESULT)))



(DEFUN RED=APPLY_LINK.INCOMPATIBILITY.TO.LINK (LINK)
						; edited:  2-aug-84 16:37:56
						; input:  'link' is a link.
						; effect: if 'link' is an operation link (s, r, p)
						;         it is checked the compatibility of its
						;         unifiers in both parent clauses.
						;         incompatible unifiers will be removed.
						; value:  nil.
						; remark: definition of incompatibility in
						;         'red=li_to.op.link'.
  (when (and (MEMBER (DS-LINK.COLOUR LINK) (RDS-LINK.COLOURS 'OPERATION))
	     (not (opt-is.completion)))
    (RED=LI_TO.INSERTED.LINK LINK (RED.SERVICE-LINK.NEGPAR LINK T) (DS-LINK.NEGLITNO LINK))
    (COND
      ((NOT (MEMBER LINK (CG-LINKS (DS-LINK.COLOUR LINK) REMOVED)))
       (RED=LI_TO.INSERTED.LINK LINK (DS-LINK.POSPAR LINK) (DS-LINK.POSLITNO LINK))))))

(DEFUN RED=APPLY_LINK.INCOMPATIBILITY.TO.CLAUSE (CLAUSE)
						; edited:  2-aug-84 16:40:00
						; input:  'clause' is a clause.
						; effect: incompatible unifiers of links incident with
						;         clause 'clause' will be removed
						;         (incompatibility relative to 'clause').
						; value:  nil.
						; remark: definition of incompatibility in
						;         'red=li_to.op.link'.
  (when (not (opt-is.completion))
    (RED.SERVICE-RENAME.POINT)
    (MAPC #'(LAMBDA (LITNOS)
	      (COND
		((CDR LITNOS)
		 (WHILE
		   (MEMBER-IF
		     #'(LAMBDA (LITNO1)
			 (MEMBER-IF
			   #'(LAMBDA (OP.LINKS)
			       (MEMBER-IF
				 #'(LAMBDA (OP.LINK)
				     (RED=LI_TO.OP.LINK OP.LINK (DS-LINK.COLOUR OP.LINK) CLAUSE LITNO1 LITNOS))
				 OP.LINKS))
			   (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION) CLAUSE LITNO1)))
		     LITNOS)))))
	  (RED=LI_PARTITION.LITERALS CLAUSE))
    (RED.SERVICE-RENAME.DELETE)
    NIL))

(DEFUN RED=LI_PARTITION.LITERALS (CLAUSE)
						; edited:  9-jul-84 15:50:21
						; input:  'clause' is a clause.
						; effect: -
						; value:  list of all sets of literal numbers for
						;         clause 'clause'
						;         (s1 ... sn), so that
						;         (1) si and sj are disjunct.
						;         (2) variables(si) and variables(sj) are
						;           disjunct.
						;         (3) n maximal.
  
  (PROG ((PARTITION NIL) (LITNO.SET NIL) PART VARIABLE.COVER)
	;; 'litno.set' becomes the list of all literal numbers.
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE)) (SETQ LITNO.SET (CONS (1+ RPTN) LITNO.SET)))
	(WHILE LITNO.SET
	  ;; invariant:
	  ;; for all elements of 'partition': conditions (1) and
	  ;; (2) of 'value' are fulfilled. no literal number
	  ;; occurring in 'partition' is member of 'litno.set'.
	  (SETQ PART (LIST (CAR LITNO.SET)))
	  (SETQ VARIABLE.COVER (DS-CLAUSE.LIT.VARIABLES CLAUSE (CAR LITNO.SET)))
	  (WHILE
	    (MEMBER-IF
	      #'(LAMBDA (OTHERLITNO)
		  (COND
		    ((INTERSECTION (DS-CLAUSE.LIT.VARIABLES CLAUSE OTHERLITNO) VARIABLE.COVER)
		     (SETQ VARIABLE.COVER (UNION (DS-CLAUSE.LIT.VARIABLES CLAUSE OTHERLITNO) VARIABLE.COVER))
		     (DELETE OTHERLITNO LITNO.SET) (SETQ PART (CONS OTHERLITNO PART)))))
	      (CDR LITNO.SET))
	    ;; after 'while' all literal numbers of literals,
	    ;; whose variable sets are not disjunct with the union
	    ;; of variables of 'part' are collected in 'part'
	    ;; (variable cover).
	    )
	  (SETQ LITNO.SET (CDR LITNO.SET)) (SETQ PARTITION (CONS PART PARTITION)))
	(RETURN PARTITION)))

(DEFUN RED=LI_TO.OP.LINK (OP.LINK COLOUR CLAUSE LITNO1 LITNOS)
						; edited:  2-aug-84 14:58:41
						; input:  'op.link' is the operation link which
						;         unifiers will be considered for
						;         incompatibility relative to clause 'clause'.
						;         'colour' is its colour.
						;         'litno1' is the number of the parent literal
						;         of 'op.link' in 'clause'.
						;         'litnos' is a set of literal numbers of
						;         'clause'.
						; effect: if a unifier of 'op.link' is incompatible
						;         relative to 'clause' it will be removed.
						;         meaning of incompatibility:
						;         ---------------------------
						;         a unifier u is incompatible relative to
						;         a set of literals (l) of a clause c
						;         (no variable of such a literal occurs in
						;         the rest of the clause, parent literal of u
						;         is in l).
						;         iff
						;         it exists no unifier merged by unifiers of
						;         links (r, riw, p) of l (for each li in l
						;         one unifier, unifier u for its parent
						;         literal).
						;         merge is done for the restriction on the
						;         variables of c and disjunct variables in
						;         the other side literals of the links.
						;         for option switched on partial it is only
						;         necessary for incompatibility to find one
						;         li in l so that all r-, riw-, and p-links
						;         of this literal are incompatible with u.
						; value:  not nil iff a unifier is removed.
  (LET ((CLAUSE.BUFFER (POP.BUFFER.STACK (RDS-BUFFER)))
	(CONDITION (RDS-RULE 'CONDITION 'RED*LINK.INCOMPATIBILITY))
	DEL)
    (MAPC #'(LAMBDA (OP.UNI)
	      (COND
		((OR (AND (EQL CONDITION 'PARTIAL)
			  (MEMBER-IF #'(LAMBDA (LITNO2)
					 (NOT
					   (OR (EQL LITNO1 LITNO2)
					       (AND (MEMBER COLOUR (RDS-LINK.COLOURS 'OPERATION.INTERNAL))
						    (EQL LITNO2 (RED.SERVICE-OTHERLITNO.INTERNAL OP.LINK LITNO1)))
					       (NULL (INTERSECTION (DS-CLAUSE.LIT.VARIABLES CLAUSE LITNO1)
								   (DS-CLAUSE.LIT.VARIABLES CLAUSE LITNO2)))
					       (RED=LI_COMPATIBLE.LITERAL OP.LINK OP.UNI CLAUSE LITNO2 LITNO1 CLAUSE.BUFFER))))
				     LITNOS))
		     (AND (NOT (EQL CONDITION 'PARTIAL))
			  CONDITION
			  (NOT (RED=LI_MERGE OP.LINK LITNO1 OP.UNI CLAUSE (REMOVE LITNO1 LITNOS) (LIST NIL) CLAUSE.BUFFER))))
		 (RED=CTL_AGENDA.UPDATE
		   (DELETE CLAUSE (RED=CTL_REMOVE.UNIFIER OP.LINK OP.UNI T "incompatible" NIL)) CLAUSE
		   (RDS-RULES 'SUCCESSOR.LINK.INCOMPATIBILITY))
		 (SETQ DEL T)))
	      (BUFFER.RESET CLAUSE.BUFFER))
	  (RED.SERVICE-SWITCHED.UNIFIERS OP.LINK LITNO1 CLAUSE))
    (PUSH.BUFFER.STACK CLAUSE.BUFFER (RDS-BUFFER))
    DEL))

(DEFUN RED=LI_TO.INSERTED.LINK (OP.LINK PARENT.CLAUSE PARENT.LITNO)
						; edited:  2-aug-84 16:33:06
						; input:  'operation.link' is an operation link (si,
						;         r, p).
						;         'parent.clause' and the number of a literal
						;         in this clause 'parent.litno' describe
						;         one of its parent literals.
						; effect: if one of the unifiers of 'operation.link'
						;         is incompatible, this unifier will be
						;         removed.
						; value:  undefined.
						; remark: definition of incompatibility in
						;         'red=li_to.op.link'.
  (MAPC #'(LAMBDA (LITNOS)
	    (COND
	      ((AND (CDR LITNOS) (MEMBER PARENT.LITNO LITNOS))
	       (RED=LI_TO.OP.LINK OP.LINK (DS-LINK.COLOUR OP.LINK) PARENT.CLAUSE PARENT.LITNO LITNOS))))
	(RED=LI_PARTITION.LITERALS PARENT.CLAUSE)))

(DEFUN RED=LI_COMPATIBLE.LITERAL (OP.LINK1 OP.UNI1 CLAUSE LITNO2 LITNO1 CLAUSE.BUFFER)
						; edited:  2-aug-84 16:41:11
						; input:  'op.uni1' is a unifier of the operation
						;         link 'op.link1' to be checked for
						;         incompatibility with option 'partial'.
						;         'oplink1' is incident with literal 'litno1'
						;         in clause 'clause'.
						;         'litno2' is the number of another literal
						;         of this clause.
						; effect: -
						; value:  not nil iff no unifier of r-, riw-, and p-
						;         links of literal 'litno2' in 'clause' is
						;         compatible with 'op.uni1'.
						;         unifier compatibility check (merge) is
						;         done for a restriction on the variables
						;         of 'clause' and disjunct variables in
						;         the other side literals of the links.
  (LET ((CHANGED.OP.UNI1.LISTED
	  (RED=LI_UNI.CHANGE (LIST (COPY-TREE OP.UNI1))
			     (SET-DIFFERENCE (DS-CLAUSE.LIT.VARIABLES CLAUSE LITNO1)
					     (DS-CLAUSE.LIT.VARIABLES (DS-LINK.OTHERPAR OP.LINK1 CLAUSE)
								      (DS-LINK.OTHERLITNO OP.LINK1 CLAUSE LITNO1)))
			     NIL NIL CLAUSE.BUFFER)))
    (MEMBER-IF #'(LAMBDA (RIW.LINKS2)
		   (MEMBER-IF
		     #'(LAMBDA (RIW.LINK2)
			 (UNI-MERGE.SUBSTITUTIONLISTS (RED.SERVICE-SWITCHED.UNIFIERS RIW.LINK2 LITNO2 CLAUSE)
						      CHANGED.OP.UNI1.LISTED NIL))
		     RIW.LINKS2))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'INCOMPATIBILITY.REASON) CLAUSE LITNO2))))

(DEFUN RED=LI_UNI.CHANGE (UNIS VARS &OPTIONAL OTHER.CLAUSE CLAUSE CLAUSE.BUFFER)
						; edited:  2-aug-84 15:48:49
						; input:  'unifiers' is a list of unifiers,
						;         'variables.attached' a set of variables.
						; effect: changes 'unifiers':
						;         components are removed, where variables
						;         are in the domain which are in
						;         'variables.attached'.
						;         variables are renamed by newly created
						;         variables if they are in 'variables.-
						;         attached' and in the codomains of
						;         'unifiers'.
						;         the new (rename) variables are stored so
						;         that they can be removed by the next
						;         'red.service-rename.remove'-call.
						; value:  the so destructively modified unifier list.
  (DECLARE (IGNORE CLAUSE))
  (COND
    ((MEMBER OTHER.CLAUSE (BUFFER.CONTENTS CLAUSE.BUFFER))
     (RED.SERVICE-RENAME (UNI-REMOVE.COMPONENTS UNIS VARS) VARS))
    (T (BUFFER.CONS OTHER.CLAUSE CLAUSE.BUFFER) (UNI-REMOVE.COMPONENTS UNIS VARS))))

(DEFUN RED=LI_MERGE (OP.LINK OP.LITNO OP.UNI CLAUSE LITNOS OP.UNIS2 CLAUSE.BUFFER)
						; edited:  2-aug-84 15:56:33
						; input:  'op.uni' is an unifier of the link 'op.link'
						;         incident with literal 'op.litno' of the
						;         clause 'clause'.
						;         'op.unis2' is a list of unifiers.
						;         'litnos' is a list of numbers of literals
						;         of 'clause'.
						;         invariant of recursion:
						;         'op.unis2' is the merge of unifiers of
						;         different literals not in 'litnos', but in
						;         'litnos' of earlier calls of this function.
						; effect: -
						; value:  not nil iff the merge of 'op.unis2',
						;         'op.uni', and for each literal in 'litnos'
						;         one unifier of a link incident with the
						;         literal is not empty.
						;         merge restricted on variables of 'clause',
						;         and disjunct variablesets of the other
						;         sides of the links.
  (COND
    ((SETQ OP.UNIS2
	   (UNI-MERGE.SUBSTITUTIONLISTS
	     (RED=LI_UNI.CHANGE (LIST (COPY-TREE OP.UNI))
				(SET-DIFFERENCE
				  (UNION (UNI-UNIFIER.CODOMAIN (DS-CLAUSE.RENAMING CLAUSE))
					 (DS-CLAUSE.LIT.VARIABLES (DS-LINK.OTHERPAR OP.LINK CLAUSE)
								  (DS-LINK.OTHERLITNO OP.LINK CLAUSE OP.LITNO)))
				  (DS-CLAUSE.LIT.VARIABLES CLAUSE OP.LITNO))
				(DS-LINK.OTHERPAR OP.LINK CLAUSE) CLAUSE CLAUSE.BUFFER)
	     OP.UNIS2 NIL))
     (OR (NULL LITNOS)
	 (MEMBER-IF #'(LAMBDA (LINKS)
			(MEMBER-IF #'(LAMBDA (LINK)
				       (MEMBER-IF #'(LAMBDA (UNI)
						      (RED=LI_MERGE LINK (CAR LITNOS) UNI CLAUSE
								    (CDR LITNOS) OP.UNIS2 CLAUSE.BUFFER))
						  (RED.SERVICE-SWITCHED.UNIFIERS LINK (CAR LITNOS) CLAUSE)))
				   LINKS))
		    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'INCOMPATIBILITY.REASON) CLAUSE (CAR LITNOS)))))))

(DEFUN RED=APPLY_LINK.TAUTOLOGY.TO.LINK (LINK)
						; edited: 14-jun-84 19:41:20
						; input:  'link' is a link.
						; effect: if colour of 'link' is operational (r, p,
						;         si), it is checked, wether it has tautologic
						;         unifiers.
						;         if not all unifiers of 'link' are
						;         tautologic, colour of it is a link tautology
						;         detection colour (r, rd, t), and recheck-
						;         option is switched on total or partial, it
						;         is checked, wether the operation links
						;         incident with the parent clauses of 'link'
						;         have tautologic unifiers.
						;         if not all unifiers of 'link' are
						;         tautologic, colour of it is colour
						;         for compliance of link condition (r, riw;
						;         colour of bridge-link), and recheck-option
						;         is switched on total, it is checked, wether
						;         after insertion of 'link' link condition
						;         is met, where it failed before.
						;         if a unifier is detected as tautologic, it
						;         will be removed or inhibited, depending on
						;         the option. links not complying with the
						;         link condition and not inhibited will
						;         be inserted into recheck information list of
						;         this rule.
						; value:  nil.
						; remark: unifier tautology and use of the option are
						;         explained in 'red=lt_attempt.to.remove'.
  (PROG
    ((CONDITION (RDS-RULE 'CONDITION 'RED*LINK.TAUTOLOGY))
     (RECHECK (RDS-RULE 'RECHECK 'RED*LINK.TAUTOLOGY))
     (COLOUR (DS-LINK.COLOUR LINK)))
    (COND ((MEMBER COLOUR (RDS-LINK.COLOURS 'OPERATION)) (RED=LT_TO.OP.LINK LINK CONDITION)))
    (COND
      ((NOT (MEMBER LINK (CG-LINKS COLOUR 'REMOVED)))
       (COND
	 ((MEMBER COLOUR (RDS-LINK.COLOURS 'TAUTOLOGY.DETECTION))
	  (COND (RECHECK (RED=LT_RECHECK.INSERT.DETECTION.LINK LINK CONDITION)))))
       (COND
	 ((AND (MEMBER COLOUR (RDS-LINK.COLOURS 'TAUTOLOGY.CONDITION)) (EQL RECHECK T))
	  (RED=LT_RECHECK.INSERT.CONDITION.LINK LINK CONDITION)))))))

(DEFUN RED=APPLY_LINK.TAUTOLOGY.TO.CLAUSE (CLAUSE)
						; edited: 14-jun-84 21:44:29
						; input:  'clause' is a clause.
						; effect: tautologic unifiers of operation links (r,
						;         p, si) incident with clause 'clause' will
						;         be removed or inhibited according to the
						;         option.
						;         links not complying with the link condition
						;         and not inhibited will be inserted into the
						;         recheck information list of this rule.
						; value:  nil.
						; remark: unifier tautology and use of the option are
						;         described in 'red=lt_attempt.to.remove'.
  (PROG ((CONDITION (RDS-RULE 'CONDITION 'RED*LINK.TAUTOLOGY)))
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (PROGN
	    (MAPC #'(LAMBDA (OP.LINKS)
		      (MAPC #'(LAMBDA (OP.LINK) (RED=LT_TO.OP.LINK OP.LINK CONDITION)) OP.LINKS))
		  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.EXTERNAL) CLAUSE (1+ RPTN)))
	    (MAPC #'(LAMBDA (OPI.LINKS)
		      (MAPC #'(LAMBDA (OPI.LINK)
				(COND
				  ((< (1+ RPTN) (RED.SERVICE-OTHERLITNO.INTERNAL OPI.LINK (1+ RPTN)))
				   (RED=LT_TO.OP.LINK OPI.LINK CONDITION))))
			    OPI.LINKS))
		  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.INTERNAL) CLAUSE (1+ RPTN)))))))

(DEFUN RED=APPLY_LINK.TAUTOLOGY.TO.RECHECK (RECHECK)
						; edited: 14-jun-84 21:13:35
						; input:  'recheck' is a link removed from the
						;         connectiongraph, but still in the data.
						; effect: if after removal of link 'recheck' link
						;         condition is fulfilled for a tautologic
						;         unifier of a link in the recheck information
						;         list of link tautology rule, it will be
						;         removed according to the options (recheck
						;         and condition).
						;         recheck information list is updated before
						;         the check.
						; value:  nil.
						; remark: unifier tautology and use of the option are
						;         described in 'red*ct_attempt.to.remove'.
  (PROG ((RECHECK.PARENTS (RED.SERVICE-LINK.PARENTS RECHECK))) (RED=LT_UPDATE.RECHECK.INFO)
	(MAPC #'(LAMBDA (TAUTOLOGY.LINK)
		  (COND
		    ((INTERSECTION (RED.SERVICE-LINK.PARENTS TAUTOLOGY.LINK) RECHECK.PARENTS)
		     (RED=LT_TO.OP.LINK TAUTOLOGY.LINK (RDS-RULE 'CONDITION 'RED*LINK.TAUTOLOGY)))))
	      (RDS-RULE 'RECHECK.INFO 'RED*LINK.TAUTOLOGY))))

(DEFUN RED=LT_TO.OP.LINK (LINK CONDITION)
						; edited: 10-jul-84 10:46:42
						; input:  'link' is a link with operational colour
						;         (p, r, si). 'condition' is the option,
						;         i.e. one of the atoms 't', 'remove',
						;         'inhibit', and 'remove-inhibit'.
						; effect: tautologic unifiers of link 'link' will
						;         be removed or inhibited according to
						;         'condition'. information about not inhibited
						;         or removed link, which has tautologic
						;         unifiers without link condition is inserted
						;         into recheck information list of rule
						;         link tautology.
						; value:  undefined.
						; remark: unifier tautology and use of 'condition'
						;         are explained in 'red=ls_attempt.to.remove'.
  (let ((POSPAR (DS-LINK.POSPAR LINK)) NEGPAR POSLITNO NEGLITNO)
    (COND ((MEMBER (DS-LINK.COLOUR LINK) (RDS-LINK.COLOURS 'OPERATION.EXTERNAL))
	   (SETQ NEGPAR (DS-LINK.NEGPAR LINK))
	   (SETQ POSLITNO (DS-LINK.POSLITNO LINK))
	   (SETQ NEGLITNO (DS-LINK.NEGLITNO LINK))))
    (unless (opt-is.kz.completion)
      (MAPC #'(LAMBDA (UNI)
		(RDS-BINDING.PUSH UNI)
		(RED=LT_ATTEMPT.TO.REMOVE (RED=LT_CANDIDATE.LINKS POSPAR NEGPAR POSLITNO NEGLITNO) POSPAR POSLITNO
					  NEGPAR NEGLITNO LINK UNI CONDITION)
		(RDS-BINDING.POP))
	    (DS-LINK.UNIFIERS LINK)))
    (when (or (and (eq (ds-link.colour link) 'p) (not (member link (cg-links 'p removed))))
	      (and (eq (ds-link.colour link) 'piw) (not (member link (cg-links 'piw removed))))
	      (and (or (opt-is.kz.completion) (opt-is.with.residues)) (eq (ds-link.colour link) 'r)))
      (let* ((TERMLIST (ds-link.result link)))
	(when (some #'(lambda (lit1)
			(some #'(lambda (lit2)
				  (and (not (eq lit1 lit2))
				       (not (eq (ds-lit.sign lit1) (ds-lit.sign lit2)))
				       (eql (ds-lit.predicate lit1) (ds-lit.predicate lit2))
				       (or (equal (ds-lit.termlist lit1) (ds-lit.termlist lit2))
					   (and (dt-predicate.is.symmetric (ds-lit.predicate lit1))
						(equal (ds-lit.termlist lit1) (reverse (ds-lit.termlist lit2)))))))
			      termlist))
		    termlist)
	  (red=ctl_remove.link link "Tautology" nil))))))

(DEFUN RED=LT_RECHECK.INSERT.CONDITION.LINK (LINK CONDITION)
						; edited: 10-jul-84 11:22:11
						; input:  'link' is a link with colour of bridge links
						;         for tautology link condition (r, riw).
						;         'condition' is the option, i.e. one of the
						;         atoms 't', 'remove', 'inhibit', and
						;         'remove-inhibit'.
						; effect: updates the recheck information list for
						;         link tautology rule, containing links
						;         with tautologic unifiers without link
						;         condition and not inhibited.
						;         if an entry link of recheck information
						;         list has an adjacent link also adjacent
						;         with link 'link', this entry link will be
						;         considered again for tautologic unifiers,
						;         according to the option 'condition'.
						; value:  undefined.
						; remark: unifier tautology and use of the option are
						;         explained in 'red=lt_attempt.to.remove'.
  (PROG
    ((POSPAR.INSERT (DS-LINK.POSPAR LINK)) (NEGPAR.INSERT (RED.SERVICE-LINK.NEGPAR LINK T))
     (POSLITNO.INSERT (DS-LINK.POSLITNO LINK)) (NEGLITNO.INSERT (DS-LINK.NEGLITNO LINK)) TAUTOLOGY.PARENTS)
    (RED=LT_UPDATE.RECHECK.INFO)
    (MAPC #'(LAMBDA (TAUTOLOGY.LINK)
	      (COND
		((AND (SETQ TAUTOLOGY.PARENTS (RED.SERVICE-LINK.PARENTS TAUTOLOGY.LINK))
		      (MEMBER-IF
			#'(LAMBDA (R.LINKS)
			    (MEMBER-IF
			      #'(LAMBDA (R.LINK) (MEMBER (DS-LINK.OTHERPAR R.LINK POSPAR.INSERT) TAUTOLOGY.PARENTS))
			      R.LINKS))
			(RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RESOLUTION.ALL) POSPAR.INSERT POSLITNO.INSERT))
		      (MEMBER-IF
			#'(LAMBDA (R.LINKS)
			    (MEMBER-IF
			      #'(LAMBDA (R.LINK) (MEMBER (DS-LINK.OTHERPAR R.LINK NEGPAR.INSERT) TAUTOLOGY.PARENTS))
			      R.LINKS))
			(RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RESOLUTION.ALL) NEGPAR.INSERT NEGLITNO.INSERT)))
		 (RED=LT_TO.OP.LINK TAUTOLOGY.LINK CONDITION))))
	  (RDS-RULE 'RECHECK.INFO 'RED*LINK.TAUTOLOGY))))

(DEFUN RED=LT_RECHECK.INSERT.DETECTION.LINK (LINK CONDITION)
						; edited:  6-jul-84 01:02:04
						; input:  'link' is a link with link tautology
						;         detection colour (ti, r, rd, t).
						;         'condition' is one of the atoms 't',
						;         'remove', 'inhibit', and 'remove-inhibit'.
						; effect: all operation links of clauses incident
						;         with link 'link' are considered to have
						;         tautologic unifiers with 'link' as
						;         detection link.
						; value:  undefined.
						; remark: unifier tautology and use of 'condition'
						;         are explained in 'red=lt_attempt.to.remove'.
  (MAPC #'(LAMBDA (PARENT)
	    (DODOWN (RPTN (DS-CLAUSE.NOLIT PARENT))
	      (MAPC #'(LAMBDA (OP.LINKS)
			(MAPC #'(LAMBDA (OP.LINK) (RED=LT_TO.OP.LINK OP.LINK CONDITION)) OP.LINKS))
		    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION) PARENT (1+ RPTN)))))
	(RED.SERVICE-LINK.PARENTS LINK)))

(DEFUN RED=LT_ATTEMPT.TO.REMOVE (DETECTION.LINKS CLAUSE1 LITNO1 CLAUSE2 LITNO2 LINK UNI CONDITION)
						; edited: 29-may-84 04:08:13
						; input:  'candidate.links' is a list of ti-, t-, r-,
						;         and rd-links with nil-unifier under the
						;         actual binding.
						;         'condition' is one of the atoms 't',
						;         'remove', 'inhibit', and 'remove-inhibit'.
						;         'litnoi' is the number of a literal of
						;         clause 'clausei', if 'link' is a r- or
						;         p-link, where 'clause1' and 'clause2' are
						;         different.
						;         if 'link' is a si-link, 'clause2' as
						;         'litno1' and 'litno2' are nil; clause
						;         'clause1' is incident with 'link'
						;         'uni' is one of the unifiers of 'link' and
						;         the actual binding.
						; effect: if 'detection.links' is not nil, 'uni' will
						;         be inhibited or removed according to
						;         meaning of condition:
						;         ---------------------
						;         't': (1) is sufficient for removal. (this
						;           case is not used in this implementation of
						;           the options.)
						;         'remove': (1) and (2) must be fulfilled for
						;           removal.
						;         'inhibit': if (1) is fulfilled, 'uni' will
						;           be inhibited.
						;         'remove-inhibit': if (1) and (2) are
						;           performed, 'uni' will be removed; if only
						;           (1) is met, 'uni' is inhibited.
						;         meaning of link tautology:
						;         --------------------------
						;         a unifier u of a r- or p-link l is
						;         tautologic iff
						;         (1) there exists a ti-link, incident with
						;           one of the parent clauses but not with a
						;           parent literal of l
						;           or
						;           there exists a r- or rd-link with rule
						;           symmetric or nil or a t-link, incident
						;           with the parent clauses but not with a
						;           parent literal of l,
						;           so that merging of the unifiers of this
						;           link and u leads to the identical
						;           substitution.
						;         (2) link condition as described in
						;           'red=lt_link.condition' is fulfilled.
						;         a unifier u of a si-link l is a tautology
						;         iff
						;         (1) there exists a ti-link, incident with
						;           the same clause as l, so that merging of
						;           the unifiers of this ti-link and u leads
						;           to the nil-unifier.
						;         (2) link condition is met too.
						;
						;         if (1) is fulfilled, but 'uni' neither can
						;         be inhibited nor removed, it will be
						;         inserted into recheck information list of
						;         rule 'red*lt_link.tautology'.
						;         snowball effect is considered as described
						;         in 'red=ctl_agenda.update'.
						; value:  Undefined.
  (DECLARE (IGNORE LITNO1 LITNO2 CLAUSE1 CLAUSE2))
  (COND
    (DETECTION.LINKS
     (PROG ((LINK.CONDITION NIL))
	   (COND
	     ((MEMBER CONDITION '(REMOVE-INHIBIT REMOVE))
	      (SETQ LINK.CONDITION
		    (MEMBER-IF
		      #'(LAMBDA (RECOGNIZE.LINK)
			  (RED.LC-LINK.TAUTOLOGY (DS-LINK.POSPAR RECOGNIZE.LINK) (DS-LINK.POSLITNO RECOGNIZE.LINK)
						 (RED.SERVICE-LINK.NEGPAR RECOGNIZE.LINK T)
						 (DS-LINK.NEGLITNO RECOGNIZE.LINK)
						 (CONS LINK UNI)))
		      DETECTION.LINKS))))
	   (CASE CONDITION
	     (REMOVE-INHIBIT
	       (COND
		 (LINK.CONDITION
		  (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.UNIFIER LINK UNI t "Tautology" NIL) NIL NIL))
		 (T (CG-INHIBIT.UNIFIER LINK UNI 'LINK.TAUTOLOGY NIL))))
	     (INHIBIT (CG-INHIBIT.UNIFIER LINK UNI 'LINK.TAUTOLOGY NIL))
	     (REMOVE
	       (COND
		 (LINK.CONDITION
		  (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.UNIFIER LINK UNI t "Tautology" NIL) NIL NIL))
		 (T
		  (RDS-RULE.PUT 'RECHECK.INFO 'RED*LINK.TAUTOLOGY
				   (CONS LINK (RDS-RULE 'RECHECK.INFO 'RED*LINK.TAUTOLOGY))))))
	     ((T) (RED=CTL_AGENDA.UPDATE (RED=CTL_REMOVE.UNIFIER LINK UNI t "Tautology" NIL) NIL NIL))
	     (OTHERWISE (ERROR "red-check - illegal condition in red=lt_attempt.to.remove: : ~a" CONDITION)))))))

(DEFUN RED=LT_CANDIDATE.LINKS (C1 C2 L1 L2)
						; edited:  6-jul-84 01:14:25
						; input:  'litnoi' is the number of a literal of
						;         'clausei' (i = 1, 2).
						;         'clause2' can be nil.
						;         if 'clause2' is nil 'litno1' and 'litno2'
						;         are nil too.
						; effect: -
						; value:  list of all tautology detection links
						;         incident with input clauses but not with
						;         input literals. links have a nil-unifier
						;         and the following colours:
						;         t, ti;
						;         r, rd with the r-iff-t property ( iff there
						;         is a r-link there must be a t-link which
						;         is not implemented).
  (PROG
    ((CANDIDATE.LINKS NIL) (NOLIT1 (DS-CLAUSE.NOLIT C1)) (NOLIT2 (COND (C2 (DS-CLAUSE.NOLIT C2)))) OTHERLITNO)
    (DODOWN (RPTN NOLIT1)
      (COND
        ((NEQ (1+ RPTN) L1)
	 (MAPC #'(LAMBDA (I.LINKS)
		   (MAPC #'(LAMBDA (I.LINK)
			     (COND
			       ((AND
				  (< (1+ RPTN) (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.INTERNAL I.LINK (1+ RPTN))))
				  (NEQ L1 OTHERLITNO) (RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS I.LINK)))
				(SETQ CANDIDATE.LINKS (CONS I.LINK CANDIDATE.LINKS)))))
			 I.LINKS))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'TAUTOLOGY.DETECTION.INTERNAL) C1 (1+ RPTN)))
	 (COND
	   (C2
	    (MAPC #'(LAMBDA (E.LINKS)
		      (MAPC #'(LAMBDA (E.LINK)
				(COND
				  ((AND (EQL C2 (RED.SERVICE-OTHERPAR.EXTERNAL E.LINK C1))
					(NEQ L2 (RED.SERVICE-OTHERLITNO.EXTERNAL E.LINK C1))
					(OR (NOT (MEMBER (DS-LINK.COLOUR E.LINK) (RDS-LINK.COLOURS 'R.IFF.T)))
					    (DS-RULE.R.IFF.T (DS-LINK.RULE E.LINK)))
					(RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS E.LINK)))
				   (SETQ CANDIDATE.LINKS (CONS E.LINK CANDIDATE.LINKS)))))
			    E.LINKS))
		  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'TAUTOLOGY.DETECTION.EXTERNAL) C1 (1+ RPTN))))))))
    (COND
      (C2
       (DODOWN (RPTN NOLIT2)
	 (COND
	   ((NEQ (1+ RPTN) L2)
	    (MAPC #'(LAMBDA (I.LINKS)
		      (MAPC #'(LAMBDA (I.LINK)
				(COND
				  ((AND
				     (< (1+ RPTN) (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.INTERNAL I.LINK (1+ RPTN))))
				     (NEQ L2 OTHERLITNO) (RED.SERVICE-NIL.IN.UNIFIERS (DS-LINK.UNIFIERS I.LINK)))
				   (SETQ CANDIDATE.LINKS (CONS I.LINK CANDIDATE.LINKS)))))
			    I.LINKS))
		  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'TAUTOLOGY.DETECTION.INTERNAL) C2 (1+ RPTN))))))))
    (RETURN CANDIDATE.LINKS)))

(DEFUN RED=LT_UPDATE.RECHECK.INFO NIL
						; edited:  6-jul-84 00:38:04
						; input:  -
						; effect: links in the recheck information list of
						;         link tautology rule (containing links
						;         with tautologic unifiers without link
						;         condition) are removed, if they are removed
						;         from connection graph or memory.
						; value:  undefined.
  (RDS-RULE.PUT 'RECHECK.INFO 'RED*LINK.TAUTOLOGY
		   (DREMAP (RDS-RULE 'RECHECK.INFO 'RED*LINK.TAUTOLOGY) NIL
			   #'(LAMBDA (TAUTOLOGY.LINK) (SETQ TAUTOLOGY.LINK (CAR TAUTOLOGY.LINK))
				     (OR (NOT (DS-LINK.IS TAUTOLOGY.LINK))
					 (MEMBER TAUTOLOGY.LINK (CG-LINKS (DS-LINK.COLOUR TAUTOLOGY.LINK) REMOVED))))
			   NIL)))


;;; Link subsumption
;;; ================

(DEFUN RED=APPLY_LINK.SUBSUMPTION.SUBJECT.TO.CLAUSE (CLAUSE)
						; edited: 19-jun-84 16:22:27
						; input:  'clause' is a clause.
						; effect: if an unifier of an operation link(r, p, si)
						;         is subsumed by clause 'clause', this
						;         unifier will be removed or inhibited
						;         according to the option.
						; value:  nil.
						; remark: meaning of subsumption of unifiers and use
						;         of the option are explained in
						;         'red=ls_attempt.to.subsume'.
  (RED=LS_OWN.PARENT.TO.CLAUSE CLAUSE (RDS-RULE 'CONDITION 'RED*LINK.SUBSUMPTION.SUBJECT))
  (MAPC #'(LAMBDA (LINK)
	    (RED=LS_ATTEMPT.TO.SUBSUME LINK CLAUSE (RDS-RULE 'CONDITION 'RED*LINK.SUBSUMPTION.SUBJECT)))
	(RED=LS_CANDIDATES.TO.BE.SUBSUMED CLAUSE))
  (red=ls_p.link.new clause)
  NIL)

(DEFUN RED=APPLY_LINK.SUBSUMPTION.OBJECT.TO.CLAUSE (CLAUSE)
						; edited: 19-jun-84 16:38:46
						; input:  'clause' is a clause.
						; effect: if one of the unifiers of operation links
						;         (r, p, si) incident with clause 'clause'
						;         is subsumed, it will be removed or inhibited
						;         according to the option.
						; value:  nil.
						; remark: meaning of subsumption of unifiers and use
						;         of the option are explained in
						;         'red=ls_attempt.to.subsume'.
  (PROG ((CONDITION (RDS-RULE 'CONDITION 'RED*LINK.SUBSUMPTION.OBJECT)))
	(DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
	  (PROGN
	    (MAPC #'(LAMBDA (OP.LINKS)
		      (MAPC #'(LAMBDA (OP.LINK)
				(RED=LS_TO.EXT.LINK OP.LINK CONDITION))
			    OP.LINKS))
		  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.EXTERNAL) CLAUSE (1+ RPTN)))
	    (MAPC #'(LAMBDA (OPI.LINKS)
		      (MAPC #'(LAMBDA (OPI.LINK) (RED=LS_TO.INT.LINK OPI.LINK CONDITION)) OPI.LINKS))
		  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.INTERNAL) CLAUSE (1+ RPTN)))))))

(DEFUN RED=APPLY_LINK.SUBSUMPTION.SUBJECT.TO.LINK (LINK)
						; edited: 19-jun-84 16:30:58
						; input:  'link' is a link.
						; effect: if insertion of link 'link' leads to the
						;         subsumption of unifiers of other links, they
						;         will be removed or inhibited depending on
						;         the options (condition and recheck).
						;         two possibilities are given:
						;         if 'link' has a colour attaching the link
						;         condition for a subsumer-subsument pair
						;         stored in the recheck information list and
						;         recheck is switched on total, it will be
						;         checked again for subsumption.
						;         if 'link' has a subsumption colour and
						;         recheck is switched on total or partial, the
						;         links of its parents will be checked to be
						;         subsumed by the other parent.
						; value:  nil.
						; remark: meaning of subsumption of unifiers and use
						;         of the option are explained in
						;         'red=ls_attempt.to.subsume'.
  (PROG
    ((CONDITION (RDS-RULE 'CONDITION 'RED*LINK.SUBSUMPTION.SUBJECT))
     (RECHECK (RDS-RULE 'RECHECK 'RED*LINK.SUBSUMPTION.SUBJECT)) (COLOUR (DS-LINK.COLOUR LINK)))
    (COND
      ((MEMBER COLOUR (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION))
       (COND ((EQL RECHECK T) (RED=LS_RECHECK.INSERT.CONDITION.LINK LINK CONDITION)))))
    (COND
      ((AND RECHECK (MEMBER COLOUR (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL)))
       (COND ((RED=LS_TO.SUBSUMPTION.LINK (DS-LINK.POSPAR LINK) (DS-LINK.NEGPAR LINK) CONDITION))
	     ((RED=LS_OWN.PARENT.TO.LITERAL (DS-LINK.NEGPAR LINK) (DS-LINK.NEGLITNO LINK) CONDITION))
	     ((RED.SERVICE-LINK.ORIENTED LINK))
	     ((RED=LS_TO.SUBSUMPTION.LINK (DS-LINK.NEGPAR LINK) (DS-LINK.POSPAR LINK) CONDITION))
	     ((RED=LS_OWN.PARENT.TO.LITERAL (DS-LINK.POSPAR LINK) (DS-LINK.POSLITNO LINK) CONDITION)))))))

(DEFUN RED=APPLY_LINK.SUBSUMPTION.OBJECT.TO.LINK (LINK)
						; edited: 19-jun-84 16:25:07
						; input:  'link' is a link.
						; effect: if link 'link' has operational colour (r, p,
						;         si) and one of its unifiers is subsumed,
						;         this unifier will be removed or inhibited
						;         according to the option.
						; value:  nil.
						; remark: meaning of subsumption of unifiers and use
						;         of the option are explained in
						;         'red=ls_attempt.to.subsume'.
  (let ((CONDITION (RDS-RULE 'CONDITION 'RED*LINK.SUBSUMPTION.OBJECT))
	(COLOUR (DS-LINK.COLOUR LINK)))
    (COND ((MEMBER COLOUR (RDS-LINK.COLOURS 'OPERATION.EXTERNAL))
	   (RED=LS_TO.EXT.LINK LINK CONDITION))
	  ((MEMBER COLOUR (RDS-LINK.COLOURS 'OPERATION.INTERNAL))
	   (RED=LS_TO.INT.LINK LINK CONDITION)))
    nil))

(DEFUN RED=APPLY_LINK.SUBSUMPTION.SUBJECT.TO.RECHECK (RECHECK)
						; edited: 19-jun-84 16:05:05
						; input:  'recheck' is a link removed from the
						;         connectiongraph, but still in memory.
						; effect: if colour of link 'recheck' is link
						;         condition relevant, recheck information list
						;         of rule 'red*link.subsumption.subject' is
						;         updated and
						;         all subsumer-subsument pairs in the
						;         information list, where the subsument link
						;         is adjacent with our link 'recheck' are
						;         checked again for subsumption, if recheck-
						;         option is switched on total.
						; value:  nil.
						; remark: meaning of subsumption of unifiers and use
						;         of the option are explained in
						;         'red=ls_attempt.to.subsume'.
  (COND
    ((MEMBER (DS-LINK.COLOUR RECHECK) (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION))
     (PROG ((PARENTS (RED.SERVICE-LINK.PARENTS RECHECK)) SUBSUMER.SUBSUMENT.PARENTS) (RED=LS_UPDATE.RECHECK.INFO)
	   (MAPC #'(LAMBDA (SUBSUMER.SUBSUMENT)
		     (COND
		       ((INTERSECTION
			  (SETQ SUBSUMER.SUBSUMENT.PARENTS (RED.SERVICE-LINK.PARENTS (CDR SUBSUMER.SUBSUMENT))) PARENTS)
			(COND
			  ((MEMBER (CAR SUBSUMER.SUBSUMENT) SUBSUMER.SUBSUMENT.PARENTS)
			   (RED=LS_OWN.PARENT.TO.SUBSUMER.SUBSUMENT (CAR SUBSUMER.SUBSUMENT)
								    (CDR SUBSUMER.SUBSUMENT)
								    (RDS-RULE 'CONDITION 'RED*LINK.SUBSUMPTION.SUBJECT)))
			  (T
			   (RED=LS_ATTEMPT.TO.SUBSUME (CDR SUBSUMER.SUBSUMENT) (CAR SUBSUMER.SUBSUMENT)
						      (RDS-RULE 'CONDITION 'RED*LINK.SUBSUMPTION.SUBJECT)))))))
		 (RDS-RULE 'RECHECK.INFO 'RED*LINK.SUBSUMPTION.SUBJECT))))))

(DEFUN RED=LS_ATTEMPT.TO.SUBSUME (OPERATION.LINK SUBSUMER C)
						; edited: 19-jun-84 16:42:14
						; input:  'operation.link' is a link with operational
						;         colour (r, p, si). 'subsumer' is a clause
						;         not incident with 'operation.link'.
						;         'condition' is one of the atoms 't',
						;         'inhibit', 'remove', and 'remove-inhibit'.
						; effect: meaning of 'condition':
						;         -----------------------
						;         't': if (1) - (3) of meaning of unifier
						;           subsumption are fulfilled for 'subsumer'
						;           and an unifier of 'operation.link',
						;           this unifier will
						;           removed from connectiongraph.
						;         'remove': if (1) - (4) are fulfilled,
						;           the unifier will be removed.
						;         'inhibit': if (1) - (3) are fulfilled the
						;           unifier will be inhibited.
						;         'remove-inhibit': if (1) - (4) are fulfilled
						;           the unifier will be
						;           removed, but if (4) fails
						;           and (1) - (3) are fulfilled, it
						;           will be handled as under 'inhibit'.
						;         meaning of subsumption:
						;         -----------------------
						;         a unifier u is subsumed by a clause c iff:
						;         (1) for the literals exists an injective
						;           function f as illustrated:
						;                  -----------------
						;                  !   !   !   !   !       c
						;                  --+---+---+---+--
						;              ______!   !   !   !______
						;             !    ______! f !______    !
						;         c1  v   v                 v   v  c2
						;           -------------  u  -------------
						;           !   !   !   !-----!   !   !   !
						;           -------------     -------------
						;         (2) between literals l in c and f(l) in c1
						;           or c2 exists a s-link. if this s-link is
						;           oriented, c must be its positive parent.
						;         (3) the merge of the unifiers of the s-links
						;           one for each pair (l, f(l)) is a matcher
						;           only modifying variables of clause c under
						;           binding u.
						;         (4) link condition described in
						;           'red=ls_link.condition' is fulfilled.
						;         a weaker case complying only with (1) - (3)
						;         sometimes also will be included in the
						;         notion subsumption.
						;         the second case of unifier subsumption (a
						;         clause subsumes a unifier of an incident
						;         link) is described in 'red=ls_own.parent'.
						; value:  not nil iff link is removed (all unifiers of
						;         it).
						;           oriented, c must be its positive parent.
  (let ((SUBSUMER.NOLIT (DS-CLAUSE.NOLIT SUBSUMER)) (OPERATION.UNIS (DS-LINK.UNIFIERS OPERATION.LINK))
	(SUBSUMENT1 (DS-LINK.POSPAR OPERATION.LINK)) SUBSUMENT2 SUBSUMENT.NOLIT2 SUBSUMENT.MARKS2 
	SUBSUMENT.NOLIT1 SUBSUMENT.MARKS1 SUBSUMER.SUBSUMENT.S.LINKS NEXT.RELATION)
    (SETQ SUBSUMENT.NOLIT1 (DS-CLAUSE.NOLIT SUBSUMENT1))
    (SETQ SUBSUMENT.MARKS1 (RDS-MARKS.CREATE SUBSUMENT.NOLIT1))
    (COND
      ((MEMBER (DS-LINK.COLOUR OPERATION.LINK) (RDS-LINK.COLOURS 'OPERATION.EXTERNAL))
       (SETQ SUBSUMENT2 (DS-LINK.NEGPAR OPERATION.LINK))
       (SETQ SUBSUMENT.NOLIT2 (DS-CLAUSE.NOLIT SUBSUMENT2))
       (SETQ SUBSUMENT.MARKS2 (RDS-MARKS.CREATE SUBSUMENT.NOLIT2))
       (RDS-MARK.PUT SUBSUMENT.MARKS1 (DS-LINK.POSLITNO OPERATION.LINK) 'REMOVE)
       (RDS-MARK.PUT SUBSUMENT.MARKS2 (DS-LINK.NEGLITNO OPERATION.LINK) 'REMOVE)))
    (SETQ SUBSUMER.SUBSUMENT.S.LINKS (RDS-MARKS.CREATE SUBSUMER.NOLIT))
    (UNTIL (NULL OPERATION.UNIS)
      (RDS-BINDING.PUSH (CAR OPERATION.UNIS))
      (COND
	((RED=LS_RELATION.MARK SUBSUMENT.MARKS1 SUBSUMENT1 SUBSUMENT.NOLIT1 SUBSUMENT.MARKS2 SUBSUMENT2
			       SUBSUMENT.NOLIT2 SUBSUMER.SUBSUMENT.S.LINKS SUBSUMER SUBSUMER.NOLIT)
	 (SETQ NEXT.RELATION
	       (RED=LS_RELATION.INJECTIVE SUBSUMER.NOLIT 1 NIL NIL OPERATION.LINK SUBSUMER
					  SUBSUMER.SUBSUMENT.S.LINKS SUBSUMENT1 SUBSUMENT2 C))
	 (RED=LS_RELATION.EXECUTE NEXT.RELATION OPERATION.LINK (CAR OPERATION.UNIS))))
      (RDS-BINDING.POP) (SETQ OPERATION.UNIS (CDR OPERATION.UNIS))
      (COND
	(OPERATION.UNIS (RDS-MARKS.RESET SUBSUMER.SUBSUMENT.S.LINKS NIL)
			(RDS-MARKS.RESET SUBSUMENT.MARKS1 NIL)
			(COND (SUBSUMENT2 (RDS-MARKS.RESET SUBSUMENT.MARKS2 NIL))))))
    (RDS-MARKS.DESTROY 0)
    (MEMBER OPERATION.LINK (CG-LINKS (DS-LINK.COLOUR OPERATION.LINK) REMOVED))))

(defun red=ls_p.link.new (clause)
  (mapc #'(lambda (link)
	    (when (and (ds-link.is link)
		       (if (opt-is.kz.completion) (not (member link (cg-links 'r removed))))
		       (not (member link (cg-links 'p removed)))
		       (not (member link (cg-links 'piw removed))))
	      (let* ((TERMLIST (ds-link.result link)))
		(when (and termlist (or (some #'(lambda (literal)
						  (and (dt-predicate.is.equality (ds-lit.predicate literal))
						       (ds-sign.is.positive (ds-lit.sign literal))
						       (uni-equal (first (ds-lit.termlist literal))
								  (second (ds-lit.termlist literal)))))
					      termlist)
					(op-subsumed.p (dt-termlist.variables termlist) termlist)
					(red=ls_all.instances.true termlist)))
		  (red=ctl_remove.link link "Subsumed" nil)))))
	(prog1 (delete-duplicates red*info_links.changed) (setq red*info_links.changed nil)))
  (mapc #'(lambda (links)
	    (mapc #'(lambda (link)
		      (let* ((TERMLIST (ds-link.result link)))
			(when (and termlist
				   (or (op-subsumed.p (dt-termlist.variables termlist) termlist clause)
				       (red=ls_all.instances.true termlist)))
			  (red=ctl_remove.link link :Subsumed.p clause))))
		  links))
	(list (cg-links 'p all)
	      (if (or (opt-is.kz.completion) (opt-is.with.residues)) (cg-links 'r all) nil)
	      (cg-links 'piw all))))

(defun RED=LS_SUBSUME.P.LINK (LINK)
						; Edited:  18-AUG-1989 13:49
						; Authors: PRCKLN
						; Input:   LINK is an external operation link
						; Effect:  If LINK is a P-Link whose parents are
						;          units then if a unifier of link LINK is subsumed by
						;          an arbitrary clause, it will be removed.
						; Value:   Undefined.
  (let ((termlist (ds-link.result link)))
    (when (or (some #'(lambda (lit)
			(and (dt-predicate.is.equality (ds-lit.predicate lit))
			     (ds-sign.is.positive (ds-lit.sign lit))
			     (uni-equal (first (ds-lit.termlist lit)) (second (ds-lit.termlist lit)))))
		    termlist)
	      (op-subsumed.p (dt-termlist.variables termlist) termlist)
	      (red=ls_all.instances.true termlist))
      (red=ctl_remove.link link "Subsumed" nil))))


(defun red=ls_all.instances.true (litlist)
  (let ((domain (ds-finite.domain.domain)))
    (when domain
      (let ((variables (ds-lits.vars litlist))
	    (is.true t))
	(when variables
	  (CARTESIAN.LOOP (make-list (length variables) :initial-element (copy-list domain))
			  #'(lambda (lists)
			      (let* ((inst.uni (zip variables (mapcar #'first lists) t))
				     (instance (UNI-APPLY.SUBSTITUTION inst.uni LITLIST T)))
				(declare (special CARTESIAN.LOOP.END))
				(unless (or (some #'(lambda (lit)
						      (and (dt-predicate.is.equality (ds-lit.predicate lit))
							   (ds-sign.is.positive (ds-lit.sign lit))
							   (uni-equal (first (ds-lit.termlist lit))
								      (second (ds-lit.termlist lit)))))
						  instance)
					    (op-subsumed.p nil instance))
				  (setq cartesian.loop.end t is.true nil)))))
	  is.true)))))

(DEFUN RED=LS_TO.EXT.LINK (LINK CONDITION)
						; edited: 27-jun-84 04:11:34
						; input:  'link' is an external operation link.
						;         'condition' is the option, i.e. one of the
						;         atoms 't', 'remove', 'inhibit', and
						;         'remove-inhibit'.
						; effect: if an unifier of link 'link' is subsumed by
						;         an arbitrary clause,
						;         it will be removed, ihibited, or information
						;         is inserted into recheck information list
						;         according to 'condition'. snowball effect
						;         is considered in removal case.
						;         there are three types of subsumption for
						;         external operation unifiers:
						;         subsumption by the own parent as described
						;           in 'red=ls_own.parent.to.unifier'.
						;         subsumption by another clause is defined
						;           in 'red=ls_attempt.to.subsume'.
						;         the third case is the removal of unifiers
						;           producing clauses with true literals.
						; value:  undefined.
  (PROG
    ((POSLITNO (DS-LINK.POSLITNO LINK))
     (POSPAR (DS-LINK.POSPAR LINK))
     (NEGPAR (DS-LINK.NEGPAR LINK))
     (NEGLITNO (DS-LINK.NEGLITNO LINK)))
    (COND ((RED=LS_SUBSUME.TRUE.LITERAL LINK POSPAR POSLITNO))
	  ((RED=LS_SUBSUME.TRUE.LITERAL LINK NEGPAR NEGLITNO))
	  ((RED=LS_OWN.PARENT.TO.LINK POSPAR POSLITNO NEGPAR NEGLITNO LINK CONDITION))
	  ((RED=LS_OWN.PARENT.TO.LINK NEGPAR NEGLITNO POSPAR POSLITNO LINK CONDITION))
	  ((MEMBER-IF #'(LAMBDA (SUBSUMING) (RED=LS_ATTEMPT.TO.SUBSUME LINK SUBSUMING CONDITION))
		      (RED=LS_CANDIDATES.TO.BE.SUBSUMING POSPAR NEGPAR POSLITNO NEGLITNO)))	  
	  ((RED=LS_SUBSUME.P.LINK LINK)))))

(DEFUN RED=LS_TO.INT.LINK (LINK CONDITION)
						; edited: 27-jun-84 04:31:45
						; input:  'link' is an internal operation link (si).
						;         'condition' is the option, i.e. one of the
						;         atoms 't', 'remove', 'inhibit', and
						;         'remove-inhibit'.
						; effect: if an unifier of link 'link' is subsumed by
						;         an arbitrary clause, this unifier
						;         will be removed, ihibited, or information
						;         is inserted into recheck information list
						;         according to 'condition'. snowball effect
						;         is considered in removal case.
						;         there are two types of subsumption for
						;         internal operation unifiers:
						;         by clauses not being the own parent, as
						;         described in 'red=ls_attempt.to.subsume'
						;         and removal of unifiers generating clauses
						;         with true literals.
						;         the case of subsumption of factors by the
						;         own parent is avoided in this way.
						; value:  undefined.
  (COND ((RED=LS_SUBSUME.TRUE.LITERAL LINK (DS-LINK.POSPAR LINK) NIL))
	((MEMBER-IF #'(LAMBDA (SUBSUMING) (RED=LS_ATTEMPT.TO.SUBSUME LINK SUBSUMING CONDITION))
		    (RED=LS_CANDIDATES.TO.BE.SUBSUMING (DS-LINK.POSPAR LINK) NIL NIL NIL)))
	((RED=LS_SUBSUME.P.LINK LINK))))

(DEFUN RED=LS_TO.SUBSUMPTION.LINK (SUBSUMER SUBSUMENT.PARENT CONDITION)
						; edited: 27-jun-84 04:23:40
						; input:  'subsumer' and 'subsument.parent' are two
						;         clauses.
						;         'condition' is the option, i.e. one of the
						;         atoms 't', 'remove', 'inhibit', and
						;         'remove-inhibit'.
						; effect: if an unifier of an operation link incident
						;         with clause 'subsument.parent' is subsumed
						;         by clause 'subsumer', then this unifier
						;         will be removed, inhibited, or information
						;         is inserted into recheck information list
						;         according to 'condition'. snowball effect
						;         is considered in removal case.
						;         the operation links only are considered,
						;         if they are not incident with clause
						;         'subsumer' (no own parent subsumption).
						; value:  nil.
  (MAPC #'(LAMBDA (LINK) (RED=LS_ATTEMPT.TO.SUBSUME LINK SUBSUMER CONDITION))
	(RED=LS_CANDIDATES.TO.BE.SUBSUMED.SPEC.PARENT SUBSUMER SUBSUMENT.PARENT)))

(DEFUN RED=LS_RECHECK.INSERT.CONDITION.LINK (LINK CONDITION)
						; edited: 27-jun-84 04:00:32
						; input:  'link' is a link with colour, so that
						;         after its insertion, link condition can be
						;         fulfilled, where it failed before (r, riw,
						;         si).
						;         'condition' is the option, i.e. one of the
						;         atoms 't', 'remove', 'inhibit', and
						;         'remove-inhibit'.
						; effect: recheck information list will be updated.
						;         its entries are pairs (clause . link),
						;         where the subsumption of an unifier of
						;         link by clause failed because link
						;         condition did not comply.
						;         if the subsumer clause of such a pair is
						;         incident with link 'link', the subsumption
						;         relation for this pair will be checked
						;         again.
						; value:  undefined.
  (PROG ((PARENTS (RED.SERVICE-LINK.PARENTS LINK))) (RED=LS_UPDATE.RECHECK.INFO)
	(MAPC #'(LAMBDA (SUBSUMER.SUBSUMENT)
		  (COND
		    ((MEMBER (CAR SUBSUMER.SUBSUMENT) PARENTS)
		     (COND
		       ((MEMBER (CAR SUBSUMER.SUBSUMENT) (RED.SERVICE-LINK.PARENTS (CDR SUBSUMER.SUBSUMENT)))
			(RED=LS_OWN.PARENT.TO.SUBSUMER.SUBSUMENT (CAR SUBSUMER.SUBSUMENT) (CDR SUBSUMER.SUBSUMENT)
								 CONDITION))
		       (T (RED=LS_ATTEMPT.TO.SUBSUME (CDR SUBSUMER.SUBSUMENT) (CAR SUBSUMER.SUBSUMENT) CONDITION))))))
	      (RDS-RULE 'RECHECK.INFO 'RED*LINK.SUBSUMPTION.SUBJECT))))

(DEFUN RED=LS_UPDATE.RECHECK.INFO NIL
						; edited:  6-jul-84 00:40:46
						; input:  -
						; effect: recheck information list of link
						;         subsumption rule contains dotted pairs
						;         (subsuming_clause .
						;         link_with_subsumed_unifiers) not complying
						;         with the link condition.
						;         the pairs are removed from the list if one
						;         of its objects is removed from connection
						;         graph or memory.
						; value:  undefined.
  (RDS-RULE.PUT 'RECHECK.INFO 'RED*LINK.SUBSUMPTION.SUBJECT
		   (DREMAP (RDS-RULE 'RECHECK.INFO 'RED*LINK.SUBSUMPTION.SUBJECT) NIL
			   #'(LAMBDA (SUBSUMER.SUBSUMENTS.TAIL)
			       (PROG ((SUBSUMER.SUBSUMENT (CAR SUBSUMER.SUBSUMENTS.TAIL)))
				     (RETURN
				       (OR (NOT (DS-CLAUSE.IS (CAR SUBSUMER.SUBSUMENT)))
					   (NOT (DS-LINK.IS (CDR SUBSUMER.SUBSUMENT)))
					   (MEMBER (CAR SUBSUMER.SUBSUMENT) (CG-CLAUSES REMOVED))
					   (MEMBER (CDR SUBSUMER.SUBSUMENT)
						   (CG-LINKS (DS-LINK.COLOUR (CDR SUBSUMER.SUBSUMENT)) REMOVED))))))
			   NIL)))

(DEFUN RED=LS_SUBSUME.TRUE.LITERAL (LINK CLAUSE REMOVE.LITNO)
						; edited: 27-jun-84 03:53:27
						; input:  'link' is an operation link incident with
						;         clause 'clause'.
						;         if 'link' is external, 'remove.litno' is
						;         the number of its parent literal in clause
						;         'clause', else 'remove.litno' is nil.
						; effect: if after application of an unifier of link
						;         'link', one of the literals of clause
						;         'clause' except 'remove.litno' is true as
						;         defined in 'red.service-true.literal.is',
						;         the unifier will be removed. snowball effect
						;         is considered.
						; value:  not nil iff all unifiers of link 'link' have
						;         been removed.
  (EVERY #'(LAMBDA (UNIFIER)
	     (RDS-BINDING.PUSH UNIFIER)
	     (PROG1 (MEMBER-IF #'(LAMBDA (POT.TRUE.LITNO)
				   (COND ((NEQ REMOVE.LITNO POT.TRUE.LITNO)
					  (CASE (RED.SERVICE-TRUE.LITERAL.IS CLAUSE POT.TRUE.LITNO (RDS-BINDING.TOP))
					    ((T) (RED=CTL_AGENDA.UPDATE
						   (RED=CTL_REMOVE.UNIFIER LINK UNIFIER t "Unifier infering a true clause" NIL)
						   NIL NIL)
					     T)
					    ((IMPOSSIBLE NIL) NIL)
					    (OTHERWISE (ERROR "red-check: Illegal value in red=ls_subsume.true.literal: ~a"
							      (RED.SERVICE-TRUE.LITERAL.IS CLAUSE POT.TRUE.LITNO
											   (RDS-BINDING.TOP))))))))
			       (DS-CLAUSE.POTENTIALLY.TRUE.LITNOS CLAUSE))
		    (RDS-BINDING.POP UNIFIER)))
	 (DS-LINK.UNIFIERS LINK)))

(DEFUN RED=LS_OWN.PARENT.TO.CLAUSE (CLAUSE CONDITION)
						; edited: 27-jun-84 03:49:33
						; input:  'clause' is a clause.
						;         'condition' is the option, i.e. one of the
						;         atoms 'remove', 'inhibit', 't', and
						;         'remove-inhibit'.
						; effect: unifiers of links incident with clause
						;         'clause', which are
						;         subsumed by 'clause' as explained
						;         in 'red=ls_own.parent.to.unifier', will
						;         be removed, inhibited, or information is
						;         inserted into recheck information list,
						;         depending on 'condition'. snowball effect
						;         is considered in removal case.
						; value:  undefined.
  (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE)) (RED=LS_OWN.PARENT.TO.LITERAL CLAUSE (1+ RPTN) CONDITION)))

(DEFUN RED=LS_OWN.PARENT.TO.LITERAL (CLAUSE LITNO CONDITION)
						; edited: 27-jun-84 03:45:12
						; input:  'litno' is the number of a literal of clause
						;         'clause'.
						;         'condition' is the option, i.e. one of the
						;         atoms 'remove', 'inhibit', 't', and
						;         'remove-inhibit'.
						; effect: unifiers of link incident with literal
						;         'litno' of clause 'clause', which are
						;         subsumed by 'clause' as explained
						;         in 'red=ls_own.parent.to.unifier' will
						;         be removed, inhibited, or information is
						;         inserted into recheck information list,
						;         depending on 'condition'. snowball effect
						;         is considered in removal case.
						; value:  undefined.
  (MAPC #'(LAMBDA (OP.LINKS)
	    (MAPC #'(LAMBDA (OP.LINK)
		      (RED=LS_OWN.PARENT.TO.LINK CLAUSE LITNO (RED.SERVICE-OTHERPAR.EXTERNAL OP.LINK CLAUSE)
						 (RED.SERVICE-OTHERLITNO.EXTERNAL OP.LINK CLAUSE) OP.LINK CONDITION))
		  OP.LINKS))
	(RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.EXTERNAL) CLAUSE LITNO)))

(DEFUN RED=LS_OWN.PARENT.TO.SUBSUMER.SUBSUMENT (SUBSUMER SUBSUMENT CONDITION)
						; edited: 27-jun-84 03:39:30
						; input:  'subsument' is an external operation link
						;         incident with clause 'subsumer'.
						;         'condition' is the option, i.e. one of the
						;         atoms 'remove', 'inhibit', 't', and
						;         'remove-inhibit'.
						; effect: unifiers of link 'subsument' which are
						;         subsumed by clause 'subsumer' as described
						;         in 'red=ls_own.parent.to.unifier' will
						;         be removed, inhibited, or information is
						;         inserted into recheck information list,
						;         depending on 'condition'. snowball effect
						;         is considered in removal case.
						; value:  undefined.
  (RED=LS_OWN.PARENT.TO.LINK SUBSUMER
			     (COND ((EQL SUBSUMER (DS-LINK.POSPAR SUBSUMENT)) (DS-LINK.POSLITNO SUBSUMENT))
				   (T (DS-LINK.NEGLITNO SUBSUMENT)))
			     (RED.SERVICE-OTHERPAR.EXTERNAL SUBSUMENT SUBSUMER)
			     (RED.SERVICE-OTHERLITNO.EXTERNAL SUBSUMENT SUBSUMER) SUBSUMENT
			     CONDITION))

(DEFUN RED=LS_OWN.PARENT.TO.LINK (OWN.SUBSUMER OWN.SUBSUMER.OP.LITNO SUBSUMENT SUBSUMENT.OP.LITNO LINK CONDITION)
						; edited: 27-jun-84 03:10:49
						; input:  'own.subsumer.op.litno' is the number of a
						;         literal of clause 'own.subsumer'.
						;         'subsument.op.litno' is the number of a
						;         literal of clause 'subsument'.
						;         'link' is an external operation link (r, p),
						;         incident with both literals introduced
						;         above.
						;         'condition' is the option, i.e. one of the
						;         atoms 'remove', 'inhibit', 't', and
						;         'remove-inhibit'.
						; effect: if there exists a s-link between literal
						;         'own.subsumer.op.litno' and a literal of
						;         clause 'subsument', but not 'subsument.op.-
						;         litno', with a unifier, so that:
						;         with a unifier of link 'link' as binding
						;         the conditions described in
						;         'red=ls_own.parent.to.unifier' are
						;         fulfilled,
						;         then this unifier (set as binding) will be
						;         removed, inhibited, or information is
						;         inserted into recheck information list,
						;         depending on option 'condition'(explained
						;         in 'red=ls_execute.relation'). snowball
						;         effect is considered in removal case.
						;         if the s-link is oriented, 'own.subsumer'
						;         must be its positive parent clause.
						; value:  undefined.
  (PROG (VARS OTHERLITNO RESULT)
	(DODOWN (RPTN (DS-CLAUSE.NOLIT OWN.SUBSUMER))
	  (COND
	    ((NEQ (1+ RPTN) OWN.SUBSUMER.OP.LITNO)
	     (SETQ VARS (UNION (DS-CLAUSE.LIT.VARIABLES OWN.SUBSUMER (1+ RPTN)) VARS)))))
	(MAPC #'(LAMBDA (UNI)
		  (RDS-BINDING.PUSH UNI)
		  (COND ((MEMBER-IF #'(LAMBDA (NC.LINKS)
					(MEMBER-IF #'(LAMBDA (NC.LINK)
						       (AND (EQL SUBSUMENT (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK OWN.SUBSUMER))
							    (NEQ SUBSUMENT.OP.LITNO
								 (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.EXTERNAL
										    NC.LINK OWN.SUBSUMER)))
							    (OR (EQL SUBSUMENT (DS-LINK.NEGPAR NC.LINK))
								(NOT (RED.SERVICE-LINK.ORIENTED NC.LINK)))
							    (MEMBER-IF #'(LAMBDA (NC.UNI)
									   (SETQ RESULT
										 (RED=LS_OWN.PARENT.TO.UNIFIER
										   NC.UNI VARS
										   (DS-CLAUSE.LIT.VARIABLES SUBSUMENT OTHERLITNO)
										   (CONS LINK UNI) SUBSUMENT
										   OWN.SUBSUMER OTHERLITNO
										   OWN.SUBSUMER.OP.LITNO CONDITION)))
								       (DS-LINK.UNIFIERS NC.LINK))))
						   NC.LINKS))
				    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) OWN.SUBSUMER
							      OWN.SUBSUMER.OP.LITNO))
			 (RED=LS_RELATION.EXECUTE RESULT LINK UNI)))
		  (RDS-BINDING.POP))
	      (DS-LINK.UNIFIERS LINK))
	(RETURN (NULL (DS-LINK.UNIFIERS LINK)))))

(DEFUN RED=LS_OWN.PARENT.TO.UNIFIER
       (S.UNIFIER REST.SUBSUMER.VARIABLES SUBSUMENT.LITERAL.VARIABLES OP.UNI SUBSUMENT SUBSUMER SUBSUMENT.LITNO
	SUBSUMER.LITNO CONDITION)
						; edited: 10-jul-84 08:40:45
						; input:  illustration of the situation:
						;
						;                s-link with unifier 's.unifier'
						;                  ___________________________
						;                 !                           !
						;         -----------              -------------------
						;         ! .*. !***!--------------!   ! .. !** ! .. !
						;         -----------    link      -------------------
						;         clause         'op.link'     clause
						;         'subsumer'                   'subsument'
						;
						;         'rest.subsumer.variables' are the variables
						;         in the literals of clause 'subsumer'
						;         marked with the '*' in the figure above.
						;         'subsument.literal.variables' are the
						;         variables of the literal in clause
						;         'subsument' marked with '**'.
						;         literal '***' has the number 'subsumer.-
						;         litno' in the clause 'subsumer', '**' the
						;         number 'subsument.litno' in clause
						;         'subsument'.
						;         'condition' is the option, i.e. one of the
						;         atoms 't', 'remove', 'inhibit', and
						;         'remove-inhibit'.
						; effect: -
						; value:  a list (indicator subsumer merge) as
						;         described in 'red=ls_relation.match' if the
						;         following two unifiers can be merged
						;         under the special binding to a matcher,
						;         which codomain variables are a subset of the
						;         variables of clause 'subsumer'.
						;         consideration of 'condition' as explained
						;         in 'red=ls_attempt.to.subsume' must give
						;         not nil too.
						;
						;         first unifier:
						;         modifying 'rest.subsumer.variables'.
						;         if a variable has a binding the unifier
						;         changes it by the binding where all
						;         variables of 'subsumer' are renamed by
						;         'subsumer's renaming.
						;         if it has no binding it is changed by its
						;         renaming variable (x<-x').
						;         example:
						;         'rest.subsumer.variables'    binding
						;         x                            -
						;         y                            f(x y)
						;         unifier:  x<-x', y<-f(x' y')
						;
						;         second unifier is 's.unifier'.
						;
						;         special binding:
						;         binding will be modified by removing
						;         bindings of variables of clause 'subsumer'
						;         and replacing variables of clause
						;         'subsumer' in the bindings of variables
						;         'subsument.literal.variables' in clause
						;         'subsument' by their renaming.
  (PROG
    ((SUBSUMER.RENAMING (DS-CLAUSE.RENAMING SUBSUMER)) (NEW.BINDING (COPY-TREE (RDS-BINDING.TOP)))
     BIND RCV.UNI SUCCESS NOT.CHECK.LITERALS INJECTIVE.FUNCTION)
    (MAPC #'(LAMBDA (VAR)
	      (WHEN (SETQ BIND (DT-VARIABLE.GET.BINDING VAR))
		(SMAPL #'(LAMBDA (BIND.TAIL)
			   (WHEN (EQL VAR (CAR BIND.TAIL))
			     (RPLACA (CDR BIND.TAIL) (UNI-APPLY.SUBSTITUTION SUBSUMER.RENAMING BIND T))))
		       #'CDDR
		       NEW.BINDING)))
	  SUBSUMENT.LITERAL.VARIABLES)
    (SETQ RCV.UNI NIL)
    (MAPC #'(LAMBDA (VAR)			;; built unifier modifying variables 'rsv'
	      ;; the variables with binding are changed by their
	      ;; binding with renamed variables, the others by their renaming.
	      (IF (SETQ BIND (DT-VARIABLE.GET.BINDING VAR))
		  (SETQ BIND (UNI-APPLY.SUBSTITUTION SUBSUMER.RENAMING BIND T))
		  (SETQ BIND (SECOND (MEMBER VAR SUBSUMER.RENAMING))))
	      (SETQ RCV.UNI (CONS VAR (CONS BIND RCV.UNI))))
	  REST.SUBSUMER.VARIABLES)
    ;; delete bindings of variables of clause 'subsumer'.
    (MAPC #'(LAMBDA (VAR)
	      (COND
		((DT-VARIABLE.GET.BINDING VAR)
		 (SSOMEL
		   #'(LAMBDA (BIND.TAIL) (COND ((EQL VAR (CAR BIND.TAIL)) (RPLACA (CDR BIND.TAIL) VAR))))
		   #'CDDR NEW.BINDING)
		 (SETQ NEW.BINDING (DELETE VAR NEW.BINDING)))))
	  (DS-CLAUSE.VARIABLES SUBSUMER))
    (RDS-BINDING.PUSH NEW.BINDING)
    (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBSUMER))
      (COND
	((EQL (1+ RPTN) SUBSUMER.LITNO)
	 (SETQ INJECTIVE.FUNCTION
	       (CONS (CONS SUBSUMER.LITNO (CONS SUBSUMENT SUBSUMENT.LITNO)) INJECTIVE.FUNCTION))
	 ;; computing the injective function for subsumption.
	 ;; each 'subsumer' literal but 'subsumer.litno' is
	 ;; subsumed by itself, 'subsumer.litno' subsumes
	 ;; 'subsument.litno'.
	 )
	(T (SETQ INJECTIVE.FUNCTION (CONS (CONS (1+ RPTN) (CONS SUBSUMER (1+ RPTN))) INJECTIVE.FUNCTION))
	   (SETQ NOT.CHECK.LITERALS (CONS (1+ RPTN) NOT.CHECK.LITERALS)))))
    (SETQ SUCCESS
	  (RED=LS_RELATION.MATCH (LIST (LIST RCV.UNI) (LIST S.UNIFIER)) INJECTIVE.FUNCTION NOT.CHECK.LITERALS
				 (UNION (UNI-UNIFIER.CODOMAIN SUBSUMER.RENAMING) (DS-CLAUSE.VARIABLES SUBSUMENT))
				 SUBSUMER OP.UNI SUBSUMER
				 SUBSUMENT CONDITION T))
    ;; old bindings are reset.
    (RDS-BINDING.POP) (RETURN SUCCESS)))

(DEFUN RED=LS_CANDIDATES.TO.BE.SUBSUMING
       (SUBSUMENT.PARENT1 SUBSUMENT.PARENT2 SUBSUMENT.PARENT.LITNO1 SUBSUMENT.PARENT.LITNO2)
						; edited:  9-jul-84 15:35:50
						; input:  'subsument.parent.litnoi' is the number of a
						;         literal of clause 'subsument.parenti'
						;         (i = 1, 2). 'subsument.parent2' can be nil.
						;         if 'subsument.parent2' is nil, both
						;         literal numbers are nil too.
						;         the literals are the parent literals of
						;         an operation link with unifiers to be
						;         subsumed.
						; effect: -
						; value:  list of clauses, which are candidates to
						;         subsume unifiers of links between input
						;         literals.
						;         each of the literals of such a candidate
						;         clause has a s-link to a literal in the
						;         input clauses without input literals.
						;         each clause has s-links to a greater or
						;         equal number of literals in the input
						;         clauses without input literals, than its
						;         own number.
						;         oriented s-links are only considered if one
						;         of the input clauses is their negative
						;         parent.
						;         'subsument.parent1' and 'subsument.parent2'
						;         are not in the candidates.
  (PROG
    ((SUBSUMER.CANDIDATES NIL) (NOLIT1 (DS-CLAUSE.NOLIT SUBSUMENT.PARENT1))
     (NOLIT2 (COND (SUBSUMENT.PARENT2 (DS-CLAUSE.NOLIT SUBSUMENT.PARENT2)) (T 2))) OTHERPAR LENGTH)
    (SETQ LENGTH (+ NOLIT1 NOLIT2 -2))
    (DODOWN (RPTN NOLIT1)
      (COND
        ((NEQ (1+ RPTN) SUBSUMENT.PARENT.LITNO1)
	 (MAPC #'(LAMBDA (NC.LINKS)
		   (MAPC #'(LAMBDA (NC.LINK)
			     (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK SUBSUMENT.PARENT1))
			     (COND
			       ((AND (NEQ OTHERPAR SUBSUMENT.PARENT2) (NOT (< LENGTH (DS-CLAUSE.NOLIT OTHERPAR)))
				     (OR (EQL OTHERPAR (DS-LINK.POSPAR NC.LINK))
					 (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK))))
				(RED=LS_CANDIDATES.INSERT.RELATION OTHERPAR
								   (RED.SERVICE-OTHERLITNO.EXTERNAL NC.LINK
												    SUBSUMENT.PARENT1)
								   SUBSUMENT.PARENT1
								   (1+ RPTN))
				(SETQ SUBSUMER.CANDIDATES (INS OTHERPAR SUBSUMER.CANDIDATES)))))
			 NC.LINKS))
	       (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBSUMENT.PARENT1
					 (1+ RPTN))))))
    (COND
      (SUBSUMENT.PARENT2
       (DODOWN (RPTN NOLIT2)
	 (COND
	   ((NEQ (1+ RPTN) SUBSUMENT.PARENT.LITNO2)
	    (MAPC #'(LAMBDA (NC.LINKS)
		      (MAPC #'(LAMBDA (NC.LINK)
				(SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK SUBSUMENT.PARENT2))
				(COND
				  ((AND (NEQ OTHERPAR SUBSUMENT.PARENT1) (NOT (< LENGTH (DS-CLAUSE.NOLIT OTHERPAR)))
					(OR (EQL OTHERPAR (DS-LINK.POSPAR NC.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK))))
				   (RED=LS_CANDIDATES.INSERT.RELATION OTHERPAR
								      (RED.SERVICE-OTHERLITNO.EXTERNAL NC.LINK SUBSUMENT.PARENT2)
								      SUBSUMENT.PARENT2
								      (1+ RPTN))
				   (SETQ SUBSUMER.CANDIDATES (INS OTHERPAR SUBSUMER.CANDIDATES)))))
			    NC.LINKS))
		  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBSUMENT.PARENT2
					    (1+ RPTN))))))))
    (RETURN
      (DREMAP SUBSUMER.CANDIDATES NIL
	      #'(LAMBDA (SUBSUMER.CANDIDATES.TAIL)
		  (PROG1
		    (<
		      (LIST-LENGTH
			(BUFFER.CONTENTS (DT-GETPROP (CAR SUBSUMER.CANDIDATES.TAIL) 'RED*LS_LITNOS.RELATION)))
		      (DS-CLAUSE.NOLIT (CAR SUBSUMER.CANDIDATES.TAIL)))
		    (PROG ((PROP (DT-GETPROP (CAR SUBSUMER.CANDIDATES.TAIL) 'RED*LS_LITNOS.RELATION)))
			  (COND
			    (PROP (PUSH.BUFFER.STACK PROP (RDS-BUFFER))
				  (DT-PUTPROP (CAR SUBSUMER.CANDIDATES.TAIL) 'RED*LS_LITNOS.RELATION NIL))))))
	      NIL))))

(DEFUN RED=LS_CANDIDATES.TO.BE.SUBSUMED (SUBSUMER)
						; edited:  9-jul-84 07:59:44
						; input:  'subsumer' is a clause.
						; effect: -
						; value:  list of operation links (r, p, si) with
						;         unifiers, which are candidates to be
						;         subsumed by clause 'subsumer'.
						;         each literal of clause 'subsumer' has a
						;         s-link to the parents of each candidate
						;         link, without parent literals of external
						;         operation links.
						;         clause 'subsumer' is adjacent about s-links
						;         with literals of the parents of each
						;         candidate link. the number of these literals
						;         for one candidate link without parent
						;         literals of external operation links is
						;         greater or equal than the number of literals
						;         of clause 'subsumer'.
  (PROG ((NOLIT (DS-CLAUSE.NOLIT SUBSUMER)) ADJACENTS CANDIDATES OTHERPAR THISPAR.NOLIT)
	(DODOWN (RPTN NOLIT)
	  (MAPC #'(LAMBDA (NC.LINKS)
		    (MAPC #'(LAMBDA (NC.LINK)
			      (COND
				((OR (EQL SUBSUMER (DS-LINK.POSPAR NC.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED NC.LINK)))
				 (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK SUBSUMER))
				 (RED=LS_CANDIDATES.INSERT.RELATION OTHERPAR (1+ RPTN) OTHERPAR
								    (RED.SERVICE-OTHERLITNO.EXTERNAL NC.LINK SUBSUMER))
				 (SETQ ADJACENTS (INS OTHERPAR ADJACENTS)))))
			  NC.LINKS))
		(RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBSUMER (1+ RPTN))))
	(MAPC #'(LAMBDA (THISPAR)
		  (SETQ THISPAR.NOLIT (DS-CLAUSE.NOLIT THISPAR))
		  (DODOWN (RPTN THISPAR.NOLIT)
		    (MAPC #'(LAMBDA (OP.LINKS)
			      (MAPC #'(LAMBDA (OP.LINK)
					(SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL OP.LINK THISPAR))
					(COND
					  ((AND (NEQ OTHERPAR SUBSUMER) (< OTHERPAR THISPAR)
						(> (+ (DS-CLAUSE.NOLIT OTHERPAR) THISPAR.NOLIT) (1+ NOLIT))
						(EQL
						  (LIST-LENGTH
						    (UNION (BUFFER.CONTENTS (DT-GETPROP OTHERPAR 'RED*LS_LITNOS.RELATION))
							   (BUFFER.CONTENTS (DT-GETPROP THISPAR 'RED*LS_LITNOS.RELATION))))
						  NOLIT))
					   (SETQ CANDIDATES (INS OP.LINK CANDIDATES)))))
				    OP.LINKS))
			  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.EXTERNAL) THISPAR (1+ RPTN)))
		    (MAPC #'(LAMBDA (OPI.LINKS)
			      (MAPC #'(LAMBDA (OPI.LINK)
					(COND ((NOT (OR (< (1+ RPTN) (RED.SERVICE-OTHERLITNO.INTERNAL OPI.LINK (1+ RPTN)))
							(< (LIST-LENGTH
							     (BUFFER.CONTENTS (DT-GETPROP THISPAR 'RED*LS_LITNOS.RELATION)))
							   NOLIT)))
					       (SETQ CANDIDATES (INS OPI.LINK CANDIDATES)))))
				    OPI.LINKS))
			  (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.INTERNAL) THISPAR (1+ RPTN)))))
	      ADJACENTS)
	(MAPC #'(LAMBDA (THISPAR)
		  (PROG ((PROP (DT-GETPROP THISPAR 'RED*LS_LITNOS.RELATION)))
			(COND
			  (PROP (PUSH.BUFFER.STACK PROP (RDS-BUFFER)) (DT-PUTPROP THISPAR 'RED*LS_LITNOS.RELATION NIL)))))
	      ADJACENTS)
	(RETURN CANDIDATES)))

(DEFUN RED=LS_CANDIDATES.TO.BE.SUBSUMED.SPEC.PARENT (SUBSUMER ONE.PARENT.OF.SUBSUMENT)
						; edited:  2-jul-84 08:06:58
						; input:  'subsumer' and 'subsument.parent' are two
						;         clauses.
						; effect: -
						; value:  list of operation links (r, p, si)
						;         incident with 'subsument.parent'.
						;         the unifiers of the links are candidates to
						;         be subsumed by clause 'subsumer'.
						;         each literal of clause 'subsumer' has a
						;         s-link to the parents of each link in the
						;         list. the s-links are not incident with the
						;         parent literal of the link in the list.
						;         oriented s-links are only considered, if
						;         the clause 'subsumer' is their positive
						;         parent.
						;         candidate links are not incident with clause
						;         'subsumer'.
  (PROG
    ((NOLIT.SUBSUMER (DS-CLAUSE.NOLIT SUBSUMER)) (NOLIT.SUBSUMENT (DS-CLAUSE.NOLIT ONE.PARENT.OF.SUBSUMENT))
     S.ADJACENTS.OF.SUBSUMER OTHERPAR ADJACENT.LITNOS.SUBSUMENT.OF.SUBSUMER SUBSUMENT.CANDIDATES)
    (DODOWN (RPTN NOLIT.SUBSUMER)
      (MAPC #'(LAMBDA (NC.LINKS)
		(MAPC #'(LAMBDA (S.LINK)
			  (COND
			    ((OR (EQL SUBSUMER (DS-LINK.POSPAR S.LINK)) (NOT (RED.SERVICE-LINK.ORIENTED S.LINK)))
			     (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL S.LINK SUBSUMER))
			     (DT-PUTPROP OTHERPAR 'RED*LS_SPEC.CANDIDATE.LITS
					 (INS (1+ RPTN) (DT-GETPROP OTHERPAR 'RED*LS_SPEC.CANDIDATE.LITS)))
			     (SETQ S.ADJACENTS.OF.SUBSUMER (INS OTHERPAR S.ADJACENTS.OF.SUBSUMER)))))
		      NC.LINKS))
	    (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBSUMER (1+ RPTN))))
    (SETQ ADJACENT.LITNOS.SUBSUMENT.OF.SUBSUMER
	  (DT-GETPROP ONE.PARENT.OF.SUBSUMENT 'RED*LS_SPEC.CANDIDATE.LITS))
    (DODOWN (RPTN NOLIT.SUBSUMENT)
      (PROGN
        (MAPC #'(LAMBDA (OP.LINKS)
		  (MAPC #'(LAMBDA (R.LINK)
			    (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK ONE.PARENT.OF.SUBSUMENT))
			    (COND
			      ((AND (NEQ OTHERPAR SUBSUMER)
				    (> (+ (DS-CLAUSE.NOLIT OTHERPAR) NOLIT.SUBSUMENT) (1+ NOLIT.SUBSUMER))
				    (EQL
				      (LIST-LENGTH
					(UNION (DT-GETPROP OTHERPAR 'RED*LS_SPEC.CANDIDATE.LITS)
					       ADJACENT.LITNOS.SUBSUMENT.OF.SUBSUMER))
				      NOLIT.SUBSUMER))
			       (SETQ SUBSUMENT.CANDIDATES (CONS R.LINK SUBSUMENT.CANDIDATES)))))
			OP.LINKS))
	      (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.EXTERNAL) ONE.PARENT.OF.SUBSUMENT (1+ RPTN)))
        (MAPC #'(LAMBDA (OPI.LINKS)
		  (MAPC #'(LAMBDA (SI.LINK)
			    (COND
			      ((AND (< (1+ RPTN) (RED.SERVICE-OTHERLITNO.INTERNAL SI.LINK (1+ RPTN)))
				    (> NOLIT.SUBSUMENT NOLIT.SUBSUMER)
				    (EQL (LIST-LENGTH ADJACENT.LITNOS.SUBSUMENT.OF.SUBSUMER) NOLIT.SUBSUMER))
			       (SETQ SUBSUMENT.CANDIDATES (CONS SI.LINK SUBSUMENT.CANDIDATES)))))
			OPI.LINKS))
	      (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'OPERATION.INTERNAL) ONE.PARENT.OF.SUBSUMENT (1+ RPTN)))))
    (MAPC #'(LAMBDA (CLAUSE) (DT-REMPROP CLAUSE 'RED*LS_SPEC.CANDIDATE.LITS)) S.ADJACENTS.OF.SUBSUMER)
    (RETURN SUBSUMENT.CANDIDATES)))

(DEFUN RED=LS_CANDIDATES.INSERT.RELATION (CLAUSE SUBSUMER.LITNO SUBSUMENT SUBSUMENT.LITNO)
						; edited:  2-aug-84 16:27:12
						; input:  'subsumer.litno' is the number of a literal
						;         in clause 'subsumer', 'subsument.litno' one
						;         in clause 'subsument'. 'clause' is one of
						;         the two clauses.
						; effect: property of clause 'clause' under the
						;         indicator 'red*ls_litnos.relation'
						;         is changed.
						;         if property exists 'subsumer.litno' is
						;         inserted into the 'car', the dotted pair
						;         ('subsument' . 'subsument.litno')
						;         is inserted into the 'cdr'.
						;         else a new property is created with
						;         these entries.
						; value:  undefined.
  (DECLARE (IGNORE SUBSUMENT SUBSUMENT.LITNO))
  (PROG ((RELATION (DT-GETPROP CLAUSE 'RED*LS_LITNOS.RELATION)))
	(COND
	  ((NULL RELATION) (SETQ RELATION (POP.BUFFER.STACK (RDS-BUFFER)))
	   (DT-PUTPROP CLAUSE 'RED*LS_LITNOS.RELATION RELATION)))
	(BUFFER.INS SUBSUMER.LITNO RELATION)))

(DEFUN RED=LS_RELATION.EXECUTE (RELATION OP.LINK OP.UNI)
						; edited:  2-jul-84 08:13:30
						; input:  'relation' is a list (indicator subsumer
						;         merge), where indicator is one of the atoms
						;         'red*ls_remove', 'red*ls_inhibit', and
						;         'red*ls_recheck', indicating wether the
						;         unifier 'op.uni' of the operation link
						;         'op.link' may be removed, inhibited, or
						;         information must be given to recheck
						;         information list. 'subsumer' and 'merge'
						;         are only used for 'cg-trace'.
						; effect: snowball effect is considered in
						;         removal case.
						;         'op.uni' is handeled as 'indicator' in
						;         'relation' says.
						; value:  undefined.
  (COND
    ((EQL (CAR RELATION) 'RED*LS_REMOVE)
     (RED=CTL_AGENDA.UPDATE
       (RED=CTL_REMOVE.UNIFIER OP.LINK OP.UNI t 'SUBSUMPTION (CONS (SECOND RELATION) (THIRD RELATION))) NIL NIL))
    ((EQL (CAR RELATION) 'RED*LS_INHIBIT)
     (CG-INHIBIT.UNIFIER OP.LINK OP.UNI 'SUBSUMPTION (CONS (SECOND RELATION) (THIRD RELATION))))
    ((EQL (CAR RELATION) 'RED*LS_RECHECK)
     (RDS-RULE.PUT 'RECHECK.INFO 'RED*LINK.SUBSUMPTION.SUBJECT
		      (ADJOIN (CONS (SECOND RELATION) OP.LINK) (RDS-RULE 'RECHECK.INFO 'RED*LINK.SUBSUMPTION.SUBJECT) :TEST
			      #'EQUAL)))))

(DEFUN RED=LS_RELATION.INJECTIVE
       (NOLIT LITNO S.UNIS S.LITERALS OPERATION.LINK SUBSUMER SUBSUMER.SUBSUMENT.S.LINKS SUBSUMENT1 SUBSUMENT2
	CONDITION)
						; edited:  2-aug-84 14:03:54
						; input:  'nolit' is the number of literals of clause
						;         'subsumer', 'litno' the number of one of
						;         its literals.
						;         's.uni' is a list of unifiers,
						;         's.literals' a list of elements like
						;         (number_of_a_literal_in_'subsumer'
						;         one_of_the_clauses_'subsument1'_or_-2' .
						;         litno_of_in_this_clause) representing the
						;         literal function.
						;         'subsumer.subsument.s.links' is an array
						;         with same length as clause 'subsumer'.
						;         cell i contains the list of s-links of
						;         literal i of clause 'subsumer' incident
						;         with the clauses 'subsument1' and
						;         'subsument2', without oriented s-links
						;         with 'subsumer' as positive parent clause.
						;         the links are represented in the
						;         following form:
						;         ((otherclause . otherlitno) othermarks .
						;         unifiers).
						;         (otherclause . otherlitno) describes a
						;         literal in 'subsument1' or 'subsument2'.
						;         'othermarks' is an array with
						;         !'othermarks'! = !'subsumentj'! (j = 1 or
						;         2). cell i of 'othermarks' is a mark of
						;         literal i of clause 'subsument1' or
						;         'subsument2'. marks can be 'remove' for
						;         by operation removed literals, 'relate' for
						;         literals in 's.literals' (soon used in
						;         relation to be found).
						;         'unifiers' are the unifiers of a s-link
						;         to the literal described by (otherclause .
						;         otherlitno).
						;         invariant of the recursion:
						;         (1) binding remains unchanged (unifier to
						;           be subsumed).
						;         (2) 'litno' - 1 = !'s.unis'! =
						;           !'s.literals'!.
						;         (3) i-th-last elements of the lists
						;           's.unis' and 's.literals' corresponds
						;           to the i-th literal of clause 'subsumer':
						;           this  s.uni is the unifier of a s-link
						;           in 'subsumer.subsument.s.links' of literal
						;           i of clause 'subsumer' to the literal
						;           described by this s.literal.
						;           all 's.literals' are marked with 'relate'
						;           in the mark arrays of 'subsument1' and
						;           'subsument2'.
						;         (4) no cdr in 's.literals' appears twice.
						; effect: before recursive call mark 'relate' is set,
						;         after call it is reset to nil.
						; value:  same as 'red=ls_relation.match' if it is
						;         called else nil.
  (let (SUCCESS VARIABLES)
    (SETQ S.LITERALS (CONS NIL S.LITERALS))
    ;; each recursion step takes new literal and unifier.
    ;; they can be changed for several possibilities.
    (SETQ S.UNIS (CONS NIL S.UNIS))
    (MEMBER-IF #'(LAMBDA (LINK.DESCRIPTOR)
		   (let ((SUBSUMENT.MARKS (SECOND LINK.DESCRIPTOR))
			 (OTHERLITNO (CDAR LINK.DESCRIPTOR)))
		     ;; 'link.descriptor' is a s-expression as described in
		     ;; 'input'.
		     (COND
		       ((NOT (RDS-MARK SUBSUMENT.MARKS OTHERLITNO))
			;; other literal of link is neither marked with
			;; 'remove' nor 'relate'.
			(RPLACA S.UNIS (CDDR LINK.DESCRIPTOR)) (RPLACA S.LITERALS (CONS LITNO (CAR LINK.DESCRIPTOR)))
			;; try it with this link.
			(COND
			  ((EQL LITNO NOLIT)
			   ;; ancre of recursion.
			   (SETQ VARIABLES
				 (UNION (COND (SUBSUMENT2 (DS-CLAUSE.VARIABLES SUBSUMENT2))) (DS-CLAUSE.VARIABLES SUBSUMENT1)))
			   (SETQ SUCCESS
				 (RED=LS_RELATION.MATCH S.UNIS S.LITERALS NIL VARIABLES SUBSUMER NIL SUBSUMENT1
							SUBSUMENT2 CONDITION)))
			  (T	;; recursive call: literal is marked as related during
			   ;; recursion.
			   (RDS-MARK.PUT SUBSUMENT.MARKS OTHERLITNO 'RELATE)
			   (SETQ SUCCESS
				 (RED=LS_RELATION.INJECTIVE NOLIT (1+ LITNO) S.UNIS S.LITERALS OPERATION.LINK SUBSUMER
							    SUBSUMER.SUBSUMENT.S.LINKS SUBSUMENT1 SUBSUMENT2 CONDITION))
			   (RDS-MARK.PUT SUBSUMENT.MARKS OTHERLITNO NIL)))))
		     (EQL (CAR SUCCESS) 'RED*LS_REMOVE)))
	       (RDS-MARK SUBSUMER.SUBSUMENT.S.LINKS LITNO))
    SUCCESS))

(DEFUN RED=LS_RELATION.MARK
       (SUBSUMENT.MARKS1 SUBSUMENT1 SUBSUMENT.NOLIT1 SUBSUMENT.MARKS2 SUBSUMENT2 SUBSUMENT.NOLIT2
	SUBSUMER.SUBSUMENT.S.LINKS SUBSUMER SUBSUMER.NOLIT)
						; edited:  2-jul-84 09:11:03
						; input:  'subsumentj' is a clause with
						;         'subsument.nolitj' literals,
						;         'subsument.marksj' is an array with length
						;         'subsument.nolitj' (j = 1, 2).
						;         'subsumer' is a clause with
						;         'subsumer.nolit' literals,
						;         'subsumer.subsument.s.links' is an array
						;         with 'subsumer.nolit' cells.
						;         cell i of an array corresponds with literal
						;         i of its clause.
						; effect: s-link between clause 'subsumer' and the
						;         clauses 'subsument1' and 'subsument2' are
						;         stored in 'subsumer.subsument.s.links' in
						;         the form explained in
						;         'red=ls_relation.s.link'.
						;         oriented s-links must have 'subsumer' as
						;         positive parent clause.
						;         only links compatible with binding and
						;         compatible with the quick matcher test are
						;         stored.
						;         form of storing:
						;         ((subsumentj . subsumentj_litno) .
						;         (subsument_marksj).
						;         unifiers_of_link_relative_to_binding)).
						; value:  not nil iff each cell of
						;         'subsumer.subsument.s.links' is not nil
						;         and the number of literals attached in
						;         the subsuments is greater or equal than
						;         'subsumer.nolit'.
  (let (OTHERPAR (FAILURE NIL))
    (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBSUMER))
      (PROGN
	(MAPC #'(LAMBDA (NC.LINKS)
		  (MAPC #'(LAMBDA (NC.LINK)
			    (COND
			      ((and (not (ds-link.sort.inhibited nc.link))
				    (OR (EQL SUBSUMER (DS-LINK.POSPAR NC.LINK))
					(NOT (RED.SERVICE-LINK.ORIENTED NC.LINK))))
			       (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL NC.LINK SUBSUMER))
			       (COND
				 ((EQL SUBSUMENT1 OTHERPAR)
				  (RED=LS_RELATION.S.LINK NC.LINK OTHERPAR SUBSUMENT.MARKS1 SUBSUMER (1+ RPTN)
							  SUBSUMER.SUBSUMENT.S.LINKS))
				 ((EQL SUBSUMENT2 OTHERPAR)
				  (RED=LS_RELATION.S.LINK NC.LINK OTHERPAR SUBSUMENT.MARKS2 SUBSUMER (1+ RPTN)
							  SUBSUMER.SUBSUMENT.S.LINKS))))))
			NC.LINKS))
	      (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'NOT.COMPLEMENTARY.EXTERNAL) SUBSUMER (1+ RPTN)))
	(COND ((NOT (RDS-MARK SUBSUMER.SUBSUMENT.S.LINKS (1+ RPTN))) (SETQ FAILURE T) (SETQ RPTN 0)))))
    (AND (NOT FAILURE)
	 (RED=LS_RELATION.CHECK SUBSUMENT.MARKS1 SUBSUMENT.MARKS2 SUBSUMENT.NOLIT1 SUBSUMENT.NOLIT2
				SUBSUMER.NOLIT))))

(DEFUN RED=LS_RELATION.S.LINK (NC.LINK SUBSUMENT SUBSUMENT.MARKS SUBSUMER SUBSUMER.LITNO SUBSUMER.SUBSUMENT.S.LINKS)
						; edited:  2-jul-84 08:32:27
						; input:  'nc.link' is a subsumption link incident
						;         with clause 'susument' and
						;         with literal with number 'subsumer.litno'
						;         in clause 'subsumer'.
						;         'subsument.marks' is a mark array with
						;         same length as clause 'subsument'.
						;         a mark 'remove' in cell i of 'subsument.-
						;         marks' means that literal i of clause
						;         'subsument' must be regarded as removed for
						;         the subsumption literal relation (resolution
						;         literal).
						;         'subsumer.subsument.s.links' is an array
						;         with the same length as clause 'subsumer'.
						; effect: if one of the unifiers of link 'nc.link'
						;         is compatible with the binding and quick
						;         test says that it can be made to a matcher
						;         two things are done:
						;         to the list in cell 'subsumer.litno' of the
						;         array 'subsumer.subsument.s.links' the
						;         following first element is constructed
						;         ((subsument . subsument_litno) .
						;         (subsument_marks .
						;         unifiers_of_nc_link_relative_to_binding)).
						;         the first three parts are for quick
						;         access, the last one for a quick merge.
						;         secondly the parent literal of 'nc.link'
						;         in the clause 'subsument' is marked with
						;         'relate' in the array 'subsument.marks'.
						; value:  undefined.
  (let ((SUBSUMENT.LITNO (RED.SERVICE-OTHERLITNO.EXTERNAL NC.LINK SUBSUMER)) UNIFIERS VARIABLES DESCRIPTOR)
    (SETQ VARIABLES (DS-CLAUSE.LIT.VARIABLES SUBSUMENT SUBSUMENT.LITNO))
    (COND ((NEQ (RDS-MARK SUBSUMENT.MARKS SUBSUMENT.LITNO) 'REMOVE)
	   (MAPC #'(LAMBDA (NC.UNI)
		     (MAPC #'(LAMBDA (MOD.BINDING.UNI)
			       (COND
				 ((UNI-UNIFIER.CANBE.MATCHER MOD.BINDING.UNI VARIABLES)
				  (SETQ UNIFIERS (CONS MOD.BINDING.UNI UNIFIERS)))))
			   (UNI-UNIFY.MIXED.TERMLIST NC.UNI)))
		 (DS-LINK.UNIFIERS NC.LINK))
	   (COND
	     (UNIFIERS
	      (SETQ DESCRIPTOR
		    (CONS (CONS (CONS SUBSUMENT SUBSUMENT.LITNO) (CONS SUBSUMENT.MARKS UNIFIERS))
			  (RDS-MARK SUBSUMER.SUBSUMENT.S.LINKS SUBSUMER.LITNO)))
	      (RDS-MARK.PUT SUBSUMER.SUBSUMENT.S.LINKS SUBSUMER.LITNO DESCRIPTOR)
	      (RDS-MARK.PUT SUBSUMENT.MARKS SUBSUMENT.LITNO 'RELATE)))))))

(DEFUN RED=LS_RELATION.CHECK
       (SUBSUMENT.MARKS1 SUBSUMENT.MARKS2 SUBSUMENT.NOLIT1 SUBSUMENT.NOLIT2 SUBSUMER.NOLIT)
						; edited:  2-jul-84 08:24:32
						; input:  'subsument.marksi' is an array with length
						;         'subsument.noliti', 'subsumer.nolit' is a
						;         natural number.
						; effect: all cells of the arrays containing the
						;         atom 'relate' are set to nil.
						; value:  not nil iff the number of cells in the
						;         arrays containing the atom 'relate' is
						;         greater or equal than 'subsumer.nolit'.
						;         (literals in parents of the link to be
						;         subsumed have enough s-links to the subsumer
						;         clause.
  (PROG ((RELATE.NUMBER 0))
	(COND
	  (SUBSUMENT.MARKS2
	   (DODOWN (RPTN SUBSUMENT.NOLIT2)
	     (COND
	       ((EQL (RDS-MARK SUBSUMENT.MARKS2 (1+ RPTN)) 'RELATE) (RDS-MARK.PUT SUBSUMENT.MARKS2 (1+ RPTN) NIL)
		(SETQ RELATE.NUMBER (1+ RELATE.NUMBER)))))))
	(DODOWN (RPTN SUBSUMENT.NOLIT1)
	  (COND
	    ((EQL (RDS-MARK SUBSUMENT.MARKS1 (1+ RPTN)) 'RELATE) (RDS-MARK.PUT SUBSUMENT.MARKS1 (1+ RPTN) NIL)
	     (SETQ RELATE.NUMBER (1+ RELATE.NUMBER)))))
	(RETURN (NOT (< RELATE.NUMBER SUBSUMER.NOLIT)))))

(DEFUN RED=LS_RELATION.MATCH (S.UNIS S.LITERALS NOT.CHECK.LITERALS VARIABLES SUBSUMER OP.LINK SUBSUMENT1 SUBSUMENT2
			      &OPTIONAL CONDITION FLAG)
						; edited:  2-jul-84 08:48:45
						; input:  's.unis' is a list of unifiers.
						;         's.literals' is a list of elements
						;         (litno . (clause . litno)), denoting the
						;         literal function from subsumer literal to
						;         a subsument literal.
						;         each element of 's.literals' corresponds
						;         with a sublist of 's.unifiers'.
						;         'not.check.literals' is a list of literal
						;         numbers. for them link condition is
						;         automatically fulfilled. it must not be
						;         checked.
						;         'variables' is a set of variables, which are
						;         not allowed to be modified by the matcher
						;         merged from 's.unis'.
						;         'subsumer', 'subsument1', and 'subsument2'
						;         are clauses.
						;         'op.link' is an operation link incident
						;         with clauses 'subsument1' and 'subsument2'.
						;         'condition' is the option, i.e. one of the
						;         atoms 't', 'remove', 'inhibit', and
						;         'remove-inhibit'.
						; effect: -
						; value:  a list (indicator subsubsumer matcher) if
						;         one of the three cases is met:
						;         indicator is
						;         'red*ls_remove': matcher merge of 's.unis'
						;           exists and 'condition' is 't' or
						;           link condition is fulfilled and
						;           condition is 'remove' or 'remove-inhibit'.
						;         'red*ls_inhibit': matcher merge exists and
						;           'condition' is 'inhibit' or link
						;           condition fails and 'condition' is
						;           'remove-inhibit'.
						;         'red*ls_recheck': link condition fails and
						;           'condition' is 'remove'. merge exists.
						;         matcher is the matcher merge of 's.unis'
						;         not modifying 'variables', subsumer is
						;         'subsumer'.
						;         value is nil else.
						; remark: link condition is explained in
						;         'red.lc-link.subsumption'.
  (PROG (MATCHER.MERGE SUBSUMPTION)
	(COND
	  ((SETQ MATCHER.MERGE (UNI-MERGE.LIST.OF.MATCHERLISTS.fit.on.sorts S.UNIS VARIABLES (RDS-BINDING.TOP)))
   (CASE CONDITION
	     ((T) (SETQ SUBSUMPTION (LIST 'RED*LS_REMOVE SUBSUMER MATCHER.MERGE)))
	     (INHIBIT (SETQ SUBSUMPTION (LIST 'RED*LS_INHIBIT SUBSUMER MATCHER.MERGE)))
	     (REMOVE-INHIBIT
	       (COND
		 ((RED.LC-LINK.SUBSUMPTION S.LITERALS NOT.CHECK.LITERALS SUBSUMER OP.LINK SUBSUMENT1 SUBSUMENT2
					   MATCHER.MERGE FLAG)
		  (SETQ SUBSUMPTION (LIST 'RED*LS_REMOVE SUBSUMER MATCHER.MERGE)))
		 (T (SETQ SUBSUMPTION (LIST 'RED*LS_INHIBIT SUBSUMER MATCHER.MERGE)))))
	     (REMOVE
	       (COND
		 ((RED.LC-LINK.SUBSUMPTION S.LITERALS NOT.CHECK.LITERALS SUBSUMER OP.LINK SUBSUMENT1 SUBSUMENT2
					   MATCHER.MERGE FLAG)
		  (SETQ SUBSUMPTION (LIST 'RED*LS_REMOVE SUBSUMER MATCHER.MERGE)))
		 (T (SETQ SUBSUMPTION (LIST 'RED*LS_RECHECK SUBSUMER MATCHER.MERGE)))))
	     (OTHERWISE (ERROR "red-check - illegal condition in red=ls_relation.match.remove: : ~a" CONDITION)))))
	(RETURN SUBSUMPTION)))

