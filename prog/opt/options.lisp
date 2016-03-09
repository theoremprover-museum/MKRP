;;; -*- mode: lisp; syntax: common-lisp; package: mkrp -*-

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

(defun opt=ignore (args expr)
						; Edited:  25-OCT-1991 20:49
						; Authors: PRCKLN
						; Input:   A list of Lisp symbols and a Lisp expression.
						; Effect:  -
						; Value:   (DECLARE (IGNORE A1 ... AN)) with A1 through AN the symbols
						;          in ARGS that are not in EXPR.
  `(declare (ignore ,@(remove-if #'(lambda (s) (in s expr)) args))))

;;; Access functions to internal structures

(eval-when (load compile eval)
(DEFUN OPT=GET.OPTION.NAME (OPTION.DESCRIPTOR)
  ;; input:-sexpr (option-descriptor=element of an assoc-   list of form (op-name def-val arg-range exp1"
  ;;    exp2..)              value: the option-name opid out of oplis (= car]])"))
  (CAR OPTION.DESCRIPTOR))

(DEFUN OPT=GET.DEFAULT.VALUE (OPTION.DESCRIPTOR)
  ;; input:-sexpr (option-descriptor=element of an assoc-   list of form (op-name def-val arg-range exp1"
  ;;    exp2..)              value: the default-value of the option opid (= cadr)"))
  (SECOND OPTION.DESCRIPTOR))

(DEFUN OPT=GET.ARGUMENT.RANGE (OPTION.DESCRIPTOR)
  ;; input:-sexpr (option-descriptor=element of an assoc-   list of form (op-name def-val arg-range exp1"
  ;;    exp2..)              value: the legal argument-range of opid (=caddr)"))
  (THIRD OPTION.DESCRIPTOR))

(DEFUN OPT=GET.OPTION.EXPLANATIONS (OPTION.DESCRIPTION)
  ;; input: -sexpr (an option-description of form (name    def.val arg.range exp1 exp2...))"
  ;; value:  the explanations (exp1...) of the option    (= cdddr option.description)."))
  (CDDDR OPTION.DESCRIPTION))

(DEFUN OPT=GET.OPTION.demon (OPTION.DESCRIPTION)
						; Edited:  25-OCT-1991 20:35
						; Authors: PRCKLN
						; Input:   an option-description of form (name def.val arg.range exp1 exp2...))
						; Effect:  -
						; Value:   The demon component of the description
  (fifth OPTION.DESCRIPTION))

(DEFUN OPT=GET.AREA.NAME (AREA.DESCRIPTOR)
  ;; input:-sexpr (area-descriptor = element of an assoc-   list of form (ar-name headers area.opts exp1"
  ;;    exp2..)              value: the name of the area (=car area)"))
  (CAR AREA.DESCRIPTOR))

(DEFUN OPT=GET.AREA.HEADER (AREA.DESCRIPTOR)
  ;; input:-sexpr (area-descriptor = element of an assoc-   list of form (ar-name headers area.opts exp1"
  ;;    exp2..)              value: the header of the area-spec. (=list of lists"
  ;;    textstrings printed as a headline for the     area)."))
  (SECOND AREA.DESCRIPTOR))

(DEFUN OPT=GET.AREA.OPTIONS (AREA.DESCRIPTOR)
  ;; input:-sexpr (area-descriptor = element of an assoc-   list of form (ar-name headers area.opts exp1"
  ;;    exp2..)              value: the list of all leagal option-names of this"
  ;;    area (op1...). may be nil if area is just     created (=caddr area)."))
  (THIRD AREA.DESCRIPTOR))

(DEFUN OPT=GET.AREA.EXPLANATIONS (AREA.DESCRIPTION)
  ;; input: -sexpr (an area-description of form (name    headlines options exp1 exp2...))"
  ;; value:  the explanations (exp1...) of the area      (= cdddr area.description)."))
  (CDDDR AREA.DESCRIPTION))

;;; Global variables containing all info
)
(eval-when (load compile eval)
(DEFparameter OPT*ALL.AREAS
	      '((TWO
		  #+:mkrp.deutsch("Erzeugung der Regelklauseln")
		  #-:mkrp.deutsch("Generation of rule clauses")
		  (TWO_RULES TWO_RULES.MAXLEVEL TWO_SUPPRESS.NORULES)
		  #+:mkrp.deutsch("Optionen zur Anwendung der (zwei-literal-) regeln")
		  #-:mkrp.deutsch("Options to apply the (two-literal-) rules"))
		(RED.I
		  #+:mkrp.deutsch("Reduktionsregeln fuer initiale objekte")
		  #-:mkrp.deutsch("Reduction rules for initial objects")
		  (RED.I_CLAUSE.MULTIPLE.LITERALS RED.I_CLAUSE.PURITY RED.I_CLAUSE.TAUTOLOGY
						  RED.I_CLAUSE.TAUTOLOGY.RECHECK RED.I_CLAUSE.SUBSUMPTION
						  RED.I_CLAUSE.SUBSUMPTION.RECHECK
						  RED.I_CLAUSE.REPL.FACTORING RED.I_CLAUSE.REPL.FACTORING.RECHECK
						  RED.I_CLAUSE.REPL.RESOLUTION
						  RED.I_CLAUSE.REPL.RESOLUTION.RECHECK RED.I_CLAUSE.REWRITING
						  RED.I_LINK.INCOMPATIBILITY
						  RED.I_LINK.TAUTOLOGY RED.I_LINK.TAUTOLOGY.RECHECK RED.I_LINK.SUBSUMPTION
						  RED.I_LINK.SUBSUMPTION.RECHECK)
		  #+:mkrp.deutsch("optionen fuer verschiedene reduktionsregeln im initialen graphen.")
		  #-:mkrp.deutsch("Options for various reductionrules in initial graph."))
		(RED.D
		  #+:mkrp.deutsch("Reduktionsregeln fuer deduzierte Objekte")
		  #-:mkrp.deutsch("Reduction rules for deduced objects")
		  (RED.D_CLAUSE.MULTIPLE.LITERALS RED.D_CLAUSE.PURITY RED.D_CLAUSE.TAUTOLOGY
						  RED.D_CLAUSE.TAUTOLOGY.RECHECK RED.D_CLAUSE.SUBSUMPTION.FORWARD
						  RED.D_CLAUSE.SUBSUMPTION.BACKWARD
						  RED.D_CLAUSE.SUBSUMPTION.RECHECK RED.D_CLAUSE.REPL.FACTORING
						  RED.D_CLAUSE.REPL.FACTORING.RECHECK
						  RED.D_CLAUSE.REPL.RESOLUTION RED.D_CLAUSE.REPL.RESOLUTION.RECHECK
						  RED.d_CLAUSE.REWRITING RED.D_LINK.INCOMPATIBILITY
						  RED.D_LINK.TAUTOLOGY RED.D_LINK.TAUTOLOGY.RECHECK RED.D_LINK.SUBSUMPTION
						  RED.D_LINK.SUBSUMPTION.RECHECK)
		  #+:mkrp.deutsch("Optionen fuer verschiedene Reduktionsregeln waehrend der Deduktion.")
		  #-:mkrp.deutsch("Options for various reductionrules during deduction."))
		(STR
		  #+:mkrp.deutsch("Steuerung der Beweissuche")
		  #-:mkrp.deutsch("Control of the proof-searching-behaviour")
		  (FAC_INITIAL FAC_EACH.STEP STR_RESOLUTION STR_LINK.DEPTH str_r.selection
			       STR_TERM.DEPTH str_finite.domain
			       TERM_UNITS TERM_ITERATIONS TERM_SET.OF.SUPPORT TERM_BREADTH.FIRST)
		  #+:mkrp.deutsch("Optionen zur Beeinflussung des Suchverhaltens waehrend eines Beweises.")
		  #-:mkrp.deutsch("Options to direct the searching-behaviour while looking for a proof."))
		(SORT
		  #+:mkrp.deutsch("Steuerung der Sortenbehandlung")
		  #-:mkrp.deutsch("Control of Sort Handling")
		  (sort_literals sort_max.unification.rule.steps sort_max.unification.tree.depth
				 sort_max.unification.tree.open.nodes sort_unifier.stop.number sort_show.variable.sorts)
		  #+:mkrp.deutsch("Optionen zur Steuerung der Behandlung von Sorten.")
		  #-:mkrp.deutsch("Options to control the handling of sorts."))
		(er
		  #+:mkrp.deutsch("Gleichheitsbeweisen")
		  #-:mkrp.deutsch("Equality reasoning")
		  (er_paramodulation er_completion er_weight.polynomials er_p.selection er_cp.reduction
				     er_ordering er_operator.ordering
				     er_knuth.bendix.weight er_polynomial.weight er_narrow.depth er_narrow.next
				     er_narrow.test)
		  #+:mkrp.deutsch("Optionen zur Steuerung des eingebauten Gleichheitsbeweisers.")
		  #-:mkrp.deutsch("Options to control the built in equality resoning procedure."))
		(GEN
		  #+:mkrp.deutsch("allgemeine optionen")
		  #-:mkrp.deutsch("Miscellaneous options")
		  (GEN_SPLITTING gen_presimplification gen_MIN.EXPRESSION.LENGTH.FOR.FILE gen_MIN.EXPRESSION.SIZE.FOR.FILE
				 GEN_MANUAL.CONTROL GEN_MAXIMUM.STEPS gen_maximum.time GEN_GRAPH.SAVING GEN_SAVE.FILE
				 GEN_LISP.GARBAGE.COLLECTION gen_common.lisp gen_other.prover)
		  #+:mkrp.deutsch("Verschiedene allgemeine Optionen die zu keinem anderen Bereich gehoeren.")
		  #-:mkrp.deutsch("Various general options, not belonging to any other option-area."))
		(TR
		  #+:mkrp.deutsch("Ablaufverfolgung")
		  #-:mkrp.deutsch("Tracing")
		  (TR_PREPROCESSING TR_STEP.MODE TR_DUMP TR_CLAUSE.MODE TR_LINK.MODE TR_TRACE.FILE TR_TERMINAL)
		  #+:mkrp.deutsch("Optionen zur Verfolgung des Programmablaufs.")
		  #-:mkrp.deutsch("Options for various program traces."))
		(PR
		  #+:mkrp.deutsch("Protokollmodi")
		  #-:mkrp.deutsch("Protocol modes")
		  (pr_latex PR_INFIX.FORM PR_PREFIX.FORM PR_OPTIONS PR_AXIOM.CLAUSES PR_SYMBOLS
			    pr_direct.proof pr_variable.print.names
			    PR_PROTOCOL PR_LEFT.MARGIN PR_RIGHT.MARGIN PR_LINELENGTH pr_literals)
		  #+:mkrp.deutsch("optionen zur auswahl verschiedener beweisprotokolle.")
		  #-:mkrp.deutsch("options to select from different proof-protocols."))))

(defparameter OPT*ALL.OPTIONS
	      '((TWO_RULES
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("der (zwei-literal-)regelklausel-mechanismus ist mit"
				  "  t/j/ja    eingeschaltet."
				  "  partial   mit strengerer regelauswahl eingeschaltet."
				  "  nil/n/nein ausgeschaltet.")
		  #-:mkrp.deutsch("the (two-literal) rule clause algorithm is"
				  "  switched on by   t/y/yes"
				  "  switched on by   partial  with less rules allowed."
				  "  switched off by  nil/n/no"))
		(TWO_RULES.MAXLEVEL
		  1 (RANGE (0 255))
		  #+:mkrp.deutsch("  nat. zahl   der regelgraph wird bis zur schicht n vervollstaendigt.")
		  #-:mkrp.deutsch("  nat. number   the rule graph is completed up to level n."))
		(TWO_SUPPRESS.NORULES
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("der regelgraph liefert nichtregeln in den graphen:"
				  "  t/j/ja    eingeschaltet."
				  "  nil/no/nein ausgeschaltet.")
		  #-:mkrp.deutsch("the rule graph generates norules for the graph:"
				  "  switched on by   t/y/yes"
				  "  switched off by  nil/n/no"))
		(RED.I_CLAUSE.MULTIPLE.LITERALS
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   loeschen von mehrfach-literalen:"
				  " t/j/ja   eingeschaltet."
				  " nil/n/nein   ausgeschaltet.")
		  #-:mkrp.deutsch("   removal of multiple literals:"
				  " t/y/yes    switched on."
				  " nil/n/no   switched off."))
		(RED.I_CLAUSE.PURITY
		  partial (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("   behandlung initialer purity-klauseln:"
				  " t/j/ja    loeschung der klauseln."
				  " nil/n/nein  keine loeschung.")
		  #-:mkrp.deutsch("   Treatment of initial pure clauses:"
				  " t/y/yes     Removal of the clauses."
				  " p/partial   Removal only if no equations are in the clauses."
				  " nil/n/no    No removal."))
		(RED.I_CLAUSE.TAUTOLOGY
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   behandlung initialer tautologie-klauseln:"
				  " t/j/ja    loeschung der klauseln."
				  " nil/n/nein  keine loeschung.")
		  #-:mkrp.deutsch("   treatment of initial tautology-clauses:"
				  " t/y/yes   removal of the clauses."
				  " nil/n/no    no removal."))
		(RED.I_CLAUSE.TAUTOLOGY.RECHECK
		  PARTIAL (SET (PARTIAL P p) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   erneuter tautologietest bei linkeinfuegungen:"
				  " partial/p   eingeschaltet fuer inzidente klauseln."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed tautology check upon link insertion:"
				  " partial/p   switched on for incident clauses."
				  " nil/n/no  switched off."))
		(RED.I_CLAUSE.SUBSUMPTION
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   behandlung initialer subsumierter klauseln:"
				  " t/j/ja    loeschung der klauseln."
				  " nil/n/nein  keine loeschung.")
		  #-:mkrp.deutsch("   treatment of initial subsumed clauses:"
				  " t/y/yes   removal of the clauses."
				  " nil/n/no    no removal."))
		(RED.I_CLAUSE.SUBSUMPTION.RECHECK
		  PARTIAL (SET (PARTIAL P p) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   erneuter subsumptionstest bei linkeinfuegungen:"
				  " partial/p   eingeschaltet fuer inzidente klauseln."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed subsumption check upon link insertion:"
				  " partial/p   switched on for incident clauses."
				  " nil/n/nein  switched off."))
		(RED.I_CLAUSE.REPL.FACTORING
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   ersetzung initialer klauseln durch replacement-faktoren:"
				  " t/j/ja    eingeschaltet."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   substitution of replacement factors for initial clauses:"
				  " t/y/yes     switched on."
				  " nil/n/no    switched off."))
		(RED.I_CLAUSE.REPL.FACTORING.RECHECK
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   erneuter test auf replacement-faktorisierung bei linkeinfuegungen:"
				  " t/j/ja      eingeschaltet."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed check for replacement factoring upon link insertion:"
				  " t/j/ja      switched on."
				  " nil/n/nein  switched off."))
		(RED.I_CLAUSE.REPL.RESOLUTION
		  SIMPLE (SET (GENERALIZING G g) (SIMPLE S s) (UNIT U u) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   ersetzung initialer klauseln durch replacement-resolventen:"
				  " generalizing/g   eines der resolutions-literale kann durch ein"
				  "                  allgemeineres der anderen beteiligten klausel(n)"
				  "                  ersetzt werden."
				  " simple/s     eingeschaltet, aber nur fuer einen schritt, ohne"
				  "              generalisierung."
				  " unit/u       eingeschaltet, aber nur fuer unit-partner."
				  " nil/n/nein   ausgeschaltet.")
		  #-:mkrp.deutsch("   substitution of replacement resolvents for initial clauses:"
				  " generalizing/g   one of the resolution literals is replaced by more"
				  "                  general of the other clause."
				  " simple/s     switched on, only for one step, without generalizing."
				  " unit/u       switched on for unit partner only."
				  " nil/n/no     switched off."))
		(RED.I_CLAUSE.REPL.RESOLUTION.RECHECK
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   erneuter test auf replacement-resolution bei linkeinfuegungen:"
				  " t/j/ja    eingeschaltet."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed check for replacement resolution upon link insertion:"
				  " t/y/yes     switched on."
				  " nil/n/no    switched off."))
		(RED.I_CLAUSE.REWRITING
		  dem (SET (T yes JA Y J) (dem demodulations demodulationen dem dem)
			   (def definitions definitionen def def) (nil no NEIN n n))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch(" t/y/yes      Replacement by rewrite rules switched on."
				  " def          Elimination of constant and function symbols by rewritng initial"
				  "              clauses only."
				  " demodulation Demodulation rules only."
				  " nil/n/no     Switched off."))
		(RED.I_LINK.INCOMPATIBILITY
		  t (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("   behandlung initialer inkompatibilitaets-links:"
				  " t/j/ja    loeschung der links."
				  " nil/n/nein  keine loeschung.")
		  #-:mkrp.deutsch("   treatment of initial incompatibility links:"
				  " t/y/yes     removal of the links."
				  " nil/n/no    no removal."))
		(RED.I_LINK.TAUTOLOGY
		  REMOVE-INHIBIT
		  (SET (T yes JA Y J) (REMOVE-INHIBIT ri RI) (INHIBIT I i) (REMOVE R r) (nil no NEIN n n))
		  #+:mkrp.deutsch("   behandlung initialer tautologie-links:"
				  " t/j/ja     loeschung der links ohne test der linkbedingung."
				  " remove-inhibit/ri  loeschung falls linkbedingung erfuellt, sonst"
				  "                    sperrung."
				  " inhibit/i    sperrung der links."
				  " remove/r     loeschung der links, die die linkbedingung erfuellen."
				  " nil/n/nein   weder loeschung noch sperrung.")
		  #-:mkrp.deutsch("   treatment of initial tautology links:"
				  " t/y/yes      removal of the links without link condition check."
				  " remove-inhibit/ri  removal of the links complying with the link"
				  "                    condition. inhibition of the others."
				  " inhibit/i    inhibition of the links."
				  " remove/r     removal of the links complying with the link"
				  "        condition."
				  " nil/n/no     no removal, no inhibition."))
		(RED.I_LINK.TAUTOLOGY.RECHECK
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("red.i_link.tautology.recheck"
				  "   erneuter linktautologietest bei linkausfuegungen:"
				  " t/j/ja    eingeschaltet."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed link tautology check upon link removal:"
				  " t/y/yes   switched on."
				  " nil/n/no  switched off."))
		(RED.I_LINK.SUBSUMPTION
		  REMOVE-INHIBIT
		  (SET (T yes JA Y J) (REMOVE-INHIBIT RI ri) (INHIBIT I i) (REMOVE R r) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   behandlung initialer subsumierter links:"
				  " t/j/ja     loeschung der links ohne test der linkbedingung"
				  " remove-inhibit/ri  loeschung falls linkbedingung erfuellt, sonst"
				  "        sperrung."
				  " inhibit/i    sperrung der links."
				  " remove/r     loeschung der links, die die linkbedingung erfuellen."
				  " nil/n/nein   weder loeschung noch sperrung.")
		  #-:mkrp.deutsch("   Treatment of initial subsumed links:"
				  " t/y/yes      Removal of the links without link condition check."
				  " remove-inhibit/ri  Removal of the links complying with the link"
				  "                    condition. Inhibition of the others."
				  " inhibit/i    Inhibition of the links."
				  " remove/r     Removal of the links complying with the link"
				  "        condition."
				  " nil/n/no     No removal, no inhibition."))
		(RED.I_LINK.SUBSUMPTION.RECHECK
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   erneuter linksubsumptionstest bei linkausfuegungen:"
				  " t/j/ja    eingeschaltet."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   Renewed link subsumption check upon link removal:"
				  " t/y/yes   switched on."
				  " nil/n/no  switched off."))
		(RED.D_CLAUSE.MULTIPLE.LITERALS
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   loeschen von mehrfach-literalen in deduzierten klauseln:"
				  " t/j/ja   eingeschaltet."
				  " nil/n/nein   ausgeschaltet.")
		  #-:mkrp.deutsch("   removal of multiple literals in deduced clauses:"
				  " t/y/yes    switched on."
				  " nil/n/no   switched off."))
		(RED.D_CLAUSE.PURITY
		  partial (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("   behandlung deduzierter purity-klauseln:"
				  " t/j/ja    loeschung der klauseln."
				  " nil/n/nein  keine loeschung.")
		  #-:mkrp.deutsch("   Treatment of deduced purity clauses:"
				  " t/y/yes     Removal of the clauses."
				  " p/partial   Removal only if no equations are in the clauses."
				  " nil/n/no    No removal."))
		(RED.D_CLAUSE.TAUTOLOGY
		  REMOVE-INHIBIT
		  (SET (T yes JA Y J) (REMOVE-INHIBIT RI ri) (INHIBIT I i) (REMOVE R r) (nil NO NEIN n))
		  #+:mkrp.deutsch("   Behandlung deduzierter Tautologie-Klauseln:"
				  " T/J/JA     Loeschung der Klauseln ohne Test der Linkbedingung."
				  " REMOVE-INHIBIT/RI  Loeschung der Klauseln. falls Linkbedingung nicht"
				  "        erfuellt, wiedereinfuegen und sperren der"
				  "        erzeugerlinks."
				  " INHIBIT/I    Loeschung der Klauseln, sowie Wiedereinfuegen und"
				  "        Sperren der Erzeugerlinks."
				  " REMOVE/R     Loeschung der Klauseln, die die Linkbedingung"
				  "        erfuellen."
				  " NIL/N/NEIN   Weder loeschung noch sperrung.")
		  #-:mkrp.deutsch("   Treatment of deduced tautology clauses:"
				  " T/Y/YES      Removal of the clauses without link condition check."
				  " REMOVE-INHIBIT/RI  Removal of the clauses. where link condition is met,"
				  "                    reinsertion and inhibition of creator links."
				  " INHIBIT/I    Removal of the clauses as well as reinsertion and"
				  "              inhibition of creator links."
				  " REMOVE/R     Removal of the links complying with the link"
				  "              condition."
				  " NIL/N/NO     No removal, no inhibition."))
		(RED.D_CLAUSE.TAUTOLOGY.RECHECK
		  Nil (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("   Erneuter tautologietest bei ein- und ausfuegungen von links:"
				  " t/j/ja    eingeschaltet fuer ein- und ausfuegungen."
				  " partial/p   eingeschaltet fuer einfuegungen inzidenter links."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   Renewed tautology check upon link removal and insertion:"
				  " t/y/yes     switched on for insertion and removal."
				  " partial/p   switched on for insertion of incident links."
				  " nil/n/no    switched off."))
		(RED.D_CLAUSE.SUBSUMPTION.FORWARD
		  REMOVE-INHIBIT
		  (SET (T yes JA Y J) (REMOVE-INHIBIT RI ri) (INHIBIT I i)
		       (REMOVE R r) (nil NO NEIN n))
		  #+:mkrp.deutsch("   behandlung deduzierter subsumierter klauseln."
				  " t/j/ja     loeschung der klauseln ohne test der linkbedingung."
				  " remove-inhibit/ri  bei deduzierten subsumierten klauseln loeschung."
				  "        falls linkbedingung nicht erfuellt, wiedereinfuegen"
				  "        und sperren des erzeugerlinks."
				  " inhibit/i    loeschung der klauseln, sowie wiedereinfuegen und"
				  "        sperren der erzeugerlinks."
				  " remove/r     loeschung der klauseln, die die linkbedingung"
				  "        erfuellen."
				  " nil/n/nein   weder loeschung noch sperrung.")
		  #-:mkrp.deutsch("   Treatment of deduced subsumed clauses."
				  " t/y/yes    removal of the clauses without link condition check."
				  " remove-inhibit/ri  in case of deduced subsumed clause, removal of the"
				  "        clause. if link condition is met reinsertion and"
				  "        inhibitation of creator link."
				  " inhibit/i    removal of the clauses as well as reinsertion and"
				  "        inhibition of creator links."
				  " remove/r     removal of the clauses complying with the link"
				  "        condition."
				  " nil/n/no     no removal, no inhibition."))
		(RED.D_CLAUSE.SUBSUMPTION.BACKWARD
		  REMOVE (SET (T yes JA Y J) (REMOVE R r) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   behandlung von klauseln, die von deduzierten klauseln subsumiert"
				  "   werden:"
				  " t/j/ja     loeschung der klauseln ohne test der linkbedingung."
				  " remove/r     loeschung der klauseln, die die linkbedingung"
				  "        erfuellen."
				  " nil/n/nein   weder loeschung noch sperrung.")
		  #-:mkrp.deutsch("   treatment of clauses, subsumed by deduced clauses:"
				  " t/y/yes    removal of the clauses without link condition check."
				  " remove/r     removal of the clauses complying with the link"
				  "        condition."
				  " nil/n/no     no removal, no inhibition."))
		(RED.D_CLAUSE.SUBSUMPTION.RECHECK
		  Nil (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("   erneuter subsumptionstest bei ein- und ausfuegungen von links:"
				  " t/j/ja    eingeschaltet fuer ein- und ausfuegungen."
				  " partial/p   eingeschaltet fuer einfuegungen inzidenter links."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed subsumption check upon link removal and insertion:"
				  " t/y/yes   switched on for insertion and removal."
				  " partial/p   switched on for insertion of incident links."
				  " nil/n/nein  switched off."))
		(RED.D_CLAUSE.REPL.FACTORING
		  t (SET (T yes JA Y J) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   ersetzung deduzierter klauseln durch replacement-faktoren:"
				  " t/j/ja    eingeschaltet."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   substitution of replacement factors for deduced clauses:"
				  " t,yes,y   switched on."
				  " nil/n/no    switched off."))
		(RED.D_CLAUSE.REPL.FACTORING.RECHECK
		  t (SET (T yes JA Y J) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   erneuter test auf replacement-faktorisierung bei linkeinfuegungen:"
				  " t/j/ja    eingeschaltet."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed check for replacement factoring upon link insertion:"
				  " t/j/ja    switched on."
				  " nil/n/nein  switched off."))
		(RED.D_CLAUSE.REPL.RESOLUTION
		  SIMPLE (SET (GENERALIZING G g) (SIMPLE S s) (UNIT U u) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   ersetzung von klauseln durch replacement-resolventen:"
				  " generalizing/g   eines der resolutions-literale kann durch ein"
				  "        allgemeineres der anderen beteiligten klausel(n)"
				  "        ersetzt werden."
				  " simple/s     eingeschaltet, aber nur fuer einen schritt, ohne"
				  "        generalisierung."
				  " unit/u     eingeschaltet, aber nur fuer unit-partner."
				  " nil/n/nein   ausgeschaltet.")
		  #-:mkrp.deutsch("   substitution of clauses by replacement resolvents:"
				  " generalizing/g   one of the resolution literals is replaced by more"
				  "        general of the other clause."
				  " simple/s     switched on, only for one step, without generalizing."
				  " unit/u     switched on for unit partner only."
				  " nil/n/no     switched off."))
		(RED.D_CLAUSE.REPL.RESOLUTION.RECHECK
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   erneuter test auf replacement-resolution bei linkeinfuegungen:"
				  " t/j/ja    eingeschaltet."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed check for replacement resolution upon link insertion:"
				  " t/y/yes   switched on."
				  " nil/n/no    switched off."))
		(RED.D_CLAUSE.REWRITING
		  dem (SET (T yes JA Y J) (dem demodulations demodulationen dem dem)
			   (def definitions definitionen def def) (nil no NEIN n n))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch(" t/y/yes      Replacement by rewrite rules switched on."
				  " def          Elimination of constant and function symbols by rewritng initial"
				  "              clauses only."
				  " demodulation Demodulation rules only."
				  " nil/n/no     Switched off."))
		(RED.D_LINK.INCOMPATIBILITY
		  t (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("   behandlung deduzierter inkompatibler links:"
				  " t/j/ja    loeschung der links."
				  " nil/n/nein  keine loeschung.")
		  #-:mkrp.deutsch("   treatment of deduced incompatibility links:"
				  " t/y/yes   removal of the links."
				  " nil/n/no    no removal."))
		(RED.D_LINK.TAUTOLOGY
		  REMOVE-INHIBIT
		  (SET (T yes JA Y J) (REMOVE-INHIBIT RI ri) (INHIBIT I i) (REMOVE R r) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   behandlung deduzierter tautologie-links:"
				  " t/j/ja     loeschung der links ohne test der linkbedingung."
				  " remove-inhibit/ri  loeschung falls linkbedingung erfuellt, sonst"
				  "        sperrung."
				  " inhibit/i    sperrung der links."
				  " remove/r     loeschung der links, die die linkbedingung erfuellen."
				  " nil/n/nein   weder loeschung noch sperrung.")
		  #-:mkrp.deutsch("   treatment of deduced tautology links:"
				  " t/y/yes    removal of the links without link condition check."
				  " remove-inhibit/ri  removal of the links complying with the link"
				  "        condition. inhibition of the others."
				  " inhibit/i    inhibition of the links."
				  " remove/r     removal of the links complying with the link"
				  "        condition."
				  " nil/n/no     no removal, no inhibition."))
		(RED.D_LINK.TAUTOLOGY.RECHECK
		  NIL (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("   erneuter linktautologietest bei aus- und einfuegungen von links:"
				  " t/j/ja    eingeschaltet fuer ein- und ausfuegungen."
				  " partial/p   eingeschaltet fuer einfuegungen adjazenter links."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed link tautology check upon link removal and insertion:"
				  " t/y/yes   switched on for removal and insertion."
				  " partial/p   switched on for insertion of adjacent links."
				  " nil/n/no  switched off."))
		(RED.D_LINK.SUBSUMPTION
		  REMOVE-INHIBIT
		  (SET (T yes JA Y J) (REMOVE-INHIBIT RI ri) (INHIBIT I i) (REMOVE R r) (NIL NO NEIN n))
		  #+:mkrp.deutsch("   behandlung deduzierter subsumierter links:"
				  " t/j/ja     loeschung der links ohne test der linkbedingung."
				  " remove-inhibit/ri  loeschung falls linkbedingung erfuellt, sonst"
				  "        sperrung."
				  " inhibit/i    sperrung der links."
				  " remove/r     loeschung der links, die die linkbedingung erfuellen."
				  " nil/n/nein   weder loeschung noch sperrung.")
		  #-:mkrp.deutsch("   treatment of deduced subsumed links:"
				  " t/y/yes    removal of links without link condition check."
				  " remove-inhibit/ri  removal of the links complying with the link"
				  "        condition. inhibition of the others."
				  " inhibit/i    inhibition of the links."
				  " remove/r     removal of the links complying with the link"
				  "        condition."
				  " nil/n/no     no removal, no inhibition."))
		(RED.D_LINK.SUBSUMPTION.RECHECK
		  NIL (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("   erneuter linksubsumptionstest bei ein- und ausfuegungen von links."
				  " t/j/ja    eingeschaltet fuer ein- und ausfuegungen."
				  " partial/p   eingeschaltet fuer einfuegungen adjazenter links."
				  " nil/n/nein  ausgeschaltet.")
		  #-:mkrp.deutsch("   renewed link subsumption check upon link removal and insertion:"
				  " t/y/yes   switched on for removal and insertion."
				  " partial/p   switched on for insertion of adjacent links"
				  " nil/n/no  switched off."))
		(FAC_INITIAL
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("  faktorisierung des initialen graphen :"
				  "  t/j/ja   eingeschaltet,"
				  "  nil/n/nein abgeschaltet.")
		  #-:mkrp.deutsch("  factorizing the initial graph :"
				  "  t/y/yes  switched on,"
				  "  nil/n/no   switched off."))
		(FAC_EACH.STEP
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("  faktorisierung der resolventen und paramodulanten :"
				  "  t/j/ja   eingeschaltet,"
				  "  nil/n/nein abgeschaltet.")
		  #-:mkrp.deutsch("    faktorizing the resolvents and paramodulants :"
				  "  t/y/yes      switched on,"
				  "  nil/n/no     switched off."))
		(STR_RESOLUTION
		  SET-OF-SUPPORT
		  (EITHER
		    (SET (all all all all) (BASIC-RESOLUTION B b BASIC) (SET-OF-SUPPORT S s SOS)
			 (UNIT-REFUTATION U u UNIT)
			 (LINEAR L l) (INDUCTION-SPECIAL I-S i-s)
			 (UNIT-REFUTATION.OR.BASIC-RESOLUTION U-B u-b) (UNIT-REFUTATION.OR.SET-OF-SUPPORT U-SOS u-sos)
			 (UNIT-REFUTATION.OR.LINEAR U-L u-l))
		    (STRUCTURE (SET (LINEAR L l) (UNIT-REFUTATION.OR.LINEAR U-L u-l)) (.AXM .THM) (RANGE (1 121))))
		  #+:mkrp.deutsch("folgende strategien stehen zur verfuegung"
				  "basic-resolution            (basic b)"
				  "set-of-support              (sos s)"
				  "unit-refutation             (unit u)"
				  "induction-special           (i-s)"
				  "linear                      (l linear.axm## linear.thm## l.axm## l.thm##)"
				  "u-b   heisst unit oder basic"
				  "u-sos heisst unit oder sos"
				  "u-l   heisst unit oder linear  (u-l u-l.axm## u-l.thm##)")
		  #-:mkrp.deutsch("Available strategies are"
				  "basic-resolution            (basic b)"
				  "set-of-support              (sos s)"
				  "unit-refutation             (unit u)"
				  "induction-special           (i-s)"
				  "linear                      (l linear.axm## linear.thm## l.axm## l.thm##)"
				  "u-b   means unit or basic"
				  "u-sos means unit or sos"
				  "u-l   means unit or linear  (u-l u-l.axm## u-l.thm##)"))
		(str_r.selection
		  (* 10 (+ 2 variables (* 2 depth) (* 3 nolit)))
		  (list (* 10 (+ 2 variables (* 2 depth) (* 3 nolit))))
		  #+:mkrp.deutsch("  WEIGHT is the weight of the link, i.e. a value computed from the"
				  "  potential result."
				  "  NOLIT is the number of literals of the link."
				  "  VARIABLES is the potential number of variables in the result clause."
				  "  DEPTH is the depth of the link in the search space."
				  "  SUPPORT is true iff one of the parents is in the set of support."
				  "  EQUATIONAL is true iff all literals in the clause are positive"
				  "  equations.")
		  #-:mkrp.deutsch("  The value of this option determines the body of the selection function for R-links"
				  "  in the H-C strategy of ER_PARAMODULATION. The smallest link relative to this function"
				  "  is selected. The value must be positive."
				  "  The argumentlist of this function is"
				  "  (WEIGHT NOLIT VARIABLES DEPTH SUPPORT EQUATIONAL)."
				  "  WEIGHT is the weight of the link, i.e. a value computed from the"
				  "  potential result. It is determined by the heuristic value function."
				  "  NOLIT is the number of literals of the link."
				  "  VARIABLES is the potential number of variables in the result clause."
				  "  DEPTH is the depth of the link in the search space."
				  "  SUPPORT is true iff one of the parents is in the set of support."
				  "  EQUATIONAL is true iff all literals in the clause are positive"
				  "  equations. For this this option EQUATIONAL is always true.")
		  (let (#+symbolics(si:inhibit-fdefine-warnings t))
		    (compile 'sel=strat_r.selection `(lambda (weight nolit variables depth support equational)
						       ,(opt=ignore '(weight nolit variables depth support equational)
								  (opt-get.option str_r.selection))
						     ,(opt-get.option str_r.selection)))))
		(STR_LINK.DEPTH
		  nil (EITHER (RANGE (1 10000000)) (SET (NIL N NO NEIN)))
		  #+:mkrp.deutsch("n/nein/nil oder positive ganze zahl"
				  "        grenzwert der linktiefe (nil = unbegrenzt)")
		  #-:mkrp.deutsch("n/no/nil or positive integer"
				  "        upper bound for link depth (nil = unbounded)"))
		(STR_TERM.DEPTH
		  nil (EITHER (RANGE (1 10000000)) (SET (NIL N NO NEIN)))
		  #+:mkrp.deutsch("n/nein/nil oder positive ganze zahl"
				  "        grenzwert der termschachtelungstiefe (nil = unbegrenzt)")
		  #-:mkrp.deutsch("n/no/nil or positive integer"
				  "        upper bound for term nesting depth (nil = unbounded)"))
		(str_finite.domain
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch(" t/j/ja      Spezielle Abarbeitungskontrolle fuer endliche Grundbereiche"
				  "             die mit Klauseln X=A1, X=A2, ..., X=AN definiert werden."
				  " n/nil/nein  Normale Abarbeitung.")
		  #-:mkrp.deutsch(" t/y/yes   Special way of handling finite domains,"
				  "           which are defined via clauses X=A1, X=A2, ..., X=AN."
				  " n/nil/no  Normal control."))
		(TERM_UNITS
		  T (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("t/j/ja        vom terminator gefundene muster zur erzeugung von unit-"
				  "              klauseln werden abgearbeitet."
				  "n/nein/nil    muster werden ignoriert.")
		  #-:mkrp.deutsch("t/y/yes       patterns leading to unit clauses found by the terminator"
				  "              are processed."
				  "nil/n/no      patterns are ignored."))
		(TERM_ITERATIONS
		  0 (RANGE (0 10000000))
		  #+:mkrp.deutsch("ganze zahl >=0     anzahl der terminator-iterationen. 0 = schnelle suche.")
		  #-:mkrp.deutsch("integer   >=0      number of terminator iterations. 0 = fast search."))
		(TERM_SET.OF.SUPPORT
		  NIL (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("nil/n/nein         entspricht unit resolution"
				  "t/j/ja             ueberlagerung von unit resolution und set of support")
		  #-:mkrp.deutsch("nil/n/no           pure unit resolution"
				  "t/y/yes            superposition of unit resolution and set of support"))
		(TERM_BREADTH.FIRST
		  NIL (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("t/j/ja           echte breitensuche."
				  "nil/n/nein       die basis-suchstrategie des terminator wird verwandt"
				  "                 (aehnlich der ueblichen linearen strategie).")
		  #-:mkrp.deutsch("t/y/yes          Pure breadth first search."
				  "nil/n/no         The basic search strategy of the terminator is used"
				  "                 (similiar to the usual linear strategies)."))		
		(sort_literals
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("Das Unifikationsproblem fuer dynamische Sorten ist im allgemeinen unentscheidbar und"
				  "vom Typ INFINITARY. Deshalb ist es notwendig, den Unifikationsalgorithmus"
				  "bezueglich Laufzeit einzuschraenken. Bricht der Algorithmus aufgrund"
				  "der Begrenzung durch einen der Parameter ab, werden die nicht geloesten"
				  "Teilprobleme als Residuen in die Resolvente uebernommen. Dadurch ist"
				  "weiterhin die Vollstaendigkeit des Verfahrens garantiert."
				  " t/j/ja      Spezielle Abarbeitungskontrolle fuer Sorten."
				  " n/nil/nein  Normale Abarbeitung.")
		  #-:mkrp.deutsch("The unification problem for dynamic sorts is in general undecidable and"
				  "of type INFINITARY. Therefore it is necessary to restrict the search tree"
				  "of the unification algorithm. This is controlled by the options"
				  "SORT_MAX.UNIFICATION.RULE.STEPS, SORT_MAX.UNIFICATION.TREE.DEPTH,"
				  "SORT_MAX.UNIFICATION.TREE.OPEN.NODES, and SORT_UNIFIER.STOP.NUMBER."
				  "If the algorithm stops due to the restriction imposed by one of the"
				  "parameters, the unsolved subproblems are added to the resolvent as"
				  "residues. Hence the completeness of the calculus is preserved."
				  " t/y/yes   Special way of handling sorts using sort literals."
				  " n/nil/no  Normal sorts."))
		(sort_max.unification.rule.steps
		  100 (range (0 10000000))
		  #+:mkrp.deutsch(" Natuerliche Zahl >= 0    Obere Grenze fuer die Anzahl"
				  "                          der durchfuehrbaren Regelschritte.")
		  #-:mkrp.deutsch(" Integer >= 0   Upper bound for the number of rule steps to"
				  "                be performed."))
		(sort_max.unification.tree.depth
		  100 (range (0 10000000))
		  #+:mkrp.deutsch(" Natuerliche Zahl >= 0    Maximale Tiefe des Unifikationsbaumes.")
		  #-:mkrp.deutsch(" Integer >= 0   Upper bound for the depth of the search tree"
				  "                in the unification algorithm."))
		(sort_max.unification.tree.open.nodes
		  20 (range (0 10000000))
		  #+:mkrp.deutsch(" Natuerliche Zahl >= 0    Maximale Anzahl der Blaetter des Unifikationsbaumes.")
		  #-:mkrp.deutsch(" Integer >= 0   Upper bound for the leaf number of the search tree"
				  "                in the unification algorithm."))
		(sort_unifier.stop.number
		  100 (range (0 10000000))
		  #+:mkrp.deutsch(" Natuerliche Zahl >= 0    Maximale Anzahl von allgemeinsten Unifikatoren, die"
				  "                          berechnet werden sollen.")
		  #-:mkrp.deutsch(" Integer >= 0   Upper bound for the number of most genera unifiers to"
				  "                be searched."))
		(sort_show.variable.sorts
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("Ausdrucken von Variablen bei eingeschaltetem SORT_LITERALS"
				  " t/j/ja      Falls SORT_LITERALS angeschaltet ist, werden"
				  "             alle Variablen mit Sorten ausgedruckt."
				  " n/nil/nein  Normale Ausgabe.")
		  #-:mkrp.deutsch(" t/y/yes   When SORT_LITERALS is switched on all Variables are printed"
				  "           together with their sorts."
				  " n/nil/no  Normal output."))
		(ER_PARAMODULATION
		  heuristic-completion (SET (heuristic-completion h-c h-c) (clause-graph c-g c-g) (zhang-kapur z-k z-k)
					    (bachmair-ganzinger b-g b-g) (snyder-lynch s-l s-l) (dershowitz d d))
		  #+:mkrp.deutsch("  Einstellung der Paramodulations-Strategie:"
				  "  heuristic-completion Paramodulation zur Behebung kritischer Paare und"
				  "                       zur Demodulation. Andere Schritte sind nur bei"
				  "                       Zutreffen von Heuristiken einer hoeheren Abstraktionsebene"
				  "                       erlaubt."
				  "  Zhang-Kapur          Strategie entsprechend Cade 88")
		  #-:mkrp.deutsch("  Choosing the paramodulation-strategy:"
				  "  heuristic-completion Paramodulation is only done to generate rewrite rules from"
				  "                       critical pairs. Resolution strategy STR_RESOLUTION is followed for"
				  "                       resolutions."
				  "  clause-graph         Ordered resolution and paramodulation with inheritance of R-links."
				  "  Zhang-Kapur          Strategy corresponding to CADE 88, Ordering is the same as"
				  "                       for HC strategy."
				  "  Bachmair-Ganzinger   Strategy corresponding to CADE 90."
				  "  Snyder-Lynch         Basic paramodulation strategy, like basic narrowing."
				  "  Dershowitz           Unit-strategy (IJCAI 91) if Horn clauses, B-G if not Horn or"
				  "                       only unit equations."))
		(er_weight.polynomials
		  ()
		  (list ())
		  #+:mkrp.deutsch("  Specification of the polynomials for the function symbols and"
				  "  the values for the constants in form of an associationlist.")
		  #-:mkrp.deutsch("  Specification of the polynomials for the weighting of function symbols and"
				  "  the values for the constants in form of an associationlist."
				  "  This weighting is used in all strategies and for all links but"
				  "  not for HEURISTIC-COMPLETION. The most similar value for ER-PARAMODULATION"
				  "  would be CLAUSE-GRAPH."
				  "  Syntax of polynomials:"
				  "  pol ::= (+ mon ... mon) | mon"
				  "  mon ::= (* ato ... ato) | ato"
				  "  ato ::= <number> | <variable>"
				  "  <number> is positive integer or zero."
				  "  <variable> is one of the argument variables."
				  "  Example: ((f (x y) (+ x y))"
				  "            (g (x y) (+ (* x y) x))"
				  "            (inv (x) (* x x))"
				  "            (one () 3)"
				  "            (zero () 3))"))
		(er_p.selection
		  (* weight (if support 1 1.5) (if EQUATIONAL 1 2))
		  (list (* weight (if support 1 1.5) (if EQUATIONAL 1 2)))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("  The value of this option determines the body of the selection function for R-links"
				  "  in the K-Z strategy of ER_PARAMODULATION and the selection function for P-links in"
				  "  all strategies. The smallest link relative to this function"
				  "  is selected. The value must be positive."
				  "  The argumentlist of this function is"
				  "  (WEIGHT NOLIT VARIABLES DEPTH SUPPORT EQUATIONAL)."
				  "  WEIGHT is the weight of the link, i.e. a value computed from the"
				  "  potential result. It is determined by the heuristic value function."
				  "  NOLIT is the number of literals of the link."
				  "  VARIABLES is the potential number of variables in the result clause."
				  "  For this option VARIABLES is always 1."
				  "  DEPTH is the depth of the link in the search space."
				  "  SUPPORT is true iff one of the parents is in the set of support."
				  "  EQUATIONAL is true iff all literals in the clause are positive"
				  "  equations.")
		  (let (#+symbolics(si:inhibit-fdefine-warnings t))
		    (compile 'sel=strat_p.selection `(lambda (weight nolit variables depth support equational)
						       ,(opt=ignore '(weight nolit variables depth support equational)
								    (opt-get.option er_p.selection))
						       ,(opt-get.option er_p.selection)))))
		(er_completion
		  unfailing (set (ignoring i i) (failing f f) (unfailing u u) (constant-congruence cc cc))
		  #+:mkrp.deutsch("   Einstellung der Vervollstaendigungsstrategie")
		  #-:mkrp.deutsch("   Selection of completion strategy:"
				  "  ignoring     If undirectable equations occur they are ignored"
				  "  failing      If undirectable equations occur the proof is aborted"
				  "  unfailing    If undirectable equations occur they are used in both"
				  "               directions and demodulation can be done with instances of"
				  "               them."
				  "  constant-congruence If undirectable equations occur they are reduced"
				  "               considering variables as constants."))
		(er_cp.reduction
		  T (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("nil/n/nein         Critical pairs are not reduced, i.e. no preview on the rewriting"
				  "                   of links."
				  "partial/p          Critical pairs are reduced just if they are constructed."
				  "t/j/ja             Critical pairs i.e. links are kept interreduced."
				  "In HEURISTIC-COMPLETION case only P- and PIW-links are considered, in the others"
				  "R-links are considered too.")
		  #-:mkrp.deutsch("nil/n/nein         Critical pairs are not reduced, i.e. no preview on the rewriting"
				  "                   of links."
				  "partial/p          Critical pairs are reduced just if they are constructed."
				  "t/j/ja             Critical pairs i.e. links are kept interreduced."
				  "In HEURISTIC-COMPLETION case only P- and PIW-links are considered, in the others"
				  "R-links are considered too."))
		(er_ordering
		  lexicographic-recursive-path (set (knuth-bendix kb kb)
						    (knuth-bendix-reverse kbr kbr)
						    (polynomial pol pol)
						    (recursive-path rp rp)
						    (lexicographic-recursive-path lp lp))
		  #+:mkrp.deutsch("   Reduktionsordnung")
		  #-:mkrp.deutsch("   Selection of the reduction ordering"
				  "  For all orderings 0, 1, minus, plus, mult are hardwired in the"
				  "  corresponding option ordering option."
				  "  Knuth-Bendix          Usual Knuth Bendix ordering."
				  "  Knuth-Bendix-Reverse  Counting subterms in reverse order, i.e. left associative."
				  "  Polynomial            Polynomial ordering."
				  "  Recursive-Path        Recursive path ordering, the ordering of the operator is"
				  "                        determined according to the knuth bendix weights in"
				  "                        ER_KNUTH.BENDIX.WEIGHT."
				  "  Lexicographic-Recursive-Path Usual extension of Recursive Path Ordering."))
		(er_operator.ordering
		  (* - + 0 1)
		  (list (- * + 0 1))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("  The operator ordering for all orderings."))
		(er_knuth.bendix.weight
		  ((+ 1) (* 1) (- 0) (0 1) (1 1))
		  (list ((+ 1) (* 1) (- 0) (0 1) (1 1)))
		  #+:mkrp.deutsch("  An associationlist associating function and constant names to"
				  "  values."
				  "  Default: ((+ 1) (* 1) (- 0) (0 1) (1 1))")
		  #-:mkrp.deutsch("  An associationlist associating function and constant names to"
				  "  values."
				  "  Default: ((+ 1) (* 1) (- 0) (0 1) (1 1))"))
		(er_polynomial.weight
		  ((+ (x y) (+ (* 2 y) x))
		   (* (x y) (+ (* x y) x))
		   (- (x) (* x x))
		   (0 () 2) (1 () 2))
		  (list ((+ (x y) (+ (* 2 y) x))
			 (* (x y) (+ (* x y) x))
			 (- (x) (* x x))
			 (0 () 2) (1 () 2)))
		  #+:mkrp.deutsch("  Specification of the polynomials for the function symbols and"
				  "  the values for the constants in form of an associationlist.")
		  #-:mkrp.deutsch("  Specification of the polynomials for the function symbols and"
				  "  the values for the constants in form of an associationlist."
				  "  Syntax of polynomials:"
				  "  pol ::= (+ mon ... mon) | mon"
				  "  mon ::= (* exp ... exp) | exp"
				  "  exp ::= (^ var <number>) | <number> | <variable>"
				  "  <number> is a positive integer."
				  "  <variable> is one of the polynomial variables."))
		(er_narrow.depth
		  0 (range (0 10))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("  Sometimes two literals with opposite sign can be made"
				  "  complementary using narrowing, that is paramodulating with"
				  "  the actually existing rewrite rules."
				  "  This parameter determines the look ahead for such"
				  "  paramodulations on non equality literals."
				  "  This option can only be switched on if the narrow subsystem"
				  "  is loaded (see manual)."))
		(er_narrow.next
		  :depth (set (:ALL :ALL :ALL) (:EQ :EQ :EQ)
			      (:CLAUSE :CLAUSE :CLAUSE) (:DEPTH :DEPTH :DEPTH)
			      (:TAU :TAU :TAU) (:TRICK :TRICK :TRICK))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("    Determines how the heuristic is computed that decides which equation in the"
				  "    narrow-tree is inspected next:"
				  "  :DEPTH  the depth of the narrow-step."
				  "  :EQ     the number of symbols in the equation."
				  "  :CLAUSE the number of symbols in the equation and the additional literals."
				  "  :TAU    the number of symbols in the substitution that instantiates the"
				  "          original equation to the actual one."
				  "  :ALL    the sum of :DEPTH and :EQ."
				  "  :TRICK  allows to use an own function (USER:NAR=TRICK)."))
		(er_narrow.test
		  (:NORM :C :DELTA :SL :N)
		  (list (:NORM :C :DELTA :SL :N))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("    choosing the narrowing-strategy and -tests :"
				  "  :NORM   normal-narrowing, normalisation after each narrow-step."
				  "  :C      commutative-narrowing."
				  "  :N      normalisation-test, successful if the substitution that instantiates"
				  "          the original equation to the actual one is reducible."
				  "  :DELTA  delta-test, successful if the substitution of a narrow-step from one"
				  "          equation has been computed for another narrow-step before."
				  "  :SL     sufficient-large-test, successful if a non-basic occurrence is reducible."))
		(GEN_SPLITTING
		  0 (EITHER (RANGE (0 10000000)) (SET (T yes JA Y J) (nil no NEIN n n)))
		  #+:mkrp.deutsch("    and-split des theorems :"
				  "  nil/n/nein   abgeschaltet."
				  "  nat. zahl    eingeschaltet.  maximale schachtelungstiefe"
				  "               bis zu der in dnf ausmultipliziert wird um"
				  "               splitting zu ermoeglichen."
				  "  t/j/ja       eingeschaltet.  multiplikation in allen"
				  "               schachtelungstiefen.")
		  #-:mkrp.deutsch("    And-split of theorem :"
				  "  nil/n/no     Switched off."
				  "  nat number   Switched on.  Maximal nesting depth up to"
				  "               which multiplication into dnf takes place"
				  "               in order to enable splitting."
				  "  t/y/yes      Switched on. Multiplication in all nesting"
				  "               depths."))
		(gen_presimplification
		  t (SET (T yes JA Y J) (partial p p) (nil no NEIN n n))
		  #+:mkrp.deutsch("  nil/n/nein   abgeschaltet."
				  "  partial/p    Switched on only the removal of obviously true and false components."
				  "  t/j/ja       eingeschaltet.")
		  #-:mkrp.deutsch("   Removing obviously true and false components and"
				  "   expanding definitions."
				  "  nil/n/no     Switched off."
				  "  partial/p    Switched on only the removal of obviously true and false components."
				  "  t/y/yes      Switched on."))
		(gen_MIN.EXPRESSION.LENGTH.FOR.FILE
		  nil (EITHER (RANGE (0 10000000)) (SET (T yes JA Y J) (nil no NEIN n n)))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("    Usage of a file to store the splitparts:"
				  "nil/n/no   Store in main memory."
				  "t/y/yes    Store in file."
				  "nat number If expression resulting from normalization is longer"
				  "           than this number, store in file, els in main memory."))
		(gen_MIN.EXPRESSION.SIZE.FOR.FILE
		  nil (EITHER (RANGE (0 10000000)) (SET (T yes JA Y J) (nil no NEIN n n)))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("    Usage of a file to store the splitparts:"
				  "nil/n/no   Store in main memory."
				  "t/y/yes    Store in file."
				  "nat number If expression resulting from normalization is larger"
				  "           than this number, store in file, els in main memory."))
		(GEN_MANUAL.CONTROL
		  NIL (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("    moeglichkeit den beweisablauf mit der hand zu beeinflussen :"
				  "  t/j/ja       eingeschaltet,"
				  "  nil/n/nein   abgeschaltet.")
		  #-:mkrp.deutsch("    influence of user on proof :"
				  "  t/y/yes      switched on,"
				  "  nil/n/no     switched off."))
		(GEN_MAXIMUM.STEPS
		  NIL (EITHER (RANGE (1 10000000)) (SET (NIL NO NEIN n)))
		  #+:mkrp.deutsch("  positive zahl  (nil  bedeutet 'unendlich')"
				  "    dies ist die maximale anzahl von deduktionsschritten eines beweises.")
		  #-:mkrp.deutsch("  positive integer   (nil means 'infinite')"
				  "    this is the maximum number of deduction-steps of a proof."))
		(GEN_MAXIMUM.time
		  NIL (EITHER (RANGE (1 10000000)) (SET (NIL NO NEIN n)))
		  #+:mkrp.deutsch("  Positive Zahl  (nil  bedeutet 'unendlich')"
				  "    Dies ist die maximale Zeit fuer die Durchfuehrung eines Beweises."
				  "    Nach jedem Beweisschritt wird ueberprueft, ob diese Zeit ueberschritten"
				  "    wurde. Falls ja wird abgebrochen.")
		  #-:mkrp.deutsch("  Positive integer   (nil means 'infinite')"
				  "    This is the maximum time for the execution of a proof."
				  "    After each step it is checked whether this time is exceeded."
				  "    If so the proof is aborted."))
		(GEN_GRAPH.SAVING
		  NIL (EITHER (RANGE (1 10000000)) (SET (NIL NO NEIN n)))
		  #+:mkrp.deutsch("  nil   _  kein effekt."
				  "  positive zahl _  zahl der deduktionsschritte, nach denen der graph"
				  "                   jeweils gesichert wird. (vgl. gen_save.file).")
		  #-:mkrp.deutsch("  nil   _  no effect."
				  "  positive interger _   number of deduction-steps between two"
				  "                        savings of the graph (see gen_save.file)."))
		(GEN_SAVE.FILE
		  SAVEDEFAULT (FILE SEE--OPT=CHECK.IS.LEGAL.FILENAME)
		  #+:mkrp.deutsch("  <dateiname>          auf diese datei wird der graph gesichert (falls"
				  "                       gen_graph.saving nicht nil ])")
		  #-:mkrp.deutsch("  <filename>           system saves graph on this file (if"
				  "                       gen_graph.saving not nil ])"))
		(GEN_LISP.GARBAGE.COLLECTION
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("    anschalten des lisp garbagecollectors, sobald gerade noch genug"
				  "    platz vorhanden ist."
				  "    t/j/ja         eingeschaltet."
				  "    nil/n/nein     ausgeschaltet.")
		  #-:mkrp.deutsch("    switch on the lisp garbage collector before address space becomes low."
				  "    t/y/yse        eingeschaltet."
				  "    nil/n/no       ausgeschaltet."))
		(GEN_common.LISP
		  #+(or symbolics explorer) nil
		  #-(or symbolics explorer) t
		  (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("    Says whether to use pure Common Lisp."
				  "    t/y/yes        Use pure Common Lisp."
				  "    nil/n/no       Use SYMBOLICS features (editor options).")
		  #-:mkrp.deutsch("    Says whether to use pure Common Lisp."
				  "    t/y/yes        Use pure Common Lisp."
				  "    nil/n/no       Use SYMBOLICS features (editor options)."))
		(GEN_other.prover
		  mkrp
		  (SET (mkrp mkrp mkrp) (C C C) (ER ER ER))
		  #+:mkrp.deutsch("    Determines the output of the CONSTRUCT subsystem."
				  "    mkrp      A clause graph is printed on the graph file in a format"
				  "              readable for the REFUTE subsystem."
				  "    C         The graph file is a compilable MAIN program for C."
				  "    ER        The graph file is a compilable LISP program."
				  "    C and ER use constructor functions which maust be defined in C or LISP, respectively.")
		  #-:mkrp.deutsch("    Determines the output of the CONSTRUCT subsystem."
				  "    mkrp      A clause graph is printed on the graph file in a format"
				  "              readable for the REFUTE subsystem."
				  "    C         The graph file is a compilable MAIN program for C."
				  "    ER        The graph file is a compilable LISP program."
				  "    C and ER use constructor functions which maust be defined in C or LISP, respectively."))
		(TR_PREPROCESSING
		  NIL (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("    trace der zwischenergebnisse der praeprozessoren"
				  "  t/j/ja         eingeschalted"
				  "  nil/n/nein     abgeschalted")
		  #-:mkrp.deutsch("    trace of the intermediate results of the preprocessors"
				  "  t/y/yes        switched on"
				  "  nil/n/no       switched off"))
		(TR_STEP.MODE
		  LR (SET  (nil no NEIN n n) (I IMPL impl) (L LOG log LOGICAL) (LR lr lr))
		  #+:mkrp.deutsch("    trace der einzelnen deduktionnsschritte"
				  "  nil/n/nein     kein trace"
				  "  i/impl         ausfuehrliche, implementierungsbezogene ausgabe der"
				  "                 aller aenderungen (klauseln und links)"
				  "  l/log          ausgabe der aenderungen in logischer form (klauseln)"
				  "  lr             wie l, zusaetzlich werden variablen umbenannt")
		  #-:mkrp.deutsch("    trace of each deductionstep"
				  "  nil/n/no       no trace is done"
				  "  i/impl         detailed implementational protocol of all changes"
				  "                 (clauses and links)"
				  "  l/log          protocol of all changes in a more logical form"
				  "                 (clauses)"
				  "  lr             same as under l, additionally variables are renamed"))
		(TR_DUMP
		  NIL (EITHER (SET (nil no NEIN n n)) (RANGE (1 1000000)))
		  #+:mkrp.deutsch("    dump des aktuellen graphen in bestimmten intervallen"
				  "  nil/n/nein         kein dump"
				  "  pos. ganze zahl    zahl der deduktionsschritte nach denen jeweils ein"
				  "                     dump erfolgt")
		  #-:mkrp.deutsch("    dump of the current graph after certain intervals"
				  "  nil/n/no        no dump is done"
				  "  pos. integer    number of deduction-steps between two subsequent dumps"))
		(TR_CLAUSE.MODE
		  I (SET (nil no NEIN n n) (I IMPL impl IMPLEMENTATION) (L LOG log LOGICAL) (LR l l lr))
		  #+:mkrp.deutsch("    format fuer die ausgabe der klauseln bei tr_dump"
				  "  nil/n/nein    keine ausgabe"
				  "  i/impl        implementierungsbezogene ausgabe"
				  "  l/log         logische ausgabe"
				  "  lr            wie l, zusaetzlich werden variablen umbenannt")
		  #-:mkrp.deutsch("    format of the output of clauses if tr_dump is set"
				  "  nil/n/no      no output at all"
				  "  i/impl        output is i]plementational"
				  "  l/log         logical output"
				  "  lr            same as under l, additionally variables are renamed"))
		(TR_LINK.MODE
		  I (SET (nil no NEIN n n) (I IMPL impl IMPLEMENTATION))
		  #+:mkrp.deutsch("    format fuer die ausgabe der links bei tr_dump"
				  "  nil/n/nein    keine ausgabe"
				  "  i/impl        implementierungsbezogene ausgabe")
		  #-:mkrp.deutsch("    format of the output of links if tr_dump is set"
				  "  nil/n/no      no output at all"
				  "  i/impl        output is implementational"))
		(TR_TRACE.FILE
		  NIL (EITHER (SET (nil no NEIN n n) (T TERMINAL terminal)) (FILE SEE_--FILENAME.CHECK--))
		  #+:mkrp.deutsch("    ausgabedatei fuer traces und dumps"
				  "  nil/n/nein    keine ausgabe"
				  "  t/terminal    ausgabe auf terminal"
				  "  <datei>       ausgabe auf <datei>")
		  #-:mkrp.deutsch("    outputfile for traces and dumps"
				  "  nil/n/no      no output at all"
				  "  t/terminal    output on terminal"
				  "  <file>        output on <file>"))
		(TR_TERMINAL
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("    kurzinformation ueber den ablauf am terminal"
				  "    (zusaetzlich zu den statistiken)"
				  "  t/j/ja        eingeschalted"
				  "  nil/n/nein    ausgeschalted")
		  #-:mkrp.deutsch("    brief information about the proof displayed on terminnal"
				  "    (in addition to the displyed statistics)"
				  "  nil/n/no      switched off"
				  "  t/y/yes       switched on"))
		(PR_INFIX.FORM
		  T (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("  infixform der eingegebenen formeln im protokoll :"
				  "    t/j/ja         eingeschaltet"
				  "    nil/n/nein     abgeschaltet")
		  #-:mkrp.deutsch("  infix form of input formulae in protocol :"
				  "    t/y/yes        switched on"
				  "    nil/n/no       switched off"))
		(PR_PREFIX.FORM
		  NIL (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch(" praefixform der eingegebenen formeln im protokoll :"
				  "    t/j/ja         eingeschaltet"
				  "    nil/n/nein     abgeschaltet")
		  #-:mkrp.deutsch(" prefix form of input formulae in protocol :"
				  "    t/y/yes        switched on"
				  "    nil/n/no       switched off"))
		(PR_OPTIONS
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("  werte der beweisoptionen im protokoll :"
				  "    t/j/ja         eingeschaltet"
				  "    nil/n/nein     abgeschaltet")
		  #-:mkrp.deutsch("  values of proof options in protocol :"
				  "    t/y/yes        switched on"
				  "    nil/n/no       switched off"))
		(PR_AXIOM.CLAUSES
		  t (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("  axiomenklauseln im protokoll :"
				  "    t/j/ja         eingeschaltet"
				  "    nil/n/nein     abgeschaltet")
		  #-:mkrp.deutsch("   axiom clauses in protocol :"
				  "    t/y/yes        switched on"
				  "    nil/n/no       switched off"))
		(PR_SYMBOLS
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("  symboltabelle im protokoll :"
				  "    t/j/ja         eingeschaltet"
				  "    nil/n/nein     abgeschaltet")
		  #-:mkrp.deutsch("   symbol table in protocol :"
				  "    t/y/yes        switched on"
				  "    nil/n/no       switched off"))
		(pr_direct.proof
		  T (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("    t/j/ja         Nur die zum Beweis fuehrenden"
				  "    nil/n/nein               Alle"
				  "  Beweisschritte werden ausgegeben.")
		  #-:mkrp.deutsch("    t/j/ja         Only steps leading to proof"
				  "    nil/n/nein          All proof steps"
				  "  are written onto the list file."))
		(pr_variable.print.names
		  (x y z u v w xx yy zz uu vv ww xxx yyy zzz uuu vvv www)
		  (EITHER (SET (Old.Variable.Names Old.names Alte.Namen)
			       (Different.Names    New.names Neue.Namen))
			  (list (x y z u v w xx yy zz uu vv ww xxx yyy zzz uuu vvv www)))
		  #+:mkrp.deutsch("  Old names: Use the same names as during the proof search"
				  "  New names: Use new names, numbered in every clause"
				  "  Other:     Must be a list of symbols or strings used as variable names"
				  "    default: (x y z u v w xx yy zz uu vv ww xxx yyy zzz uuu vvv www)")
		  #-:mkrp.deutsch("  Alte Namen: Dieselben Namen werden verwendet"
				  "              wie waehrend der Beweissuche"
				  "  Neue Namen: Neue Namen werden verwendet,"
				  "              die in jeder Klausel durchnumeriert sind"
				  "  Other:      Eine Liste von Symbolen oder Strings, die als"
				  "              Variablennamen verwendet werden sollen"
				  "     default: (x y z u v w xx yy zz uu vv ww xxx yyy zzz uuu vvv www)"))
		(PR_PROTOCOL
		  STANDARD (SET (Standard Standard Standard T yes JA Y J) (POst Post Post) (nil no NEIN n n))
		  #+:mkrp.deutsch("    Rohdaten fuer das Protokoll werden auf Datei geschrieben."
				  "  Standard/t/j/ja    eingeschaltet fuer MKRP-Betrieb"
				  "  Post               das Code-File wird in der Sprache Post beschrieben."
				  "  nil/n/nein         ausgeschaltet")
		  #-:mkrp.deutsch("    Raw data for the protocol is written on file."
				  "    Standard/t/y/yes     switched on for MKRP mode"
				  "    Post                 the code file is written in the POST-language."
				  "    nil/n/no             switched off"))
		(PR_LEFT.MARGIN
		  0 (RANGE (0 50))
		  #+:mkrp.deutsch("  Erste beschriebene Position einer Zeile :")
		  #-:mkrp.deutsch("   First position to be printed in each line :"))
		(PR_RIGHT.MARGIN
		  117 (RANGE (50 255))
		  #+:mkrp.deutsch("  Letzte beschriebene Position einer Zeile")
		  #-:mkrp.deutsch("   First position to be printed in each line"))
		(PR_latex
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch("   The protocol is written in a latex readable way"
				  "    t/j/ja         eingeschaltet"
				  "    nil/n/nein     abgeschaltet")
		  #-:mkrp.deutsch("   The protocol is written in a latex readable way"
				  "   The program automatically selects widths and heights of characters"
				  "   and formulae parts."
				  "    t/y/yes        switched on"
				  "    nil/n/no       switched off"))
		(PR_LINELENGTH
		  117 (RANGE (32 256))
		  #+:mkrp.deutsch("  anzahl der zeichen pro zeile :")
		  #-:mkrp.deutsch("  number of characters per line :"))
		(pr_literals
		  nil (SET (T yes JA Y J) (nil no NEIN n n))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("    Protocol assignment of literals in clauses to atoms in original input formulae."
				  "    t/y/yes     switched on"
				  "    nil/n/no    switched off"))))



(DEFMACRO OPT-GET.OPTION (OPTID)
  ;; input: -an litatom (the name of an option without the opt*-prefix)
  ;; value: global-value of the internal representation opt*optid of the option optid (=val of opt)
  `(opt=get ,optid))

(DEFMACRO OPT=get (OPTID)
  ;; input:-an litatom the name of an option without the opt*-prefix.
  ;; value: global-value of the internal representation opt*optid of the option optid (=val of opt)
  (INTERN (format nil "OPT*~A" OPTID) (find-package "MKRP")))
)
(DEFUN OPT=PUT (OPTID VALUE)
						; Edited:  28-MAR-1992 02:28
						; Authors: PRCKLN
						; Input:   an option-identifier (litatom) without the 'opt* - prefix
						; Effect:  sets the global-var opt*optid to value
						; Value:   value (= input)
  (SETf (symbol-value (mkrp-INTERN (format nil "OPT*~A" OPTID))) value)
  (eval (opt=get.option.demon (ASSOC optid OPT*ALL.OPTIONS))))

(DEFMACRO OPT-PUT.OPTION (OPTID VALUE)
  ;; input:  -an litatom (the name of an option without the opt*-prefix)
  ;;         -the value of the option (any lisp-object)
  ;; value:  t -- if the option-value was within the li-
  ;;         mits of its arg-range nil -- else
  ;; effect: checks if value is legal (see argrange in opt*all.options) and sets it to the corres-
  ;;         ponding standardized value. if it is not legal nothing happens
  `(LET ((STVAL (OPT=CHECK ',OPTID ,VALUE)))
     (COND ((EQL 'ILLEGAL.VALUE STVAL) NIL)
	   (T (OPT=PUT ',OPTID STVAL) T))))

(DEFMACRO OPT-SET.STANDARD (&optional (defvarflag nil))
  ;; input: If true a list of defvars else a list of setqs
  ;; value: undefined
  ;; effect: defines or sets all options (in opt*all.options) to the corresponding default-value.
  `(PROGN ,@(MAPCAR #'(LAMBDA (OPTION.DESCRIPTOR)
			(if defvarflag
			    `(DEFvar ,(INTERN (format nil "OPT*~A" (OPT=GET.OPTION.NAME OPTION.DESCRIPTOR))
					      (find-package "MKRP")))
			    `(opt=put ',(OPT=GET.OPTION.NAME OPTION.DESCRIPTOR)
				      ',(OPT=GET.DEFAULT.VALUE OPTION.DESCRIPTOR))))
		    OPT*ALL.OPTIONS)))

(DEFUN OPT-GET.LIST.OPTIONS NIL
  ;; input: -none
  ;; value: a list of form (-- (option-indicator . value)
  ;;        --) of all options. value is the actual value  of the option with the 'opt* - prefix.
  (MAPCAR #'(LAMBDA (OPTION.DESCRIPTOR)
	      (CONS (OPT=GET.OPTION.NAME OPTION.DESCRIPTOR)
		    (SYMBOL-VALUE (INTERN (format nil "OPT*~A" (OPT=GET.OPTION.NAME OPTION.DESCRIPTOR))
					  (find-package "MKRP")))))
	  OPT*ALL.OPTIONS))

(DEFUN OPT-GET.LIST.AREA.OPTIONS (AREA)
  ;; edited: 15-dec-83.
  ;; input:  name of an area descriptor.
  ;; effect: -
  ;; value:  a list of dotted pairs (a . b) where a is
  ;;         the name of an option of area 'area' and b is its actual value.
  (let ((RESULT NIL))
    (MAPC #'(LAMBDA (DESCRIPTOR)
	      (when (STRING= (STRING AREA)
			     (SUBSEQ (STRING (OPT=GET.OPTION.NAME DESCRIPTOR))
				     0 (PRINT-LENGTH AREA)))
		(SETQ RESULT (CONS (CONS (OPT=GET.OPTION.NAME DESCRIPTOR)
					 (SYMBOL-VALUE (INTERN (format nil "OPT*~A" (OPT=GET.OPTION.NAME DESCRIPTOR))
							       (find-package "MKRP"))))
				   RESULT))))
	  OPT*ALL.OPTIONS)
    RESULT))

(DEFUN OPT-PUT.LIST.OPTIONS (NAME.VALUE.LIST &optional err)
					; input:  NAME.VALUE.LIST is a list of dotted pairs
					;         (....(optid . value)....) with optid = name
					;         of an option (without opt*-prefix) and value =
					;         the value it is to be set.
					; value:  t   -- if all values are legal (see argranges
					;                of the optids in opt*all.options).
					;         nil -- else.
					; effect: Checks all values of the optids and sets 
					;         the vars opt*optid to the standardised values
					;         for the legal ones.
  (let (NORMALIZED.VALUE LIST.OF.NORMALIZED.VALUES illegal)
    (mapc #'(lambda (NAME.VALUE)
	      (SETQ NORMALIZED.VALUE (OPT=CHECK (CAR NAME.VALUE) (CDR NAME.VALUE)))
	      (if (EQL NORMALIZED.VALUE 'ILLEGAL.VALUE)
		  (progn (setq illegal t)
			 (when err (warn "MKRP option ~A illegal" (CAR NAME.VALUE))))
		(SETQ LIST.OF.NORMALIZED.VALUES (NCONC1 LIST.OF.NORMALIZED.VALUES
							(cons (car name.value)
							      NORMALIZED.VALUE)))))
	  NAME.VALUE.LIST)
    (MAPC #'(LAMBDA (NAME.VALUE) (OPT=PUT (CAR NAME.VALUE) (cdr name.value)))
	  LIST.OF.NORMALIZED.VALUES)
    (not illegal)))

(DEFUN OPT-ALL.OPTIONS NIL
  ;; input: -none               
  ;; value: list of all option-identifiers in the system
  (MAPCAR (FUNCTION (LAMBDA (OPTION.DESCRIPTOR) (OPT=GET.OPTION.NAME OPTION.DESCRIPTOR)))
	  OPT*ALL.OPTIONS))

(DEFUN OPT-SAVE (FILE)
  ;; input:  nil or name of an open file.
  ;; effect: computes an sexpression which when evaluated
  ;;         restores the actual values of all options.    if file <> nil, this expression is written
  ;;         on file, otherwise it is returned as value. value:  if file = nil then the computed expression,
  ;;         else nil.
  (PROG ((EXPRESSION (LIST 'OPT-PUT.LIST.OPTIONS (KWOTE (OPT-GET.LIST.OPTIONS)))))
	(COND (FILE (PROGN (PRINC EXPRESSION FILE) (TERPRI FILE))) (T (RETURN EXPRESSION)))))

(DEFUN OPT-GET.DEFAULT.VALUE (OPTION.NAME)
  ;; input: -atom (=name of an option without prefix)  value:  the default-value of the option (=cadr of
  ;;     the opt-descriptor (name def legal.args exp1    ...) ).
  (OPT=GET.DEFAULT.VALUE (ASSOC OPTION.NAME OPT*ALL.OPTIONS)))

(DEFUN OPT-GET.OPTION.TEXT (OPTION.NAME)
  ;; input:-the name of an option (without opt*-prefix]])value: the explanation of the option, its effect and
  ;;    values in the language specified during the compilation of the system
  ;; remarks: the explanations are part of the optiondes-   cription of opname in opt*all.options
  (let (EXPLANATIONS
	(OPTION.DESCRIPTOR (ASSOC OPTION.NAME OPT*ALL.OPTIONS)))
    (SETQ EXPLANATIONS (OPT=GET.OPTION.EXPLANATIONS OPTION.DESCRIPTOR))
    (first EXPLANATIONS)))

(DEFUN OPT-ALL.AREAS NIL
  ;; input:-none               value: list of all area-names in the system."
  ;; remarks: uses the common-var opt*all.areas."))
  (MAPCAR (FUNCTION (LAMBDA (AREA.DESCRIPTOR) (OPT=GET.AREA.NAME AREA.DESCRIPTOR))) OPT*ALL.AREAS))

(DEFUN OPT-GET.AREA.OPTIONS (AREA)
  ;; input:-atom (=name of an area-descriptor)     value: list of all option-names (op1....) of this"
  ;;    area."))
  (PROG ((AREA.DESCRIPTOR (ASSOC AREA OPT*ALL.AREAS)))
	(RETURN (COND (AREA.DESCRIPTOR (OPT=GET.AREA.OPTIONS AREA.DESCRIPTOR)) (T NIL)))))

(DEFUN OPT-GET.AREA.HEADLINE (AREA.NAME)
  ;; input:  atom (=name of an area-descriptor)
  ;; value:  list of string (str..) from headlines of the
  ;;         area with name=input.
  ;; effect: the strings are printed as a headline of the
  ;;         area. it is assumed that the headline is printed
  ;;         in the language specified when compiling the module
  (let ((AREA.DESCRIPTOR (ASSOC AREA.NAME (SYMBOL-VALUE 'OPT*ALL.AREAS))))
    (OPT=GET.AREA.HEADER AREA.DESCRIPTOR)))

(DEFUN OPT-GET.AREA.EXPLANATION (AREA.NAME)
  ;; input:-atom (=name of an option-descriptor)   value: list of string (str..) from the cdddr of the"
  ;;    area with name=input.        effect: the strings are printed as an explan. of the"
  ;;    area. it is assumed that the str-lists exp1..   are ordered in the same way as opt*all.lan-"
  ;;    guages, so that the explanation is printed in   the language specified when compiling the module
  (let (AREA.EXPLANATIONS
	(AREA.DESCRIPTOR (ASSOC AREA.NAME OPT*ALL.AREAS)))
    (SETQ AREA.EXPLANATIONS (OPT=GET.AREA.EXPLANATIONS AREA.DESCRIPTOR))
    (first area.explanations)))

(DEFUN OPT=CHECK (OPTION.NAME VALUE)
  ;; input:-an option-identifier (a lit-atom]])      -the value this option is to be set to]]"
  ;; value: standardized value if value was within the    limits of the arg-range of the option]]"
  ;;    the atom illegal.value else]]      effect: checks if value is legal (see argrange in"
  ;;    opt*all.options]]) for this option and re-    turns the standardized value."))
  (let (NORMALIZED.VALUE ARGUMENT.RANGE)
    (SMAPC #'IGNORE
	   #'(LAMBDA (OPL)
	       (COND ((EQL (OPT=GET.OPTION.NAME (CAR OPL)) OPTION.NAME)
		      (SETQ ARGUMENT.RANGE (OPT=GET.ARGUMENT.RANGE (CAR OPL)))
		      NIL)
		     (T (CDR OPL))))
	   (SYMBOL-VALUE 'OPT*ALL.OPTIONS))
    (SETQ NORMALIZED.VALUE
	  (COND ((OR (NULL ARGUMENT.RANGE) (EQL 'ILLEGAL.VALUE ARGUMENT.RANGE))
		 'ILLEGAL.VALUE)
		(T (OPT=CHECK.AND.STANDARDIZE VALUE ARGUMENT.RANGE NIL))))
    NORMALIZED.VALUE))

(defun opt=check.is.list (option.value)
  (if (and (listp OPTION.VALUE)
	   (every #'(lambda (var)
		      (and (not (eq 'quote var))
			   (or (symbolp var)
			       (numberp var)
			       (stringp var)
			       (and (consp var)
				    (or (symbolp (first var)) (numberp (first var)))))))
		  option.value))
      option.value
      'ILLEGAL.VALUE))

(DEFUN OPT=CHECK.AND.STANDARDIZE (OPTION.VALUE ARGUMENT.RANGE CHECK?)
  ;; input:-value of an option (an atom or a number]]])    -a list = allowed values for this option"
  ;;    (=argument-range of the option). see remarks]  -check-flag (if =t then the syntax of argval"
  ;;    and arg.range is checked too]]])     value: the standardized value of argval if it is"
  ;;    within arg.range, 'illegal.value else]]]]  remarks: there are 5 different arg.range-types:"
  ;;    1) set: (.list of legal vals.) (arg=litatom])   2) range: (..(lower upper)..)-bound] (=nums])"
  ;;    3) file: a legal bs200-filename (=litatom]])    4) structure: (..typ1-5 sep typ1-5..)"
  ;;      typ1-5: 1 out of range 1)-5)        sep: atom or list (used as sep-chars)"
  ;;      example: l.ax:1=<(set l)(.,-)(set ax)(.)>   5) either: disjunction of <a list of> 1)-5)"))
  (PROG (NORMALIZED.VALUE)
	(CASE (CAR ARGUMENT.RANGE)
	  (SET (SETQ NORMALIZED.VALUE (OPT=CHECK.IS.ELEMENT.OF.SET OPTION.VALUE (CDR ARGUMENT.RANGE) CHECK?)))
	  (RANGE (SETQ NORMALIZED.VALUE (OPT=CHECK.IS.IN.RANGE OPTION.VALUE (CDR ARGUMENT.RANGE) CHECK?)))
	  (FILE (SETQ NORMALIZED.VALUE (COND ((FILENAME.CHECK OPTION.VALUE) OPTION.VALUE) (T 'ILLEGAL.VALUE))))
	  (STRUCTURE (SETQ NORMALIZED.VALUE (OPT=CHECK.IS.OF.STRUCTURE OPTION.VALUE (CDR ARGUMENT.RANGE) CHECK?)))
	  (EITHER (SETQ NORMALIZED.VALUE (OPT=CHECK.IS.EITHER.RANGE OPTION.VALUE (CDR ARGUMENT.RANGE) CHECK?)))
	  (list (SETQ NORMALIZED.VALUe (opt=check.is.list option.value)))
	  (OTHERWISE
	    (SETQ NORMALIZED.VALUE
		  (COND
		    (CHECK?
		     (PROGN (PRIN1 "---  error in opt=check.and.standardize  ---  illegal rangetype]  ---" T) (TERPRI T)
			    (PROGN (PRINC ARGUMENT.RANGE) (TERPRI)) 'ILLEGAL.VALUE))
		    (T 'ILLEGAL.VALUE)))))
	(COND
	  ((AND CHECK? (EQL 'ILLEGAL.VALUE NORMALIZED.VALUE)) (PRIN1 "argument is illegal, see range-description: " T)
	   (PROGN (PRINC OPTION.VALUE) (TERPRI)))
	  ((AND CHECK? (EQL 'SYNTAX.ERROR NORMALIZED.VALUE)) (SETQ NORMALIZED.VALUE 'ILLEGAL.VALUE)) (T NIL))
	(RETURN NORMALIZED.VALUE)))

(DEFUN OPT=CHECK.IS.ELEMENT.OF.SET (OPTION.VALUE LIST.OF.SETS CHECK?)
  ;; input:-value of an option of type set (=litatom]])    -list of sets (lists) of legal arguments, with"
  ;;    the standard-value as the first element]]]   -check-flag (if t the input-syntax is checked"
  ;;    too]  =nil --> nothing happens])     value: the standardized argument-value if the argu-"
  ;;    ment was legal, 'illegal.value' or      'syntax.error' else."))
  (PROG (NORMALIZED.VALUE)
	(COND
	  (CHECK?
	   (COND
	     ((CONSP LIST.OF.SETS)
	      (MAPC
		(FUNCTION
		  (LAMBDA (SET)
		    (COND ((AND (CONSP SET) (EVERY (FUNCTION ATOM) SET)) T)
			  (T (PRIN1 "this element of range=set is not a list of atoms: " T) (PROGN (PRINC SET) (TERPRI))
			     (SETQ NORMALIZED.VALUE 'SYNTAX.ERROR)))))
		LIST.OF.SETS))
	     (T (SETQ NORMALIZED.VALUE 'SYNTAX.ERROR) (PRIN1 "this is not a list of sets (set=list of atoms): " T)
		(PROGN (PRINC LIST.OF.SETS) (TERPRI))))
	   (COND ((EQL 'SYNTAX.ERROR NORMALIZED.VALUE) (RETURN 'SYNTAX.ERROR)) (T (C "proceed]] input-syntax is ok ]] " *))))
	  (T NIL))
	(COND ((NOT (ATOM OPTION.VALUE)) (SETQ NORMALIZED.VALUE 'ILLEGAL.VALUE))
	      (T
	       (SMAPC #'IGNORE
		      (FUNCTION
			(LAMBDA (SUBLIST.OF.SETS)
			  (COND ((MEMBER OPTION.VALUE (CAR SUBLIST.OF.SETS)) (SETQ NORMALIZED.VALUE (CAAR SUBLIST.OF.SETS)) NIL)
				(T (SETQ NORMALIZED.VALUE 'ILLEGAL.VALUE) (CDR SUBLIST.OF.SETS)))))
		      LIST.OF.SETS)))
	(RETURN NORMALIZED.VALUE)))

(DEFUN OPT=CHECK.IS.IN.RANGE (OPTION.VALUE VALUE.RANGES CHECK?)
  ;; input:   -value of an option of range-type=range (=integer)
  ;;          -list of allowed val-ranges of opval (..(lower bound upper-bound)...)
  ;;          -check-flag (if =t then the input-syntax is checked too)
  ;; value:   argval if within ranges, 'illegal.value' or 'syntax.error'  else.
  ;; remarks: only integers are accepted as argvals
  (PROG (NORMALIZED.VALUE)
	(COND
	  (CHECK?
	   (COND
	     ((AND (CONSP VALUE.RANGES) (EVERY (FUNCTION LISTP) VALUE.RANGES))
	      (MAPC
		(FUNCTION
		  (LAMBDA (RANGE)
		    (COND
		      ((OR (NOT (CONSP RANGE)) (NEQ (LIST-LENGTH RANGE) 2) (NOT (INTEGERP (CAR RANGE))) (NOT (INTEGERP (SECOND RANGE)))
			   (> (CAR RANGE) (SECOND RANGE)))
		       (PRIN1 "this is not a legal numeric argument-range: " T) (PROGN (PRINC RANGE) (TERPRI))
		       (SETQ NORMALIZED.VALUE 'SYNTAX.ERROR))
		      (T NIL))))
		VALUE.RANGES))
	     (T (SETQ NORMALIZED.VALUE 'SYNTAX.ERROR) (PRIN1 "this is not a numeric-range description: " T)
		(PROGN (PRINC VALUE.RANGES) (TERPRI))))
	   (COND ((EQL 'SYNTAX.ERROR NORMALIZED.VALUE) (RETURN NORMALIZED.VALUE)) (T NIL)))
	  (T NIL))
	(COND
	  ((INTEGERP OPTION.VALUE)
	   (SMAPC #'IGNORE
		  (FUNCTION
		    (LAMBDA (LIST.OF.RANGES)
		      (COND
			((AND (NOT (< OPTION.VALUE (CAAR LIST.OF.RANGES))) (NOT (> OPTION.VALUE (CADAR LIST.OF.RANGES))))
			 (SETQ NORMALIZED.VALUE OPTION.VALUE) NIL)
			(T (SETQ NORMALIZED.VALUE 'ILLEGAL.VALUE) (CDR LIST.OF.RANGES)))))
		  VALUE.RANGES))
	  (T (SETQ NORMALIZED.VALUE 'ILLEGAL.VALUE)))
	(RETURN NORMALIZED.VALUE)))

(DEFUN OPT=CHECK.IS.OF.STRUCTURE (OPTION.VALUE STRUCTURE.DESCRIPTION CHECK?)
  ;; input: -option-value of type structure (=atom]])
  ;;        -the structure-descriptionlist of form <(type k....) sep (typej...) .......>
  ;;         with type=set,range,file,structure,composite
  ;;         and sep=(atom or list of atoms) which are taken as char-strings who separate parts of
  ;;         argval
  ;;        -check-flag (if =t then the input-syntax is
  ;;         checked too)
  ;; value: the standardized argument argval if it is
  ;;        legal according to the structure-description
  ;;        -- illegal.value / syntax.error -- else.
  ;; remarks: calls opt=check.and.standardize, so parts of the structure-descr. may be structure-
  ;;          descriptions as well
  (PROG ((RESULT
	   (TESTEVAL
	     (PROG (SEPARATORS FOUND.SEPARATOR-CHARS LEGAL.PARTS FOUND.SUBSTRINGS (STRING.STARTPOS 1)
		    (STRING.ENDPOS (PRINT-LENGTH OPTION.VALUE NIL)) SUBSTRING.END NORMALIZED.VALUES NORMALIZED.PART)
		   (COND (CHECK? (COND ((CONSP STRUCTURE.DESCRIPTION)
					(SMAPL #'(LAMBDA (DESCRIPTION)
						   (COND
						     ((OR (ATOM (CAR DESCRIPTION))
							  (NOT (MEMBER (CAAR DESCRIPTION) (SYMBOL-VALUE 'OPT*ALL.RANGE.TYPES))))
						      (PRIN1 "illegal structure-definition. this is not a range: " T)
						      (PROGN (PRINC (CAR DESCRIPTION)) (TERPRI))
						      (SETQ NORMALIZED.VALUES 'SYNTAX.ERROR))
						     ((NULL (CDR DESCRIPTION)) T)
						     ((OR (ATOM (SECOND DESCRIPTION)) (EVERY (FUNCTION ATOM)
											     (SECOND DESCRIPTION))) T)
						     (T (PRIN1 "illegal structure-definition. allowed are catom or (a1,a2...)e" T)
							(TERPRI T)
							(PRIN1 "as separators, not: " T)
							(PROGN (PRINC (SECOND DESCRIPTION))
							       (TERPRI))
							(SETQ NORMALIZED.VALUES 'SYNTAX.ERROR))))
					       (FUNCTION CDDR) STRUCTURE.DESCRIPTION))
				       (T (SETQ NORMALIZED.VALUES 'SYNTAX.ERROR) (PRIN1 "this is not a structure-description: " T)
					  (PROGN (PRINC STRUCTURE.DESCRIPTION) (TERPRI)))))
			 (T NIL))
		   (COND ((EQL NORMALIZED.VALUES 'SYNTAX.ERROR) (RETURN NORMALIZED.VALUES))
			 (T (C "you may continue, the input-sytax was ok]] " *)))
		   (sMAPL (FUNCTION IGNORE)
			  (FUNCTION
			    (LAMBDA (DESCRIPTION)
			      (COND ((CONSP (CAR DESCRIPTION)) (SETQ LEGAL.PARTS (CONS (CAR DESCRIPTION) LEGAL.PARTS)))
				    (T (SETQ NORMALIZED.VALUES 'ILLEGAL.VALUE)))
			      (COND ((NULL (CDR DESCRIPTION)) NIL)
				    ((ATOM (SECOND DESCRIPTION)) (SETQ SEPARATORS (CONS (CONS (SECOND DESCRIPTION) NIL) SEPARATORS))
				     (CDDR DESCRIPTION))
				    ((CONSP (SECOND DESCRIPTION)) (SETQ SEPARATORS (CONS (SECOND DESCRIPTION) SEPARATORS))
				     (CDDR DESCRIPTION))
				    (T
				     (ERROR "---  error  ---  illegal structure-def.]]] check opt*all.options]]] ~a"
					    (LIST (CAR DESCRIPTION) STRUCTURE.DESCRIPTION))
				     NIL))))
			  STRUCTURE.DESCRIPTION)
		   (COND ((EQL NORMALIZED.VALUES 'ILLEGAL.VALUE) (RETURN NORMALIZED.VALUES))
			 (T (C "you may continue] allowed parts, seps are listed" *)))
		   (SETQ LEGAL.PARTS (NREVERSE LEGAL.PARTS)) (C "allowed parts of the arg. are collected]]     " *)
		   (SETQ SEPARATORS (NREVERSE SEPARATORS)) (C "list of sep-chars in between are collected now]]  " *)
		   (SETQ FOUND.SUBSTRINGS
			 (MAPCAR #'(LAMBDA (LIST.OF.SEPARRATOR-CHARS)
				     (MAPL #'IGNORE
					   #'(LAMBDA (SEPARRATOR-CHARS)
					       (COND
						 ((INTEGERP (SETQ SUBSTRING.END (POSITION (CAR SEPARRATOR-CHARS)
											  OPTION.VALUE STRING.STARTPOS)))
						  (SETQ SUBSTRING.END (1- SUBSTRING.END))
						  ;; memorize the end of first substring
						  (SETQ FOUND.SEPARATOR-CHARS (CONS (CAR SEPARRATOR-CHARS)
										    FOUND.SEPARATOR-CHARS))
						  ;; 1. hit of sep-list for sep-pos]] end
						  NIL)
						 (T	; try next sep of sep-list of sep-pos
						; take substring up to found pos=st.e 
						  (SETQ SUBSTRING.END STRING.ENDPOS) (CDR SEPARRATOR-CHARS))))
					   LIST.OF.SEPARRATOR-CHARS)
				     (SETQ FOUND.SUBSTRINGS (INTERN (SUBSEQ OPTION.VALUE (1- STRING.STARTPOS) SUBSTRING.END)
								    (find-package "MKRP")))
				     (SETQ STRING.STARTPOS (+ 1 SUBSTRING.END
							      (PRINT-LENGTH (OR (CAR FOUND.SEPARATOR-CHARS) ) NIL)))
				     ;; return list of found substrings
				     FOUND.SUBSTRINGS)
				 SEPARATORS))
		   (SETQ FOUND.SUBSTRINGS
			 (NCONC1 FOUND.SUBSTRINGS (INTERN (SUBSEQ OPTION.VALUE (1- STRING.STARTPOS) STRING.ENDPOS)
							  (find-package "MKRP"))))
		   (SMAPC
		     (FUNCTION
		       (LAMBDA (LEGAL.PART SUBSTRING)
			 (CASE (CAR LEGAL.PART)
			   (SET (SETQ NORMALIZED.PART (OPT=CHECK.IS.ELEMENT.OF.SET SUBSTRING (CDR LEGAL.PART) CHECK?)))
			   (RANGE (SETQ NORMALIZED.PART (OPT=CHECK.IS.IN.RANGE SUBSTRING (CDR LEGAL.PART) CHECK?)))
			   (FILE (SETQ NORMALIZED.PART (COND ((FILENAME.CHECK SUBSTRING) SUBSTRING) (T 'ILLEGAL.VALUE))))
			   (STRUCTURE (SETQ NORMALIZED.PART (OPT=CHECK.IS.OF.STRUCTURE SUBSTRING (CDR LEGAL.PART) CHECK?)))
			   (EITHER (SETQ NORMALIZED.PART (OPT=CHECK.IS.EITHER.RANGE SUBSTRING (CDR LEGAL.PART) CHECK?)))
			   (list (SETQ NORMALIZED.part (opt=check.is.list option.value)))
			   (OTHERWISE
			     (SETQ NORMALIZED.PART
				   (COND
				     (CHECK?
				      (PROGN (PRIN1 "---  error in opt=check.is.of.structure  ---  illegal rangetype]  ---" T)
					     (TERPRI T) (PROGN (PRINC LEGAL.PART) (TERPRI)) 'ILLEGAL.VALUE))
				     (T 'ILLEGAL.VALUE)))))
			 (COND
			   ((AND (NEQ NORMALIZED.PART 'ILLEGAL.VALUE) (NEQ NORMALIZED.PART 'SYNTAX.ERROR))
			    (SETQ NORMALIZED.VALUES (NCONC1 NORMALIZED.VALUES NORMALIZED.PART)))
			   (T (SETQ NORMALIZED.VALUES NORMALIZED.PART)))))
		     (FUNCTION
		       (LAMBDA (LIS)
			 (COND ((OR (EQL NORMALIZED.VALUES 'ILLEGAL.VALUE) (EQL NORMALIZED.VALUES 'SYNTAX.ERROR)) NIL)
			       (T (CDR LIS)))))
		     LEGAL.PARTS FOUND.SUBSTRINGS)
		   (SETQ FOUND.SEPARATOR-CHARS (NREVERSE FOUND.SEPARATOR-CHARS))	; list of all seps found in arg
						; now all parts of arg.val found are tested
						; and legal (see: legal.parts)]] the stan- 
						; dardized parts are listed in stval, the  
						; found sep-chars in sep.chars]] the stan- 
						; dardized argval is the concatenation of: 
						; <lp1, sp1,...lpj, spj,...lpn, spn, lpn+1>
						; which is the pname of the returned atom]]
		   (RETURN
		     (COND ((OR (EQL NORMALIZED.VALUES 'ILLEGAL.VALUE) (EQL NORMALIZED.VALUES 'SYNTAX.ERROR)) NORMALIZED.VALUES)
			   (T
			    (INTERN
			      (COERCE
				(CONS (CAR NORMALIZED.VALUES)
				      (MAPCAR
					#'(LAMBDA (SEP STV) (CONCATENATE 'STRING (PRINC-TO-STRING SEP) (PRINC-TO-STRING STV)))
					FOUND.SEPARATOR-CHARS
					(CDR NORMALIZED.VALUES)))
				'STRING)
			      (find-package "MKRP")))))))))
	(RETURN (COND ((NEQ 'ERROR RESULT) RESULT) (T 'ILLEGAL.VALUE)))))

(DEFUN OPT=CHECK.IS.EITHER.RANGE (OPTION.VALUE RANGE.DESCRIPTIONS CHECK?)
  ;; input: -option-value of type = either (= disjunction of r-typpes 1-5] => opt=check.and.stadardize)
  ;;        -list of range-description of r-types 1-5 (see opt*all.option.types)
  ;;        -check-flag (if =t the the input-syntax is checked too.
  ;; value: the normalized option-value argval if it is legal according to the either-range-descript.
  ;;        'illegal.argument' or 'syntax.error' else.
  (PROG ((NORMALIZED.VALUE T))
	(COND
	  (CHECK?
	   (COND
	     ((CONSP RANGE.DESCRIPTIONS)
	      (MAPC
		(FUNCTION
		  (LAMBDA (RANGE)
		    (COND ((AND (CONSP RANGE) (MEMBER (CAR RANGE) (SYMBOL-VALUE 'OPT*ALL.RANGE.TYPES))) T)
			  (T (PRIN1 "this is not a part of an either-range-description: " T) (PROGN (PRINC RANGE) (TERPRI))
			     (SETQ NORMALIZED.VALUE 'SYNTAX.ERROR)))))
		RANGE.DESCRIPTIONS))
	     (T (SETQ NORMALIZED.VALUE 'SYNTAX.ERROR) (PRIN1 "this is no an either-range description: " T)
		(PROGN (PRINC RANGE.DESCRIPTIONS) (TERPRI))))
	   (COND ((EQL 'SYNTAX.ERROR NORMALIZED.VALUE) (RETURN 'SYNTAX.ERROR)) (T NIL)))
	  (T NIL))
	(SMAPC #'IGNORE
	       (FUNCTION
		 (LAMBDA (LIST.OF.RANGE-DESCRIPTIONS)
		   (CASE (CAAR LIST.OF.RANGE-DESCRIPTIONS)
		     (SET
		       (SETQ NORMALIZED.VALUE (OPT=CHECK.IS.ELEMENT.OF.SET OPTION.VALUE
									   (CDAR LIST.OF.RANGE-DESCRIPTIONS) CHECK?)))
		     (RANGE (SETQ NORMALIZED.VALUE (OPT=CHECK.IS.IN.RANGE OPTION.VALUE (CDAR LIST.OF.RANGE-DESCRIPTIONS)
									  CHECK?)))
		     (FILE (SETQ NORMALIZED.VALUE (COND ((FILENAME.CHECK OPTION.VALUE) OPTION.VALUE) (T 'ILLEGAL.VALUE))))
		     (STRUCTURE
		       (SETQ NORMALIZED.VALUE (OPT=CHECK.IS.OF.STRUCTURE OPTION.VALUE (CDAR LIST.OF.RANGE-DESCRIPTIONS) CHECK?)))
		     (EITHER
		       (SETQ NORMALIZED.VALUE (OPT=CHECK.IS.EITHER.RANGE OPTION.VALUE (CDAR LIST.OF.RANGE-DESCRIPTIONS) CHECK?)))
		     (list (SETQ NORMALIZED.VALUe (opt=check.is.list option.value)))
		     (OTHERWISE
		       (SETQ NORMALIZED.VALUE
			     (COND
			       (CHECK?
				(PROGN (PRIN1 "---  error in opt=check.is.either.range  ---  illegal rangetype]  ---" T)
				       (TERPRI T)
				       (PROGN (PRINC LIST.OF.RANGE-DESCRIPTIONS) (TERPRI)) 'ILLEGAL.VALUE))
			       (T 'ILLEGAL.VALUE)))))
		   (COND ((EQL NORMALIZED.VALUE 'SYNTAX.ERROR) NIL) ((NEQ NORMALIZED.VALUE 'ILLEGAL.VALUE) NIL)
			 (T (SETQ NORMALIZED.VALUE 'ILLEGAL.VALUE) (CDR LIST.OF.RANGE-DESCRIPTIONS)))))
	       RANGE.DESCRIPTIONS)
	(RETURN NORMALIZED.VALUE)))

(defun opt-init () nil)



(DEFVAR OPT*ALL.RANGE.TYPES '(SET RANGE FILE EITHER list STRUCTURE))



(defun opt-is.completion ()
						; Edited:  29-JAN-1990 15:18
						; Authors: PRCKLN
						; Input:   -
						; Effect:  -
						; Value:   True iff any kb-completion is selected
  (member (opt-get.option er_paramodulation)
	  '(zhang-kapur heuristic-completion bachmair-ganzinger clause-graph snyder-lynch dershowitz)))


(defun opt-is.heuristic.completion ()
						; Edited:  29-JAN-1990 15:18
						; Authors: PRCKLN
						; Input:   -
						; Effect:  -
						; Value:   True iff heuristic completion is selected
  (eq (opt-get.option er_paramodulation) 'heuristic-completion))


(defun opt-is.kz.completion ()
						; Edited:  29-JAN-1990 15:18
						; Authors: PRCKLN
						; Input:   -
						; Effect:  -
						; Value:   True iff kapur-zhang completion is selected
  (member (opt-get.option er_paramodulation) '(zhang-kapur bachmair-ganzinger snyder-lynch dershowitz)))

(defun opt-is.with.residues ()
						; Edited:  12-MAR-1991 01:35
						; Authors: PRCKLN
						; Input:   -
						; Effect:  -
						; Value:   True iff a strategy with residue literals is chosen.
  (opt-get.option sort_literals))



(OPT-SET.STANDARD 'with.defvars)
(opt-set.standard nil)

#+(or symbolics explorer)
(PROCLAIM '(SPECIAL WND*MAINWINDOW WND*OPT-FLAG-FOR-EXPLAIN WND*EDITWINDOW WND*ACT-AREA WND*OPT-ALLOPTIONS WND*FLAG-FOR-EXPLAIN))

#+(or symbolics explorer)
(DEFUN WND=MK-GENERAL-ITEMLIST (OPTION)
  `(,(STRING OPTION)
    :BUTTONS ((NIL :VALUE ,OPTION)
	      (NIL :EVAL (PROGN (WND=OPT.AREA.HELP (QUOTE ,OPTION)) :AGAIN))
	      (NIL :EVAL (PROGN (OPT=WND_SET-AREA-TO-DEFAULTS (QUOTE ,OPTION)) :AGAIN)))
    :DOCUMENTATION ,(format nil "~A     L: Edit   M: Explain   R: Set to default" (CAR (OPT-GET.AREA.HEADLINE OPTION)))))


(DEFUN OPT-WND_MAINWINDOW-EXPOSE NIL
  #+(or symbolics explorer)
  (let ((old.fs.path (fs:default-pathname)))
    (unwind-protect
	(progn (fs:set-default-pathname (mkrp-make.pathname t "default" "lisp" nil))
	       (PROG (CHOICE)
		     (SETQ WND*MAINWINDOW
			   (TV:MAKE-WINDOW 'TV:MOMENTARY-MENU
					   :superior *terminal-io*
					   :LABEL " Choose option "
					   :BORDERS 3
					   :GEOMETRY '(1)
					   :VSP 5
					   :ITEM-LIST
					   `((" " :NO-SELECT NIL)
					     ,@(MAPCAR #'WND=MK-GENERAL-ITEMLIST (OPT-ALL.AREAS))
					     (" " :NO-SELECT NIL)
					     (" Store options " :BUTTONS
					      ((NIL :EVAL (PROGN (OS=OPT.WRITE) :AGAIN))
					       (NIL :EVAL (PROGN (WND=OPT.AREA.HELP 'W) :AGAIN))
					       (NIL :EVAL (PROGN (BEEP) ':AGAIN)))
					      :documentation "Store options     L: Do it   M: Explain")
					     (" Restore options " :BUTTONS
					      ((NIL :EVAL (PROGN (OS=OPT.READ) (OPT-WND_INIT) :AGAIN))
					       (NIL :EVAL (PROGN (WND=OPT.AREA.HELP 'R) :AGAIN))
					       (NIL :EVAL (PROGN (BEEP) :AGAIN)))
					      :documentation "Restore options     L: Do it   M: Explain")
					     (" Dump  options " :BUTTONS
					      ((NIL :EVAL (PROGN (OS=OPT.PRETTYPRINT) :AGAIN))
					       (NIL :EVAL (PROGN (WND=OPT.AREA.HELP 'PP) (OPT-WND_INIT) :AGAIN))
					       (NIL :EVAL (PROGN (BEEP) ':AGAIN)))
					      :documentation "Dump  options     L: Do it   M: Explain")
					     (" " :NO-SELECT NIL))))
		  LOOP
		     (SETQ CHOICE (GLOBAL:SEND WND*MAINWINDOW :CHOOSE))
		     (COND ((NULL CHOICE) (GLOBAL:SEND WND*MAINWINDOW :DEEXPOSE))
			   ((EQL CHOICE :AGAIN) (GO LOOP))
			   (T (WND=EDIT-OPTION CHOICE) (GO LOOP)))))
      (fs:set-default-pathname old.fs.path))))

#+(or symbolics explorer)
(DEFUN OPT=WND_SET-AREA-TO-DEFAULTS (AREA)
  (let ((AREAOPTIONS (OPT-GET.AREA.OPTIONS AREA)) VAL (optlist nil))
    (when (TV:MOUSE-Y-OR-N-P (format nil "  Set all options of area ~A to default values." area))
      (DOLIST (OPTION (OPT-GET.AREA.OPTIONS AREA))
	(SETQ OPTLIST (APPEND OPTLIST (OPT=WND_MK-OPTLIST-TO-EDIT OPTION))))
      (MAPCAR #'(LAMBDA (OPTION)
		  (EVAL `(OPT-PUT.OPTION ,OPTION (OPT-GET.DEFAULT.VALUE ',OPTION))))
	      AREAOPTIONS)
      (MAPCAR #'(LAMBDA (OPTION)
		  (SETQ VAL (EVAL `(OPT-GET.OPTION ,(CAR OPTION))))
		  (OPT=WND_PUT-INTERNAL-VALUE
		    (CAR OPTION)
		    (if (ATOM (SECOND OPTION))
			VAL
			(PROGN (OPT=WND_PUT-INTERNAL-VALUE
				 (OPT=WND_MK-OTHERNAME (CAR OPTION))
				 (COND ((NOT (MEMBER VAL (MAPCAR #'CDR (CADADR OPTION)) :TEST (FUNCTION EQUAL))) VAL)
				       ((EQL (CAAR (LAST OPTION)) ':NUMBER) 0)
				       (T NIL)))
			       (SETQ VAL (EVAL `(OPT-GET.OPTION ,(CAR OPTION))))
			       (IF (MEMBER VAL (MAPCAR #'CDR (CADADR OPTION)) :TEST (FUNCTION EQUAL)) VAL 'OTHER)))))
	      (MAPCAR #'(LAMBDA (X) (ASSOC X WND*OPT-ALLOPTIONS)) AREAOPTIONS))
      (mapc #'(lambda (item) (when (eq (symbol-value (first item)) 'other)
			       (setf (symbol-value (first item))
				     (symbol-value (OPT=WND_MK-OTHERNAME (intern (subseq (symbol-name (first item)) 4)
										 (find-package "MKRP")))))))
	    optlist))))

#+(or symbolics explorer)
(DEFUN OPT=WND_PUT.OPTION (OPTID VALUE)
  (let ((STVAL (OPT=CHECK OPTID VALUE)))
    (COND ((EQL 'ILLEGAL.VALUE STVAL) NIL)
	  (T (eval `(OPT=PUT ',OPTID ',STVAL)) T))))

#+(or symbolics explorer)
(DEFUN WND=OPT.AREA.HELP (UNKNOWN)
  (COND ((AND (BOUNDP 'WND*OPT-FLAG-FOR-EXPLAIN) WND*OPT-FLAG-FOR-EXPLAIN) (OPT-GET.OPTION.TEXT UNKNOWN))
	(T (CASE UNKNOWN
	     ((H HELP) (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_HELP)))
	     ((P PRINT) (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_PRINT)))
	     ((PP PPRINT) (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_PPRINT)))
	     ((R READ) (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_READ)))
	     ((W WRITE) (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_WRITE)))
	     ((L LISP) (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_LISP)))
	     (OK (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_OK)))
	     (V (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_V)))
	     (OTHERWISE
	       (COND ((MEMBER UNKNOWN (OPT-ALL.OPTIONS)) (WND=NOTIFY (OPT-GET.OPTION.TEXT UNKNOWN)))
		     ((MEMBER UNKNOWN (OPT-ALL.AREAS))
		      (WND=NOTIFY
			(APPEND (OPT-GET.AREA.EXPLANATION UNKNOWN) (OS.E-GET.EXPLANATION 'OPT.HELP_AREA.ABBREVIATION))))
		     (T (WND=NOTIFY (OS.E-GET.EXPLANATION 'OPT.HELP_ALL.COMMANDS)))))))))

#+(or symbolics explorer)
(DEFUN WND=GET-OPTION-HEADER (OPTION)
  (PROG ((TXT (OPT-GET.OPTION.TEXT OPTION)))
    (COND ((AND (CONSP TXT) (> (LENGTH TXT) 1)) (RETURN (SECOND TXT))) (T (RETURN TXT)))))

#+(or symbolics explorer)
(DEFUN WND=EDIT-OPTION (AREA)
  (let ((OPTLIST NIL) MARGINS)
    (SETQ WND*ACT-AREA AREA)
    (DOLIST (OPTION (OPT-GET.AREA.OPTIONS AREA))
      (SETQ OPTLIST (APPEND OPTLIST (OPT=WND_MK-OPTLIST-TO-EDIT OPTION))))
    (SETQ MARGINS '(("Explain" (OPT=WND_EXPLAIN))))
    (SETQ OPTLIST (STRINGS-EQUALIZE-LENGTH OPTLIST 'CADR))
    (TV:CHOOSE-VARIABLE-VALUES
      OPTLIST
      :superior *terminal-io*
      :LABEL (format nil " Edit the options of area ~A!" area)
      :FUNCTION 'OPT=WND_CHECK-AND-ASSIGN
      :MARGIN-CHOICES MARGINS)
    (mapc #'(lambda (item) (when (eq (symbol-value (first item)) 'other)
			     (setf (symbol-value (first item))
				   (symbol-value (OPT=WND_MK-OTHERNAME (mkrp-intern (subseq (symbol-name (first item)) 4)))))))
	  optlist)))

#+(or symbolics explorer)
(DEFUN OPT=WND_EXPLAIN NIL
  (PROG ((OPTIONS (OPT-GET.AREA.OPTIONS WND*ACT-AREA))
	 UNKNOWN-OPTION)
     LOOP
	(IF (SETQ UNKNOWN-OPTION
		  (GLOBAL:SEND
		    (TV:MAKE-WINDOW 'TV:MOMENTARY-MENU
				    :superior *terminal-io*
				    :LABEL " Select option to be explained! "
				    :BORDERS 2
				    :ITEM-LIST `(,@(MAPCAR #'(LAMBDA (EL) (STRING EL))
							   OPTIONS)))
		    ':CHOOSE))
	    (WND=NOTIFY (cons UNKNOWN-OPTION (OPT-GET.OPTION.TEXT (INTERN UNKNOWN-OPTION (find-package "MKRP"))))))
	(IF (NULL UNKNOWN-OPTION) (RETURN NIL) (GO LOOP))))

#+(or symbolics explorer)
(DEFUN OPT=WND_MK-OPTLIST-TO-EDIT (OPTION)
  (let ((TYPE (CDR (ASSOC OPTION WND*OPT-ALLOPTIONS)))
	(*print-length* 2))
    `((,(OPT=WND_MK-NAME OPTION)
       ,(FORMAT NIL " ~A  {Default = ~A} "  option (case (OPT-GET.DEFAULT.VALUE OPTION)
						     ((nil)
						      #+:mkrp.deutsch 'nein
						      #-:mkrp.deutsch 'no)
						     ((t) 
						      #+:mkrp.deutsch 'ja
						      #-:mkrp.deutsch  'yes)
						     (otherwise (OPT-GET.DEFAULT.VALUE OPTION))))
       ,@(COND ((ATOM (CAR TYPE)) TYPE)
	       (T (CAR TYPE))))
      ,@(COND ((ATOM (CAR TYPE)) NIL)
	      (T `((,(OPT=WND_MK-OTHERNAME OPTION)
		    "  Other value of above :"
		    ,@(CADR TYPE))))))))

#+(or symbolics explorer)
(DEFUN OPT=WND_MK-NAME (OPTION) (INTERN (format nil "OPT*~A" OPTION) (find-package "MKRP")))

#+(or symbolics explorer)
(DEFUN OPT=WND_MK-OTHERNAME (OPTION) (INTERN (format nil "OPT*OTHER-~A" OPTION) (find-package "MKRP")))

#+(or symbolics explorer)
(DEFUN OPT=WND_CHECK-AND-ASSIGN (WINDOW OPTION OLD NEW)
  (PROG ((NAME (OPT=WND_REMOVE-ANYPREFIX OPTION)) (SUCCESS NIL) (REFRESH NIL))
	(COND
	  ((OPT=WND_IS-OTHER OPTION) (SETQ REFRESH T)
	   (COND
	     ((SETQ SUCCESS (OPT=WND_PUT.OPTION NAME NEW)) (OPT=WND_PUT-INTERNAL-VALUE (OPT=WND_MK-NAME NAME) 'OTHER)
	      (OPT=WND_PUT-INTERNAL-VALUE OPTION NEW))))
	  ((EQL NEW 'OTHER)
	   (AND (SETQ SUCCESS (OPT=WND_PUT.OPTION NAME (OPT=WND_GET-INTERNAL-VALUE (OPT=WND_MK-OTHERNAME NAME))))
		(OPT=WND_PUT-INTERNAL-VALUE NAME 'OTHER)))
	  (T (AND (SETQ SUCCESS (OPT=WND_PUT.OPTION NAME NEW)) (OPT=WND_PUT-INTERNAL-VALUE OPTION NEW))))
	(COND
	  ((NULL SUCCESS) (SET OPTION OLD) (GLOBAL:BEEP)
	   (WND=NOTIFY
	     (list* (FORMAT NIL ">>>  ~a  is not a legal value for"
			    (IF (EQL NEW 'OTHER) (OPT=WND_GET-INTERNAL-VALUE (OPT=WND_MK-OTHERNAME NAME)) NEW))
		    " "
		    (OPT-GET.OPTION.TEXT (OPT=WND_REMOVE-ANYPREFIX OPTION))))
	   (SETQ WND*OPT-FLAG-FOR-EXPLAIN NIL)))
	(COND (REFRESH (GLOBAL:SEND WINDOW ':REFRESH) (RETURN T)) (T (RETURN NIL)))))

(DEFUN STRINGS-EQUALIZE-LENGTH (STRINGS ACCESS-FUNCTION)
  (LET
    ((LENGTH
       (EVAL
         (CONS 'MAX
           (MAPCAR (FUNCTION (LAMBDA (STRING) (LENGTH (FUNCALL (OR ACCESS-FUNCTION 'QUOTE) STRING)))) STRINGS))))
      STRING)
    (MAPL (FUNCTION
        (LAMBDA (TAIL) (DECLARE (SPECIAL TAIL))
          (COND
            (ACCESS-FUNCTION (SETQ TAIL (CAR TAIL)) (SETQ STRING (FUNCALL ACCESS-FUNCTION TAIL))
              (EVAL
                (LIST 'SETF (LIST ACCESS-FUNCTION 'TAIL)
                  (CONCATENATE 'STRING STRING
                    (MAKE-ARRAY (LIST (- LENGTH (LENGTH STRING))) :ELEMENT-TYPE 'STRING-CHAR :INITIAL-ELEMENT #\sPACE)))))
            (T (SETQ STRING (CAR TAIL))
              (RPLACA TAIL
                (CONCATENATE 'STRING STRING
                  (MAKE-ARRAY (LIST (- LENGTH (LENGTH STRING))) :ELEMENT-TYPE 'STRING-CHAR :INITIAL-ELEMENT #\sPACE)))))))
      (COPY-LIST STRINGS))))

#+(or symbolics explorer)
(DEFUN OPT=WND_REMOVE-OTHERPREFIX (OPTION)
  (COND ((OPT=WND_IS-PREFIXED OPTION) (OPT=WND_REMOVE-ANYPREFIX OPTION)) (T OPTION)))

#+(or symbolics explorer)
(DEFUN OPT=WND_REMOVE-ANYPREFIX (OPTION)
  (let (LEN)
    (INTERN
      (SUBSEQ (STRING OPTION)
	      (1- (COND ((SETQ LEN (OPT=WND_IS-OTHER OPTION)) (+ LEN 11))
			((SETQ LEN (OPT=WND_IS-PREFIXED OPTION)) (+ LEN 5))
			(T 0)))
	      (LENGTH (STRING OPTION)))
      (find-package "MKRP"))))

#+(or symbolics explorer)
(DEFUN OPT=WND_IS-PREFIXED (OPTION) (SEARCH "OPT*" (STRING OPTION)))

#+(or symbolics explorer)
(DEFUN OPT=WND_IS-OTHER (OPTION) (SEARCH "OPT*OTHER-" (STRING OPTION)))

#+(or symbolics explorer)
(DEFUN OPT=WND_PUT-INTERNAL-VALUE (OPTION VALUE)
  (COND ((OPT=WND_IS-PREFIXED OPTION) (SET OPTION VALUE))
	(T (SET (OPT=WND_MK-NAME OPTION) VALUE))))

#+(or symbolics explorer)
(DEFUN OPT=WND_GET-INTERNAL-VALUE (OPTION)
  (COND ((OPT=WND_IS-PREFIXED OPTION) (EVAL OPTION)) (T (EVAL (OPT=WND_MK-NAME OPTION)))))

#+(or symbolics explorer)
(DEFUN OPT-WND_INIT NIL
  (LET (VAL)
    (OPT=WND_MK-OPTIONLIST-FOR-INTERNAL-USE)
    (MAPCAR #'(LAMBDA (OPTION)
		;(DECLARE (SPECIAL OPTION))
		(SETQ VAL (EVAL `(OPT-GET.OPTION ,(CAR OPTION))))
		(OPT=WND_PUT-INTERNAL-VALUE
		  (CAR OPTION)
		  (COND ((ATOM (SECOND OPTION)) (EVAL `(OPT-GET.OPTION ,(CAR OPTION))))
			(T (OPT=WND_PUT-INTERNAL-VALUE (OPT=WND_MK-OTHERNAME (CAR OPTION))
						       (COND ((NOT (MEMBER VAL (MAPCAR #'CDR (CADADR OPTION))
									   :TEST (FUNCTION EQUAL))) VAL)
							     ((EQL (CAAR (LAST OPTION)) :NUMBER) 0) (T NIL)))
			   (IF (MEMBER VAL (MAPCAR #'CDR (CADADR OPTION)) :TEST (FUNCTION EQUAL))
			       VAL
			       val))))) ;'OTHER))))) Doesn't work, Axel
	    WND*OPT-ALLOPTIONS)
    (SETQ WND*OPT-FLAG-FOR-EXPLAIN NIL))
  'DONE)

#+(or symbolics explorer)
(DEFUN WND=NOTIFY (MESSAGE)
  (let* ((WINDOW (TV:MAKE-WINDOW 'TV:MOMENTARY-MENU
				 :BORDERS 4
				 :superior *terminal-io*
				 :default-character-style '(:fix :roman :normal)
				 :LABEL " Explanation:"
				 :COLUMNS 1
				 :VSP 4))
	 (ITEMLIST `((" " :NO-SELECT T)
		     ,@(mapcar #'(lambda (item) (LIST item :NO-SELECT T)) message)
		     (" " :NO-SELECT t)
		     (" Move the mouse out to get rid of this!" :NO-SELECT t)
		     (" " :NO-SELECT t)))
	 (max (apply #'max (mapcar #'(lambda (item) (length (first item))) itemlist))))
    (GLOBAL:SEND WINDOW :SET-ITEM-LIST (mapc #'(lambda (item)
						 (setf (first item)
						       (replace (make-string (1+ max) :initial-element #\space) (first item))))
					     ITEMLIST))
    (GLOBAL:SEND WINDOW :CHOOSE)
    nil))

#+(or symbolics explorer)
(DEFUN OPT=WND_MK-OPTIONLIST-FOR-INTERNAL-USE NIL
  (LET ((OPTLIST NIL)
	(ATP.OPT.LIST (OPT-ALL.OPTIONS))
	TYPE)
    (DOLIST (ITEM ATP.OPT.LIST)
      (SETQ TYPE    (THIRD (ASSOC ITEM OPT*ALL.OPTIONS))
	    OPTLIST (APPEND OPTLIST (LIST (CONS ITEM (OPT=WND_DETERMINE-TYPE-FOR-CHOOSE-VARIABLE-VALUES ITEM TYPE))))))
    (SETQ WND*OPT-ALLOPTIONS OPTLIST)
    'DONE))

#+(or symbolics explorer)
(DEFUN OPT=WND_DETERMINE-TYPE-FOR-CHOOSE-VARIABLE-VALUES (OPTION TYPE)
  (LET (VARIABLETYPE)
    (CASE (CAR TYPE)
      (SET (DOLIST (ITEM (CDR TYPE))
	     (SETQ VARIABLETYPE (APPEND VARIABLETYPE (LIST (CONS #+:mkrp.deutsch (third item)
								 #-:mkrp.deutsch (second item)
								 (car item))))))
	   (LIST ':ASSOC VARIABLETYPE))
      (RANGE '(:NUMBER))
      (FILE '(:PATHNAME))
      (EITHER (OPT=WND_HANDLE-EITHER-TYPE OPTION TYPE))
      (OTHERWISE '(:SEXP)))))

#+(or symbolics explorer)
(DEFUN OPT=WND_HANDLE-EITHER-TYPE (OPTION TYPE)
  (COND ((AND (EQL (CAADR (ASSOC 'SET (CDR TYPE))) NIL)
	      (ASSOC 'RANGE (CDR TYPE))
	      (EQL (LENGTH (CDR (ASSOC 'SET (CDR TYPE)))) 1))
	 '(:NUMBER-OR-NIL))
	((ASSOC 'SET (CDR TYPE))
	 (LIST
	   (OPT=WND_DETERMINE-TYPE-FOR-CHOOSE-VARIABLE-VALUES
	     OPTION `(SET ,@(CDR (ASSOC 'SET (CDR TYPE))) (OTHER OTHER)))
	   (OPT=WND_DETERMINE-TYPE-FOR-CHOOSE-VARIABLE-VALUES
	     OPTION (IF (EQL (CAADR TYPE) 'SET) (THIRD TYPE) (SECOND TYPE)))))
	(T '(:SEXP))))



#+(or symbolics explorer)
(OPT-WND_INIT)

(opt-set.standard nil)