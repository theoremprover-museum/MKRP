;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for MKRP version 53.9
;;; Reason: Variable MKRP::OPT*ALL.AREAS:  other.prover
;;; Variable MKRP::OPT*ALL.OPTIONS:  dito
;;; c
;;; er
;;; Written by mkrp, 5/04/92 17:26:31
;;; while running on JS-SFBUX1 from FEP0:>Genera-8-0-Inc-kkl-beweiser.ilod.1
;;; with Genera 8.0.2, IP-TCP 422.9, IP-TCP Documentation 404.0, CLX 419.3,
;;; X Remote Screen 418.1, X Documentation 403.0, Network RPC 415.5,
;;; NFS Client 415.3, NFS Documentation 404.0,
;;; Logical Pathnames Translation Files NEWEST,
;;; Karlsruher Kaiserslauterner Lisp 24.1, HADES 19.0, Waltz 8.0, COLUMN 9.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.8, GENTRAFO 3.0,
;;; Ivory Revision 4 (FPA enabled), FEP 322, FEP0:>I322-Loaders.flod(222),
;;; FEP0:>I322-Info.flod(157), FEP0:>I322-Debug.flod(158), FEP0:>I322-Lisp.flod(145),
;;; , Boot ROM version 316, Device PROM version 322, Genera application 2.2.1.2,
;;; UX Support Server 2.2.1.1, Ivory-life program 2.2.1.4,
;;; UX kernel life support 2.2.1.4, SunOS (JS370.26APR92) 4.1.1,
;;; 1037x760 1-bit STATIC-GRAY X Screen INTERNET|134.96.236.11:0.0 with 0 Genera fonts (MIT X Consortium R5000),
;;; Machine serial number 519.



;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
; From buffer options.lisp /home1/mkrp/prog/opt/ JS-SFBSUN:
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")

;;; Global variables containing all info

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
		  #+:mkrp.deutsch("optionen fuer verschiedene reduktionsregeln waehrend der deduktion.")
		  #-:mkrp.deutsch("Options for various reductionrules during deduction."))
		(STR
		  #+:mkrp.deutsch("steuerung der beweissuche")
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
				     er_narrow.test er_compile)
		  #+:mkrp.deutsch("Optionen zur Steuerung des eingebauten Gleichheitsbeweisers.")
		  #-:mkrp.deutsch("Options to control the built in equality resoning procedure."))
		(GEN
		  #+:mkrp.deutsch("allgemeine optionen")
		  #-:mkrp.deutsch("Miscellaneous options")
		  (GEN_SPLITTING gen_presimplification gen_MIN.EXPRESSION.LENGTH.FOR.FILE gen_MIN.EXPRESSION.SIZE.FOR.FILE
				 GEN_MANUAL.CONTROL GEN_MAXIMUM.STEPS GEN_GRAPH.SAVING GEN_SAVE.FILE
				 GEN_LISP.GARBAGE.COLLECTION gen_common.lisp gen_other.prover)
		  #+:mkrp.deutsch("verschiedene allgemeine optionen die zu keinem anderen bereich gehoeren.")
		  #-:mkrp.deutsch("Various general options, not belonging to any other option-area."))
		(TR
		  #+:mkrp.deutsch("ablaufverfolgung")
		  #-:mkrp.deutsch("Tracing")
		  (TR_PREPROCESSING TR_STEP.MODE TR_DUMP TR_CLAUSE.MODE TR_LINK.MODE TR_TRACE.FILE TR_TERMINAL)
		  #+:mkrp.deutsch("optionen zur verfolgung des programmablaufs.")
		  #-:mkrp.deutsch("options for various program traces."))
		(PR
		  #+:mkrp.deutsch("Protokollmodi")
		  #-:mkrp.deutsch("Protocol modes")
		  (pr_latex PR_INFIX.FORM PR_PREFIX.FORM PR_OPTIONS PR_AXIOM.CLAUSES PR_SYMBOLS
			    pr_direct.proof pr_variable.print.names
			    PR_PROTOCOL PR_LEFT.MARGIN PR_RIGHT.MARGIN PR_LINELENGTH pr_literals)
		  #+:mkrp.deutsch("optionen zur auswahl verschiedener beweisprotokolle.")
		  #-:mkrp.deutsch("options to select from different proof-protocols."))))


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
; From buffer options.lisp /home1/mkrp/prog/opt/ JS-SFBSUN:
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")

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
		(er_compile
		  NIL (EITHER (RANGE (1 1000)) (SET (NIL NO NEIN n) (tree-interpreter ti ti)
						    (always-tree-compile atc atc)
						    (canonical-tree-compile ctc ctc)))
		  #+:mkrp.deutsch()
		  #-:mkrp.deutsch("  NIL            Single non compiled rules are used for rewriting."
				  "  1 - 1000       Specifies the number of steps after which"
				  "                 the single rewrite rules are compiled."
				  "  tree-interpreter A tree is constructed for each function symbol"
				  "                 and dynamically updated if when deleting and inserting rules."
				  "  always-tree-compile These trees are newly compiled to Lisp after"
				  "                 each dynamical change."
				  "  canonical-tree-compile The trees are only compiled once, when it"
				  "                 is clear wether the system of rules is canonical."))
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


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
; From buffer options.lisp /home1/mkrp/prog/opt/ JS-SFBSUN:
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- mode: lisp; syntax: common-lisp; package: mkrp -*-")

(OPT-SET.STANDARD 'with.defvars)

(OPT-SET.STANDARD nil)

(OPT-WND_INIT)

(opt-set.standard nil)


;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
; From buffer preparation.lisp /home1/mkrp/prog/c/ JS-SFBSUN:
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-")


(DEFUN PREP.c-INITIAL.GRAPH (PROBLEM.FILE GRAPH.FILE CODE.FILE COMMENT)
						; input   names of three files and a comment for the
						;         proof protocol.
						; effect: for the axioms and theorems resp. for all
						;         splitparts specified on problem.file the
						;         initial graphs are constructed and saved
						;         on the graph.file. the raw data for the
						;         protocol is written on code.file
						; value:  a legal total.result.
						; remark: code.file and graph.file are respected to
						;         be open and remain so
  (declare (ignore code.file comment))
  (let (RESULT)
    (PREP.C=INIT PROBLEM.FILE GRAPH.FILE)
    (unwind-protect (progn (PREP=READ.PROBLEM.FILE)
			   (SETF PREP*AXIOMS.PREFIX   (PREP=REMOVE.COMMENTS PREP*AXIOMS.PREFIX)
				 PREP*THEOREMS.PREFIX (PREP=REMOVE.COMMENTS PREP*THEOREMS.PREFIX)
				 RESULT               (PSIM-APPLY.DEFINITIONS PREP*AXIOMS.PREFIX PREP*THEOREMS.PREFIX)
				 PREP*AXIOMS.PREFIX   (CAR RESULT)
				 PREP*THEOREMS.PREFIX (SECOND RESULT)
				 RESULT               (PREP.C=CREATE.AXIOMGRAPH PREP*AXIOMS.PREFIX))
			   (unless RESULT (SETQ RESULT (PREP.C=CREATE.INITIAL.GRAPHS PREP*THEOREMS.PREFIX))))
      (PREP.C=END))
    RESULT))

(DEFUN PREP.C=INIT (PROBLEM.FILE GRAPH.FILE)
						; INPUT : TWO FILENAMES
						; REMARK: FILES ARE EXPECTED TO BE OPEN
						; EFFECT: INITIALIZES ALL NECESSARY VARIABLES
						; VALUE:  UNDEFINED
  (SETQ PREP*PROBLEM.FILE PROBLEM.FILE)
  (SETQ PREP*GRAPH.FILE GRAPH.FILE)
  (SETQ PREP*SEVERAL.SPLITPARTS NIL)
  (SETQ PREP*STEPNUMBER 0
	prep*objects nil)  
  (SETQ PREP*NUMBER.OF.AXIOM.CLAUSES 0)
  (SETQ PREP*AXIOMS.INFIX NIL)
  (SETQ PREP*AXIOMS.PREFIX NIL)
  (SETQ PREP*THEOREMS.INFIX NIL)
  (SETQ PREP*THEOREMS.PREFIX NIL)
  (SETQ PREP*COMMENT NIL))

(DEFUN PREP.C=END NIL
						; INPUT:  NONE
						; EFFECT: ENDS ALL NECESSARY SUBMODULES
						; VALUE:  UNDEFINED
  )

(DEFUN PREP.C=CREATE.AXIOMGRAPH (FORMULALIST)
						      ; EDITED:  6-MAR-84 19:46:34
						      ; INPUT:  A LIST OF FOPC-EXPRESSIONS PREFIX-FORM
						      ; EFFECT: THE FORMULAS ARE PRESIMPLIFIED AND NORMA-
						      ;         LIZED. THE ATTRIBUTE CLAUSES AND AXIOM
						      ;         CLAUSES ARE CONSTRUCTUED AND ADDED TO THE
						      ;         AXIOMGRAPH
						      ; VALUE:  A LEGAL TOTAL.RESULT
  (PROG (CLAUSELIST PRESIMPLIFIED.FORMULA NORMALIZED.FORMULA RESULT)
	(SETQ CLAUSELIST
	      (SMAPCAR #'(LAMBDA (FORMULA)
			   (SETQ PRESIMPLIFIED.FORMULA (PSIM-PRESIMPLIFICATION FORMULA))
			   (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'PRESIMPLIFIED PRESIMPLIFIED.FORMULA)
			   (COND ((MEMBER PRESIMPLIFIED.FORMULA '(TRUE FALSE)) PRESIMPLIFIED.FORMULA)
				 (T (SETQ NORMALIZED.FORMULA (CAR (NORM-NORMALIZATION PRESIMPLIFIED.FORMULA NIL)))
				    (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'NORMALIZED NORMALIZED.FORMULA) NORMALIZED.FORMULA)))
		       #'(LAMBDA (TAIL) (COND ((NEQ PRESIMPLIFIED.FORMULA 'FALSE) (CDR TAIL))))
		       FORMULALIST))
	(COND
	  ((MEMBER 'FALSE CLAUSELIST) (SETQ RESULT (LIST 'SUCCESS 'AXIOMS.UNSATISFIABLE))))
	(COND
	  ((NOT RESULT)
	   (SETQ CLAUSELIST (DELETE 'TRUE CLAUSELIST))
	   (SETQ CLAUSELIST (APPLY (FUNCTION NCONC) CLAUSELIST))
	   (SETQ CLAUSELIST
		 (SORT CLAUSELIST #'(LAMBDA (CLAUSE1 CLAUSE2) (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2)))))
						      ;(cons-CONSTRUCT.ATTRIBUTE.CLAUSES)
						      ;   ADDING THE AXIOM-CLAUSES TO THE GRAPH
	   (MAPC #'(LAMBDA (CLAUSE)
		     (prep.c=print.clause.objects clause prep*graph.file))
		 CLAUSELIST)
	   (format prep*graph.file "(setq ectl*false ")
	   (prep.c=print.term (dt-predicate.false) prep*graph.file)
	   (format prep*graph.file "~%   ectl*true ")
	   (prep.c=print.term (dt-predicate.true) prep*graph.file)
	   (format prep*graph.file "~%   ectl*equality ")
	   (prep.c=print.term (first (dt-predicate.equalities)) prep*graph.file)
	   (format prep*graph.file "~%   ectl*AXIOMS (list ")
	   (MAPC #'(LAMBDA (CLAUSE)
		     (prep.c=print.clause clause prep*graph.file))
		 CLAUSELIST)))
	(format prep*graph.file ")")
	(RETURN RESULT)))

(DEFUN PREP.C=CREATE.INITIAL.GRAPHS (THEOREMS.PREFIX)
						; EDITED:  6-MAR-84 20:36:11
						; INPUT:  A LIST OF FOPC EXPRESSIONS IN PREFIXFORM
						; EFFECT: THE EXPRESSIONS ARE PREPROCESSED.IF THERE
						;         EXISTS SEVERAL SPLITPARTS THE AXIOM GRAPH IS
						;         SAVED ON FILE IF NON-EMPTY.IF THE LIST IS
						;         EMPTY OR THE NEGATED CONJUNCTION IS
						;         RECOGNIZED AS VALID THEN THE INITIAL
						;         REDUCTIONS ARE PERFORMED AND THE GRAPH IS
						;         SAVED ON THE INITIAL.GRAPH FILE.
						; VALUE:  A LEGAL TOTAL.RESULT
  (PROG (RESULT)
	(SETQ RESULT (PREP=THEOREMS.CLAUSE.LISTS THEOREMS.PREFIX))
	(COND
	  ((EQL RESULT 'FALSE)
	   (SETQ RESULT '(SUCCESS THEOREMS.VALID))
	   (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT)
	   (SETQ RESULT 'SUCCESS))
	  ((EQL RESULT 'TRUE)
	   (SETQ PREP*SEVERAL.SPLITPARTS NIL)
	   (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT)
	   (SETQ RESULT (COND (RESULT (CAR RESULT)))))
	  (T (SETQ PREP*SEVERAL.SPLITPARTS (NORM-SEVERAL.SPLITPARTS?))
	     (SETQ RESULT (PREP.C=CREATE.SPLITGRAPHS))))
	(RETURN RESULT)))

(DEFUN PREP.C=CREATE.SPLITGRAPHS NIL		; EDITED:  6-MAR-84 20:59:34
						; EFFECT: FOR ALL SPLITPARTS THE INITIAL GRAPHS ARE
						;         CONSTRUCTED AND SAVED ON THE INITIAL.GRAPH
						;         FILE
						; VALUE:  A LEGAL TOTAL.RESULT
  (let ((RESULT nil)
	NORMALIZED.THEOREMS)
    (format prep*graph.file "~2% ectl*THEOREMS (list ")
    (WHILE (NORM-more.SPLITPARTS?)
      (princ "(list " prep*graph.file)
      (SETQ NORMALIZED.THEOREMS (SORT (NORM-NEXT.SPLITPART)
				      #'(LAMBDA (CLAUSE1 CLAUSE2)
					  (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2)))))
      (MAPC #'(LAMBDA (CLAUSE)
		(prep.c=print.clause clause prep*graph.file))
	    NORMALIZED.THEOREMS)
      (princ ")" prep*graph.file))
    (format prep*graph.file "))")
    RESULT))

(defun prep.c=print.clause (clause st)
  (princ "(list " st)
  (mapc #'(lambda (lit)
	    (prep.c=print.term (if (ds-sign.is.negative (ds-lit.sign lit))
				 (list (first (dt-predicate.equalities))
				       (cons (ds-lit.predicate lit) (ds-lit.termlist lit))
				       (dt-predicate.false))
				 (cons (ds-lit.predicate lit) (ds-lit.termlist lit)))
			     st))
	clause)
  (format st ")~%"))

(defun prep.c=print.term (term st)
  (if (consp term)
      (progn (princ "(eds-term_create " st)
	     (prep.c=print.term (first term) st)
	     (princ "(list " st)
	     (mapc #'(lambda (subterm) (prep.c=print.term subterm st))
		   (rest term))
	     (princ "))" st))
      (if (numberp term)
	  (if (member term prep*objects)
	      (format st "'#~A#" term)
	      (progn (push term prep*objects)
		     (format st "'#~A=#.~A" term (cond ((dt-variable.is term)
						       (format nil "(eds-var_create ~S)" (dt-pname term)))
						      ((dt-function.is term)
						       (format nil "(eds-fct_create ~S ~A ~A)"
							       (dt-pname term) (dt-function.arity term) (ord=symbol.weight term)))
						      ((dt-constant.is term)
						       (format nil "(eds-const_create ~S ~A)" (dt-pname term)
							       (ord=symbol.weight term)))
						      ((dt-predicate.is term)
						       (format nil "(eds-fct_create ~S ~A ~A)"
							       (dt-pname term) (length (DT-PREDICATE.DOMAINSORTS term))
							       (ord=symbol.weight term)))))))
	  (print term st))))

(defun prep.c=print.clause.objects (clause st)
  (mapc #'(lambda (lit)
	    (prep.c=print.term.objects (if (ds-sign.is.negative (ds-lit.sign lit))
					   (list (first (dt-predicate.equalities))
						 (cons (ds-lit.predicate lit) (ds-lit.termlist lit))
						 (dt-predicate.false))
					   (cons (ds-lit.predicate lit) (ds-lit.termlist lit)))
				       st))
	clause))

(defun prep.c=print.term.objects (term st)
  (if (consp term)
      (progn (prep.c=print.term.objects (first term) st)
	     (mapc #'(lambda (subterm) (prep.c=print.term.objects subterm st))
		   (rest term)))
      (if (numberp term)
	  (if (member term prep*objects)
	      nil
	      (progn (push term prep*objects)
		     (format st "term term~A = ~A" term (cond ((dt-variable.is term)
							       (format nil " TCreate(~S,0,TlEmpty());~%" term))
							      ((dt-function.is term)
							       (format nil "~A;~%" term))
							      ((dt-constant.is term)
							       (format nil "~A;~%" term))
							      ((dt-predicate.is term)
							       (format nil "~A;~%" term))))))
	  (print term st))))

;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
; From buffer preparation.lisp /home1/mkrp/prog/e/ JS-SFBSUN:
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-")


(defvar prep*objects nil)

(DEFUN PREP.Er-INITIAL.GRAPH (PROBLEM.FILE GRAPH.FILE CODE.FILE COMMENT)
						; input   names of three files and a comment for the
						;         proof protocol.
						; effect: for the axioms and theorems resp. for all
						;         splitparts specified on problem.file the
						;         initial graphs are constructed and saved
						;         on the graph.file. the raw data for the
						;         protocol is written on code.file
						; value:  a legal total.result.
						; remark: code.file and graph.file are respected to
						;         be open and remain so
  (declare (ignore code.file comment))
  (let (RESULT)
    (PREP.ER=INIT PROBLEM.FILE GRAPH.FILE)
    (unwind-protect (progn (PREP=READ.PROBLEM.FILE)
			   (SETF PREP*AXIOMS.PREFIX   (PREP=REMOVE.COMMENTS PREP*AXIOMS.PREFIX)
				 PREP*THEOREMS.PREFIX (PREP=REMOVE.COMMENTS PREP*THEOREMS.PREFIX)
				 RESULT               (PSIM-APPLY.DEFINITIONS PREP*AXIOMS.PREFIX PREP*THEOREMS.PREFIX)
				 PREP*AXIOMS.PREFIX   (CAR RESULT)
				 PREP*THEOREMS.PREFIX (SECOND RESULT)
				 RESULT               (PREP.ER=CREATE.AXIOMGRAPH PREP*AXIOMS.PREFIX))
			   (unless RESULT (SETQ RESULT (PREP.ER=CREATE.INITIAL.GRAPHS PREP*THEOREMS.PREFIX))))
      (PREP.ER=END))
    RESULT))

(DEFUN PREP.ER=INIT (PROBLEM.FILE GRAPH.FILE)
						; INPUT : TWO FILENAMES
						; REMARK: FILES ARE EXPECTED TO BE OPEN
						; EFFECT: INITIALIZES ALL NECESSARY VARIABLES
						; VALUE:  UNDEFINED
  (SETQ PREP*PROBLEM.FILE PROBLEM.FILE)
  (SETQ PREP*GRAPH.FILE GRAPH.FILE)
  (SETQ PREP*SEVERAL.SPLITPARTS NIL)
  (SETQ PREP*STEPNUMBER 0
	prep*objects nil)  
  (SETQ PREP*NUMBER.OF.AXIOM.CLAUSES 0)
  (SETQ PREP*AXIOMS.INFIX NIL)
  (SETQ PREP*AXIOMS.PREFIX NIL)
  (SETQ PREP*THEOREMS.INFIX NIL)
  (SETQ PREP*THEOREMS.PREFIX NIL)
  (SETQ PREP*COMMENT NIL))

(DEFUN PREP.ER=END NIL
						; INPUT:  NONE
						; EFFECT: ENDS ALL NECESSARY SUBMODULES
						; VALUE:  UNDEFINED
  )

(DEFUN PREP.ER=CREATE.AXIOMGRAPH (FORMULALIST)
						; EDITED:  6-MAR-84 19:46:34
						; INPUT:  A LIST OF FOPC-EXPRESSIONS PREFIX-FORM
						; EFFECT: THE FORMULAS ARE PRESIMPLIFIED AND NORMA-
						;         LIZED. THE ATTRIBUTE CLAUSES AND AXIOM
						;         CLAUSES ARE CONSTRUCTUED AND ADDED TO THE
						;         AXIOMGRAPH
						; VALUE:  A LEGAL TOTAL.RESULT
  (format prep*graph.file "(setq ectl*false ")
  (prep.er=print.term (dt-predicate.false) prep*graph.file)
  (format prep*graph.file "ectl*true ")
  (prep.er=print.term (dt-predicate.true) prep*graph.file)
  (format prep*graph.file "ectl*equality ")
  (prep.er=print.term (first (dt-predicate.equalities)) prep*graph.file)
  (format prep*graph.file "ectl*AXIOMS (list ")
  (PROG (CLAUSELIST PRESIMPLIFIED.FORMULA NORMALIZED.FORMULA RESULT)
	(SETQ CLAUSELIST
	      (SMAPCAR #'(LAMBDA (FORMULA)
			   (SETQ PRESIMPLIFIED.FORMULA (PSIM-PRESIMPLIFICATION FORMULA))
			   (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'PRESIMPLIFIED PRESIMPLIFIED.FORMULA)
			   (COND ((MEMBER PRESIMPLIFIED.FORMULA '(TRUE FALSE)) PRESIMPLIFIED.FORMULA)
				 (T (SETQ NORMALIZED.FORMULA (CAR (NORM-NORMALIZATION PRESIMPLIFIED.FORMULA NIL)))
				    (PREP=PR_PREPROCESSED.PREFIX.FORMULA 'NORMALIZED NORMALIZED.FORMULA) NORMALIZED.FORMULA)))
		       #'(LAMBDA (TAIL) (COND ((NEQ PRESIMPLIFIED.FORMULA 'FALSE) (CDR TAIL))))
		       FORMULALIST))
	(COND
	  ((MEMBER 'FALSE CLAUSELIST) (SETQ RESULT (LIST 'SUCCESS 'AXIOMS.UNSATISFIABLE))))
	(COND
	  ((NOT RESULT)
	   (SETQ CLAUSELIST (DELETE 'TRUE CLAUSELIST))
	   (SETQ CLAUSELIST (APPLY (FUNCTION NCONC) CLAUSELIST))
	   (SETQ CLAUSELIST
		 (SORT CLAUSELIST #'(LAMBDA (CLAUSE1 CLAUSE2) (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2)))))
						;    CONSTRUCTION OF THE ATTRIBUTE CLAUSES
						;    THERE IS NO CG-FIX ALLOWED UNTIL THE FIRST
						;    CALL OF PREP.ER=ADD.CLAUSE
	   (some #'(lambda (clause)
		     (when (and (opt-get.option str_finite.domain) (ds-lit.finite.domain clause))
		       (ds-finite.domain.set clause (ds-lit.finite.domain clause))))
		 clauselist)
	   ;(cons-CONSTRUCT.ATTRIBUTE.CLAUSES)
						;   ADDING THE AXIOM-CLAUSES TO THE GRAPH
	   (MAPC #'(LAMBDA (CLAUSE)
		     (prep.er=print.clause clause prep*graph.file))
		 CLAUSELIST)))
	(format prep*graph.file ")")
	(RETURN RESULT)))

(DEFUN PREP.ER=CREATE.INITIAL.GRAPHS (THEOREMS.PREFIX)
						; EDITED:  6-MAR-84 20:36:11
						; INPUT:  A LIST OF FOPC EXPRESSIONS IN PREFIXFORM
						; EFFECT: THE EXPRESSIONS ARE PREPROCESSED.IF THERE
						;         EXISTS SEVERAL SPLITPARTS THE AXIOM GRAPH IS
						;         SAVED ON FILE IF NON-EMPTY.IF THE LIST IS
						;         EMPTY OR THE NEGATED CONJUNCTION IS
						;         RECOGNIZED AS VALID THEN THE INITIAL
						;         REDUCTIONS ARE PERFORMED AND THE GRAPH IS
						;         SAVED ON THE INITIAL.GRAPH FILE.
						; VALUE:  A LEGAL TOTAL.RESULT
  (PROG (RESULT)
	(SETQ RESULT (PREP=THEOREMS.CLAUSE.LISTS THEOREMS.PREFIX))
	(COND
	  ((EQL RESULT 'FALSE)
	   (SETQ RESULT '(SUCCESS THEOREMS.VALID))
	   (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT)
	   (SETQ RESULT 'SUCCESS))
	  ((EQL RESULT 'TRUE)
	   (SETQ PREP*SEVERAL.SPLITPARTS NIL)
	   (PREP=VIRTUAL.INITIAL.GRAPH '(1) NIL RESULT)
	   (SETQ RESULT (COND (RESULT (CAR RESULT)))))
	  (T (SETQ PREP*SEVERAL.SPLITPARTS (NORM-SEVERAL.SPLITPARTS?))
	     (SETQ RESULT (PREP.ER=CREATE.SPLITGRAPHS))))
	(RETURN RESULT)))

(DEFUN PREP.ER=CREATE.SPLITGRAPHS NIL		; EDITED:  6-MAR-84 20:59:34
						; EFFECT: FOR ALL SPLITPARTS THE INITIAL GRAPHS ARE
						;         CONSTRUCTED AND SAVED ON THE INITIAL.GRAPH
						;         FILE
						; VALUE:  A LEGAL TOTAL.RESULT
  (let ((RESULT nil)
	NORMALIZED.THEOREMS)
    (format prep*graph.file "~2% ectl*THEOREMS (list ")
    (WHILE (NORM-more.SPLITPARTS?)
      (princ "(list " prep*graph.file)
      (SETQ NORMALIZED.THEOREMS (SORT (NORM-NEXT.SPLITPART)
				      #'(LAMBDA (CLAUSE1 CLAUSE2)
					  (< (LIST-LENGTH CLAUSE1) (LIST-LENGTH CLAUSE2)))))
      (MAPC #'(LAMBDA (CLAUSE)
		(prep.er=print.clause clause prep*graph.file))
	    NORMALIZED.THEOREMS)
      (princ ")" prep*graph.file))
    (format prep*graph.file "))")
    RESULT))

(defun prep.er=print.clause (clause st)
  (princ "(list " st)
  (mapc #'(lambda (lit)
	    (prep.er=print.term (if (ds-sign.is.negative (ds-lit.sign lit))
				 (list (first (dt-predicate.equalities))
				       (cons (ds-lit.predicate lit) (ds-lit.termlist lit))
				       (dt-predicate.false))
				 (cons (ds-lit.predicate lit) (ds-lit.termlist lit)))
			     st))
	clause)
  (format st ")~%"))

(defun prep.er=print.term (term st)
  (if (consp term)
      (progn (princ "(eds-term_create " st)
	     (prep.er=print.term (first term) st)
	     (princ "(list " st)
	     (mapc #'(lambda (subterm) (prep.er=print.term subterm st))
		   (rest term))
	     (princ "))" st))
      (if (numberp term)
	  (if (member term prep*objects)
	      (format st "'#~A#" term)
	      (progn (push term prep*objects)
		     (format st "'#~A=#.~A" term (cond ((dt-variable.is term)
						       (format nil "(eds-var_create ~S)" (dt-pname term)))
						      ((dt-function.is term)
						       (format nil "(eds-fct_create ~S ~A ~A)"
							       (dt-pname term) (dt-function.arity term) (ord=symbol.weight term)))
						      ((dt-constant.is term)
						       (format nil "(eds-const_create ~S ~A)" (dt-pname term)
							       (ord=symbol.weight term)))
						      ((dt-predicate.is term)
						       (format nil "(eds-fct_create ~S ~A ~A)"
							       (dt-pname term) (length (DT-PREDICATE.DOMAINSORTS term))
							       (ord=symbol.weight term)))))))
	  (print term st))))

