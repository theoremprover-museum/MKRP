;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10; Patch-File: T -*-
;;; Patch file for MKRP version 53.8
;;; Reason: Function MKRP::OP-CREATE.INSTANCE:  pr-operation before rewrite
;;; Written by mkrp, 4/28/92 19:22:26
;;; while running on JS-SFBUX2 from FEP0:>Genera-8-0-Inc-kkl-beweiser.ilod.1
;;; with Genera 8.0.2, IP-TCP 422.9, IP-TCP Documentation 404.0, CLX 419.3,
;;; X Remote Screen 418.1, X Documentation 403.0, Network RPC 415.5,
;;; NFS Client 415.3, NFS Documentation 404.0,
;;; Logical Pathnames Translation Files NEWEST,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0, Waltz 8.0, COLUMN 9.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.7, GENTRAFO 3.0,
;;; Ivory Revision 4 (FPA enabled), FEP 322, FEP0:>I322-Loaders.flod(222),
;;; FEP0:>I322-Info.flod(157), FEP0:>I322-Debug.flod(158), FEP0:>I322-Lisp.flod(145),
;;; , Boot ROM version 316, Device PROM version 322, Genera application 2.2.1.2,
;;; UX Support Server 2.2.1.1, Ivory-life program 2.2.1.4,
;;; UX kernel life support 2.2.1.4, SunOS (JS370.26APR92) 4.1.1,
;;; 1037x760 1-bit STATIC-GRAY X Screen INTERNET|134.96.236.11:0.0 with 0 Genera fonts (MIT X Consortium R5000),
;;; Machine serial number 505.

;;; Patch file for MKRP version 53.8
;;; Written by mkrp, 4/29/92 21:44:06
;;; while running on JS-SFBUX1 from FEP0:>Genera-8-0-Inc-kkl-beweiser.ilod.1
;;; with Genera 8.0.2, IP-TCP 422.9, IP-TCP Documentation 404.0, CLX 419.3,
;;; X Remote Screen 418.1, X Documentation 403.0, Network RPC 415.5,
;;; NFS Client 415.3, NFS Documentation 404.0,
;;; Logical Pathnames Translation Files NEWEST,
;;; Karlsruher Kaiserslauterner Lisp 24.0, HADES 19.0, Waltz 8.0, COLUMN 9.0,
;;; Experimental MARKGRAF KARL REFUTATION PROCEDURE 53.7, GENTRAFO 3.0,
;;; Ivory Revision 4 (FPA enabled), FEP 322, FEP0:>I322-Loaders.flod(222),
;;; FEP0:>I322-Info.flod(157), FEP0:>I322-Debug.flod(158), FEP0:>I322-Lisp.flod(145),
;;; , Boot ROM version 316, Device PROM version 322, Genera application 2.2.1.2,
;;; UX Support Server 2.2.1.1, Ivory-life program 2.2.1.4,
;;; UX kernel life support 2.2.1.4, SunOS (JS370.26APR92) 4.1.1,
;;; 1037x760 1-bit STATIC-GRAY X Screen INTERNET|134.96.236.11:0.0 with 0 Genera fonts (MIT X Consortium R5000),
;;; Machine serial number 519.




;=====================================
(SYSTEM-INTERNALS:BEGIN-PATCH-SECTION)
; From buffer operation.lisp /home1/mkrp/prog/op/ JS-SFBSUN:
(SYSTEM-INTERNALS:PATCH-SECTION-ATTRIBUTES
  "-*- Package: MKRP; Syntax: Common-lisp; Mode: LISP -*-")

(DEFUN OP-create.instance (UNIFIER clause &optional list.result)
						      ; Edited:  19-AUG-1990 15:17
						      ; Authors: PRCKLN
						      ; INPUT:  UNIFIER IS A UNIFIER OF LINK.
						      ;         AND A clause
						      ; EFFECT: THE FACTOR OF FLINK IS CREATED AND ADDED TO
						      ;         THE GRAPH. THE PREDICATE OCCURENCE LISTS ARE
						      ;         UPDATED IF THE FLAG IS T
						      ; VALUE:  NAME OF THE FACTOR (OR NIL).
  (let* ((ORIGINS (LIST NIL)) 
	 (PNAME (DS-CLAUSE.PNAME CLAUSE))
	 (DEPTH (DS-CLAUSE.DEPTH CLAUSE))
	 (REN.SUBST (DT-VARIABLE.RENAMING.SUBSTITUTION (DS-CLAUSE.VARIABLES CLAUSE)))
	 (LITLIST (OP=TRANSMIT.LITERALS CLAUSE nil UNIFIER NIL ORIGINS nil)))
    (if list.result
	(UNI-APPLY.SUBSTITUTION REN.SUBST LITLIST T)
	(let* ((pname (OP=inst_NEWNAME pname))
	       (new.litlist (UNI-APPLY.SUBSTITUTION unifier (UNI-APPLY.SUBSTITUTION ren.subst LITLIST T)))
	       (instance (DS-CLAUSE.CREATE pname (LIST CLAUSE) DEPTH new.litlist))
	       (pars (list instance)))      
	  (CG-INSERT.CLAUSE INSTANCE UNIFIER (CAR ORIGINS) "Instantiation" nil)
	  (setq origins nil)
	  (PR-OPERATION 'instantiate UNIFIER clause instance)
	  (red-rewrite instance)
	  (ds-rewrite.update instance)
	  (cond ((or (opt-is.kz.completion) (opt-get.option sort_literals))
		 (OP=POT.T.AND.F.LITNOS (list instance))
		 (cons-construct.links (list instance) nil))
		((NOT (ZEROP (DS-CLAUSE.NOLIT INSTANCE)))
		 (OP=INHERIT_POT.TRUE.AND.FALSE.LITNOS INSTANCE)
		 (OP=INHERIT.LINKS INSTANCE nil UNIFIER ren.subst ren.subst ren.subst)
		 (OP=create_P.PD.AND.PIW.LINKS instance unifier nil)
		 (OP=CONNECT.TO.PARENTS instance)))
	  pars))))

