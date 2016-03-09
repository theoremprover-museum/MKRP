;;; -*- package: MKRP; syntax: common-lisp; mode: lisp -*-

(IN-PACKAGE "MKRP" :USE '("CL"))

(DEFUN RED.LC-CLAUSE.TAUTOLOGY (CLAUSE LITNO1 LITNO2)
  ;; edited: 25-apr-84 16:14:34
  ;; input:  'litno1' and 'litno2' are numbers of two
  ;;         different literals of clause 'clause'.
  ;;         there exists a ti-link between these
  ;;         literals with identical substitution.
  ;; effect: -
  ;; value:  not nil iff link condition is fulfilled,
  ;;         i.e. each unifier in the merge of unifiers
  ;;         of a r-link1 and a r-link2 is an instance
  ;;         of a r-link.condition (names are used as
  ;;         in remark).
  ;; remark: for all 'lc=taut_'-functions the
  ;;         following conventions are made:
  ;;         otherpar1                        otherpar2
  ;;         otherlitno1                      otherlitno2
  ;;         ------        r.link.condition        ------
  ;;         !    !--------------------------------!    !
  ;;         ------                                ------
  ;;           ! r.link1                      r.link2 !
  ;;           ! unifiers1                  unifiers2 !
  ;;           !__________                  __________!
  ;;                      !                !
  ;;                   ------------------------
  ;;         clause     !    !          !    !
  ;;                   ------------------------
  ;;                    litno1          litno2
  ;;         a mark of a literal is a dotted pair
  ;;         (list_of_r-links . anything). 
  ;;         as property indicator for marks
  ;;         'red.lc*otherlitno' is used.
  (PROG
    ((CONDITION.COLOURS.EXTERNAL (RDS-LINK.COLOURS 'RESOLUTION))
     (CONDITION.COLOURS.INTERNAL (RDS-LINK.COLOURS 'RESOLUTION.INTERNAL))
     (LINK.LISTS1 (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RESOLUTION) CLAUSE LITNO1))
     (LINK.LISTS2 (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RESOLUTION) CLAUSE LITNO2)) (NOT.FAILURE T)
     OTHER.LITERAL.MARKS1 R.LINK2 LINK.BLOCKS)
    (WHILE LINK.LISTS1 (SETQ OTHER.LITERAL.MARKS1 (RED.LC=TAUT_MARK.OTHERS CLAUSE LITNO1 (CAR LINK.LISTS1)))
	   (MAPC
	     #'(LAMBDA (R.LINKS2.LISTS) (SETQ R.LINK2 (CAAR R.LINKS2.LISTS))
		       (RED.LC=TAUT_MARK.INTERSECTION (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK2 CLAUSE)
						      (RED.SERVICE-OTHERLITNO.EXTERNAL R.LINK2 CLAUSE)
						      (CAR CONDITION.COLOURS.EXTERNAL)
						      (CAR CONDITION.COLOURS.INTERNAL))
		       (SETQ LINK.BLOCKS
			     (NCONC (RED.LC=TAUT_MARK.RESET OTHER.LITERAL.MARKS1 (CAR R.LINKS2.LISTS)) LINK.BLOCKS))
		       (DS-CLAUSE.LIT.REMPROP (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK2 CLAUSE)
					      (RED.SERVICE-OTHERLITNO.EXTERNAL R.LINK2 CLAUSE) 'RED.LC*OTHERLITNO))
	     (RED.LC=TAUT_MARK.OTHERS CLAUSE LITNO2 (CAR LINK.LISTS2)))
	   (COND
	     ((NOT
		(RED.LC=TAUT_INSTANCE LINK.BLOCKS (CG-CLAUSE_CREATOR.UNIFIER CLAUSE) CLAUSE CLAUSE LITNO1 LITNO2))
	      (SETQ NOT.FAILURE NIL) (SETQ LINK.LISTS1 NIL))
	     (T (SETQ LINK.LISTS1 (CDR LINK.LISTS1)) (SETQ LINK.LISTS2 (CDR LINK.LISTS2))
		(SETQ CONDITION.COLOURS.EXTERNAL (CDR CONDITION.COLOURS.EXTERNAL))
		(SETQ CONDITION.COLOURS.INTERNAL (CDR CONDITION.COLOURS.INTERNAL))))
	   (MAPC
	     #'(LAMBDA (OTHER.LITERAL.MARK1)
		 (let ((R.LINK1 (CAAR OTHER.LITERAL.MARK1)))
		   (DS-CLAUSE.LIT.REMPROP (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK1 CLAUSE)
					  (RED.SERVICE-OTHERLITNO.EXTERNAL R.LINK1 CLAUSE) 'RED.LC*OTHERLITNO)))
	     OTHER.LITERAL.MARKS1))
    (RETURN NOT.FAILURE)))

(DEFUN RED.LC-LINK.TAUTOLOGY (CLAUSE1 LITNO1 CLAUSE2 LITNO2 REMOVE.UNIFIER)
  ;; edited:  8-jun-84 17:13:39
  ;; input:  'litnoi' is the number of a literal of
  ;;         clause 'clausei'. 'clause1' and 'clause2'
  ;;         are not necessarily different.
  ;;         bindings may be present.
  ;;         between the two literals exists a ti- or
  ;;         t-link, or a r- or rd-link with
  ;;         r-iff-t-rule (there 'exists' also a not
  ;;         implemented t-link).
  ;;         this link has the identical substitution
  ;;         under the current binding.
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed for bridges.
  ;; effect: -
  ;; value:  not nil iff link condition is fulfilled,
  ;;         i.e. each unifier in the merge of unifiers
  ;;         of a r-link1 and a r-link2 is an instance
  ;;         of an unifier of a r-link.condition (names
  ;;         are used as in remark).
  ;; remark: for all link condition functions the
  ;;         following conventions are made:
  ;;         otherpar1                          otherpar2
  ;;         otherlitno1                      otherlitno2
  ;;           -------     r.link.condition     -------
  ;;         ..!     !--------------------------!     !..
  ;;           -------                          -------
  ;;              !                                !
  ;;              !_r.link1               r.link2 _!
  ;;                !                            !
  ;;             -------                      -------
  ;;           ..!     !..                  ..!     !..
  ;;             -------litno1          litno2-------
  ;;                    literal 1    literal 2
  ;;                    clause1        clause2
  ;;         the links called r-links may also be
  ;;         riw-links.
  ;;         a mark of a literal is a dotted pair
  ;;         (list_of_r-_or_riw-links . anything).
  ;;         as property indicator for marks
  ;;         'red.lc*otherlitno' is used.
  (PROG
    ((CONDITION.COLOURS.EXTERNAL (RDS-LINK.COLOURS 'RESOLUTION))
     (CONDITION.COLOURS.INTERNAL (RDS-LINK.COLOURS 'RESOLUTION.INTERNAL))
     (LINK.LISTS1 (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RESOLUTION) CLAUSE1 LITNO1))
     (LINK.LISTSI1 (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RESOLUTION.INTERNAL) CLAUSE1 LITNO1))
     (NOT.FAILURE T)
     (LINK.LISTS2 (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RESOLUTION) CLAUSE2 LITNO2))
     (LINK.LISTSI2 (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'RESOLUTION.INTERNAL) CLAUSE2 LITNO2))
     OTHER.LITERAL.MARKS1 R.LINKS1 R.LINK2 LINK.BLOCKS)
    (WHILE LINK.LISTS1                          ; lists are used for possibility of using more colours
						; than r for link condition.
      (SETQ R.LINKS1 (NCONC (COPY-TREE (CAR LINK.LISTSI1)) (CAR LINK.LISTS1)))
      (SETQ OTHER.LITERAL.MARKS1 (RED.LC=TAUT_MARK.OTHERS CLAUSE1 LITNO1 R.LINKS1))
      (MAPC #'(LAMBDA (R.LINKS2.LISTS)
		(SETQ R.LINK2 (CAAR R.LINKS2.LISTS))	; building lists of triples (r-links1
						; r-links.condition r-links2) so that all links
						; between the corresponding literals are collected
						; in these blocks.
		(RED.LC=TAUT_MARK.INTERSECTION (DS-LINK.OTHERPAR R.LINK2 CLAUSE2)
					       (DS-LINK.OTHERLITNO R.LINK2 CLAUSE2 LITNO2) (CAR CONDITION.COLOURS.EXTERNAL)
					       (CAR CONDITION.COLOURS.INTERNAL))
		;; all marks of other literals of all r- and riw-links
		;; of literal 1 are reset to nil for collecting the
		;; next triples.
		(SETQ LINK.BLOCKS
		      (NCONC (RED.LC=TAUT_MARK.RESET OTHER.LITERAL.MARKS1 (CAR R.LINKS2.LISTS)) LINK.BLOCKS))
		(DS-CLAUSE.LIT.REMPROP (DS-LINK.OTHERPAR R.LINK2 CLAUSE2) (DS-LINK.OTHERLITNO R.LINK2 CLAUSE2 LITNO2)
				       'RED.LC*OTHERLITNO))
	    (RED.LC=TAUT_MARK.OTHERS CLAUSE2 LITNO2 (NCONC (COPY-TREE (CAR LINK.LISTSI2)) (CAR LINK.LISTS2))))
      (COND
        ((NOT (RED.LC=TAUT_INSTANCE LINK.BLOCKS REMOVE.UNIFIER CLAUSE1 CLAUSE2 LITNO1 LITNO2)) (SETQ NOT.FAILURE NIL)
	 (SETQ LINK.LISTS1 NIL))
        (T (SETQ LINK.LISTS1 (CDR LINK.LISTS1)) (SETQ LINK.LISTS2 (CDR LINK.LISTS2)) (SETQ LINK.LISTSI1 (CDR LINK.LISTSI1))
	   (SETQ LINK.LISTSI2 (CDR LINK.LISTSI2)) (SETQ CONDITION.COLOURS.EXTERNAL (CDR CONDITION.COLOURS.EXTERNAL))
	   (SETQ CONDITION.COLOURS.INTERNAL (CDR CONDITION.COLOURS.INTERNAL))))
      (MAPC
        #'(LAMBDA (R.LINK1)
            (DS-CLAUSE.LIT.REMPROP (DS-LINK.OTHERPAR R.LINK1 CLAUSE1) (DS-LINK.OTHERLITNO R.LINK1 CLAUSE1 LITNO1)
				   'RED.LC*OTHERLITNO))
        R.LINKS1))
    (RETURN NOT.FAILURE)))

(DEFUN RED.LC=TAUT_MARK.OTHERS (CLAUSE1 LITNO R.LINKS1)
  ;; edited: 25-apr-84 16:43:20
  ;; input:  'litno' is the number of a literal of
  ;;         clause 'clause1'.
  ;; effect: a property
  ;;         'red.lc*otherlitno' with
  ;;         value dotted pair (list_of_r-links . nil) is
  ;;         inserted into propertylist of the other
  ;;         parentliterals of all r-links of literal
  ;;         denoted by 'litno'.
  ;;         r-links list contains all r-links between
  ;;         the two literals.
  ;; value:  list of these dotted pairs.
  ;; remark: list is for a quick access on the
  ;;         properties. coming from another literal or
  ;;         link to the literal, access on the normal
  ;;         way(getprop) is necessary.
  (PROG ((OTHER.LITERAL.MARKS1 NIL) OTHER.LITERAL.MARK1 OTHERPAR1 OTHERLITNO1)
    (MAPC
      #'(LAMBDA (R.LINK1) (SETQ OTHERPAR1 (DS-LINK.OTHERPAR R.LINK1 CLAUSE1))
          (SETQ OTHERLITNO1 (DS-LINK.OTHERLITNO R.LINK1 CLAUSE1 LITNO))
          (COND
            ((SETQ OTHER.LITERAL.MARK1 (DS-CLAUSE.LIT.GETPROP OTHERPAR1 OTHERLITNO1 'RED.LC*OTHERLITNO))
              (RPLACA OTHER.LITERAL.MARK1 (CONS R.LINK1 (CAR OTHER.LITERAL.MARK1))))
            (T (SETQ OTHER.LITERAL.MARKS1 (CONS (CONS (LIST R.LINK1) NIL) OTHER.LITERAL.MARKS1))
              (DS-CLAUSE.LIT.PUTPROP OTHERPAR1 OTHERLITNO1 'RED.LC*OTHERLITNO (CAR OTHER.LITERAL.MARKS1)))))
      R.LINKS1)
    (RETURN OTHER.LITERAL.MARKS1)))

(DEFUN RED.LC=TAUT_MARK.RESET (OTHER.LITERAL.MARKS1 R.LINKS2)
  ;; edited: 25-apr-84 16:54:02
  ;; input:  'other.literal.marks1' is a list of dotted
  ;;         pairs (list_of_r-links1 .
  ;;         list_of_r-links.condition). it is
  ;;         the list of all properties
  ;;         'red.lc*otherlitno' of the
  ;;         other parent literals of r-links of literal
  ;;         1.
  ;; effect: all 'cdr's of 'other.literal.marks1' are set
  ;;         to nil.
  ;; value:  list of triples (r-links1
  ;;         r-links.condition r-links2), r-linksi are
  ;;         all r-links between a literal i and an
  ;;         other literal, r-links.condition are
  ;;         all bridge links between these other
  ;;         literals.
  (MAPCAR
    #'(LAMBDA (OTHER.LITERAL.MARK1)
        (PROG1 (LIST (CAR OTHER.LITERAL.MARK1) (CDR OTHER.LITERAL.MARK1) R.LINKS2) (RPLACD OTHER.LITERAL.MARK1 NIL)))
    OTHER.LITERAL.MARKS1))

(DEFUN RED.LC=TAUT_MARK.INTERSECTION (OTHERPAR2 OTHERLITNO2 COLOUR.EXTERNAL COLOUR.INTERNAL)
  ;; edited: 25-apr-84 16:58:08
  ;; input:  'otherlitno2' is the number of a literal of
  ;;         clause 'otherpar2'.
  ;;         this is the other parent literal of one
  ;;         r-link incident with literal 'litno2' in
  ;;         clause 'clause2'.
  ;; effect: if the other parentliteral of a r- or riw-
  ;;         link of literal 'otherlitno2' has the
  ;;         property 'red.lc*otherlitno'
  ;;         it is an other_literal_1. in the 'cdr' of
  ;;         the mark of this other_literal_1 all bridge
  ;;         links are collected incident with the
  ;;         actual other_literal_2 and this
  ;;         other_literal_1.
  ;; value:  undefined.
  (PROG (OTHER.LITERAL.MARK1)
    (MAPC
      #'(LAMBDA (R.LINK.CONDITION)
          (COND
            ((SETQ OTHER.LITERAL.MARK1
               (DS-CLAUSE.LIT.GETPROP (RED.SERVICE-OTHERPAR.EXTERNAL R.LINK.CONDITION OTHERPAR2)
                 (RED.SERVICE-OTHERLITNO.EXTERNAL R.LINK.CONDITION OTHERPAR2) 'RED.LC*OTHERLITNO))
              (RPLACD OTHER.LITERAL.MARK1 (CONS R.LINK.CONDITION (CDR OTHER.LITERAL.MARK1))))))
      (DS-CLAUSE.LINKS COLOUR.EXTERNAL OTHERPAR2 OTHERLITNO2))
    (MAPC
      #'(LAMBDA (R.LINK.CONDITION)
          (COND
            ((SETQ OTHER.LITERAL.MARK1
               (DS-CLAUSE.LIT.GETPROP OTHERPAR2 (RED.SERVICE-OTHERLITNO.INTERNAL R.LINK.CONDITION OTHERLITNO2)
                 'RED.LC*OTHERLITNO))
              (RPLACD OTHER.LITERAL.MARK1 (CONS R.LINK.CONDITION (CDR OTHER.LITERAL.MARK1))))))
      (DS-CLAUSE.LINKS COLOUR.INTERNAL OTHERPAR2 OTHERLITNO2))))

(DEFUN RED.LC=TAUT_INSTANCE (LINK.BLOCKS REMOVE.UNIFIER CLAUSE1 CLAUSE2 LITNO1 LITNO2)
  ;; edited: 31-jul-84 14:33:17
  ;; input:  'litnoi' is the number of a literal of
  ;;         clause 'clausei' (i = 1, 2).
  ;;         'clause1' and 'clause2' can be equal.
  ;;         'link.blocks' is a list of triples
  ;;         (r-links1 r-links.condition r_links2),
  ;;         r-linksi are all r- or riw-links between
  ;;         literal 'litnoi' of clause 'clausei' and
  ;;         another literal in the graph. 
  ;;         'r-links.condition' are all r- or riw-links
  ;;         between these other literals. 
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed for bridges.
  ;; effect: -
  ;; value:  not nil iff each merge unifier of r-links1
  ;;         with r-links2 is an instance of an unifier
  ;;         of r-links.condition.
  (EVERY
    #'(LAMBDA (LINK.BLOCK)
        (let* ((CARRIER.LINKS1 (CAR LINK.BLOCK))
	      (BRIDGE.LINKS (SECOND LINK.BLOCK))
	      (CARRIER.LINKS2 (THIRD LINK.BLOCK))
	      (NEW.CLAUSE1 CLAUSE1)
	      (NEW.CLAUSE2 CLAUSE2)
	      (NEW.LITNO1 LITNO1)
	      (NEW.LITNO2 LITNO2)
	      (COLOUR.CARRIER1 (DS-LINK.COLOUR (CAR CARRIER.LINKS1)))
	      (COLOUR.BRIDGE (DS-LINK.COLOUR (CAR BRIDGE.LINKS)))
	      (COLOUR.CARRIER2 (DS-LINK.COLOUR (CAR CARRIER.LINKS2)))
	      OTHER.CLAUSE1 OTHER.CLAUSE2 OTHER.LITNO1 OTHER.LITNO2
	      RENAMING.OTHER.CLAUSE1 RENAMING.OTHER.CLAUSE2)
          (COND
            ((MEMBER COLOUR.CARRIER1 (RDS-LINK.COLOURS 'RESOLUTION))
	     (let ((HELP COLOUR.CARRIER1))
	       (SETQ COLOUR.CARRIER1 COLOUR.CARRIER2)
	       (SETQ COLOUR.CARRIER2 HELP)
	       (SETQ HELP NEW.CLAUSE1)
	       (SETQ NEW.CLAUSE1 NEW.CLAUSE2)
	       (SETQ NEW.CLAUSE2 HELP)
	       (SETQ HELP NEW.LITNO1)
	       (SETQ NEW.LITNO1 NEW.LITNO2)
	       (SETQ NEW.LITNO2 HELP)
	       (SETQ HELP CARRIER.LINKS1)
	       (SETQ CARRIER.LINKS1 CARRIER.LINKS2)
	       (SETQ CARRIER.LINKS2 HELP))))
          (SETQ OTHER.CLAUSE1 (DS-LINK.OTHERPAR (CAR CARRIER.LINKS1) NEW.CLAUSE1))
          (SETQ OTHER.CLAUSE2 (DS-LINK.OTHERPAR (CAR CARRIER.LINKS2) NEW.CLAUSE2))
          (SETQ OTHER.LITNO1 (DS-LINK.OTHERLITNO (CAR CARRIER.LINKS1) NEW.CLAUSE1 NEW.LITNO1))
          (SETQ OTHER.LITNO2 (DS-LINK.OTHERLITNO (CAR CARRIER.LINKS2) NEW.CLAUSE2 NEW.LITNO2))
          (SETQ RENAMING.OTHER.CLAUSE1 (DS-CLAUSE.RENAMING OTHER.CLAUSE1))
          (SETQ RENAMING.OTHER.CLAUSE2 (DS-CLAUSE.RENAMING OTHER.CLAUSE2))
          (COND ((MEMBER COLOUR.BRIDGE (RDS-LINK.COLOURS 'RESOLUTION))
		 ;; the cases are explained in the functions called in
		 ;; this 'cond'.
		 (COND ((AND (MEMBER COLOUR.CARRIER1 (RDS-LINK.COLOURS 'RESOLUTION))
			     (MEMBER COLOUR.CARRIER2 (RDS-LINK.COLOURS 'RESOLUTION)))
			(COND
			  ((OR (EQL OTHER.CLAUSE1 CLAUSE2) (EQL OTHER.CLAUSE2 CLAUSE1))
			   (RED.LC=TAUT_INSTANCE.C.C.C.RENAME CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 NEW.CLAUSE1
							      NEW.CLAUSE2 OTHER.LITNO1 OTHER.LITNO2 RENAMING.OTHER.CLAUSE2
							      RENAMING.OTHER.CLAUSE1 REMOVE.UNIFIER))
			  (T (RED.LC=TAUT_INSTANCE.C.C.C CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 OTHER.CLAUSE1
							 OTHER.CLAUSE2 OTHER.LITNO1 OTHER.LITNO2 REMOVE.UNIFIER))))
		       ((MEMBER COLOUR.CARRIER2 (RDS-LINK.COLOURS 'RESOLUTION))
			(RED.LC=TAUT_INSTANCE.CIW.C.C CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 NEW.CLAUSE1
						      NEW.LITNO1 OTHER.CLAUSE2 OTHER.LITNO1 OTHER.LITNO2 RENAMING.OTHER.CLAUSE1
						      RENAMING.OTHER.CLAUSE2 REMOVE.UNIFIER))
		       (T (RED.LC=TAUT_INSTANCE.CIW.C.CIW CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 NEW.CLAUSE1 NEW.CLAUSE2
							  NEW.LITNO1 NEW.LITNO2 OTHER.LITNO1 OTHER.LITNO2 RENAMING.OTHER.CLAUSE1
							  RENAMING.OTHER.CLAUSE2 REMOVE.UNIFIER))))
		((AND (MEMBER COLOUR.CARRIER1 (RDS-LINK.COLOURS 'RESOLUTION))
		      (MEMBER COLOUR.CARRIER2 (RDS-LINK.COLOURS 'RESOLUTION)))
		 (RED.LC=TAUT_INSTANCE.C.CIW.C CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 OTHER.CLAUSE1
					       OTHER.LITNO1 OTHER.LITNO2 RENAMING.OTHER.CLAUSE1))
		((NOT (OR (MEMBER COLOUR.CARRIER1 (RDS-LINK.COLOURS 'RESOLUTION))
			  (MEMBER COLOUR.CARRIER2 (RDS-LINK.COLOURS 'RESOLUTION))))
		 (RED.LC=TAUT_INSTANCE.CIW.CIW.CIW CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 NEW.CLAUSE1
						   NEW.LITNO1 NEW.LITNO2 OTHER.LITNO1 OTHER.LITNO2 RENAMING.OTHER.CLAUSE1))
		(T (RED.LC=TAUT_INSTANCE.CIW.CIW.C CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 NEW.CLAUSE1
						   NEW.LITNO1 OTHER.LITNO1 OTHER.LITNO2 RENAMING.OTHER.CLAUSE1)))))
    LINK.BLOCKS))

(DEFUN RED.LC=TAUT_INSTANCE.C.C.C
  (CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 OTHER.CLAUSE1 OTHER.CLAUSE2 OTHER.LITNO1 OTHER.LITNO2
    REMOVE.UNIFIER)
  ;; edited: 26-jul-84 14:17:47
  ;; input:  illustration of the situation:
  ;;
  ;;         other.clause1                  other.clause2
  ;;                         bridge.links
  ;;                 ____________________________
  ;;                !                            !
  ;;         --------------                --------------
  ;;         ! .. !* ! .. !                ! .. !**! .. !
  ;;         --------------                --------------
  ;;               !                              !
  ;;               !carrier.-            carrier.-!
  ;;               !   links1            links2   !
  ;;               !                              !
  ;;               !                              !
  ;;         -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
  ;;         one clause being a tautology
  ;;         or one or two clauses incident with a link
  ;;         with tautologic unifier (unifier is the
  ;;         current binding).
  ;;         carrier.links1 and carrier.links2 are
  ;;         incident with parent literals of the
  ;;         tautology detection link in this clauses.
  ;;
  ;;         *     other.litno1
  ;;         **    other.litno2
  ;;
  ;;         the carrier.links1, bridge.links,
  ;;         carrier.links2 are all same coloured links
  ;;         between the corresponding literals.
  ;; effect: -
  ;; value:  not nil iff all merges of unifiers of
  ;;         'carrier.links1' and 'carrier.links2'
  ;;         are instances of unifiers of 'bridge.links'.
  ;;         'remove.unifier' are regarded as removed
  ;;         in the unifier lists of 'bridge.links'.
  ;;         instance check is restricted to variables
  ;;         of the other clauses appearing outside
  ;;         of 'otherlitno1' and 'otherlitno2' of the
  ;;         corresponding clauses.
  (RED.LC=TAUT_INSTANCE.TEST
    (UNION (RED.LC=TAUT_VARIABLES OTHER.CLAUSE1 OTHER.LITNO1 NIL)
	   (RED.LC=TAUT_VARIABLES OTHER.CLAUSE2 OTHER.LITNO2 NIL))
    (RED.LC=TAUT_UNIFIERS.STANDARD CARRIER.LINKS1 NIL)
    (RED.LC=TAUT_UNIFIERS.STANDARD BRIDGE.LINKS REMOVE.UNIFIER)
    (RED.LC=TAUT_UNIFIERS.STANDARD CARRIER.LINKS2 NIL)))

(DEFUN RED.LC=TAUT_INSTANCE.C.C.C.RENAME
  (CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 OTHER.CLAUSE2 OTHER.CLAUSE1 OTHER.LITNO1 OTHER.LITNO2
    RENAMING.OTHER.CLAUSE2 RENAMING.OTHER.CLAUSE1 REMOVE.UNIFIER)
  ;; edited: 26-jul-84 13:22:22
  ;; input:  illustration of the situation:
  ;;              ____________________________
  ;;             !carrier.links2              !
  ;;             !                            !
  ;;             !      ______________________!____
  ;;             !     !carrier.links1        !    !
  ;;             !     !                      !    !
  ;;             !     ! tautology detection  !    !
  ;;             !     ! ________link________ !    !
  ;;             !     !!                    !!    !
  ;;         ------------------        ------------------
  ;;         !..!**!..!l1!..! !--------! !..!l2!..!* !..!
  ;;         ------------------  link  ------------------
  ;;             !          with tautologic         !
  ;;             !             unifier,             !
  ;;             !           which is the           !
  ;;             !          current binding         !
  ;;             !__________________________________!
  ;;                         bridge links
  ;;         other.clause2             other.clause1 with
  ;;         with                  renaming.other.clause1
  ;;         renaming.other.clause2
  ;;
  ;;                                           ----------
  ;;                            other.clause2  !..!**!..!
  ;;                                           ----------
  ;;                                               !
  ;;                            carrier.links2     !
  ;;                            bridge.links       !
  ;;                                               !
  ;;         -------------   tautology    ---------------
  ;;         !..!l1!..!  !----------------!  !..!l2,*!..!
  ;;         -------------                ---------------
  ;;             !!_______detection______________!!
  ;;             !________________________________!
  ;;                      carrier.links1
  ;;                                       other.clause1
  ;;
  ;;         l1      litno1
  ;;         l2      litno2
  ;;         *       other.litno1
  ;;         **      other.litno2
  ;;
  ;;         the carrier.links1, bridge.links,
  ;;         carrier.links2 are all same coloured links
  ;;         between the corrresponding literals.
  ;;
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed for bridges.
  ;; effect: -
  ;; value:  not nil iff all merges of unifiers of
  ;;         'carrier.links1' and 'carrier.links2'
  ;;         are instances of unifiers of 'bridge.links'.
  ;;         'remove.unifier' are regarded as removed
  ;;         in the unifier lists of 'bridge.links'.
  ;;         the two clauses must be regarded as the
  ;;         result of operation on the tautologic
  ;;         unifier and as the ancestor clauses of this
  ;;         operation.
  ;;         at the ancestors site the variables must be
  ;;         renamed, because no binding has to be
  ;;         considered there.
  ;;         instance check is restricted to variables
  ;;         of the other clauses (renamed)
  ;;         appearing outside
  ;;         of 'otherlitno1' and 'otherlitno2' of the
  ;;         corresponding clauses regarded as ancestors.
  (RED.LC=TAUT_INSTANCE.TEST
    (UNION (RED.LC=TAUT_VARIABLES OTHER.CLAUSE2 OTHER.LITNO2 RENAMING.OTHER.CLAUSE2)
	   (RED.LC=TAUT_VARIABLES OTHER.CLAUSE1 OTHER.LITNO1 RENAMING.OTHER.CLAUSE1))
    (RED.LC=TAUT_UNIFIERS.RENAME CARRIER.LINKS1 RENAMING.OTHER.CLAUSE1 NIL)
    (RED.LC=TAUT_UNIFIERS.RENAME BRIDGE.LINKS (APPEND RENAMING.OTHER.CLAUSE2 RENAMING.OTHER.CLAUSE1)
      REMOVE.UNIFIER)
    (RED.LC=TAUT_UNIFIERS.RENAME CARRIER.LINKS2 RENAMING.OTHER.CLAUSE2 NIL)))

(DEFUN RED.LC=TAUT_INSTANCE.CIW.C.C
       (CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 CLAUSE1 LITNO1 OTHER.CLAUSE2 OTHER.LITNO1 OTHER.LITNO2
	RENAMING.CLAUSE1 RENAMING.OTHER.CLAUSE2 REMOVE.UNIFIER)
  ;; edited: 10-aug-84 03:20:00
  ;; input:  illustration of the situation:
  ;;
  ;;         other.clause2 with renaming.other.clause2
  ;;         ----------
  ;;         !..!**!..!
  ;;         ----------            carrier.links2
  ;;             !!___________________________
  ;;             !                            !
  ;;         bridge.-                         !
  ;;          links                           !
  ;;             !       tautology detection  !
  ;;             !       ________link________ !
  ;;             !      !                    !!
  ;;         ------------------        ------------
  ;;         !..!* !..!l1!..! !--------! !..!  !..!
  ;;         ------------------  link  ------------
  ;;             !     !    with tautologic
  ;;             !     !    unifier, which is the
  ;;             !     !    current binding
  ;;             !     !
  ;;             !_____!
  ;;         carrier.links1
  ;;
  ;;         clause1 with 
  ;;         renaming.clause1
  ;;
  ;;         l1      litno1
  ;;         *       other.litno1
  ;;         **      other.litno2
  ;;
  ;;         the carrier.links1, bridge.links,
  ;;         carrier.links2 are all same coloured links
  ;;         between the corrresponding literals.
  ;;
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed for bridges.
  ;; effect: -
  ;; value:  not nil iff all merges of unifiers of
  ;;         'carrier.links1' and 'carrier.links2'
  ;;         are instances of unifiers of 'bridge.links'.
  ;;         'remove.unifier' are regarded as removed
  ;;         in the unifier lists of 'bridge.links'.
  ;;         clause 'clause1' must be regarded as a
  ;;         part of the result of operation on the
  ;;         tautologic unifier and as one of the
  ;;         ancestor clauses of this operation.
  ;;         at the ancestors site the variables must be
  ;;         renamed, because no binding has to be
  ;;         considered there.
  ;;         instance check is restricted to variables
  ;;         of the other clauses (renamed)
  ;;         appearing outside
  ;;         of the other literals.
  (RED.LC=TAUT_INSTANCE.TEST
    (UNION (RED.LC=TAUT_VARIABLES CLAUSE1 OTHER.LITNO1 RENAMING.CLAUSE1)
	   (RED.LC=TAUT_VARIABLES OTHER.CLAUSE2 OTHER.LITNO2 RENAMING.OTHER.CLAUSE2))
    (RED.LC=TAUT_UNIFIERS.SWITCH CARRIER.LINKS1 CLAUSE1 LITNO1)
    (RED.LC=TAUT_UNIFIERS.RENAME BRIDGE.LINKS (APPEND RENAMING.OTHER.CLAUSE2 RENAMING.CLAUSE1) REMOVE.UNIFIER)
    (RED.LC=TAUT_UNIFIERS.RENAME CARRIER.LINKS2 RENAMING.OTHER.CLAUSE2 REMOVE.UNIFIER)))

(DEFUN RED.LC=TAUT_INSTANCE.CIW.C.CIW
  (CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 CLAUSE1 CLAUSE2 LITNO1 LITNO2 OTHER.LITNO1 OTHER.LITNO2
    RENAMING.CLAUSE1 RENAMING.CLAUSE2 REMOVE.UNIFIER)
  ;; edited: 10-aug-84 03:25:00
  ;; input:  illustration of the situation:
  ;;
  ;;          carrier.links1              carrier.links2
  ;;              _____                        ____
  ;;             !     !                      !    !
  ;;             !     !                      !    !
  ;;             !     ! tautology detection  !    !
  ;;             !     ! ________link________ !    !
  ;;             !     !!                    !!    !
  ;;         ------------------        ------------------
  ;;         !..!* !..!l1!..! !--------! !..!l2!..!**!..!
  ;;         ------------------  link  ------------------
  ;;             !          with tautologic         !
  ;;             !             unifier,             !
  ;;             !           which is the           !
  ;;             !          current binding         !
  ;;             !__________________________________!
  ;;                         bridge links
  ;;         clause1                              clause2
  ;;         with renaming.clause1  with renaming.clause2
  ;;
  ;;         l1      litno1
  ;;         l2      litno2
  ;;         *       other.litno1
  ;;         **      other.litno2
  ;;
  ;;         the carrier.links1, bridge.links,
  ;;         carrier.links2 are all same coloured links
  ;;         between the corrresponding literals.
  ;; effect: -
  ;; value:  not nil iff all merges of unifiers of
  ;;         'carrier.links1' and 'carrier.links2'
  ;;         are instances of unifiers of 'bridge.links'.
  ;;         'remove.unifier' are regarded as removed
  ;;         in the unifier lists of 'bridge.links'.
  ;;         the two clauses must be regarded as the
  ;;         result of operation on the tautologic
  ;;         unifier and as the ancestor clauses of this
  ;;         operation.
  ;;         at the ancestors site the variables must be
  ;;         renamed, because no binding has to be
  ;;         considered there.
  ;;         instance check is restricted to variables
  ;;         of the other clauses (renamed)
  ;;         appearing outside
  ;;         of 'otherlitno1' and 'otherlitno2' of the
  ;;         corresponding clauses regarded as ancestors.
  (RED.LC=TAUT_INSTANCE.TEST
    (UNION (RED.LC=TAUT_VARIABLES CLAUSE1 OTHER.LITNO1 RENAMING.CLAUSE1)
	   (RED.LC=TAUT_VARIABLES CLAUSE2 OTHER.LITNO2 RENAMING.CLAUSE2))
    (RED.LC=TAUT_UNIFIERS.SWITCH CARRIER.LINKS1 CLAUSE1 LITNO1)
    (RED.LC=TAUT_UNIFIERS.RENAME BRIDGE.LINKS (APPEND RENAMING.CLAUSE1 RENAMING.CLAUSE2) REMOVE.UNIFIER)
    (RED.LC=TAUT_UNIFIERS.SWITCH CARRIER.LINKS2 CLAUSE2 LITNO2)))

(DEFUN RED.LC=TAUT_INSTANCE.C.CIW.C
  (CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 OTHER.CLAUSE OTHER.LITNO1 OTHER.LITNO2 RENAMING.OTHER.CLAUSE)
  ;; edited: 26-jul-84 14:11:45
  ;; input:  illustration of the situation:
  ;;
  ;;               ________ bridge.links
  ;;              !        !
  ;;              !        !
  ;;         -------------------- other.clause
  ;;         ! ..!* ! .. !**!.. ! with
  ;;         -------------------- renaming.other.clause
  ;;              !        !
  ;;              !        !
  ;;              !        !
  ;;         carrier.-  carrier.-
  ;;          links1      links2
  ;;              !        !
  ;;              !        !
  ;;         -_-_-_-_-_-_-_-_-_-_
  ;;         one clause being a tautology
  ;;         or one or two clauses incident with a link
  ;;         with tautologic unifier (unifier is the
  ;;         current binding).
  ;;         carrier.links1 and carrier.links2 are
  ;;         incident with parent literals of the
  ;;         tautology detection link in this clauses.
  ;;
  ;;         *     other.litno1
  ;;         **    other.litno2
  ;;
  ;;         the carrier.links1, bridge.links,
  ;;         carrier.links2 are all same coloured links
  ;;         between the corresponding literals.
  ;; effect: -
  ;; value:  not nil iff all merges of unifiers of
  ;;         'carrier.links1' and 'carrier.links2'
  ;;         are instances of unifiers of 'bridge.links'.
  ;;         in one of carrier unifier lists the
  ;;         variables must be renamed according to the
  ;;         riw-bridge-links.
  ;;         instance check is restricted to variables
  ;;         of the other clauses appearing outside
  ;;         of the other literals.
  (RED.LC=TAUT_INSTANCE.TEST
    (UNION (RED.LC=TAUT_VARIABLES OTHER.CLAUSE OTHER.LITNO1 RENAMING.OTHER.CLAUSE)
      (RED.LC=TAUT_VARIABLES OTHER.CLAUSE OTHER.LITNO2 NIL))
    (RED.LC=TAUT_UNIFIERS.RENAME CARRIER.LINKS1 RENAMING.OTHER.CLAUSE NIL)
    (RED.LC=TAUT_UNIFIERS.SWITCH BRIDGE.LINKS OTHER.CLAUSE OTHER.LITNO2)
    (RED.LC=TAUT_UNIFIERS.STANDARD CARRIER.LINKS2 NIL)))

(DEFUN RED.LC=TAUT_INSTANCE.CIW.CIW.C
  (CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 CLAUSE1 LITNO1 OTHER.LITNO1 OTHER.LITNO2 RENAMING.CLAUSE1)
  ;; edited: 10-aug-84 03:27:00
  ;; input:  illustration of the situation:
  ;;
  ;;                                      carrier.links2
  ;;              __________________________________
  ;;             !                                  !
  ;;             !                                  !
  ;;             !             tautology detection  !
  ;;             !             ________link________ !
  ;;             !            !                    !!
  ;;         ------------------------        ------------
  ;;         !..!**!..!* !..!l1!..! !--------! !..!  !..!
  ;;         ------------------------  link  ------------
  ;;             !     !!     !   with tautologic
  ;;             !     !!     !      unifier,
  ;;             !     !!     !    which is the
  ;;             !     !!     !   current binding
  ;;             !_____!!_____!
  ;;         bridge.-      carrier.-
  ;;            links         links1
  ;;
  ;;         clause1
  ;;         with renaming.clause1
  ;;
  ;;         l1      litno1
  ;;         *       other.litno1
  ;;         **      other.litno2
  ;;
  ;;         the carrier.links1, bridge.links,
  ;;         carrier.links2 are all same coloured links
  ;;         between the corrresponding literals.
  ;; effect: -
  ;; value:  not nil iff all merges of unifiers of
  ;;         'carrier.links1' and 'carrier.links2'
  ;;         are instances of unifiers of 'bridge.links'.
  ;;         clause 'clause1' must be regarded as a part
  ;;         of the result of operation on the
  ;;         tautologic unifier and as one of the
  ;;         ancestor clauses of this operation.
  ;;         at the ancestors site the variables must be
  ;;         renamed, because no binding has to be
  ;;         considered there.
  ;;         instance check is restricted to variables
  ;;         of the other clauses appearing outside
  ;;         of the other literals.
  (RED.LC=TAUT_INSTANCE.TEST
    (UNION (RED.LC=TAUT_VARIABLES CLAUSE1 OTHER.LITNO2 NIL)
	   (RED.LC=TAUT_VARIABLES CLAUSE1 OTHER.LITNO1 RENAMING.CLAUSE1))
    (RED.LC=TAUT_UNIFIERS.SWITCH CARRIER.LINKS1 CLAUSE1 LITNO1)
    (RED.LC=TAUT_UNIFIERS.SWITCH BRIDGE.LINKS CLAUSE1 OTHER.LITNO2)
    (RED.LC=TAUT_UNIFIERS.STANDARD CARRIER.LINKS2 NIL)))

(DEFUN RED.LC=TAUT_INSTANCE.CIW.CIW.CIW
  (CARRIER.LINKS1 BRIDGE.LINKS CARRIER.LINKS2 CLAUSE LITNO1 LITNO2 OTHER.LITNO1 OTHER.LITNO2 RENAMING.CLAUSE)
  ;; edited: 26-jul-84 14:01:43
  ;; input:  illustration of the situation:
  ;;
  ;;               link with tautologic unifier,
  ;;               which is the current binding.
  ;;                     ___________________
  ;;                    !                   !
  ;;         -------------------------------------------
  ;;         ! ..!* !..! !..!l1! ... !l2!..! !..!**!.. !
  ;;         -------------------------------------------
  ;;              !!         !!_______!!         !!
  ;;              !!         !tautology!         !!
  ;;              !!         !detection!         !!
  ;;              !!         !  link   !         !!
  ;;              !!_________!         !_________!!
  ;;              !carrier.links1   carrier.links2!
  ;;              !                               !
  ;;              !_______________________________!
  ;;                        bridge.links
  ;;
  ;;         clause with renaming.clause
  ;;
  ;;         l1      litno1
  ;;         l2      litno2
  ;;         *       other.litno1
  ;;         **      other.litno2
  ;;
  ;;         the carrier.links1, bridge.links,
  ;;         carrier.links2 are all same coloured link
  ;;         between the corresponding literals.
  ;; effect: -
  ;; value:  not nil iff all merges of unifiers of
  ;;         'carrier.links1' and 'carrier.links2'
  ;;         are instances of unifiers of 'bridge.links'.
  ;;         clause 'clause' must be regarded as
  ;;         the result of factoring on the
  ;;         tautologic unifier and as the 
  ;;         ancestor clause of this operation.
  ;;         at the ancestors site the variables must be
  ;;         renamed, because no binding has to be
  ;;         considered there.
  ;;         instance check is restricted to variables
  ;;         of the other clauses (renamed, duplicated)
  ;;         appearing outside
  ;;         of the other literals.
  (PROG (ANOTHER.RENAMING.CLAUSE RENAME.THIS.OTHER.RENAMING.CLAUSE)
    (SETQ ANOTHER.RENAMING.CLAUSE
      (RED.SERVICE-RENAME (COPY-TREE RENAMING.CLAUSE)
        (PROG (VARS)
          (SMAPL #'(LAMBDA (R.TAIL) (SETQ VARS (CONS (CAR R.TAIL) VARS))) #'CDDR (CDR RENAMING.CLAUSE))
          (RETURN VARS))))
    (SMAPC
      #'(LAMBDA (VDOM VCOD)
          (SETQ RENAME.THIS.OTHER.RENAMING.CLAUSE (CONS VDOM (CONS VCOD RENAME.THIS.OTHER.RENAMING.CLAUSE))))
      #'CDDR (CDR RENAMING.CLAUSE) (CDR ANOTHER.RENAMING.CLAUSE))
    (RETURN
      (RED.LC=TAUT_INSTANCE.TEST
        (UNION (RED.LC=TAUT_VARIABLES CLAUSE OTHER.LITNO2 RENAMING.CLAUSE)
          (RED.LC=TAUT_VARIABLES CLAUSE OTHER.LITNO1 ANOTHER.RENAMING.CLAUSE))
        (RED.LC=TAUT_UNIFIERS.RENAME.SWITCH CARRIER.LINKS1 RENAME.THIS.OTHER.RENAMING.CLAUSE CLAUSE LITNO1)
        (RED.LC=TAUT_UNIFIERS.RENAME.SWITCH BRIDGE.LINKS ANOTHER.RENAMING.CLAUSE CLAUSE OTHER.LITNO1)
        (RED.LC=TAUT_UNIFIERS.SWITCH CARRIER.LINKS2 CLAUSE LITNO2)))))

(DEFUN RED.LC=TAUT_INSTANCE.TEST (RESTRICTION.VARIABLES UNIFIERS.CARRIER1 UNIFIERS.BRIDGE UNIFIERS.CARRIER2)
  ;; edited: 31-jul-84 13:36:55
  ;; input:  'unifiers.carrier1', 'unifiers.carrier2',
  ;;         and 'unifiers.bridge' are three lists of
  ;;         unifiers.
  ;;         'restriction.variables' is a list of
  ;;         variables.
  ;; effect: -
  ;; value:  not nil iff all the merge unifiers of
  ;;         'unifiers.carrier1' and 'unifiers.carrier2'
  ;;         are instances of an unifier in
  ;;         'unifiers.bridge'.
  ;;         instance check is restricted to
  ;;         'restriction.variables'.
  (EVERY #'(LAMBDA (CARRIER.MERGE)
	     (MEMBER-IF #'(LAMBDA (BRIDGE.UNI)
			    (PROG2 (RDS-BINDING.PUSH NIL)
				   (UNI-WEAK.INSTANCE CARRIER.MERGE BRIDGE.UNI RESTRICTION.VARIABLES NIL t)
				   (RDS-BINDING.POP)))
			UNIFIERS.BRIDGE))
	 (UNI-MERGE.SUBSTITUTIONLISTS UNIFIERS.CARRIER1 UNIFIERS.CARRIER2 (RDS-BINDING.TOP))))

(DEFUN RED.LC=TAUT_UNIFIERS.RENAME (LINKS RENAMING REMOVE.UNIFIER)
  ;; edited: 31-jul-84 13:42:33
  ;; input:  'links' is a list of links.
  ;;         'renaming' is a renaming substitution.
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed for bridges.
  ;; effect: -
  ;; value:  list of all unifiers of 'links' without
  ;;         'remove.unifier' changed by application
  ;;         of unifier 'renaming'.
  (MAPCAN #'(LAMBDA (LINK)
	      (MAPCAR #'(LAMBDA (UNI) (UNI-APPLY.SUBSTITUTION RENAMING UNI T))
		      (SET-DIFFERENCE (DS-LINK.UNIFIERS LINK)
				      (COND ((EQL LINK (CAR REMOVE.UNIFIER)) (LIST (CDR REMOVE.UNIFIER)))))))
	  LINKS))

(DEFUN RED.LC=TAUT_UNIFIERS.RENAME.SWITCH (WEAK.LINKS RENAMING CLAUSE WISHED.NEGLITNO)
  ;; edited: 31-jul-84 12:15:48
  ;; input:  a list of internal weak links 'weak.links',
  ;;         their parent clause 'parent', and the number
  ;;         of one of their parent literals
  ;;         'wished.neglitno'. all links have the same
  ;;         parent literals.
  ;;         'renaming' is a renaming substitution.
  ;; effect: -
  ;; value:  the list of unfiers of 'weak.links', where
  ;;         the renaming for weak unification is not
  ;;         applied to literal 'wished.neglitno', i.e.
  ;;         applied to the other parent literal, and
  ;;         unifier 'renaming' is applied to this
  ;;         list.
  (MAPCAR #'(LAMBDA (UNI) (UNI-APPLY.SUBSTITUTION RENAMING UNI T))
    (RED.LC=TAUT_UNIFIERS.SWITCH WEAK.LINKS CLAUSE WISHED.NEGLITNO)))

(DEFUN RED.LC=TAUT_UNIFIERS.STANDARD (LINKS REMOVE.UNIFIER)
  ;; edited: 31-jul-84 13:47:43
  ;; input:  'links' is a list of links.
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed for bridges.
  ;; effect: -
  ;; value:  list of all unifiers of 'links' without
  ;;         'remove.unifier'.
  (MAPCAN #'(LAMBDA (LINK)
	      (COPY-LIST (SET-DIFFERENCE (DS-LINK.UNIFIERS LINK)
					 (IF (EQL LINK (CAR REMOVE.UNIFIER))
					     (LIST (CDR REMOVE.UNIFIER))))))
	  LINKS))

(DEFUN RED.LC=TAUT_UNIFIERS.SWITCH (LINKS CLAUSE LITNO)
  ;; edited: 31-jul-84 12:10:48
  ;; input:  a list of internal weak links 'weak.links',
  ;;         their parent clause 'parent', and the number
  ;;         of one of their parent literals
  ;;         'wished.neglitno'. all links have the same
  ;;         parent literals.
  ;; effect: -
  ;; value:  the list of unfiers of 'weak.links', where
  ;;         the renaming for weak unification is not
  ;;         applied to literal 'wished.neglitno', but is
  ;;         applied to the other parent literal.
  (MAPCAN
    #'(LAMBDA (LINK)
        (COND ((EQL LITNO (DS-LINK.POSLITNO LINK)) (RED.SERVICE-SWITCHED.UNIFIERS LINK LITNO CLAUSE))
	      (T (COPY-LIST (DS-LINK.UNIFIERS LINK)))))
    LINKS))

(DEFUN RED.LC=TAUT_VARIABLES (CLAUSE NOT.CONSIDER.LITNO RENAMING)
  ;; edited: 31-jul-84 13:46:13
  ;; input:  'not.consider.litno' is the number of a
  ;;         literal of clause 'clause'.
  ;;         'renaming' is a renaming substitution.
  ;; effect: -
  ;; value:  list of variables of clause 'clause'
  ;;         appearing outside of literal
  ;;         'not.consider.litno', changed by application
  ;;         of unifier 'renaming'.
  (PROG (V)
    (DODOWN (RPTN (DS-CLAUSE.NOLIT CLAUSE))
      (COND ((NEQ NOT.CONSIDER.LITNO (1+ RPTN)) (SETQ V (UNION (DS-CLAUSE.LIT.VARIABLES CLAUSE (1+ RPTN)) V)))))
    (SMAPL #'(LAMBDA (REN.TAIL) (SUBST (SECOND REN.TAIL) (CAR REN.TAIL) V))
	   #'CDDR RENAMING)
    (RETURN V)))

(DEFUN RED.LC-CLAUSE.SUBSUMPTION (LITERALS SUBSUMER SUBSUMENT MATCHERS)
  ;; edited: 21-may-84 23:28:24
  ;; input:  'subsumer' and 'subsument' are two clauses,
  ;;         so that (1) - (3) of the definition of
  ;;         subsumption in 'red=ls:attempt.to.subsume'
  ;;         hold, and 'literals' is a list of literal
  ;;         numbers of clause 'subsument' 
  ;;         (l_nolit_of_subsumer, ..., l_1), so that
  ;;         f(i) = l_i (f is the function in the
  ;;         definition). 
  ;; effect: -
  ;; value:  not nil iff subsumption link condition is
  ;;         fulfilled, i.e. (4) of subsumption
  ;;         definition more precisely:
  ;;         (4a) if l1 and l2 are numbers of literals
  ;;           of clause 'subsumer' and there are si- or
  ;;           riw-links between literals f(l1) and
  ;;           f(l2) of clause 'subsument', then each
  ;;           unifier of these links is an instance of
  ;;           an unifier of
  ;;           a si- or riw-link between literals
  ;;           l1 and l2 of clause 'subsumer'.
  ;;         illustration:           _______
  ;;                                !       !unis
  ;;                              -------------
  ;;                              !l1 !   !l2 !
  ;;                              -------------
  ;;                          f     !   !   !
  ;;                                v   v   v
  ;;                          ---------------------
  ;;                          !   !fl1!   !fl2!   !
  ;;                          ---------------------
  ;;                                !_______!funis
  ;;           each funi is an instance of some uni.
  ;;         (4b) if l is the number of a literal of
  ;;           clause 'subsumer' and there are r- or
  ;;           s-links
  ;;           incident with literal f(l) and another
  ;;           arbitrary literal of the graph, then each
  ;;         unifier of these links is an instance
  ;;         of an unifier of
  ;;           a r-, riw, s-, or si-link between this
  ;;           other literal and literal l of clause
  ;;           'subsumer'.
  ;;         illustration:
  ;;               -------------  unis    ---------
  ;;               !   !   !   !----------!   !
  ;;               -------------          ---------
  ;;                 !   !   !              !
  ;;                 v   v   v              !
  ;;           ---------------------        !
  ;;           !   !   !   !   !   !        !
  ;;           ---------------------        !funis
  ;;                         !______________!
  ;;           each funi is an instance of some uni.
  (PROG
    ((OTHER.LITERALS.SUBSUMER (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT SUBSUMER)))
      (OTHER.LITERALS.SUBSUMENT (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT SUBSUMENT))))
    (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBSUMER))
      (PROGN ;; 'other.literals.subsumer' is an array representing
        ;; the function f of 'red=ls:attempt.to.subsume' in
        ;; another form as 'literals'. access without searching
        ;; nth position in a list (transformation of
        ;; representation).
        (RDS-MARK.PUT OTHER.LITERALS.SUBSUMER (1+ RPTN) (CONS SUBSUMENT (CAR LITERALS)))
        ;; 'other.literals.subsument' is the partial inversion
        ;; of f.
        (RDS-MARK.PUT OTHER.LITERALS.SUBSUMENT (CAR LITERALS) (1+ RPTN)) (SETQ LITERALS (CDR LITERALS))))
    (RETURN
      (MEMBER-IF
        #'(LAMBDA (MATCHER.BINDING)
            (LET ((NOT.FAILURE T))
              (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBSUMER))
                (PROG
                  ((SUBSUMENT.LITNO (CDR (RDS-MARK OTHER.LITERALS.SUBSUMER (1+ RPTN)))) (SUBSUMER.LITNO (1+ RPTN)))
                  (COND
                    ((PROG2 (RDS-BINDING.PUSH MATCHER.BINDING)
			    (AND (RED.LC=SUBS_INTERNAL
				   OTHER.LITERALS.SUBSUMER
				   (RED.SERVICE-CLAUSE.LINKS
				     (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INTERNAL) SUBSUMER SUBSUMER.LITNO)
				   SUBSUMER SUBSUMER.LITNO OTHER.LITERALS.SUBSUMENT
				   (RED.SERVICE-CLAUSE.LINKS
				     (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INTERNAL) SUBSUMENT SUBSUMENT.LITNO)
				   NIL SUBSUMENT SUBSUMENT.LITNO NIL NIL
				   (LIST (CDR (CG-CLAUSE_CREATOR.UNIFIER SUBSUMENT))))
				 (RED.LC=SUBS_EXTERNAL
				   (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.EXT.INT)
							     SUBSUMER SUBSUMER.LITNO)
				   (RED.SERVICE-CLAUSE.LINKS
				     (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INTERNAL) SUBSUMER SUBSUMER.LITNO)
				   SUBSUMER SUBSUMER.LITNO (LIST (CDR (CG-CLAUSE_CREATOR.UNIFIER SUBSUMENT)))
				   (RED.SERVICE-CLAUSE.LINKS (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.EXT.INT)
							     SUBSUMENT SUBSUMENT.LITNO)
				   NIL SUBSUMENT SUBSUMENT.LITNO NIL))
			    (RDS-BINDING.POP))
		     ;; check for (4a) and (4b).
		     )
                    (T (SETQ RPTN 0) (SETQ NOT.FAILURE NIL)))))
              NOT.FAILURE))
        (MAPCAN
          #'(LAMBDA (MATCHER) (UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) MATCHER)) MATCHERS)))))

(DEFUN RED.LC-LINK.SUBSUMPTION
       (LITERALS NOT.CHECK.LITERALS SUBSUMER REMOVE.UNIFIER SUBSUMENT1 SUBSUMENT2 MATCHERS FLAG)
  ;; edited: 10-aug-84 03:38:00
  ;; input:  'literals' is a list of expressions like
  ;;         (litno_subsumer subsument . litno_subsument)
  ;;         representing the literal function of
  ;;         subsumption f. f(litno_subsumer) is
  ;;         literal litno_subsument in clause subsument.
  ;;         'not.check.literals' is a list of literal
  ;;         numbers of clause 'subsumer' which must not
  ;;         be checked for link condition (it is allways
  ;;         fulfilled).
  ;;         'subsumer' is the clause subsuming an
  ;;         unifier of a link between clauses
  ;;         'subsument1' and 'subsument2'.
  ;;         'subsument2' is nil if 'link' has the
  ;;         colour si.
  ;;         'remove.unifier' is a list of unifiers to
  ;;         be regarded as removed for link condition.
  ;;         'matchers' is a list of subsumption
  ;;         matchers only modifying variables of clause
  ;;         'subsumer'.
  ;; effect: -
  ;; value:  not nil iff subsumption link condition for
  ;;         literals of 'subsumer' not in 
  ;;         'not.check.literals' is fulfilled, i.e. (4)
  ;;         of subsumption definition is met:
  ;;         (4a) if l1 and l2 are numbers of literals
  ;;           in clause 'subsumer' and there are s-,
  ;;           r-, si-, or riw-links between literals
  ;;           f(l1) and f(l2) in the clauses
  ;;           'subsument2' and 'subsument1' then each
  ;;           unifier of these links is an instance
  ;;           of an unifier of a si- or riw-link
  ;;           between literals l1 and l2. 
  ;;           illustration:
  ;;                 ______________________________
  ;;                !            funis             !
  ;;           --------------              --------------
  ;;           !..!fl1!..!  !--------------!  !..!fl2!..!
  ;;           --------------              --------------
  ;;
  ;;                        ----------------
  ;;                        ..!l1! .. !l2!..
  ;;                        ----------------
  ;;                            !______!
  ;;                              unis
  ;;           each funy is an instance of some uni.
  ;;         (4b) if l is the number of a literal of
  ;;           clause 'subsumer' and there are riw-, r-,
  ;;           or s-links incident with literal f(l)
  ;;           and another arbitrary literal of the
  ;;           graph, then each unifier of these links
  ;;           is an instance of an unifier of a link
  ;;           with same colour prefix between this other
  ;;           literal and literal l of 'subsumer'.
  ;;           illustration:
  ;;                 ______________________________
  ;;                !            funis             !
  ;;           -------------                -------------
  ;;           !..!fl!..!  !----------------!  !..!  !..!
  ;;           -------------                -------------
  ;;                                 ______________!
  ;;                                !          unis
  ;;                       ------------------
  ;;                       ! ...  !l !  ... !
  ;;                       ------------------
  ;;           each funi is an instance of some uni.
  (PROG
    ((OTHER.LITERALS.SUBSUMER (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT SUBSUMER)))
     (OTHER.LITERALS.SUBSUMENT1 (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT SUBSUMENT1))) OTHER.LITERALS.SUBSUMENT2)
    (COND (SUBSUMENT2 (SETQ OTHER.LITERALS.SUBSUMENT2 (RDS-MARKS.CREATE (DS-CLAUSE.NOLIT SUBSUMENT2)))))
    (MAPC #'(LAMBDA (LA)
	      (RDS-MARK.PUT OTHER.LITERALS.SUBSUMER (CAR LA) (CDR LA))
	      (RDS-MARK.PUT
		(COND ((EQL SUBSUMENT1 (SECOND LA)) OTHER.LITERALS.SUBSUMENT1) (T OTHER.LITERALS.SUBSUMENT2)) (CDDR LA) (CAR LA))
	      (SETQ LITERALS (CDR LITERALS)))
	  LITERALS)
    (RETURN
      (PROG1
        (MEMBER-IF
          #'(LAMBDA (MATCHER.BINDING)
              (LET ((NOT.FAILURE T))
                (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBSUMER))
                  (COND
                    ((NOT (MEMBER (1+ RPTN) NOT.CHECK.LITERALS))
		     (PROG
		       ((SUBSUMENT.LITERAL.DESCRIPTOR (RDS-MARK OTHER.LITERALS.SUBSUMER (1+ RPTN)))
			(SUBSUMER.LITNO (1+ RPTN)) SUBSUMENT SUBSUMENT.LITNO OTHER.SUBSUMENT
			OTHER.LITERALS.SUBSUMENT OTHER.LITERALS.OTHER.SUBSUMENT)
		       (SETQ SUBSUMENT (CAR SUBSUMENT.LITERAL.DESCRIPTOR))
		       (SETQ SUBSUMENT.LITNO (CDR SUBSUMENT.LITERAL.DESCRIPTOR))
		       (COND
			 ((EQL SUBSUMENT SUBSUMENT1) (SETQ OTHER.SUBSUMENT SUBSUMENT2)
			  (SETQ OTHER.LITERALS.OTHER.SUBSUMENT OTHER.LITERALS.SUBSUMENT2)
			  (SETQ OTHER.LITERALS.SUBSUMENT OTHER.LITERALS.SUBSUMENT1))
			 (T (SETQ OTHER.SUBSUMENT SUBSUMENT1)
                            (SETQ OTHER.LITERALS.OTHER.SUBSUMENT OTHER.LITERALS.SUBSUMENT1)
                            (SETQ OTHER.LITERALS.SUBSUMENT OTHER.LITERALS.SUBSUMENT2)))
		       (COND
			 ((PROG2 (RDS-BINDING.PUSH MATCHER.BINDING)
				 (AND
				   (OR FLAG
				       (RED.LC=SUBS_INTERNAL OTHER.LITERALS.SUBSUMER
							     (RED.SERVICE-CLAUSE.LINKS
							       (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INTERNAL)
							       SUBSUMER
							       SUBSUMER.LITNO)
							     SUBSUMER SUBSUMER.LITNO OTHER.LITERALS.SUBSUMENT
							     (RED.SERVICE-CLAUSE.LINKS
							       (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INTERNAL)
							       SUBSUMENT
							       SUBSUMENT.LITNO)
							     (COND
							       (SUBSUMENT2
								(RED.SERVICE-CLAUSE.LINKS
								  (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.EXT.INT)
								  SUBSUMENT
								  SUBSUMENT.LITNO)))
							     SUBSUMENT SUBSUMENT.LITNO
							     OTHER.LITERALS.OTHER.SUBSUMENT OTHER.SUBSUMENT
							     REMOVE.UNIFIER))
				   (RED.LC=SUBS_EXTERNAL
				     (RED.SERVICE-CLAUSE.LINKS
				       (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.EXT.INT) SUBSUMER
				       SUBSUMER.LITNO)
				     (RED.SERVICE-CLAUSE.LINKS
				       (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INTERNAL) SUBSUMER
				       SUBSUMER.LITNO)
				     SUBSUMER SUBSUMER.LITNO REMOVE.UNIFIER
				     (RED.SERVICE-CLAUSE.LINKS
				       (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.EXT.INT) SUBSUMENT
				       SUBSUMENT.LITNO)
				     (RED.SERVICE-CLAUSE.LINKS
				       (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INTERNAL) SUBSUMENT
				       SUBSUMENT.LITNO)
				     SUBSUMENT SUBSUMENT.LITNO OTHER.SUBSUMENT))
				 (RDS-BINDING.POP)))
			 (T (SETQ RPTN 0) (SETQ NOT.FAILURE NIL)))))))
                NOT.FAILURE))
          (MAPCAN
            #'(LAMBDA (MATCHER) (UNI-MERGE.BINDING.WITH.SUBSTITUTION (RDS-BINDING.TOP) MATCHER)) MATCHERS))))))

(DEFUN RED.LC=SUBS_INTERNAL (OTHER.LITERALS.SUBSUMER SUBSUMER.LINK.LISTS.INT SUBSUMER SUBSUMER.LITNO OTHER.LITERALS.SUBSUMENT
			     SUBSUMENT.LINK.LISTS.INT SUBSUMENT.LINK.LISTS.EXT.INT SUBSUMENT SUBSUMENT.LITNO
			     OTHER.LITERALS.OTHER.SUBSUMENT OTHER.SUBSUMENT REMOVE.UNIFIER)
  ;; edited: 22-may-84 00:07:19
  ;; input:  'subsumer.litno' and 'subsument.litno' are
  ;;         numbers of literals of the clauses
  ;;         'subsumer' and 'subsument'.
  ;;         'subsumer.link.lists.int',
  ;;         'subsument.link.lists.int', and
  ;;         'subsument.link.lists.ext.int'
  ;;         are lists of lists of
  ;;         links of these literals. sublists i of all
  ;;         lists have links with the same colour
  ;;         prefix.
  ;;         'other.literals.subsumer',
  ;;         'other.literals.other.subsument', and
  ;;         'other.literals.subsument' are arrays'
  ;;         denoting the injective function and its
  ;;         partial inversion.
  ;;         'other.subsument.litno' can be a literal of
  ;;         clause 'other.subsument'. both can be nil
  ;;         if subsumed literals are all in one clause
  ;;         (clause subsumption). then the identifiers
  ;;         'other.literals.other.subsument' and
  ;;         'subsument.link.lists.ext.int' are nil too.
  ;; value:  not nil iff (4a) of
  ;;         'red.lc-link.subsumption' is fulfilled.
  (PROG ((NOT.FAILURE T))
	(WHILE SUBSUMENT.LINK.LISTS.INT ;; invariant:
	       ;; 'not.failure' = t and for all link lists checked,
	       ;; (4a) is fulfilled.
	       (RED.LC=SUBS_INTERNAL.MARK.OTHERS OTHER.LITERALS.SUBSUMENT (CAR SUBSUMENT.LINK.LISTS.INT)
						 (CAR SUBSUMENT.LINK.LISTS.EXT.INT) SUBSUMENT
						 SUBSUMENT.LITNO OTHER.LITERALS.OTHER.SUBSUMENT
						 OTHER.SUBSUMENT)
	       (RED.LC=SUBS_INTERNAL.MARK.INTERSECTION OTHER.LITERALS.SUBSUMER (CAR SUBSUMER.LINK.LISTS.INT)
						       SUBSUMER.LITNO)
	       (COND
		 ((RED.LC=SUBS_INTERNAL.INSTANCE
		    (RED.LC=SUBS_INTERNAL.LINK.BLOCKS OTHER.LITERALS.SUBSUMER SUBSUMER SUBSUMER.LITNO SUBSUMENT
						      SUBSUMENT.LITNO)
		    SUBSUMER SUBSUMER.LITNO SUBSUMENT SUBSUMENT.LITNO OTHER.SUBSUMENT REMOVE.UNIFIER)
		  ;; the condition (4a) is checked for the current link
		  ;; list (one link colour).
		  (SETQ SUBSUMER.LINK.LISTS.INT (CDR SUBSUMER.LINK.LISTS.INT))
		  (SETQ SUBSUMENT.LINK.LISTS.INT (CDR SUBSUMENT.LINK.LISTS.INT))
		  (SETQ SUBSUMENT.LINK.LISTS.EXT.INT (CDR SUBSUMENT.LINK.LISTS.EXT.INT)))
		 (T ;; terminate loop and return nil (failure).
		  (SETQ SUBSUMENT.LINK.LISTS.INT NIL)
		    (SETQ NOT.FAILURE NIL))))
	(RETURN NOT.FAILURE)))

(DEFUN RED.LC=SUBS_INTERNAL.MARK.OTHERS
  (OTHER.LITERALS.SUBSUMENT SUBSUMENT.LINKS.INT SUBSUMENT.LINKS.EXT SUBSUMENT SUBSUMENT.LITNO
    OTHER.LITERALS.OTHER.SUBSUMENT OTHER.SUBSUMENT)
  ;; edited: 31-jul-84 16:57:33
  ;; input:  'other.literals.subsument' and
  ;;         'other.literal.other.subsument' are arrays
  ;;         representing the partial inversion of
  ;;         subsumption literal function. 
  ;;         cell i contains the number of the literal
  ;;         in clause 'subsumer' subsuming literal i
  ;;         of clause 'subsument' or 'other.subsument'.
  ;;         'subsument.litno' is
  ;;         the number of the literal in clause
  ;;         'subsument' to be subsumed.
  ;;         'subsument.links.int' and
  ;;         'subsument.links.ext' are lists of
  ;;         links with the same colour prefix incident
  ;;         with literal 'subsument.litno' of clause
  ;;         'subsument'. 
  ;; effect: the other literals of the input links will
  ;;         be marked with a property under the
  ;;         indicator 'red.lc*otherlitno'.
  ;;         other literals of external link must be
  ;;         in clause 'other.subsument' to be marked.
  ;;         other literals must be in the image of the
  ;;         literal function of subsumption.
  ;;         property has the form
  ;;         (list_of_links), where the list_of_links
  ;;         contains the links to the literal with
  ;;         this property.
  ;; value:  undefined.
  (PROG (MARK OTHERLITNO)
    (MAPC
      #'(LAMBDA (LINK) (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.INTERNAL LINK SUBSUMENT.LITNO))
          (COND
            ((RDS-MARK OTHER.LITERALS.SUBSUMENT OTHERLITNO)
              (COND
                ((SETQ MARK (DS-CLAUSE.LIT.GETPROP SUBSUMENT OTHERLITNO 'RED.LC*OTHERLITNO))
                  (RPLACA MARK (CONS LINK (CAR MARK))))
                (T (DS-CLAUSE.LIT.PUTPROP SUBSUMENT OTHERLITNO 'RED.LC*OTHERLITNO (LIST (LIST LINK))))))))
      SUBSUMENT.LINKS.INT)
    (MAPC
      #'(LAMBDA (LINK) (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.EXTERNAL LINK SUBSUMENT))
          (COND
            ((AND (EQL OTHER.SUBSUMENT (RED.SERVICE-OTHERPAR.EXTERNAL LINK SUBSUMENT))
               (RDS-MARK OTHER.LITERALS.OTHER.SUBSUMENT OTHERLITNO))
              (COND
                ((SETQ MARK (DS-CLAUSE.LIT.GETPROP OTHER.SUBSUMENT OTHERLITNO 'RED.LC*OTHERLITNO))
                  (RPLACA MARK (CONS LINK (CAR MARK))))
                (T (DS-CLAUSE.LIT.PUTPROP OTHER.SUBSUMENT OTHERLITNO 'RED.LC*OTHERLITNO (LIST (LIST LINK))))))))
      SUBSUMENT.LINKS.EXT)))

(DEFUN RED.LC=SUBS_INTERNAL.MARK.INTERSECTION (OTHER.LITERALS.SUBSUMER SUBSUMER.LINKS.INT SUBSUMER.LITNO)
  ;; edited: 31-jul-84 17:05:33
  ;; input:  'other.literals.subsumer' is an array
  ;;         representing the
  ;;         subsumption literal function. 
  ;;         cell i contains a dotted pair (clause litno)
  ;;         denoting a literal subsumed by literal i
  ;;         of clause 'subsumer'.
  ;;         'subsumer.litno' is the number of a literal
  ;;         of clause 'subsumer'.
  ;;         'subsumer.links.int' is a list of internal
  ;;         links with the same colour incident with
  ;;         literal 'subsumer.litno' of clause
  ;;         'subsumer'.
  ;; effect: the property of literal in the cells
  ;;         other_literals_of_input_links under the
  ;;         indicator 'red.lc*otherlitno' will be
  ;;         modified if it exists.
  ;;         the cdrs of the properies become the list
  ;;         of links to the domain (literal function)
  ;;         literals.
  ;;         now properties contain two lists of links.
  ;; value:  undefined.
  (PROG (OTHER.LITERAL MARK)
    (MAPC
      #'(LAMBDA (LINK)
          (SETQ OTHER.LITERAL
            (RDS-MARK OTHER.LITERALS.SUBSUMER (RED.SERVICE-OTHERLITNO.INTERNAL LINK SUBSUMER.LITNO)))
          (COND
            ((SETQ MARK (DS-CLAUSE.LIT.GETPROP (CAR OTHER.LITERAL) (CDR OTHER.LITERAL) 'RED.LC*OTHERLITNO))
              (RPLACD MARK (CONS LINK (CDR MARK))))))
      SUBSUMER.LINKS.INT)))

(DEFUN RED.LC=SUBS_INTERNAL.LINK.BLOCKS
  (OTHER.LITERALS.SUBSUMER SUBSUMER SUBSUMER.LITNO SUBSUMENT SUBSUMENT.LITNO)
  ;; edited: 31-jul-84 16:44:02
  ;; input:  'other.literals.subsumer' is an array
  ;;         representing the subsumption literal
  ;;         function. cell i contains a dotted pair
  ;;         (clause litno) denoting the literal subsumed
  ;;         by literal i of clause 'subsumer'.
  ;;         'subsumer.litno' is the number of a literal
  ;;         of clause 'subsumer', 'subsument.litno' is
  ;;         the number of the literal in clause
  ;;         'subsument' subsumed by the subsuming
  ;;         literal.
  ;; effect: properties 'red.lc*otherlitno' in
  ;;         subsument clauses will be removed.
  ;; value:  the link lists in the properties are
  ;;         newly sorted.
  ;;         each element can be splitted into three
  ;;         blocks.
  ;;         possible orientations:
  ;;         left   away from subsumer subsument
  ;;         right
  ;;         none
  ;;         illustration of the new order:
  ;;         subsumer links  subsument links
  ;;         left,none       left             (block1)
  ;;         right,none      right            (block2)
  ;;         none            none             (block3)
  ;;         all subsument links left oriented can be
  ;;         subsumed by left or none oriented subsumer
  ;;         links, but not by right oriented ones (link
  ;;         condition seen as subsumption of links).
  ;;         list of new pairs (subsumer_links .
  ;;         subsument_links) created as shown above is
  ;;         returned, where pairs with car = cdr = nil
  ;;         are removed. 
  (PROG (BLOCKS)
    (DODOWN (RPTN (DS-CLAUSE.NOLIT SUBSUMER))
      (PROG ((OL (RDS-MARK OTHER.LITERALS.SUBSUMER (1+ RPTN))) MARK THER AER UER THNT ANT UNT)
        (COND
          ((SETQ MARK (DS-CLAUSE.LIT.GETPROP (CAR OL) (CDR OL) 'RED.LC*OTHERLITNO))
            (DS-CLAUSE.LIT.REMPROP (CAR OL) (CDR OL) 'RED.LC*OTHERLITNO)
            (MAPC
              #'(LAMBDA (NT)
                  (COND
                    ((RED.SERVICE-LINK.ORIENTED NT)
                      (COND
                        ((AND (EQL SUBSUMENT (DS-LINK.POSPAR NT)) (EQL SUBSUMENT.LITNO (DS-LINK.POSLITNO NT)))
                          (SETQ THNT (CONS NT THNT)))
                        (T (SETQ ANT (CONS NT ANT)))))
                    (T (SETQ UNT (CONS NT UNT)))))
              (CAR MARK))
            (MAPC
              #'(LAMBDA (ER)
                  (COND
                    ((RED.SERVICE-LINK.ORIENTED ER)
                      (COND ((EQL SUBSUMER.LITNO (DS-LINK.POSLITNO ER)) (SETQ THER (CONS ER THER))) (T (SETQ AER (CONS ER AER)))))
                    (T (SETQ UER (CONS ER UER)))))
              (CDR MARK))
            (COND ((OR UER UNT) (SETQ BLOCKS (CONS (CONS UER UNT) BLOCKS))))
            (COND ((OR AER ANT) (SETQ BLOCKS (CONS (CONS AER ANT) BLOCKS))))
            (COND ((OR THER THNT) (SETQ BLOCKS (CONS (CONS THER THNT) BLOCKS))))))))
    (RETURN BLOCKS)))

(DEFUN RED.LC=SUBS_INTERNAL.INSTANCE
  (LINK.BLOCKS SUBSUMER SUBSUMER.LITNO SUBSUMENT SUBSUMENT.LITNO OTHER.SUBSUMENT REMOVE.UNIFIER)
  ;; edited: 31-jul-84 18:56:57
  ;; input:  literal 'subsumer.litno' of clause
  ;;         'subsumer' subsumes literal
  ;;         'subsument.litno' of clause 'subsument'.
  ;;         'other.subsument' is a clause in case
  ;;         of link subsumption ('subsument' and
  ;;         'other.subsument' are the parent clauses
  ;;         of the link of the unifier to be subsumed).
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed.
  ;;         'link.blocks' is a list of dotted pairs
  ;;         (list_of_subsumer_links .
  ;;         list_of_subsument_links).
  ;;         the parent literals of the links are related
  ;;         by the literal function of subsumption.
  ;; effect: -
  ;; value:  not nil iff each unifier of the subsuments
  ;;         links is an instance of an unifier of the
  ;;         subsumer links.
  (EVERY
    #'(LAMBDA (LINK.BLOCK)
        (PROG
          ((SUBSUMER.LINKS (CAR LINK.BLOCK)) (SUBSUMENT.LINKS (CDR LINK.BLOCK))
            (RENAMING.SUBSUMER (DS-CLAUSE.RENAMING SUBSUMER)) (RENAMING.SUBSUMENT (DS-CLAUSE.RENAMING SUBSUMENT))
            (RENAMING.OTHER.SUBSUMENT (COND (OTHER.SUBSUMENT (DS-CLAUSE.RENAMING OTHER.SUBSUMENT)))))
          (RETURN
            (COND
              ((OR
                 (MEMBER (DS-LINK.COLOUR (CAR SUBSUMER.LINKS))
                   (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INT.EXT))
                 (MEMBER (DS-LINK.COLOUR (CAR SUBSUMENT.LINKS)) (RDS-LINK.COLOURS 'SLC.WEAK.INHERITANCE)))
                (COND
                  ((MEMBER (DS-LINK.COLOUR (CAR SUBSUMENT.LINKS))
                     (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.EXTERNAL))
                    (RED.LC=SUBS_INTERNAL.INSTANCE.CIW.C SUBSUMER.LINKS SUBSUMENT.LINKS SUBSUMER
                      OTHER.SUBSUMENT SUBSUMER.LITNO
                      (RED.SERVICE-OTHERLITNO.EXTERNAL (CAR SUBSUMENT.LINKS) SUBSUMENT) RENAMING.SUBSUMER
                      RENAMING.SUBSUMENT RENAMING.OTHER.SUBSUMENT))
                  (T
                    (RED.LC=SUBS_INTERNAL.INSTANCE.CIW.CIW SUBSUMER.LINKS SUBSUMENT.LINKS SUBSUMER SUBSUMENT
                      SUBSUMER.LITNO SUBSUMENT.LITNO RENAMING.SUBSUMER
                      (APPEND RENAMING.OTHER.SUBSUMENT RENAMING.SUBSUMENT)))))
              (T (RED.LC=SUBS_INTERNAL.INSTANCE.CI.CI.OR.C SUBSUMER.LINKS SUBSUMENT.LINKS REMOVE.UNIFIER))))))
    LINK.BLOCKS))

(DEFUN RED.LC=SUBS_INTERNAL.INSTANCE.CI.CI.OR.C (SUBSUMER.LINKS SUBSUMENT.LINKS REMOVE.UNIFIER)
  ;; edited: 31-jul-84 18:20:53
  ;; input:  illustration of the situation:
  ;;
  ;;             subsument.links
  ;;                ________
  ;;               !        !
  ;;         ----------------------
  ;;         ! .. !l2! .. !  ! .. !  subsument
  ;;         ----------------------
  ;;               .        .
  ;;               .        .
  ;;         ----------------------
  ;;         ! .. !l1! .. !  ! .. !  subsumer
  ;;         ----------------------
  ;;               !________!
  ;;             subsumer.links
  ;;
  ;;         subsument                    other.subsument
  ;;
  ;;                       subsument.links 
  ;;                ______________________________
  ;;               !                              !
  ;;         --------------                --------------
  ;;         ! .. !l2! .. !                ! .. !  ! .. !
  ;;         --------------                --------------
  ;;               .        .......................
  ;;               .        .
  ;;         ----------------------
  ;;         ! .. !l1! .. !  ! .. !  subsumer
  ;;         ----------------------
  ;;               !________!
  ;;             subsumer.links
  ;;
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed.
  ;; effect: -
  ;; value:  not nil iff all unifiers of
  ;;         'subsument.links' are instances of unifiers
  ;;         of 'subsumer.links' under the current
  ;;         binding.
  ;;         'remove.unifier' are regarded as removed
  ;;         for this check.
  (RED.LC=SUBS_INSTANCE.TEST (RED.LC=SUBS_UNIFIERS.STANDARD SUBSUMER.LINKS REMOVE.UNIFIER)
    (RED.LC=SUBS_UNIFIERS.STANDARD SUBSUMENT.LINKS REMOVE.UNIFIER)))

(DEFUN RED.LC=SUBS_INTERNAL.INSTANCE.CIW.CIW
  (SUBSUMER.LINKS SUBSUMENT.LINKS SUBSUMER SUBSUMENT SUBSUMER.LITNO SUBSUMENT.LITNO RENAMING.SUBSUMER
    RENAMING.SUBSUMENT)
  ;; edited: 31-jul-84 18:15:53
  ;; input:  illustration of the situation:
  ;;
  ;;             subsument.links
  ;;                ________
  ;;               !        !
  ;;         ----------------------
  ;;         ! .. !l2! .. !  ! .. !  subsument
  ;;         ----------------------
  ;;               .        .
  ;;               .        .
  ;;         ----------------------
  ;;         ! .. !l1! .. !  ! .. !  subsumer
  ;;         ----------------------
  ;;               !________!
  ;;             subsumer.links
  ;;
  ;;         l1    subsumer.litno
  ;;         l2    subsument.litno
  ;;
  ;; effect: -
  ;; value:  not nil iff all unifiers of
  ;;         'subsument.links' are instances of unifiers
  ;;         of 'subsumer.links' under the current
  ;;         binding.
  ;;         renaming must be applied on the same
  ;;         side of the links.
  (RDS-BINDING.PUSH
    (APPEND (RDS-BINDING.TOP)
      (UNI-APPLY.SUBSTITUTION (APPEND RENAMING.SUBSUMER RENAMING.SUBSUMENT) (RDS-BINDING.TOP) T)))
  (PROG1
    (RED.LC=SUBS_INSTANCE.TEST (RED.LC=SUBS_UNIFIERS.SWITCH SUBSUMER.LINKS SUBSUMER SUBSUMER.LITNO)
      (RED.LC=SUBS_UNIFIERS.SWITCH SUBSUMENT.LINKS SUBSUMENT SUBSUMENT.LITNO))
    (RDS-BINDING.POP)))

(DEFUN RED.LC=SUBS_INTERNAL.INSTANCE.CIW.C (SUBSUMER.LINKS SUBSUMENT.LINKS SUBSUMER OTHER.SUBSUMENT
					    SUBSUMER.LITNO OTHER.SUBSUMENT.LITNO
					    RENAMING.SUBSUMER RENAMING.SUBSUMENT RENAMING.OTHER.SUBSUMENT)
  (DECLARE (IGNORE OTHER.SUBSUMENT OTHER.SUBSUMENT.LITNO))
  ;; edited: 31-jul-84 18:10:53
  ;; input:  illustration of the situation:
  ;;
  ;;         subsument                    other.subsument
  ;;
  ;;                       subsument.links 
  ;;                ______________________________
  ;;               !                              !
  ;;         --------------                --------------
  ;;         ! .. !l2! .. !                ! .. !l3! .. !
  ;;         --------------                --------------
  ;;               .        .......................
  ;;               .        .
  ;;         ----------------------
  ;;         ! .. !l1! .. !  ! .. !  subsumer
  ;;         ----------------------
  ;;               !________!
  ;;             subsumer.links
  ;;
  ;;         l1    subsumer.litno
  ;;         l2    subsument.litno
  ;;         l3    other.subsument.litno
  ;;
  ;; effect: -
  ;; value:  not nil iff all unifiers of
  ;;         'subsument.links' are instances of unifiers
  ;;         of 'subsumer.links' under the current
  ;;         binding.
  ;;         renaming must be applied on the same
  ;;         side of the links.
  (RDS-BINDING.PUSH
    (APPEND (RDS-BINDING.TOP)
	    (UNI-APPLY.SUBSTITUTION (APPEND RENAMING.SUBSUMER RENAMING.SUBSUMENT RENAMING.OTHER.SUBSUMENT)
				    (RDS-BINDING.TOP) T)))
  (PROG1
    (RED.LC=SUBS_INSTANCE.TEST (RED.LC=SUBS_UNIFIERS.SWITCH SUBSUMER.LINKS SUBSUMER SUBSUMER.LITNO)
			       (RED.LC=SUBS_UNIFIERS.RENAME SUBSUMENT.LINKS RENAMING.OTHER.SUBSUMENT NIL))
    (RDS-BINDING.POP)))

(DEFUN RED.LC=SUBS_EXTERNAL
  (SUBSUMER.LINK.LISTS.EXT SUBSUMER.LINK.LISTS.INT SUBSUMER SUBSUMER.LITNO REMOVE.UNIFIER
    SUBSUMENT.LINK.LISTS.EXT SUBSUMENT.LINK.LISTS.INT SUBSUMENT SUBSUMENT.LITNO OTHER.SUBSUMENT)
  ;; edited: 22-may-84 00:29:04
  ;; input:  'subsumer.litno' and 'subsument.litno' are
  ;;         numbers of literals of the clauses
  ;;         'subsumer' and 'subsument'. the other four
  ;;         inputs are lists of lists of links of these
  ;;         literals, so that for links of sublist i of
  ;;         'subsumer.link.lists.ext' are external,
  ;;         'subsumer.link.lists.int' are internal weak,
  ;;         'subsument.link.lists.ext' are external, and
  ;;         'subsument.link.lists.int' are internal
  ;;         weak or internal strong.
  ;;         all links in the four sublists have the
  ;;         same colour prefix. here two sublists for
  ;;         each list (r, riw, r, riw), (s, siw, s, si).
  ;;         'remove.unifier' is a list of unifiers.
  ;; effect: -
  ;; value:  not nil iff (4b) is fulfilled.
  (PROG ((OTHERS.SUBSUMENT NIL) (NOT.FAILURE T))
    (WHILE SUBSUMER.LINK.LISTS.EXT
      ;; invariant:
      ;; 'not.failure' = t and (4b) complies with all link
      ;; lists processed before.
      ;; all literals obtainable with a link of the first
      ;; list of 'subsument.link.lists.ext' and
      ;; 'subsument.link.lists.int' of literal 
      ;; 'subsument.litno' of clause 'subsument' are marked
      ;; with elements ((own_clause . own_litno) (link_1 ...
      ;; link_n)). this
      ;; marks are listed in 'others.subsument'.
      (SETQ OTHERS.SUBSUMENT
	    (RED.LC=SUBS_EXTERNAL.MARK.OTHERS SUBSUMER SUBSUMER.LITNO (CAR SUBSUMENT.LINK.LISTS.EXT)
					      (CAR SUBSUMENT.LINK.LISTS.INT) SUBSUMENT SUBSUMENT.LITNO))
      ;; if literals obtainable with a link of the first list
      ;; of 'subsumer.link.lists.ext' or
      ;; 'subsumer.link.lists.int' from literal
      ;; 'subsumer.litno' of clause 'subsumer' are marked
      ;; with elements as above, in the cddr of the mark
      ;; subsumer link is inserted.
      (RED.LC=SUBS_EXTERNAL.MARK.INTERSECTION (CAR SUBSUMER.LINK.LISTS.EXT) (CAR SUBSUMER.LINK.LISTS.INT) SUBSUMER
					      SUBSUMER.LITNO)
      (COND
        ((RED.LC=SUBS_EXTERNAL.INSTANCE
           (RED.LC=SUBS_EXTERNAL.LINK.BLOCKS OTHERS.SUBSUMENT SUBSUMER SUBSUMER.LITNO SUBSUMENT
					     SUBSUMENT.LITNO)
           SUBSUMER SUBSUMER.LITNO SUBSUMENT SUBSUMENT.LITNO OTHER.SUBSUMENT REMOVE.UNIFIER)
	 (SETQ SUBSUMER.LINK.LISTS.EXT (CDR SUBSUMER.LINK.LISTS.EXT))
	 (SETQ SUBSUMER.LINK.LISTS.INT (CDR SUBSUMER.LINK.LISTS.INT))
	 (SETQ SUBSUMENT.LINK.LISTS.EXT (CDR SUBSUMENT.LINK.LISTS.EXT))
	 (SETQ SUBSUMENT.LINK.LISTS.INT (CDR SUBSUMENT.LINK.LISTS.INT)))
        (T ;; no compliance --> termination of loop and returning
	 ;; of nil.
	 (SETQ SUBSUMER.LINK.LISTS.EXT NIL)
	 (SETQ NOT.FAILURE NIL))))
    (RETURN NOT.FAILURE)))

(DEFUN RED.LC=SUBS_EXTERNAL.MARK.OTHERS
  (SUBSUMER SUBSUMER.LITNO SUBSUMENT.LINKS.EXT SUBSUMENT.LINKS.INT SUBSUMENT SUBSUMENT.LITNO)
  ;; edited: 22-may-84 00:50:21
  ;; input:  'subsument.links.ext' is the list of all
  ;;         links with the same external link condition
  ;;         relevant colour incident
  ;;         with literal 'subsument.litno' of clause
  ;;         'subsument'. 
  ;;         'subsumer' is a clause.
  ;; effect: all other parent literals of the links are
  ;;         marked under the property
  ;;         'red.lc*otherlitno' with an
  ;;         element ((clause . litno) (link_1 ...
  ;;         link_n))
  ;;         describing themselves and the connections
  ;;         with literal 'subsument.litno'.
  ;; value:  list of all marks as described above.
  (PROG (OTHERS.SUBSUMENT OTHERPAR OTHERLITNO OTHER.LITERAL.MARK)
    (MAPC
      #'(LAMBDA (SUBSUMENT.LINK.INT) (SETQ OTHERPAR SUBSUMENT)
          (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.INTERNAL SUBSUMENT.LINK.INT SUBSUMENT.LITNO))
          (COND
            ((MEMBER (DS-LINK.COLOUR SUBSUMENT.LINK.INT)
               (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INT.EXT))
              (COND
                ((SETQ OTHER.LITERAL.MARK (DS-CLAUSE.LIT.GETPROP OTHERPAR OTHERLITNO 'RED.LC*OTHERLITNO))
                  (RPLACA (CDR OTHER.LITERAL.MARK) (CONS SUBSUMENT.LINK.INT (SECOND OTHER.LITERAL.MARK))))
                (T
                  (SETQ OTHERS.SUBSUMENT
                    (CONS (LIST (CONS OTHERPAR OTHERLITNO) (LIST SUBSUMENT.LINK.INT)) OTHERS.SUBSUMENT))
                  (DS-CLAUSE.LIT.PUTPROP OTHERPAR OTHERLITNO 'RED.LC*OTHERLITNO (CAR OTHERS.SUBSUMENT)))))))
      SUBSUMENT.LINKS.INT)
    (MAPC
      #'(LAMBDA (SUBSUMENT.LINK.EXT)
          (SETQ OTHERPAR (RED.SERVICE-OTHERPAR.EXTERNAL SUBSUMENT.LINK.EXT SUBSUMENT))
          (SETQ OTHERLITNO (RED.SERVICE-OTHERLITNO.EXTERNAL SUBSUMENT.LINK.EXT SUBSUMENT))
          (COND
            ((OR
               (MEMBER (DS-LINK.COLOUR SUBSUMENT.LINK.EXT)
                 (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.EXTERNAL))
               (AND (EQL OTHERPAR SUBSUMER) (NEQ OTHERLITNO SUBSUMER.LITNO)))
              (COND
                ((SETQ OTHER.LITERAL.MARK (DS-CLAUSE.LIT.GETPROP OTHERPAR OTHERLITNO 'RED.LC*OTHERLITNO))
                  (RPLACA (CDR OTHER.LITERAL.MARK) (CONS SUBSUMENT.LINK.EXT (SECOND OTHER.LITERAL.MARK))))
                (T
                  (SETQ OTHERS.SUBSUMENT
                    (CONS (LIST (CONS OTHERPAR OTHERLITNO) (LIST SUBSUMENT.LINK.EXT)) OTHERS.SUBSUMENT))
                  (DS-CLAUSE.LIT.PUTPROP OTHERPAR OTHERLITNO 'RED.LC*OTHERLITNO (CAR OTHERS.SUBSUMENT)))))))
      SUBSUMENT.LINKS.EXT)
    (RETURN OTHERS.SUBSUMENT)))

(DEFUN RED.LC=SUBS_EXTERNAL.MARK.INTERSECTION
       (SUBSUMER.LINKS.EXT SUBSUMER.LINKS.INT SUBSUMER SUBSUMER.LITNO)
  ;; edited: 22-may-84 00:57:08
  ;; input:  'subsumer.links.ext' and
  ;;         'subsumer.links.int' are the lists of all
  ;;         colourexternal- and colourinternalweak-
  ;;         coloured links (r, riw) of literal
  ;;         'subsumer.litno' of clause 'subsumer'.
  ;; effect: if the other parent literals of the links
  ;;         are marked with property
  ;;         'red.lc*otherlitno':
  ;;         ((other_clause . other_litno) 
  ;;         (subsument_link1 ... subsument_linkn)
  ;;         subsumer_link1 ... subsumer_linkm)
  ;;         then the links are inserted into
  ;;         subsumer_links in the property.
  ;; value:  undefined.
  (let (OTHER.MARK)
    (MAPC #'(LAMBDA (SUBSUMER.LINK.EXT)
	      (COND ((SETQ OTHER.MARK
			   (DS-CLAUSE.LIT.GETPROP (RED.SERVICE-OTHERPAR.EXTERNAL SUBSUMER.LINK.EXT SUBSUMER)
						  (RED.SERVICE-OTHERLITNO.EXTERNAL SUBSUMER.LINK.EXT SUBSUMER)
						  'RED.LC*OTHERLITNO))
		     (RPLACD (CDR OTHER.MARK) (CONS SUBSUMER.LINK.EXT (CDDR OTHER.MARK))))))
	  SUBSUMER.LINKS.EXT)
    (MAPC #'(LAMBDA (SUBSUMER.LINK.INT)
	      (COND ((SETQ OTHER.MARK
			   (DS-CLAUSE.LIT.GETPROP SUBSUMER
						  (RED.SERVICE-OTHERLITNO.INTERNAL SUBSUMER.LINK.INT SUBSUMER.LITNO)
						  'RED.LC*OTHERLITNO))
		     (RPLACD (CDR OTHER.MARK) (CONS SUBSUMER.LINK.INT (CDDR OTHER.MARK))))))
	  SUBSUMER.LINKS.INT)))

(DEFUN RED.LC=SUBS_EXTERNAL.LINK.BLOCKS (OTHERS.SUBSUMENT SUBSUMER SUBSUMER.LITNO SUBSUMENT SUBSUMENT.LITNO)
  (DECLARE (IGNORE SUBSUMER))
  ;; edited: 31-jul-84 16:54:02
  ;; input:  'others.subsument' is a list of expressions
  ;;         like ((clause .litno) subsument_links .
  ;;         subsumer_links). each expression is a
  ;;         property of literal litno of clause clause
  ;;         under the indicator 'red.lc*otherlitno'.
  ;;         all links in the two lists are incident
  ;;         with this literal in the same expression.
  ;;         'subsumer.litno' is the number of a literal
  ;;         of clause 'subsumer', 'subsument.litno' is
  ;;         the number of the literal in clause
  ;;         'subsument' subsumed by the subsumer
  ;;         literal.
  ;; effect: removes properties.
  ;; value:  the link lists are newly sorted.
  ;;         each element can be splitted into three
  ;;         blocks.
  ;;         possible orientations:
  ;;         left   away from subsumer subsument
  ;;         right
  ;;         none
  ;;         illustration of the new order:
  ;;         subsumer links  subsument links
  ;;         left,none       left             (block1)
  ;;         right,none      right            (block2)
  ;;         none            none             (block3)
  ;;         all subsument links left oriented can be
  ;;         subsumed by left or none oriented subsumer
  ;;         links, but not by right oriented ones (link
  ;;         condition seen as subsumption of links).
  ;;         list of new pairs (subsumer_links .
  ;;         subsument_links) created as shown above is
  ;;         returned, where pairs with car = cdr = nil
  ;;         are removed. 
  (PROG (BLOCKS)
    (MAPC
      #'(LAMBDA (MARK)
          (PROG (THER AER UER THNT ANT UNT) (DS-CLAUSE.LIT.REMPROP (CAAR MARK) (CDAR MARK) 'RED.LC*OTHERLITNO)
            (MAPC
              #'(LAMBDA (NT)
                  (COND
                    ((RED.SERVICE-LINK.ORIENTED NT)
                      (COND
                        ((AND (EQL SUBSUMENT (DS-LINK.POSPAR NT)) (EQL SUBSUMENT.LITNO (DS-LINK.POSLITNO NT)))
                          (SETQ THNT (CONS NT THNT)))
                        (T (SETQ ANT (CONS NT ANT)))))
                    (T (SETQ UNT (CONS NT UNT)))))
              (SECOND MARK))
            (MAPC
              #'(LAMBDA (ER)
                  (COND
                    ((RED.SERVICE-LINK.ORIENTED ER)
                      (COND ((EQL SUBSUMER.LITNO (DS-LINK.POSLITNO ER)) (SETQ THER (CONS ER THER))) (T (SETQ AER (CONS ER AER)))))
                    (T (SETQ UER (CONS ER UER)))))
              (CDDR MARK))
            (COND ((OR UER UNT) (SETQ BLOCKS (CONS (CONS UER UNT) BLOCKS))))
            (COND ((OR AER ANT) (SETQ BLOCKS (CONS (CONS AER ANT) BLOCKS))))
            (COND ((OR THER THNT) (SETQ BLOCKS (CONS (CONS THER THNT) BLOCKS))))))
      OTHERS.SUBSUMENT)
    (RETURN BLOCKS)))

(DEFUN RED.LC=SUBS_EXTERNAL.INSTANCE
  (LINK.BLOCKS SUBSUMER SUBSUMER.LITNO SUBSUMENT SUBSUMENT.LITNO OTHER.SUBSUMENT REMOVE.UNIFIER)
  ;; edited: 31-jul-84 19:05:44
  ;; input:  literal 'subsumer.litno' of clause
  ;;         'subsumer' subsumes literal
  ;;         'subsument.litno' of clause 'subsument'.
  ;;         'other.subsument' is a clause in case
  ;;         of link subsumption ('subsument' and
  ;;         'other.subsument' are the parent clauses
  ;;         of the link of the unifier to be subsumed).
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed.
  ;;         'link.blocks' is a list of dotted pairs
  ;;         (list_of_subsumer_links .
  ;;         list_of_subsument_links).
  ;;         the other parent literals of all the links
  ;;         in these two lists
  ;;         is the same (this side in 'subsumer' and
  ;;         'subsument').
  ;; effect: -
  ;; value:  not nil iff each unifier of the subsuments
  ;;         links is an instance of an unifier of the
  ;;         subsumer links.
  (EVERY
    #'(LAMBDA (LINK.BLOCK)
        (PROG
          ((SUBSUMER.LINKS (CAR LINK.BLOCK)) (SUBSUMENT.LINKS (CDR LINK.BLOCK))
	   (RENAMING.SUBSUMER (DS-CLAUSE.RENAMING SUBSUMER)) (RENAMING.SUBSUMENT (DS-CLAUSE.RENAMING SUBSUMENT))
	   (RENAMING.OTHER.SUBSUMENT (COND (OTHER.SUBSUMENT (DS-CLAUSE.RENAMING OTHER.SUBSUMENT)))))
          (RETURN
            (COND
              ((MEMBER (DS-LINK.COLOUR (CAR SUBSUMER.LINKS))
                 (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.EXTERNAL))
                (COND
                  ((MEMBER (DS-LINK.COLOUR (CAR SUBSUMENT.LINKS))
                     (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INT.EXT))
                    (RED.LC=SUBS_EXTERNAL.INSTANCE.C.CIW SUBSUMER.LINKS SUBSUMENT.LINKS SUBSUMENT
                      SUBSUMENT.LITNO RENAMING.SUBSUMENT REMOVE.UNIFIER))
                  ((EQL OTHER.SUBSUMENT (RED.SERVICE-OTHERPAR.EXTERNAL (CAR SUBSUMER.LINKS) SUBSUMER))
                    (RED.LC=SUBS_EXTERNAL.INSTANCE.C.C.RENAME SUBSUMER.LINKS SUBSUMENT.LINKS
                      RENAMING.OTHER.SUBSUMENT))
                  (T (RED.LC=SUBS_EXTERNAL.INSTANCE.C.C SUBSUMER.LINKS SUBSUMENT.LINKS NIL))))
              ((MEMBER (DS-LINK.COLOUR (CAR SUBSUMER.LINKS))
                 (RDS-LINK.COLOURS 'SUBSUMPTION.LINK.CONDITION.INT.EXT))
                (RED.LC=SUBS_EXTERNAL.INSTANCE.CIW.C SUBSUMER.LINKS SUBSUMENT.LINKS SUBSUMER SUBSUMER.LITNO
                  RENAMING.SUBSUMER))
              (T (RED.LC=SUBS_EXTERNAL.INSTANCE.CI.C SUBSUMER.LINKS SUBSUMENT.LINKS))))))
    LINK.BLOCKS))

(DEFUN RED.LC=SUBS_EXTERNAL.INSTANCE.C.C (SUBSUMER.LINKS SUBSUMENT.LINKS REMOVE.UNIFIER)
  ;; edited: 31-jul-84 18:30:53
  ;; input:  illustration of the situation:
  ;;
  ;;         --------------                --------------
  ;;         ! .. !  ! .. !  subsument     ! .. !  ! .. !
  ;;         --------------                -------------
  ;;               .!____________________________!!
  ;;               .         subsument.links      !
  ;;               . _____________________________!
  ;;               .!        subsumer.links
  ;;         --------------
  ;;         ! .. !  ! .. !  subsumer
  ;;         --------------
  ;;
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed.
  ;; effect: -
  ;; value:  not nil iff all unifiers of
  ;;         'subsument.links' are instances of unifiers
  ;;         of 'subsumer.links' under the current
  ;;         binding.
  (RED.LC=SUBS_INSTANCE.TEST (RED.LC=SUBS_UNIFIERS.STANDARD SUBSUMER.LINKS REMOVE.UNIFIER)
    (RED.LC=SUBS_UNIFIERS.STANDARD SUBSUMENT.LINKS REMOVE.UNIFIER)))

(DEFUN RED.LC=SUBS_EXTERNAL.INSTANCE.C.C.RENAME (SUBSUMER.LINKS SUBSUMENT.LINKS RENAMING.OTHER.SUBSUMENT)
  ;; edited: 31-jul-84 18:35:53
  ;; input:  illustration of the situation:
  ;;
  ;;         subsument                    other.subsument
  ;;
  ;;                        subsument.links
  ;;                 ____________________________
  ;;                !                            !
  ;;         --------------                --------------
  ;;         ! .. !  ! .. !                ! .. !* ! .. !
  ;;         --------------                -------------
  ;;               . ____________________________!
  ;;               .!                 subsumer.links
  ;;         --------------
  ;;         ! .. !  ! .. !  subsumer
  ;;         --------------
  ;;
  ;; effect: -
  ;; value:  not nil iff all unifiers of
  ;;         'subsument.links' are instances of unifiers
  ;;         of 'subsumer.links' under the current
  ;;         binding.
  ;;         the variables of literal * are renamed for
  ;;         the instance check.
  (RED.LC=SUBS_INSTANCE.TEST (RED.LC=SUBS_UNIFIERS.RENAME SUBSUMER.LINKS RENAMING.OTHER.SUBSUMENT NIL)
    (RED.LC=SUBS_UNIFIERS.RENAME SUBSUMENT.LINKS RENAMING.OTHER.SUBSUMENT NIL)))

(DEFUN RED.LC=SUBS_EXTERNAL.INSTANCE.CIW.C
  (SUBSUMER.LINKS SUBSUMENT.LINKS SUBSUMER SUBSUMER.LITNO RENAMING.SUBSUMER)
  ;; edited: 31-jul-84 18:20:53
  ;; input:  illustration of the situation:
  ;;
  ;;         ----------------------
  ;;         ! .. !  ! .. !  ! .. !  subsument
  ;;         ----------------------
  ;;               . ______!.subsument.links
  ;;               .!       .
  ;;         ----------------------
  ;;         ! .. !* ! .. !l ! .. !  subsumer
  ;;         ----------------------
  ;;               !________!
  ;;             subsumer.links
  ;;
  ;;         l    subsumer.litno
  ;;
  ;; effect: -
  ;; value:  not nil iff all unifiers of
  ;;         'subsument.links' are instances of unifiers
  ;;         of 'subsumer.links' under the current
  ;;         binding.
  ;;         for the instance check variables of the
  ;;         literal * are renamed in the unifiers.
  (RED.LC=SUBS_INSTANCE.TEST (RED.LC=SUBS_UNIFIERS.SWITCH SUBSUMER.LINKS SUBSUMER SUBSUMER.LITNO)
    (RED.LC=SUBS_UNIFIERS.RENAME SUBSUMENT.LINKS RENAMING.SUBSUMER NIL)))

(DEFUN RED.LC=SUBS_EXTERNAL.INSTANCE.C.CIW
  (SUBSUMER.LINKS SUBSUMENT.LINKS SUBSUMENT SUBSUMENT.LITNO RENAMING.SUBSUMENT REMOVE.UNIFIER)
  ;; edited: 31-jul-84 18:40:53
  ;; input:  illustration of the situation:
  ;;
  ;;             subsument.links
  ;;                 ______
  ;;                !      !
  ;;         ----------------------
  ;;         ! .. !* ! .. !  ! .. !  subsument
  ;;         ----------------------
  ;;               .!       .
  ;;               .!______ .subsumer.links
  ;;               .       !.
  ;;               .       !.
  ;;         ----------------------
  ;;         ! .. !  ! .. !  ! .. !  subsumer
  ;;         ----------------------
  ;;
  ;; effect: -
  ;; value:  not nil iff all unifiers of
  ;;         'subsument.links' are instances of unifiers
  ;;         of 'subsumer.links' under the current
  ;;         binding.
  ;;         the variables of literal * are renamed for
  ;;         the instance check.
  (RED.LC=SUBS_INSTANCE.TEST
    (RED.LC=SUBS_UNIFIERS.RENAME SUBSUMER.LINKS RENAMING.SUBSUMENT REMOVE.UNIFIER)
    (RED.LC=SUBS_UNIFIERS.SWITCH SUBSUMENT.LINKS SUBSUMENT SUBSUMENT.LITNO)))

(DEFUN RED.LC=SUBS_EXTERNAL.INSTANCE.CI.C (SUBSUMER.LINKS SUBSUMENT.LINKS)
  ;; edited: 31-jul-84 18:30:53
  ;; input:  illustration of the situation:
  ;;
  ;;         ----------------------
  ;;         ! .. !  ! .. !  ! .. !  subsument
  ;;         ----------------------
  ;;               . ______!.subsument.links
  ;;               .!       .
  ;;         ----------------------
  ;;         ! .. !  ! .. !  ! .. !  subsumer
  ;;         ----------------------
  ;;               !________!
  ;;             subsumer.links
  ;; effect: -
  ;; value:  not nil iff all unifiers of
  ;;         'subsument.links' are instances of unifiers
  ;;         of 'subsumer.links' under the current
  ;;         binding.
  (RED.LC=SUBS_INSTANCE.TEST (RED.LC=SUBS_UNIFIERS.STANDARD SUBSUMER.LINKS NIL)
    (RED.LC=SUBS_UNIFIERS.STANDARD SUBSUMENT.LINKS NIL)))

(DEFUN RED.LC=SUBS_INSTANCE.TEST (SUBSUMER.UNIS SUBSUMENT.UNIS)
  ;; edited: 31-jul-84 17:55:59
  ;; input:  two lists of unifiers.
  ;; effect: -
  ;; value:  not nil iff each unifier in 'subsument.unis'
  ;;         is an instance of an unifier in
  ;;         'subsumer.unis'.
  ;;         binding is neither considered nor destroyed.
  (EVERY
    #'(LAMBDA (SUBSUMENT.UNI)
        (MEMBER-IF
          #'(LAMBDA (SUBSUMER.UNI)
              (PROG2 (RDS-BINDING.PUSH NIL) (UNI-WEAK.INSTANCE SUBSUMENT.UNI SUBSUMER.UNI NIL T)
                (RDS-BINDING.POP)))
          SUBSUMER.UNIS))
    SUBSUMENT.UNIS))

(DEFUN RED.LC=SUBS_UNIFIERS.STANDARD (LINKS REMOVE.UNIFIER)
  ;; edited: 31-jul-84 18:05:57
  ;; input:  'links' is a list of links,
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed.
  ;; effect: -
  ;; value:  list of unifiers of links 'links'
  ;;         under the current binding.
  (PROG (UNIS)
    (MAPC
      #'(LAMBDA (LINK)
          (MAPC
            #'(LAMBDA (UNI)
                (COND
                  ((NOT (AND (EQL LINK (CAR REMOVE.UNIFIER)) (EQL UNI (CDR REMOVE.UNIFIER))))
		   (SETQ UNIS (UNI-UNIFY.MIXED.TERMLIST UNI)))))
            (DS-LINK.UNIFIERS LINK)))
      LINKS)
    (RETURN UNIS)))

(DEFUN RED.LC=SUBS_UNIFIERS.SWITCH (LINKS CLAUSE WISHED.NEGLITNO)
  ;; edited: 31-jul-84 18:00:57
  ;; input:  'links' is a list of weak internal links
  ;;         incident with literal 'wished.neglitno'
  ;;         of clause 'clause'.
  ;; effect: -
  ;; value:  list of unifiers of links 'links' modified
  ;;         so that renaming for weak unification
  ;;         is applied to the other parent literal of
  ;;         the links (not 'wished.neglitno')
  ;;         under the current binding.
  (PROG (UNIS)
    (MAPC
      #'(LAMBDA (LINK)
          (MAPC #'(LAMBDA (UNI) (SETQ UNIS (UNI-UNIFY.MIXED.TERMLIST UNI)))
            (RED.SERVICE-SWITCHED.UNIFIERS LINK WISHED.NEGLITNO CLAUSE)))
      LINKS)
    (RETURN UNIS)))

(DEFUN RED.LC=SUBS_UNIFIERS.RENAME (LINKS RENAMING REMOVE.UNIFIER)
  ;; edited: 31-jul-84 17:58:37
  ;; input:  'links' is a list of links,
  ;;         'renaming' a renaming substitution, and
  ;;         'remove.unifier' is nil or a dotted pair
  ;;         (link . unifier), denoting a unifier to
  ;;         be regarded as removed.
  ;; effect: -
  ;; value:  list of unifiers of links 'links'
  ;;         modified by application of unfier
  ;;         'renaming' under the current binding.
  (PROG (UNIS)
    (MAPC
      #'(LAMBDA (LINK)
          (MAPC
            #'(LAMBDA (UNI)
                (COND
                  ((NOT (AND (EQL LINK (CAR REMOVE.UNIFIER)) (EQL UNI (CDR REMOVE.UNIFIER))))
                    (SETQ UNIS (UNI-UNIFY.MIXED.TERMLIST (UNI-APPLY.SUBSTITUTION RENAMING UNI T))))))
            (DS-LINK.UNIFIERS LINK)))
      LINKS)
    (RETURN UNIS)))

(DEFUN RED.LC=SUBS_UNIFIERS.RENAME.BINDING (LINKS POSCLAUSE WISHED.POSLITNO RENAMING)
  ;; edited: 31-jul-84 18:10:57
  ;; input:  'links' is a list of external links
  ;;         incident with literal 'wished.poslitno' of
  ;;         clause 'posclause'.
  ;;         'renaming' is a renaming substitution
  ;;         (union of renamings of parent clauses of
  ;;         'links').
  ;; effect: -
  ;; value:  list of unifiers of links 'links' inherited
  ;;         under the current binding.
  ;;         'links' become weak internal. 
  ;;         'renaming' is used in literal 
  ;;         'wished.poslitno' of clause 'posclause'.
  (LET (UNIS (BINDING (COPY-TREE (RDS-BINDING.TOP))))
    (MAPC
      #'(LAMBDA (VAR)
          (COND
            ((DT-VARIABLE.GET.BINDING VAR)
              (NSUBST (SECOND (MEMBER VAR RENAMING)) VAR (SECOND (MEMBER VAR BINDING))))
            (T (SETQ BINDING (CONS VAR (CONS (SECOND (MEMBER VAR RENAMING)) BINDING))))))
      (DS-CLAUSE.LIT.VARIABLES POSCLAUSE WISHED.POSLITNO))
    (RDS-BINDING.PUSH BINDING)
    (MAPC
      #'(LAMBDA (LINK)
          (MAPC #'(LAMBDA (UNI) (SETQ UNIS (UNI-UNIFY.MIXED.TERMLIST UNI))) (DS-LINK.UNIFIERS LINK)))
      LINKS)
    UNIS))

