;;; -*- Package: TH-AC-MKRP; Mode: LISP; Syntax: Common-Lisp -*-

(in-package "TH-AC-MKRP")

;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND     SHORTENED FORM                  TITLE
;;;   module    ac-match                        associativ and commutative matching
;;;
;;;  VERSION
;;;
;;;  DEPENDENT SOFTWARE UNITS
;;;    SHORTENED FORM                ATTRIBUTE
;;;    e-compl*ac-functions          global-variable
;;;
;;;
;;;  DOCUMENTATION
;;;    DATASTRUCTURES
;;;      NAME                     REPRESENTATION
;;;      substitution-list        Liste von 2-elementigen Listen.
;;;                               1. Elenemt : Variablenname
;;;                               2. Element : Substitution  
;;;
;;;      normalform               Normalform eines ac-terms.
;;;                               Aufbau :
;;;                               ac-term = variable | konstante | normal-functor-ausdruck | ac-functor-ausdruck
;;;                               normal-functor-ausdruck = (functor {ac-term}* )
;;;                               ac-functor-ausdruck = (ac-functor {multi-set-entry}* )  multi-set geordnet
;;;                               multi-set-entry = (ac-term . Anzahl der Vorkommen)
;;;
;;;      
;;;      gleichungs-list          Liste von diophantischen Gleichungen.
;;;                               Elemente einer Gleichung :
;;;                               - functor : Functor der ac-Liste aus der die Gleichung entstand 
;;;                               - varlist : Liste von ac-functor-ausdruecken, deren ac-terme Variablen sind
;;;                               - arglist : Liste von multi-set-entrys
;;;
;;;                               Zugriffsfunktionen :
;;;                               (give-functor-from-gl gleichung)
;;;                               (give-varlist-from-gl gleichung)
;;;                               (give-arglist-from-gl gleichung)
;;;
;;;                               Erzeugung eines Gleichungseintrags :
;;;                               (create-gl-entry functor varlist arglist)
;;;
;;;      Liste von moeglichen     Im Rahmen des Loesens einer Menge von Gleichungen werden in einem ersten
;;;      Substitutionen fuer      Schritt alle moeglichen Substitutionen fuer eine Variable bestimmt. Diese
;;;      eine Variable            werden in dieser Datenstruktur festgehalten.
;;;                               Weiter ist die naechste Substitution aus der Datenstruktur bestimmbar.
;;;                               Elemente :
;;;                               - variablenname : Name der Variable, fuer die die Substitutionen enthalten sind
;;;                               - functor       : Functor , aus dem bei mehrfach-Auswahlen die Substitution
;;;                                                 zusammengesetzt wird
;;;                               - argumentliste : Liste der moeglichen Substitutionen
;;;                                                 Dabei gibt es zwei Typen
;;;                                                 - Listen , aus deren Eintraegen eine Substitution zusammen-
;;;                                                   gesetzt werden kann
;;;                                                 - Listen , deren Elemente die Substitution darstellen
;;;                               - composition-  : Y , wenn die Eintraege von argumentliste zusammengesetzt
;;;                                 possible            werden duerfen
;;;                                                 N , sonst
;;;
;;;                               Zugriffsfunktionen :
;;;                               (get-varname element)
;;;                               (get-functor element)
;;;                               (get-argumentlist element)
;;;                               (get-composition-possible element)
;;;
;;;                               Erzeugung eines Eintrags :
;;;                               (create-varentry var.-name functor arglist)
;;;
;;;                               Manipulation der Eintraege :
;;;                               (set-argumentlist element arglist)
;;;                               (set-composition-impossible element)
;;;                               
;;;
;;;      Argumentlisten, aus      Ein Eintrag in einer solchen Argumentliste besteht aus
;;;      denen Substitutionen     - arg    : der ac-term des  Arguments
;;;      zusammengesetzt werden   - count  : der Anzahl der Vorkommen des Terms, die erlaubt sind
;;;                               - actual : der Anzahl der Vorkommen des Terms, die momentan zur Substitution 
;;;                                          benutzt werden
;;;
;;;                               Zugriffsfunctionen :
;;;                               (get-composs-arg element)
;;;                               (get-composs-count element)
;;;                               (get-composs-actual element)
;;;
;;;                               Erzeugung eines Eintrags :
;;;                               (create-composs-arg arg count)
;;;
;;;                               Manipulation der Eintraege :
;;;                               (set-composs-count element count)
;;;                               (set-composs-actual element count)
;;;
;;;
;;;
;;;    TASK
;;;    Durch den Aufruf der Funktion ac-match mit zwei Argumenten Pattern und Term wird versucht, Term mit Pattern
;;;    zu matchen, wobei dabei die AC-Eigenschaft von manchen Funktionssymbolen der beiden Argumente beruecksichtigt
;;;    wird. Ist ein AC-match moeglich, so wird die gefundene Substitution (gesammelt in der globalen Variable
;;;    *ac-erg-subst*) als Ergebnis zurueckgegeben. Ist kein match moeglich, liefert ac-match 'nil.
;;;    Weiter stellt das Modul Funktionen zum Vergleich und zur Sortierung von AC-termen zur Verfuegung.
;;;    Dabei wurde folgende Ordnung gewaehlt :
;;;               Term1                  Term2            Ergebnis
;;;             Konstante             Konstante         lexikographischer Vergleich der Namen
;;;             Konstante             sonstiger Term    Term1 kleiner
;;;             Variable              Variable          lexikographischer Vergleich der Namen
;;;             Variable              sonstiger Term    Term1 groesser
;;;             AC-Funktion           AC-Funktion       lexikographischer Vergleich der Namen , falls gleich
;;;                                                     lexikographischer Vergleich der Argumente
;;;             AC-Funktion           sonstige Fkt.     Term1 groesser
;;;             Normale Funktion      Normale Fkt.      lexikographischer Vergleich der Namen , falls gleich
;;;                                                     lexikographischer Vergleich der Argumente
;;;
;;;
;;;
;;;    CONDITIONS
;;;    Funktionen mit variabler Stelligkeit , sowie AC-Funktionen mit 3 oder mehr Argumenten sind nicht erlaubt
;;;
;;;
;;;  DETAILED INFORMATION
;;;    REFERENCES
;;;      TITLE                                       AUTHOR
;;;
;;;
;;;    DESIGNER 
;;;    Joerg Denzinger


;;; Globale Variablen

(defvar *controll-block* 'nil)

(defvar *ac-match-nec* 'nil)
 
(defvar *ac-erg-subst* 'nil) 

(defvar *alle-loesungen* (cons '! 'nil))       

(defvar e-compl*ac-functions (list 'f 'g 'f1))  

(defvar *ac-time* 0)

(defvar *help-actime* 0)

(defvar *ac-match-count* 0)

(defvar *fail-matches* 0)

(defvar *normalisiere-time* 0)

(defvar *help-normtime* 0)

(defvar *single-match-time* 0)

(defvar *single-match-help* 0)

(defvar *great-single-match-time* 0)

;;; Macros zur Realisation der Datenstrukturen

(defmacro ac-function (func)
  `(member ,func e-compl*ac-functions))

(defmacro give-functor-from-gl (entry)
  `(car ,entry))

(defmacro give-varlist-from-gl (entry)
  `(cadr ,entry))

(defmacro give-arglist-from-gl (entry)
  `(caddr ,entry))

(defmacro create-gl-entry (functor varlist arglist)
  `(cons (list ,functor ,varlist ,arglist) 'nil))

(defmacro get-varname (varentry)
  `(car ,varentry))

(defmacro get-composition-possible (varentry)
  `(cdadr ,varentry))

(defmacro get-argumentlist (varentry)
  `(cddr ,varentry))

(defmacro get-functor (varentry)
  `(caadr ,varentry))

(defmacro create-varentry (varname functor arglist)
  `(append (list ,varname (cons ,functor 'y)) ,arglist))

(defmacro set-composition-impossible (varentry)
  `(rplacd (cadr ,varentry) 'n))

(defmacro set-argumentlist (varentry arglist)
  `(rplacd (cdr ,varentry) ,arglist))

(defmacro create-composs-arg (arg count)
  `(list 0 ,count ,arg))

(defmacro get-composs-arg (argentry)
  `(caddr ,argentry))

(defmacro get-composs-count (argentry)
  `(cadr ,argentry))

(defmacro get-composs-actual (argentry)
  `(car ,argentry))

(defmacro set-composs-actual (argentry count)
  `(rplaca ,argentry ,count))

(defmacro set-composs-count (argentry count)
  `(rplaca (cdr ,argentry) ,count))

(defmacro create-normal-arglist (arglist)
  `(nconc (cons ,arglist 'nil) ,arglist))
                                                  

;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    ac-normalform               transformiere Term in AC-term 
;;;    ----------------------------------------------------------------------------
;;;    function    merge-sort                  sortiere Listen von multi-set-entrys
;;;
;;;  VERSION
;;;  18.8.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE
;;;    termequal                    used function
;;;    termgreater                  used function
;;;    ac-match                     superior function
;;;
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Umwandlung eines normalen Terms in einen flachen AC-Term , wobei die Repraesentation eines Terms mit
;;;  AC-Functor ein geordneter Multiset ist.
;;;
;;;  CONDITIONS
;;;  Beim ersten Aufruf ist functor = nil.
;;;
;;;  INPUT                                          OUTPUT
;;;  term : ein Term in Input-form                  ein AC-term aequivalent zu term
;;;  functor : falls term ein Argument eines
;;;            groesseren Terms ist, der
;;;            Functor des gr. Terms
;;;
;;;  CATCHWORDS
;;;  flache Terme , multi-set-ordnung , normalform

(defun ac-normalform (term functor)
  (let ((func (if (consp term)
                  (car term)
                  term)))
    (cond ((atom term) (if (ac-function functor)    ;; term = Variable
                           (cons (cons term 1) 'nil)
                           term))
          ((null (cdr term)) (if (ac-function functor)  ;; term = Konstante
                                 (cons (cons term 1) 'nil)
                                 term))
          ((equal func functor)
               (if (ac-function functor)
                   (merge-sort (ac-normalform (cadr term) func)
                               (ac-normalform (caddr term) func))
                   (let ((ergterm (cons '! 'nil)))
                     (do ((restterm (cdr term) (cdr restterm)))
                         ((null restterm)
                          (cons func (cdr ergterm)))
                       (nconc ergterm (cons (ac-normalform (car restterm) func) 'nil))))))
          ((ac-function func) (if (ac-function functor)
                                  (cons (cons (cons func (merge-sort 
                                                           (ac-normalform (cadr term) func)
                                                           (ac-normalform (caddr term) func)))
                                              1)
                                        'nil)
                                  (cons func (merge-sort (ac-normalform (cadr term) func)
                                                         (ac-normalform (caddr term) func)))))
          ('t (let ((ergterm (cons '! 'nil)))
                (do ((restterm (cdr term) (cdr restterm)))
                    ((null restterm)
                     (if (ac-function functor)
                         (cons (cons (cons func (cdr ergterm)) 1) 'nil)
                         (cons func (cdr ergterm))))
                  (nconc ergterm (cons (ac-normalform (car restterm) func) 'nil))))))))





(defun merge-sort (termlist1 termlist2)
  (let ((erglist (cons '! 'nil))
        (found1 'nil)
        (found2 'nil))
    (do ((rest1 termlist1 (if found1
                              (progn (setq found1 'nil)
                                     (cdr rest1))
                              rest1))
         (rest2 termlist2 (if found2
                              (progn (setq found2 'nil)
                                     (cdr rest2))
                              rest2)))
        ((or (null rest1) (null rest2))
         (if (null rest1)
             (cdr (nconc erglist rest2))
             (cdr (nconc erglist rest1))))
      (let ((term1 (car rest1))
            (term2 (car rest2)))
        (cond ((termequal (car term1) (car term2))
                 (setq found1 (setq found2 't))
                 (nconc erglist (cons (cons (car term1) (+ (cdr term1) (cdr term2))) 'nil)))
              ((termgreater (car term1) (car term2))
                 (setq found2 't)
                 (nconc erglist (cons term2 'nil)))
              (t (setq found1 't)
                 (nconc erglist (cons term1 'nil))))))))



;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    termequal                   Gleiche Terme
;;;    function    termgreater                 Ist ein groesserer Term 
;;;    ------------------------------------------------------------------
;;;    function    check-greater-set           Vergleich eines multi-sets
;;;    function    check-greater-list          Vergleich einer Liste
;;;
;;;  VERSION
;;;  18.8.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE
;;;    check-greater-set            help function
;;;    check-greater-list           help function
;;;    merge-sort                   superior function
;;;    ac-match-set                 superior function
;;;    check-for-right-subst        superior function
;;;    streiche-X                   superior function
;;;
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Realisierung einer Ordnung auf AC-Termen.
;;;
;;;  CONDITIONS
;;;  Vor Aufruf von termgreater muss mit termequal die Gleichheit abgefangen werden.
;;;  
;;;  INPUT                                          OUTPUT
;;;  term1 , term2 : AC-Terme                       t , wenn term1 = term2 (>) , nil sonst
;;;
;;;  ALGORITHM
;;;  Rekursiver Aufruf , bis Unterschied aufgetreten. Danach Entscheidung.
;;;
;;;  CATCHWORDS
;;;  Ordnung , Gleichheit , Vergleich von AC-Termen

(defun termequal (term1 term2)
  (cond ((atom term1) (eql term1 term2))
        ((atom term2) 'nil)
        ((null term1) (null term2))
        ((null term2) 'nil)
        (t (and (termequal (car term1) (car term2))
                (termequal (cdr term1) (cdr term2))))))



(defun termgreater (term1 term2)
  (cond ((and (atom term1) (atom term2))    ;;2 Variablen
         (string> (write-to-string term1) (write-to-string term2)))
        ((atom term1) 't)
        ((atom term2) 'nil)
        ((and (null (cdr term1)) (null (cdr term2)))  ;;2 Konstanten
         (string> (write-to-string (car term1)) (write-to-string (car term2))))
        ((null (cdr term1))      ;;term1 Konstante  term2 Funktion
         'nil)
        ((null (cdr term2))      ;;term2 Konstante  term1 Funktion
         't)
        ((and (ac-function (car term1)) (ac-function (car term2)))
         (if (eq (car term1) (car term2))
             (check-greater-set (cdr term1) (cdr term2))
             (string> (write-to-string (car term1)) (write-to-string (car term2)))))
        ((ac-function (car term1)) 't)
        ((ac-function (car term2)) 'nil)
        ((eq (car term1) (car term2))
         (check-greater-list (cdr term1) (cdr term2)))
        (t (string> (write-to-string (car term1)) (write-to-string (car term2))))))



(defun check-greater-set (set1 set2)
  (let ((merke 'nil))
    (do ((restset1 set1 (cdr restset1))
         (restset2 set2 (cdr restset2)))
        ((or (not (setq merke (termequal (caar restset1) (caar restset2))))
             (not (eql (cdar restset1) (cdar restset2))))
         (if merke
             (> (cdar restset1) (cdar restset2))
             (termgreater (caar restset1) (caar restset2)))))))



(defun check-greater-list (termlist1 termlist2)
  (do ((rest1 termlist1 (cdr rest1))
       (rest2 termlist2 (cdr rest2)))
      ((not (termequal (car rest1) (car rest2)))
       (termgreater (car rest1) (car rest2)))))    




;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    ac-match                    ac-match 
;;;    ------------------------------------------------------------------------------------
;;;    function    match-arg                   ac-match fuer ein einzelnes Argument
;;;    function    ac-match-list               ac-match fuer eine Liste von Argumenten
;;;    function    ac-match-set                ac-match fuer einen multi-set von Argumenten
;;;
;;;  VERSION
;;;  18.8.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE
;;;    next-statement               used function
;;;    add-gleichung                used function
;;;
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Durchfuehrung eines AC-matches
;;;
;;;  CONDITIONS
;;;  e-compl*ac-functions muss mit allen AC-Funktionen besetzt sein.
;;;
;;;  INPUT                                          OUTPUT
;;;  pattX :Pattern-AC-Term                         'nil , wenn kein Match moeglich
;;;  arg   :AC-Term                                 't , wenn ein Match moeglich --> Liste der Substitutionen in
;;;  controll-block : noch vorzunehmende                                              ac-match
;;;     Funktions-aufrufe der 3 Funktionen
;;;
;;;  CATCHWORDS
;;;  AC-Match , Normalform , Rekursion

(defun ac-match (pattern arg)
  (setq *help-normtime* (get-internal-run-time))
  (if (or (atom pattern) (and (not (atom arg)) (eq (car pattern) (car arg))))
      (let ((pattern-nf (ac-normalform pattern 'nil))
            (arg-nf (ac-normalform arg 'nil)))
        (setq *normalisiere-time* (+ *normalisiere-time* (- (get-internal-run-time)
                                                        *help-normtime*)))
        (setq *ac-match-nec* 'nil)
        (setq *controll-block* 'nil)
        (setq *ac-erg-subst* 'nil)
        (if (match-arg pattern-nf arg-nf *controll-block*)
            (if (null *ac-erg-subst*)
                't
                *ac-erg-subst*)
            'nil))
      'nil))


(defun match-arg (pattern arg controll-block)
  (cond ((atom pattern)
         (let ((subst (substituted pattern)))
           (if subst
               (if (termequal subst (make-nf-to-arg arg))
                   (next-statement controll-block)
                   'nil)
               (progn (setq *ac-erg-subst* (append (cons (list pattern (make-nf-to-arg arg)) 'nil)
                                                   *ac-erg-subst*))
                      (next-statement controll-block)))))
        ((null (cdr pattern))
         (if (consp arg)
             (if (eq (car pattern) (car arg))
                 (next-statement controll-block)
                 'nil)
             'nil))
        ((atom arg) 'nil)
        ((null (cdr arg)) 'nil)
        (t (let ((pattfunc (car pattern))
                 (argfunc (car arg)))
             (if (not (eq pattfunc argfunc))
                 'nil
                 (if (ac-function pattfunc)
                     (ac-match-set (cdr pattern) (cdr arg) 'nil pattfunc controll-block)
                     (ac-match-list (cdr pattern) (cdr arg) controll-block)))))))



(defun ac-match-list (pattlist arglist controll-block)
  (if (null pattlist)
      (next-statement controll-block)
      (let ((pattrest (cdr pattlist))
            (argrest (cdr arglist))
            (patt (car pattlist))
            (arg (car arglist)))
        (setq *controll-block* (nconc (cons (list 'ac-match-list pattrest argrest) 'nil)
                                      *controll-block*))
        (match-arg patt arg *controll-block*))))


(defun ac-match-set (patt arg restarg functor controll-block)
  (cond ((null patt) (if (and (null arg) (null restarg))
                         (next-statement controll-block)
                         'nil))
        ((atom (caar patt)) (if (add-gleichung patt arg restarg functor)  ;; (caar patt) ist Variable
                                (next-statement controll-block)
                                'nil))
        ((null arg) 'nil)
        ((null (cdaar patt)) (cond ((termgreater (caar arg) (caar patt))  ;; (caar patt) ist Konstante
                                    'nil)
                                   ((eq (caaar patt) (caaar arg))
                                    (cond ((equal (cdar patt) (cdar arg))
                                           (ac-match-set (cdr patt) (cdr arg) restarg functor controll-block))
                                          ((> (cdar patt) (cdar arg)) 'nil)
                                          (t (ac-match-set (cdr patt) (cdr arg)
                                                           (append restarg
                                                                   (cons (cons (caar patt)
                                                                               (- (cdar arg) (cdar patt)))
                                                                         'nil))
                                                           functor controll-block))))
                                   (t (ac-match-set patt (cdr arg) (append restarg
                                                                           (cons (car arg) 'nil))
                                                    functor controll-block))))
        ((atom (caar arg))   ;; (caar arg) ist eine Variable 
         'nil) 
        ((null (cdaar arg))  ;; (caar arg) ist eine Konstante, aber (caar patt) nicht
         (ac-match-set patt (cdr arg) (append restarg
                                              (cons (car arg) 'nil))
                       functor controll-block))
        (t                ;; (car patt) == Funktion
         (cond ((and (ac-function (caaar patt)) (not (ac-function (caaar arg))))
                (ac-match-set patt (cdr arg) (append restarg
                                                     (cons (car arg) 'nil))
                              functor controll-block))
               ((and (not (ac-function (caaar patt))) (ac-function (caaar arg)))
                'nil)
               ((string> (write-to-string (caaar patt)) (write-to-string (caaar arg)))
                (ac-match-set patt (cdr arg) (append restarg
                                                     (cons (car arg) 'nil))
                              functor controll-block))
               ((string> (write-to-string (caaar arg)) (write-to-string (caaar patt)))
                'nil)                                                                                                       
               (t ;; Funktionssymbole gleich
                (let ((subst-list *ac-erg-subst*)
                      (gleichungs-list *ac-match-nec*)
                      (found 'nil))
                  (do ((rest-arglist arg (cdr rest-arglist))
                       (more-restarg 'nil (if found
                                              'nil
                                              (append more-restarg (cons (car rest-arglist) 'nil)))))
                      ((or (null rest-arglist)
                           found
                           (atom (caar rest-arglist))
                           (not (eq (caaar patt) (caaar rest-arglist))))
                        found )
                    (let ((pat (caar patt))
                          (arg (caar rest-arglist))
                          (pattcount (cdar patt))
                          (argcount (cdar rest-arglist)))
                      (if (> pattcount argcount)
                          'nil  ;kein match moeglich, naechstes argument pruefen
                          (let ((controllentry (list 'ac-match-set (cdr patt)
                                                     (append more-restarg
                                                             (if (equal pattcount argcount)
                                                                 (cdr rest-arglist)
                                                                 (append (cons (cons arg
                                                                                     (- argcount pattcount))
                                                                               'nil)
                                                                         (cdr rest-arglist))))
                                                     restarg
                                                     functor)))
                             (setq *controll-block* (nconc (cons controllentry 'nil) *controll-block*))
                             (if (match-arg pat arg *controll-block*)
                                 (setq found 't)
                                 (progn (setq *controll-block* controll-block)
                                        (setq *ac-erg-subst* subst-list)
                                        (setq *ac-match-nec* gleichungs-list)))))))))))))



;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    add-gleichung               Erstelle und speichere eine diophantische Gleichung
;;;    -------------------------------------------------------------------------------------------
;;;    function    gleichungseintrag           Erzeuge einer Eintrag fuer eine Gleichung
;;;
;;;  VERSION
;;;  18.8.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE
;;;    substituted                  used function
;;;    check-for-right-subst        used function 
;;;    check-subst-rech-possible    used function
;;;    
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Pruefung, ob eine Menge von Variablen und eine Menge von Argumenten eine nicht-triviale und nicht
;;;  zum jetzigen Zeitpunkt widerspruechliche Gleichung ergeben. Ist dies der Fall, wird diese Gleichung
;;;  in der *ac-match-nec* Liste gespeichert.
;;;
;;;  CONDITIONS 
;;;  Die Terme in pattrest, argrest, restarg sind aufsteigend geordnet.
;;;
;;;  INPUT                                          OUTPUT  
;;;  pattrest : Liste von multi-set-entrys.         'nil , falls Gleichung nicht loesbar
;;;             Entstanden aus pattern.             't , sonst
;;;  argrest, restarg : Listen von multi-set-
;;;                     entrys. Entstanden aus
;;;                     Beispiel-term.
;;;  functor : Der Funktor der Gleichung
;;;
;;;  CATCHWORDS
;;;  Diophantische Gleichung , Substitution

(defun add-gleichung (pattrest argrest restarg functor)
  (let ((subst 'nil))
    (cond ((and (null argrest) (null restarg))
           (if (null pattrest)
               't
               'nil))
          ((equal (length pattrest) 1)
           (if (setq subst (substituted (caar pattrest)))
               (check-for-right-subst subst (cdar pattrest) argrest restarg functor)
               (if (equal (cdar pattrest) 1)
                   (progn (setq *ac-erg-subst* (append (cons (list (caar pattrest)
                                                                   (make-set-to-arg functor argrest restarg))
                                                             'nil)
                                                       *ac-erg-subst*))
                          't)
                   (let ((hilfe (check-subst-rech-possible (cdar pattrest) functor (append restarg argrest))))
                     (if hilfe
                         (progn (setq *ac-erg-subst*
                                      (append (cons (list (caar pattrest) hilfe) 'nil)
                                              *ac-erg-subst*))
                                't)
                         'nil)))))
           ((minimal-count-of-args pattrest argrest restarg)
            (setq *ac-match-nec* (append (cons (gleichungseintrag pattrest functor argrest restarg)
                                               'nil)
                                         *ac-match-nec*)))
           (t 'nil))))



(defun gleichungseintrag (varlist functor argrest restarg)
  (let ((arglist (append restarg argrest)))
    (list functor varlist arglist)))


;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    make-nf-to-arg              Wandle einen AC-Term in einen normalen Term um
;;;    ----------------------------------------------------------------------------------------
;;;    function    make-set-to-arg             Wandle einen Multi-set in einen normalen Term um
;;;
;;;  VERSION
;;;  19.8.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE   
;;;
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Umformung eines AC-Terms in die externe Darstellung eines Terms (Umkehrfunktion zu ac-normalform).
;;;
;;;  INPUT                                          OUTPUT
;;;  arg : AC-Term                                  ein zu arg aequivalenter normaler Term
;;;
;;;  CATCHWORDS
;;;  Normalform , AC-Term , Term in Baumstruktur

(defun make-nf-to-arg (arg)
  (cond ((or (atom arg) (null (cdr arg))) arg)
        ((not (ac-function (car arg)))
         (let ((erg (cons (car arg) 'nil)))
           (do ((rest (cdr arg) (cdr rest)))
               ((null rest)
                erg)
             (nconc erg (cons (make-nf-to-arg (car rest)) 'nil)))))
        (t (make-set-to-arg (car arg) 'nil (cdr arg)))))




(defun make-set-to-arg (functor argrest restarg)
  (let ((erg 'nil)
        (revargrest (reverse argrest))
        (revrestarg (reverse restarg)))
    (if (null revargrest)
        (let ((printarg (make-nf-to-arg (caar revrestarg))))
          (setq erg printarg)
          (do ((zaehler 1 (+ zaehler 1)))
              ((equal zaehler (cdar revrestarg))
               't)
            (setq erg (list functor printarg erg))))
        (let ((printarg (make-nf-to-arg (caar revargrest))))
          (setq erg printarg)
               (do ((zaehler 1 (+ zaehler 1)))
                   ((equal zaehler (cdar revargrest))
                    't)
                 (setq erg (list functor printarg erg)))))
    (do ((restargrest (cdr revargrest) (cdr restargrest)))
        ((null restargrest)
         (do ((restrestarg (if (null revargrest)
                               (cdr revrestarg)
                               revrestarg)        (cdr restrestarg)))
             ((null restrestarg)
              erg)
           (let ((arg (make-nf-to-arg (caar restrestarg)))
                 (count (cdar restrestarg)))
             (do ((zaehler 0 (+ zaehler 1)))
                 ((equal zaehler count)
                  't)
               (setq erg (list functor arg erg))))))
      (let ((arg (make-nf-to-arg (caar restargrest)))
            (count (cdar restargrest)))
        (do ((zaehler 0 (+ zaehler 1)))
            ((equal zaehler count)
             't)
          (setq erg (list functor arg erg))))))) 




(defun next-statement (kontrolle)
  (if (null kontrolle)
      (loese-gleichungssys)
      (let ((statement (append (car kontrolle) (cons (cdr kontrolle) 'nil))))
        (setq *controll-block* (cdr kontrolle))
        (apply (symbol-function (car statement)) (cdr statement)))))


;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    substituted                 Ist wie substituiert?
;;;    function    check-for-right-subst       Ist so substituiert ?
;;;    function    check-subst-rech-possible   Kann so substituiert werden.
;;;
;;;  VERSION
;;;  20.10.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE      
;;;    termequal                    used function
;;;    make-nf-to-arg               used function
;;;    make-set-to-arg              user function
;;;
;;;DOCUMENTATION
;;;
;;;  TASK 
;;;  Tests von Substitutionen von Variablen.
;;;  substituted : Ist var in liste substituiert? Ergebnis : substitution
;;;  check-for-right-subst : Ist subst genau count-mal in argrest + restarg vorhanden ? -> t, nil
;;;  check-subst-rech-possible : Laesst sich aus functor und arglist eine Substitution ermitteln, die genau count-mal
;;;                              vorkommt ? -> substitution / nil
;;;
;;;  CONDITIONS
;;;
;;;  INPUT                                          OUTPUT    
;;;  siehe oben
;;;
;;;  CATCHWORDS
;;;  Substitutionen

(defun substituted (var &OPTIONAL (liste *ac-erg-subst*))
  (do ((subst-list liste (cdr subst-list)))
      ((null subst-list)
       'nil)
    (let ((subst (car subst-list)))
      (when (eq (car subst) var)
          (return (cadr subst))))))


(defun check-for-right-subst (subst count argrest restarg functor)
  (let ((arglist (append restarg argrest)))
    (if (and (equal (length arglist) 1)
             (equal (cdar arglist) 1))
        (termequal subst (make-nf-to-arg (caar arglist)))
        (termequal subst (check-subst-rech-possible count functor arglist))))) 


(defun check-subst-rech-possible (count functor arglist)
  (let ((erglist (cons '! 'nil)))
    (do ((rest arglist (cdr rest)))
        ((null rest)
         (make-set-to-arg functor (cdr erglist) 'nil))
      (let ((term (caar rest))
            (termcount (cdar rest)))
        (if (zerop (mod termcount count))
            (nconc erglist (cons (cons term (/ termcount count)) 'nil))
            (return 'nil))))))



;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    streiche-subst              Streiche Substitutionen aus einer Liste von moeglichen Argumenten
;;;    ---------------------------------------------------------------------------------------------------------
;;;    function    streiche-argumente          Entferne eine Liste von Argumenten
;;;
;;;  VERSION
;;;  18.8.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE     
;;;    termequal                    used function
;;;    termgreater                  used function
;;;    make-nf-to-arg               used function
;;;    ac-normalform                used function
;;;
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Aus einer Liste von Argumenten, die die eine Seite einer diophantischen Gleichung bilden, sind die Argumente
;;;  zu streichen, die nach Vornahme einer Substitution auch auf der anderen Seite stehen.
;;;  Falls dies moeglich ist, ist die veraenderte Liste zurueckzugeben.
;;;
;;;  INPUT                                          OUTPUT
;;;  subst : ein normaler Term                      veraenderte Liste , wenn das Streichen erfolgreich war
;;;  functor : der Functor, der in der              (kann auch nil sein)
;;;            Gleichung zu arglist steht           'keinmatch , wenn subst nicht gestrichen werden konnte
;;;  arglist : Seite einer Gleichung
;;;  count : Anzahl, wie oft subst zu loeschen
;;;          ist
;;;
;;;  CATCHWORDS
;;;  Substitution , Loesen von Gleichungen , Kuerzen einer Gleichung

(defun streiche-subst (subst functor arglist count)
  (let ((erglist (cons '! 'nil))
        (found 'nil))
    (if (or (atom subst) (not (eq (car subst) functor)))
        (do ((rest arglist (cdr rest)))
            ((or (null rest) found)
             (if found
                 (cdr (nconc erglist rest))
                 'keinmatch))
          (let ((toparg (caar rest))
                (topcount (cdar rest)))
            (cond ((atom subst)
                   (cond ((eq subst toparg)
                          (if (< topcount count)
                              (return 'keinmatch)
                              (let ((restcount (- topcount count)))
                                 (setq found 't)
                                 (when (not (zerop restcount))
                                     (nconc erglist (cons (cons toparg restcount) 'nil))))))
                         ((termgreater toparg subst) (return 'keinmatch))
                         (t (nconc erglist (cons (car rest) 'nil)))))
                  ((null (cdr subst)) ;; subst ist eine Konstante
                   (cond ((and (not (atom toparg)) (eq (car subst) (car toparg)))
                          (if (< topcount count)
                              (return 'keinmatch)
                              (let ((restcount (- topcount count)))
                                 (setq found 't)
                                 (when (not (zerop restcount))
                                     (nconc erglist (cons (cons toparg restcount) 'nil))))))
                         ((termgreater toparg subst)
                          (return 'keinmatch))
                         (t (nconc erglist (cons (car rest) 'nil))))) 
                  ((and (not (atom toparg)) (null (cdr toparg)))  ;; toparg ist eine Konstante, aber subst keine 
                   (nconc erglist (cons (car rest) 'nil)))
                  (t (if (or (atom toparg)
                             (and (or (and (ac-function (car toparg)) (ac-function (car subst)))
                                      (and (not (ac-function (car toparg))) (not (ac-function (car subst)))))
                                  (string> (write-to-string (car toparg)) (write-to-string (car subst)))))
                         (return 'keinmatch)
                         (if (eq (car subst) (car toparg))
                             (let ((argarg (make-nf-to-arg toparg)))
                               (if (termequal subst argarg)
                                   (if (< topcount count)
                                       (return 'keinmatch)
                                       (let ((restcount (- topcount count)))
                                          (setq found 't)
                                          (when (not (zerop restcount))
                                              (nconc erglist (cons (cons toparg restcount) 'nil)))))
                                   (nconc erglist (cons (car rest) 'nil))))
                             (nconc erglist (cons (car rest) 'nil))))))))
         (let ((subst-nf (ac-normalform subst 'nil)))
           (setq erglist (streiche-argumente (cdr subst-nf) arglist count))
           erglist))))


(defun streiche-argumente (welche aus wieoft)
  (let ((erglist (cons '! 'nil)) 
        (kein-match 'nil)
        (rest aus))
    (do ((gesucht welche (cdr gesucht))) 
        ((or (null gesucht) kein-match)
         (if kein-match
             'keinmatch
             (progn (nconc erglist rest)
                    (cdr erglist))))
      (let ((arg (car gesucht))
            (found 'nil)
            (ende 'nil))
        (let ((termarg (car arg))
              (termcount (* (cdr arg) wieoft)))
          (do ((restrest rest (cdr restrest)))
              ((or (null restrest) ende found)
               (if found 
                   't
                   (setq kein-match 't)))
            (let ((restarg (caar restrest))
                  (restcount (cdar restrest)))
              (cond ((termequal termarg restarg)
                     (if (> termcount restcount)
                         (progn (setq rest (cdr rest))
                                (setq ende 't))
                         (if (equal termcount restcount)
                             (progn (setq found 't)
                                    (setq rest (cdr rest)))
                             (progn (setq found 't)
                                    (setq rest (cdr rest))
                                    (nconc erglist (cons (cons restarg
                                                               (- restcount termcount))
                                                         'nil))))))
                    ((termgreater restarg termarg) (setq ende 't))
                    (t (setq rest (cdr rest))
                       (nconc erglist (cons (car restrest) 'nil)))))))))))
                                                                               
         
;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    loesche-subst-aus-gl        loesche Substitutionen aus einer Liste von diophantischen Gleichungen   
;;;
;;;  VERSION
;;;  19.8.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE
;;;    substituted                  used function
;;;    streiche-subst               used function 
;;;    check-subst-rech-possible    used function
;;;    
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Kuerzen von Gleichungen durch Streichen von Variablen, deren Substitutionen schon bekannt sind, auf der
;;;  Variablenseite , und von Argumenten, die die Substitutionen bilden, auf der anderen Seite der Gleichungen.
;;;
;;;  INPUT                                          OUTPUT
;;;  subst-list : Liste von neuen Substitutionen    'keinmatch , falls die Gleichungen durch die Substitutionen 
;;;  gleichungslist : Liste von diophantischen                   unloesbar sind
;;;                   Gleichungen                   eine Liste von gekuerzten Gleichungen sonst
;;;
;;;  CATCHWORDS
;;;  diophantische Gleichungen , Anwendung von Substitutionen

(defun loesche-subst-aus-gl (subst-list gleichungslist)
  (let ((new-subst-list 'nil) 
        (kein-match 'nil)
        (new-gl-list (cons '! 'nil)))
    (do ((rest-gl-list gleichungslist (cdr rest-gl-list)))
        ((or (null rest-gl-list) kein-match)
         (if kein-match
             'keinmatch
             (if (null new-subst-list)
                 (cdr new-gl-list)
                 (loesche-subst-aus-gl new-subst-list (cdr new-gl-list)))))
      (let ((functor (give-functor-from-gl (car rest-gl-list)))
            (varlist (give-varlist-from-gl (car rest-gl-list)))
            (arglist (give-arglist-from-gl (car rest-gl-list)))
            (new-varlist (cons '! 'nil))
            (ende 'nil))
        (do ((restvars varlist (cdr restvars)))
            ((or ende
                 (null restvars))
             (cond (ende (setq kein-match 't))
                   ((null (cdr new-varlist))
                    (if (null arglist)
                        't
                        (setq kein-match 't)))
                   ((null arglist) (setq kein-match 't))
                   ((null (cddr new-varlist))      ;; nur eine Variable uebrig
                    (let ((subst (substituted (caadr new-varlist))))
                      (if subst
                          (let ((erg (streiche-subst subst functor arglist (cdadr new-varlist))))
                            (if (null erg)
                                't
                                (setq kein-match 't)))
                          (let ((new-subst (check-subst-rech-possible (cdadr new-varlist) functor arglist)))
                            (if new-subst
                                (progn (setq *ac-erg-subst* (append (cons (list (caadr new-varlist) new-subst)
                                                                          'nil)
                                                                    *ac-erg-subst*))
                                       (setq new-subst-list (append (cons (list (caadr new-varlist) new-subst)
                                                                          'nil)
                                                                    new-subst-list)))
                                (setq kein-match 't))))))
                   (t (nconc new-gl-list (create-gl-entry functor (cdr new-varlist) arglist)))))
           (let ((var (caar restvars))
                 (varcount (cdar restvars)))
             (let ((varsubst (substituted var subst-list)))
               (if (null varsubst)
                   (nconc new-varlist (cons (car restvars) 'nil))
                   (progn (setq arglist (streiche-subst varsubst functor arglist varcount))
                          (when (eq arglist 'keinmatch)
                              (setq ende 't)))))))))))



;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE
;;;    function    check-possible-solutions    Finde moegliche Substitutionen fuer alle vorhandenen Variablen
;;;    ------------------------------------------------------------------------------------------------------
;;;    function    look-for-varsolutions       Existiert schon eine Liste fuer eine Variable
;;;    function    filter-var-solutions        Filtere eine existierende Liste
;;;    function    create-var-sol-entry        Erschaffe einen Eintrag fuer eine Variable
;;;
;;;  VERSION
;;;  19.8.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE
;;;    filter-min                   used function
;;;    combine-args-and-filter      used function
;;;    change-comp-to-normal        used function
;;;    loese-gleichungssys          superior function
;;;
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Aufbau einer Datenstruktur zur Darstellung und Verwaltung der ,bezueglich einer Menge von Gleichungen,
;;;  moeglichen Substitutionen fuer die in den Gleichungen vorkommenden Variablen.
;;;
;;;  INPUT                                          OUTPUT
;;;  gl-list : eine Liste von diophantischen        eine Liste von moeglichen Substitutionen fuer alle nicht
;;;            Gleichungen                          substituierten Variablen
;;;
;;;  CATCHWORDS
;;;  Liste von moeglichen Substitutionen , Argumentlisten fuer Substitutionen

(defun check-possible-solutions (gl-list)
 (let ((keinmatch 'nil))
  (let ((var-list-of-solutions (cons '! 'nil)))
    (do ((rest-gl-list gl-list (cdr rest-gl-list)))
        ((null rest-gl-list)
         (if keinmatch
             'nil
             (cdr var-list-of-solutions)))
      (let ((functor (give-functor-from-gl (car rest-gl-list)))
            (varlist (give-varlist-from-gl (car rest-gl-list)))
            (arglist (give-arglist-from-gl (car rest-gl-list)))
            (ende 'nil))
        (do ((restvars varlist (cdr restvars)))
            ((null restvars)
             (when ende 
                 (setq keinmatch 't)))
          (let ((var (caar restvars))
                (varcount (cdar restvars)))
            (let ((varentry (look-for-varsolutions var var-list-of-solutions)))
              (if varentry
                  (when (not (filter-var-solutions varentry varcount arglist functor))
                      (setq ende 't))
                  (when (not (create-var-sol-entry var varcount var-list-of-solutions functor arglist))
                      (setq ende 't)))))))))))




(defun look-for-varsolutions (name list)
  (do ((rest-list (cdr list) (cdr rest-list)))
      ((or (null rest-list)
           (eq name (get-varname (car rest-list))))
       (if rest-list
           (car rest-list)
           'nil))))



(defun filter-var-solutions (varentry count arglist functor)
  (let ((var-arglist (get-argumentlist varentry))
        (var-functor (get-functor varentry)))
    (if (eq functor var-functor)
        (if (eq (get-composition-possible varentry) 'y)
            (filter-min var-arglist arglist count varentry)
            (combine-args-and-filter var-arglist functor arglist count))
        (if (eq (get-composition-possible varentry) 'y)
            (progn (set-composition-impossible varentry)
                   (change-comp-to-normal var-arglist var-functor functor arglist count varentry))
            (combine-args-and-filter var-arglist functor arglist count)))))


(defun combine-args-and-filter (old-arglist new-functor new-arglist count)
  (let ((new-arg-rest new-arglist)
        (found 'nil))
    (do ((rest-old (cdr old-arglist) (cdr rest-old))
         (pre-rest old-arglist (if found
                                   (progn (setq found 'nil)
                                          pre-rest)
                                   (cdr pre-rest))))
        ((null rest-old)
         (if (null (cdr old-arglist))
             'nil
             (rplaca old-arglist (cdr old-arglist)))) 
      (let ((arg (car rest-old)))
        (if (and (not (atom arg)) (eq (car arg) new-functor))
            (if (check-for-enthalten arg count new-arglist)
                't
                (progn (setq found 't)
                       (rplacd pre-rest (cddr pre-rest))))
            (let ((remove 'nil)
                  (halt 'nil))
              (do ((rest-new new-arg-rest (cdr rest-new)))
                  ((or remove
                       halt
                       (null rest-new))
                   (when remove
                       (rplacd pre-rest (cddr pre-rest))
                       (setq found 't))
                   )
                (let ((new-arg (caar rest-new))
                      (new-count (cdar rest-new)))
                  (cond ((termequal arg new-arg)
                         (if (< new-count count)
                             (setq remove 't)
                             (setq halt 't))
                         (setq new-arg-rest (cdr new-arg-rest)))
                        ((termgreater arg new-arg) (setq new-arg-rest (cdr new-arg-rest)))
                        (t (setq remove 't)
                           ))
                  ))))))))



(defun filter-min (old-arglist new-arglist count varentry)
  (let ((erg-arglist (cons '! 'nil))
        (new-rest new-arglist))
    (do ((old-rest old-arglist (cdr old-rest)))
        ((null old-rest)
         (if (null (cdr erg-arglist))
             'nil
             (set-argumentlist varentry (cdr erg-arglist))))
      (let ((old-arg (get-composs-arg (car old-rest)))
            (old-count (get-composs-count (car old-rest)))
            (found 'nil))
        (do ((rest-new new-rest (cdr rest-new)))
            ((or found
                 (null rest-new))
             't)
          (let ((new-arg (caar rest-new))
                (new-count (cdar rest-new)))
            (cond ((termequal old-arg new-arg)
                   (if (> new-count (* old-count count))
                       (nconc erg-arglist (cons (car old-rest) 'nil))
                       (let ((restcount (mod new-count count)))
                         (let ((real-new-count (/ (- new-count restcount) count)))
                           (when (not (zerop real-new-count))
                               (set-composs-count (car old-rest) real-new-count)
                               (nconc erg-arglist (cons (car old-rest) 'nil))))))
                   (setq found 't)
                   (setq new-rest (cdr new-rest)))
                  ((termgreater old-arg new-arg)
                   (setq new-rest (cdr new-rest)))
                  (t (setq found 't))))))))) 



(defun change-comp-to-normal (old-arglist old-functor new-functor new-arglist count varentry)
  (let ((new-var-list (cons '! 'nil))
        (rest-new new-arglist))
    (do ((old-rest old-arglist (cdr old-rest)))
        ((null old-rest)
         (when (not (null rest-new))
             (do ((new-rest rest-new (cdr new-rest)))
                 ((null new-rest)
                  't)
               (let ((new-arg (caar new-rest))
                     (new-count (cdar new-rest)))
                 (when (and (not (atom new-arg)) (eq (car new-arg) old-functor)
                            (not (< new-count count))
                            (check-for-enthalten-composs new-arg 1 old-arglist))
                     (nconc new-var-list (cons new-arg 'nil))))))
         (if (null (cdr new-var-list))
             'nil
             (set-argumentlist varentry (cons (cdr new-var-list) (cdr new-var-list)))))
      (let ((old-arg (get-composs-arg (car old-rest)))
            (found 'nil))
        (if (and (not (atom old-arg)) (eq (car old-arg) new-functor))
            (when (check-for-enthalten old-arg count new-arglist)
                (nconc new-var-list (cons old-arg 'nil)))
            (do ((new-rest rest-new (cdr new-rest)))
                ((or found
                     (null new-rest))
                 't)
              (let ((new-arg (caar new-rest))
                    (new-count (cdar new-rest)))
                (if (and (not (atom new-arg)) (eq (car new-arg) old-functor))
                    (progn (if (and (not (< new-count count))
                                    (check-for-enthalten-composs new-arg 1 old-arglist))
                               (nconc new-var-list (cons new-arg 'nil)))
                           (setq rest-new (cdr rest-new)))
                    (cond ((termequal old-arg new-arg)
                           (when (not (< new-count count))
                               (nconc new-var-list (cons new-arg 'nil)))
                           (setq found 't)
                           (setq rest-new (cdr rest-new)))
                          ((termgreater old-arg new-arg)
                           (setq rest-new (cdr rest-new)))
                          (t (setq found 't)))))))))))



;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE 
;;;    function    next-composs-argument       naechste moegliche Substitution bei Kompossition
;;;    function    next-normal-argument        naechste moegliche Substitution ohne Kompossition
;;;    -----------------------------------------------------------------------------------------
;;;    function    read-var-subst              erzeuge bei Kompossition die Substitution
;;;
;;;  VERSION
;;;  20.10.87
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE  
;;;    make-set-to-arg              used function
;;;    get-composs-actual           used macro
;;;    get-composs-count            used macro
;;;    set-composs-actual           used macro
;;;
;;;DOCUMENTATION
;;;
;;;  TASK
;;;
;;;  INPUT                                          OUTPUT
;;;
;;;  ALGORITHM
;;;
;;;  CATCHWORDS
;;;

(defun next-composs-argument (arglists functor)
  (let ((found 'nil))
    (do ((rest arglists (cdr rest)))
        ((or found
             (null rest))
         (if found
             (make-set-to-arg functor (read-var-subst arglists) 'nil)
             'nil))
      (let ((composs-arg (car rest)))
        (if (equal (get-composs-actual composs-arg) (get-composs-count composs-arg))
            (set-composs-actual composs-arg 0)
            (progn (setq found 't)
                   (set-composs-actual composs-arg (+ 1 (get-composs-actual composs-arg)))))))))


(defun read-var-subst (arglists)
  (let ((erglist (cons '! 'nil)))
    (do ((rest arglists (cdr rest)))
        ((null rest)
         (cdr erglist))
      (when (not (zerop (get-composs-actual (car rest))))
          (nconc erglist (cons (cons (get-composs-arg (car rest))
                                     (get-composs-actual (car rest)))
                               'nil))))))


(defun next-normal-argument (arglist functor)
  (let ((new-arg (make-nf-to-arg (caar arglist))))
     (rplaca arglist (cdar arglist))
     new-arg))


(defun create-var-sol-entry (var varcount var-list-of-solutions functor arglist)
  (let ((new-arglist (cons '! 'nil)))
    (do ((rest arglist (cdr rest)))
        ((null rest)
         (if (null (cdr new-arglist))
             'nil
             (let ((varentry (create-varentry var functor (cdr new-arglist))))
               (insert-varentry var-list-of-solutions varentry var)
               't)))
      (let ((arg (caar rest))
            (argcount (cdar rest)))
        (let ((realcount (if (> varcount argcount)
                             0
                             (let ((remrest (mod argcount varcount)))
                               (/ (- argcount remrest) varcount))))) 
          (when (not (zerop realcount))
              (nconc new-arglist (cons (create-composs-arg arg realcount) 'nil))))))))



(defun insert-varentry (in-list entry varname)
  (do ((rest (cdr in-list) (cdr rest))
       (pre-rest in-list (cdr pre-rest)))
      ((or (null rest)
           (termgreater (get-varname (car rest)) varname))
       (if (null rest)
           (nconc in-list (cons entry 'nil))
           (let ((new-rest (nconc (cons entry 'nil) rest)))
             (rplacd pre-rest new-rest))))))



(defun check-for-enthalten (argument count new-arglist)
  (let ((rest-new new-arglist)
        (stop 'nil))
    (do ((rest (cdr argument) (cdr rest)))
        ((or stop
             (null rest))
         (if stop
             'nil
             't))
      (let ((arg (caar rest))
            (realcount (* (cdar rest) count))
            (found 'nil))
        (do ((new-rest rest-new (cdr new-rest)))
            ((or (null new-rest)
                 found
                 stop)
             (when (not found)
                 (return 'nil)))
          (let ((restarg (caar new-rest))
                (restcount (cdar new-rest)))
            (cond ((termequal arg restarg)
                   (if (< restcount realcount)
                       (setq stop 't)
                       (setq found 't)))
                  ((termgreater restarg arg) (setq stop 't))
                  (t 't))
            (setq rest-new (cdr rest-new))))))))



(defun check-for-enthalten-composs (argument count new-arglist)
  (let ((rest-new new-arglist)
        (stop 'nil))
    (do ((rest (cdr argument) (cdr rest)))
        ((or stop
             (null rest))
         (if stop
             'nil
             't))
      (let ((arg (caar rest))
            (realcount (* (cdar rest) count))
            (found 'nil))
        (do ((new-rest rest-new (cdr new-rest)))
            ((or (null new-rest)
                 found
                 stop)
             (when (not found)
                 (setq stop 't)))
          (let ((restarg (get-composs-arg (car new-rest)))
                (restcount (get-composs-count (car new-rest))))
            (cond ((termequal arg restarg)
                   (if (< restcount realcount)
                       (setq stop 't)
                       (setq found 't)))
                  ((termgreater restarg arg) (setq stop 't))
                  (t 't))
            (setq rest-new (cdr rest-new))))))))



;;;IDENTIFICATION
;;;  SOFTWARE PRODUCT
;;;    KIND        SHORTENED FORM              TITLE 
;;;    function    loese-gleichungssys-normal  Loese ein Gleichungssystem (eine Loesung)
;;;    function    loese-gleichungssys-alle    Loese ein Gleichungssystem (alle Loesungen)
;;;    function    probiere-loesungen-normal   Probiere alle moeglichen Loesungen einer Menge von Gleichungen
;;;                                            (bis eine Loesung gefunden ist)
;;;    function    probiere-loesungen-alle     Probiere alle moeglichen Loesungen einer Menge von Gleichungen
;;;                                            (bis alle tatsaechlichen Loesungen gefunden sind)
;;;    ------------------------------------------------------------------------------------------------------
;;;    function    check-for-not-subst         Sind alle Variablen substituiert ?
;;;
;;;  VERSION
;;;  23.6.88
;;;
;;;  DEPENDANT SOFTWARE UNITS
;;;    SHORTENED FORM               ATTRIBUTE
;;;    substituted                  used function
;;;    loesche-subst-aus-gl         used function
;;;
;;;DOCUMENTATION
;;;
;;;  TASK
;;;  Bestimmen einer Loesung (= AC-Match) fuer eine Menge von diophantischen Gleichungen.  
;;;
;;;  CONDITIONS
;;;  Den Funktionen loese-gleichungssys und probiere-loesungen wird entweder die Funktion mit dem
;;;  Suffix "normal" (beiden zugleich) oder mit dem Suffix "alle" zugeordnet.
;;;  Initial ist der Suffix "normal" gesetzt. Werden alle Loesungen gesucht (Funktion ac-match-alle),
;;;  So wird dort automatisch der Suffix "alle" gesetzt. Das kann durch Aufruf der Funktion 
;;;  schalte-normal wieder rueckgaengig gemacht werden.
;;;
;;;  INPUT                                          OUTPUT
;;;  var-list-of-solutions : Liste aller moegl.     't , falls eine Loesung existiert   
;;;       Substitutionen aller Variablen            'nil , sonst
;;;  gl-list : Liste von diophantischen Gleichungen                                     
;;;
;;;  ALGORITHM
;;;
;;;  CATCHWORDS
;;;  Loesen von diophantischen Gleichungen , Ausprobieren , Filtern von Gleichungen

(defun loese-gleichungssys-normal ()
  (if (null *ac-match-nec*)
      't
      (let ((gleichungslist (loesche-subst-aus-gl *ac-erg-subst* *ac-match-nec*)))
        (cond ((eq gleichungslist 'keinmatch) 'nil)
              ((null gleichungslist) 't)
              ((check-for-linear-terms gleichungslist)
               (loese-linear-terms gleichungslist))
              (t (let ((var-list-of-solutions (find-good-order (check-possible-solutions gleichungslist))))
                   (probiere-loesungen var-list-of-solutions gleichungslist)))))))

(defun loese-gleichungssys-alle ()
  (if (null *ac-match-nec*)
      (rette-loesung)
      (let ((gleichungslist (loesche-subst-aus-gl *ac-erg-subst* *ac-match-nec*)))
        (cond ((eq gleichungslist 'keinmatch) 'nil)
              ((null gleichungslist) (rette-loesung))
              (t (let ((var-list-of-solutions (find-good-order (check-possible-solutions gleichungslist))))
                   (probiere-loesungen var-list-of-solutions gleichungslist)))))))



(defun probiere-loesungen-normal (var-list-of-solutions gl-list)
  (if (null var-list-of-solutions)
      (if (null gl-list)
          't
          'nil)
      (if (null gl-list)
          (check-for-not-subst var-list-of-solutions)
          (let ((arglist (get-argumentlist (car var-list-of-solutions)))
                (varname (get-varname (car var-list-of-solutions)))
                (functor (get-functor (car var-list-of-solutions)))
                (composs-possible (get-composition-possible (car var-list-of-solutions))))
            (if (substituted varname)
                (probiere-loesungen (cdr var-list-of-solutions) gl-list)
                (let ((subst-list *ac-erg-subst*)
                      (found 'nil)
                      (suchfunction (if (eq composs-possible 'y)
                                        'next-composs-argument 
                                        (progn (rplaca arglist (cdr arglist))
                                               'next-normal-argument))))
                  (do ((new-subst (funcall (symbol-function suchfunction) arglist functor) 
                                  (funcall (symbol-function suchfunction) arglist functor)))
                      ((or (null new-subst)
                           found)
                       (if found
                           't
                           'nil))
                    (let ((new-gl-list (loesche-subst-aus-gl (cons (list varname new-subst) 'nil) gl-list)))
                      (if (eq new-gl-list 'keinmatch)
                          (setq *ac-erg-subst* subst-list)
                          (progn 
                             (setq *ac-erg-subst* (append (cons (list varname new-subst) 'nil) *ac-erg-subst*))
                             (if (probiere-loesungen (cdr var-list-of-solutions) new-gl-list)
                                 (setq found 't)
                                 (setq *ac-erg-subst* subst-list))))))))))))


(defun probiere-loesungen-alle (var-list-of-solutions gl-list)
  (if (null var-list-of-solutions)
      (if (null gl-list)
          (rette-loesung)
          'nil)
      (if (null gl-list)
          (if (check-for-not-subst var-list-of-solutions)
              (rette-loesung)
              'nil)
          (let ((arglist (get-argumentlist (car var-list-of-solutions)))
                (varname (get-varname (car var-list-of-solutions)))
                (functor (get-functor (car var-list-of-solutions)))
                (composs-possible (get-composition-possible (car var-list-of-solutions))))
            (if (substituted varname)
                (probiere-loesungen (cdr var-list-of-solutions) gl-list)
                (let ((subst-list *ac-erg-subst*)
                      (found 'nil)
                      (suchfunction (if (eq composs-possible 'y)
                                        'next-composs-argument 
                                        (progn (rplaca arglist (cdr arglist))
                                               'next-normal-argument))))
                  (do ((new-subst (funcall (symbol-function suchfunction) arglist functor) 
                                  (funcall (symbol-function suchfunction) arglist functor)))
                      ((or (null new-subst)
                           found)
                       (if found
                           (rette-loesung)
                           'nil))
                    (let ((new-gl-list (loesche-subst-aus-gl (cons (list varname new-subst) 'nil) gl-list)))
                      (if (eq new-gl-list 'keinmatch)
                          (setq *ac-erg-subst* subst-list)
                          (progn 
                             (setq *ac-erg-subst* (append (cons (list varname new-subst) 'nil) *ac-erg-subst*))
                             (if (probiere-loesungen (cdr var-list-of-solutions) new-gl-list)
                                 (setq found 't)
                                 (setq *ac-erg-subst* subst-list))))))))))))




(defun check-for-not-subst (varlist)
  (do ((rest-list varlist (cdr rest-list)))
      ((null rest-list)
       't)
    (let ((varname (get-varname (car rest-list))))
      (when (not (substituted varname))
          (return 'nil)))))


(defun find-good-order (var-list-of-solutions)
  (let ((composs-list (cons '! 'nil))
        (normal-list (cons '! 'nil)))
    (do ((rest-list var-list-of-solutions (cdr rest-list)))
        ((null rest-list)
         (let ((erg (cons '! 'nil)))
           (do ((rest (cdr normal-list) (cdr rest)))
               ((null rest)
                't)
             (nconc erg (cons (cdar rest) 'nil)))
           (do ((rest (cdr composs-list) (cdr rest)))
               ((null rest)
                't)
             (nconc erg (cons (cdar rest) 'nil)))
           (cdr erg)))
       (if (eq (get-composition-possible (car rest-list)) 'y)
           (let ((found 'nil)
                 (count (get-composs-value (get-argumentlist (car rest-list)))))
              (do ((search-list (cdr composs-list) (cdr search-list))
                   (pre-list composs-list search-list))
                  ((or found (null search-list))
                   (when (not found)
                       (nconc composs-list (cons (cons count (car rest-list)) 'nil))))
                (when (> (caar search-list) count)
                    (setq found 't)
                    (let ((rest-rest (nconc (cons (cons count (car rest-list)) 'nil) search-list)))
                      (rplacd pre-list rest-rest)))))
            (let ((found 'nil)
                 (count (length (cdr (get-argumentlist (car rest-list))))))
              (do ((search-list (cdr normal-list) (cdr search-list))
                   (pre-list normal-list search-list))
                  ((or found (null search-list))
                   (when (not found)
                       (nconc normal-list (cons (cons count (car rest-list)) 'nil))))
                (when (> (caar search-list) count)
                    (setq found 't)
                    (let ((rest-rest (nconc (cons (cons count (car rest-list)) 'nil) search-list)))
                      (rplacd pre-list rest-rest)))))))))

(defun check-for-linear-terms (gleichungslist)
  (let ((variablenlist (cons '! 'nil))
        (ende 'nil))
    (do ((restgleichungen gleichungslist (cdr restgleichungen)))
        ((or (null restgleichungen) ende)
         (if ende
             'nil
             't))
      (let ((varlist (give-varlist-from-gl (car restgleichungen))))
        (do ((restvars varlist (cdr restvars)))
            ((or (null restvars) ende)
             't)
          (let ((var (caar restvars)))
            (if (or (member var variablenlist) (not (equal (cdar restvars) 1)))
                (setq ende 't)
                (nconc variablenlist (cons var 'nil)))))))))

(defun loese-linear-terms (gleichungslist)
  (do ((restgleich gleichungslist (cdr restgleich))
       (ende 'nil))
      ((or (null restgleich) ende)
       (if ende
           'nil
           't))
    (let ((gleichung (car restgleich)))
      (let ((varlist (give-varlist-from-gl gleichung))
            (arglist (give-arglist-from-gl gleichung)))
        (if (not (minimal-count-of-args varlist arglist 'nil))
            (setq ende 't)
            (let ((restargs arglist))
              (do ((restvars varlist (cdr restvars)))
                  ((null (cdr restvars))
                   (setq *ac-erg-subst* (append (cons (list (caar restvars)
                                                            (make-set-to-arg (give-functor-from-gl gleichung)
                                                                             restargs 'nil))
                                                      'nil)
                                                *ac-erg-subst*)))
                (let ((var (caar restvars)))
                  (setq *ac-erg-subst* (append (cons (list var
                                                           (make-nf-to-arg (caar restargs)))
                                                     'nil)
                                               *ac-erg-subst*))
                  (if (equal (cdar restargs) 1)
                      (setq restargs (cdr restargs))
                      (setq restargs (append (cons (cons (caar restargs)
                                                         (- (cdar restargs) 1))
                                                   'nil)          (cdr restargs))))))))))))   

(defun minimal-count-of-args (pattrest argrest restarg)
  (let ((pattcount (count-minimal-args pattrest))
        (argcount (count-minimal-args (if (null restarg)
                                          argrest
                                          (append restarg argrest)))))
    (if (> pattcount argcount)
        'nil
        't)))



(defun count-minimal-args (arglist)
  (let ((count 0))
    (do ((restarg arglist (cdr restarg)))
        ((null restarg)
         count)
      (setq count (+ count (cdar restarg))))))




(defun rette-loesung ()
  (let ((loesung (cons '! 'nil)))
    (do ((restlist *ac-erg-subst* (cdr restlist)))
        ((null restlist)
         (when (not (null (cdr loesung)))
            (nconc *alle-loesungen* (cons (cdr loesung )'nil)))
         'nil)
      (nconc loesung (cons (car restlist) 'nil)))))


(defun ac-match-alle (pattern arg)
  (setq *alle-loesungen* (cons '! 'nil))
  (setf (symbol-function 'loese-gleichungssys) (symbol-function 'loese-gleichungssys-alle))
  (setf (symbol-function 'probiere-loesungen) (symbol-function 'probiere-loesungen-alle))
  (ac-match pattern arg)
  (do ((rest (cdr *alle-loesungen*) (cdr rest))
       (count 1 (+ count 1)))
      ((null rest)
       (if (null (cdr *alle-loesungen*))
           'nil
           't))
    (format t "~%~s. Loesung : ~%~s" count (car rest))))


(defun get-composs-value (arg-list)
  (let ((value 1))
    (do ((rest-list arg-list (cdr rest-list)))
        ((null rest-list)
         value)
      (setq value (* value (get-composs-count (car rest-list)))))))

           

 (defun su=ac-match (patt arg)
  (setq *ac-match-count* (+ *ac-match-count* 1))
  (setq *help-actime* (get-internal-run-time))
  (setq *single-match-help* (get-internal-run-time))
  (let ((erg (ac-match patt arg)))
    (setq *ac-time* (+ *ac-time* (- (get-internal-run-time) *help-actime*)))
    (setq *single-match-time* (- (get-internal-run-time) *single-match-help*))
    (match-statistik)
    (cond ((null erg) (setq *fail-matches* (+ *fail-matches* 1))
                      'fail)
          ((atom erg) 'nil)
          (t (convert-subst-to-adler erg)))))


(defun convert-subst-to-adler (substlist)
  (if (null substlist)
      'nil
      (let ((resterg (convert-subst-to-adler (cdr substlist)))
            (vorne (car substlist)))
        (let ((new-erg (cons (car vorne) (cadr vorne))))
          (append (cons new-erg 'nil) resterg)))))  

(defun match-statistik ()
  (when (> *single-match-time* *great-single-match-time*)
     (setq *great-single-match-time* *single-match-time*)))


                  
(defun schalte-normal ()
  (setf (symbol-function 'loese-gleichungssys) (symbol-function 'loese-gleichungssys-normal))
  (setf (symbol-function 'probiere-loesungen) (symbol-function 'probiere-loesungen-normal)))                                                                                

(schalte-normal)


     
