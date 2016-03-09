;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

(defvar ord*pol_limit 10000000)

(defun ord-pol_set.limit (n)
  (setq ord*pol_limit n))



(defvar ord*pol_default 1)

(defun ord=pol_default (arity)
						; Edited:  15-FEB-1991 21:48
						; Authors: PRCKLN
						; Input:   An integer 0<=ARITY
						; Effect:  -
						; Value:   1. A list of different symbols with length equal ARITY.
						;          2. An arithmetic expression using +, *, integers, and the
						;             symbols in the first value.
  (let ((variables (do* ((n arity (1- n))
			 (l nil (cons (intern (format nil "X~A" n) (find-package "MKRP")) l)))
			((zerop n) l))))
    (values variables (list* '+ (incf ord*pol_default) variables))))


(defun ord=pol_greater (term1 term2) 
						; Edited:  12-OCT-1990 21:27
						; Authors: PRCKLN
						; Input:   Two terms.
						; Effect:  -
						; Value:   True iff TERM1 > TERM2 in the ordering defined in options.
  (tr=pol term1 term2))



;;; Steinbach


;--------------------------------------------------------------------------------------------------------------------------------
;    Definition of the data type "Monomial in NAT"
;--------------------------------------------------------------------------------------------------------------------------------

(defstruct (monomial  (:conc-name monom-)
                      (:copier nil))

  (coeff 1 :type fixnum)

  (exponent (make-array 0 :element-type 'fixnum) :type (vector fixnum)))



(defun copy-monomial (m)

  (declare (type monomial m))

;;; This function generates a complete copy of a monomial

  (make-monomial :coeff (monom-coeff m) :exponent (copy-seq (monom-exponent m))))



(defun monom-lex-order (m1 m2)

  (declare (type monomial m1 m2))

;;; This predicate compares two monomials (of the same "type") using the lexicographic ordering on their exponent vectors.

  (let ((pos (mismatch (monom-exponent m1) (monom-exponent m2))))
    
    (when pos
      (> (aref (monom-exponent m1) pos) (aref (monom-exponent m2) pos)))))



(defun exponent-nat-order (exp1 exp2)

   (declare (type simple-vector exp1 exp2))

;;; This predicate compares two exponent vectors (of the same "type") using the "natural" ordering.

  (dotimes (i (length exp1) t)

     (when (< (aref exp1 i) (aref exp2 i))
           (return nil))))


;--------------------------------------------------------------------------------------------------------------------------------
;    Definition of the data type "Polynomial in NAT"
;--------------------------------------------------------------------------------------------------------------------------------

(defstruct (polynomial  (:conc-name poly-)
                        (:copier nil))

  (variables nil :type list)
;;;  actually a list of strings denoting variables

  (monomials nil :type list))
;;;  actually a non-empty and ordered list of monomials the exponents of which "agree" with
;;;  the list of variables



(defun make-constant (c variables)

   (declare (type fixnum c)
            (type list variables))

;;; This function returns the constant polynomial <c>

  (make-polynomial :variables variables
                   :monomials (list (make-monomial :coeff c
                                                   :exponent (make-array (length variables)
                                                                         :element-type 'fixnum
                                                                         :initial-element 0)))))

                   
(defun copy-polynomial (p)

  (declare (type polynomial p))

;;; This function generates a complete copy of a polynomial

  (make-polynomial  :variables  (poly-variables p)
                    :monomials  (mapcar #'copy-monomial (poly-monomials p))))



(defun max-degree (var-pos p)

   (declare (type fixnum var-pos)
            (type polynomial p))

;;; This function determines the greatest exponent of the variable at position <var-pos>
;;; in (poly-variables <p>) in any monomial of <p>.
;;; PRECONDITION :  0 <= <var-pos> < (length (poly-variables <p>))

   (let ((l (poly-monomials p)))

     (if (zerop var-pos)                         ; first variable
         (aref (monom-exponent (first l)) 0)     ;  --> find max-degree in the first component of the first monomial

         (let ((max 0))                          ; else test all monomials at <var-pos> to find max-degree
              (declare (type fixnum max))

           (dolist (m (poly-monomials p) max)
                   (declare (type monomial m))

              (when (> (aref (monom-exponent m) var-pos) max)
                    (setq max (aref (monom-exponent m) var-pos))))))))



(defun min-var-polynomial (p &optional (ret-polynomial t))

   (declare (type polynomial p))

;;; This function determines the variables that actually have a non-zero exponent in any monomial of <p>.
;;; Using this minimal set of variables the new representation of <p> is computed and returned.
;;; If <p> turns out to be a constant then its integer value is returned.
;;; If <ret-polynomial> is NIL then only the minimal set of variables is returned.
;;; PRECONDITION :  (not (null (poly-variables <p>)))

   (let* ((new-vars nil)
          (len-vars (length (poly-variables p)))
          (a (make-array len-vars :element-type t :initial-element nil)))
         (declare (type fixnum len-vars))

;;; determine the minimal set of variables in <p>

     (dotimes (i len-vars)                           ; check each variable for occurence in <p>
              (declare (type fixnum i))

        (setf (aref a i)                             ; a[i] = t  iff  "i-th variable occurs in <p>"

              (dolist (m (poly-monomials p))
                      (declare (type monomial m))

                 (unless (zerop (aref (monom-exponent m) i))
                         (return t))))

        (when (aref a i)
              (setq new-vars (append new-vars (list (nth i (poly-variables p)))))))

     (unless ret-polynomial (return-from min-var-polynomial new-vars))

;;; compute the new representation of <p> w.r.t. <new-vars>

     (let ((len-new-vars (length new-vars)))
          (declare (type fixnum len-new-vars))

       (cond ((= len-vars len-new-vars) p)           ; (poly-variables <p>) is minimal

             ((null new-vars)                        ; <p> is actually a constant
              (monom-coeff (first (poly-monomials p))))

             (t                                      ; else create reduced representation of <p>
                (make-polynomial
                   :variables new-vars
                   :monomials (mapcar
                                #'(lambda (m)
                                          (declare (type monomial m))

                                    (let* ((j 0)
                                           (m1 (make-monomial :coeff (monom-coeff m)
                                                  :exponent (make-array len-new-vars :element-type 'fixnum
                                                                        :initial-element 0)))
                                           (exp-old (monom-exponent m))
                                           (exp-new (monom-exponent m1)))

                                       (dotimes (i len-vars)      ; conditional copying of <exp-old> into <exp-new>
                                         (when (aref a i)
                                               (setf (aref exp-new j) (aref exp-old i))
                                               (incf j)))

                                       m1))

                                (poly-monomials p))))))))


                     
(defmacro app-elem (l new-elem)

;;; This macro destructively adds <new-elem> to <l> at the right end of <l>. The CAR of <l> must
;;; point to the last CONS-cell in <l>.

  `(setf (cdar ,l) (cons ,new-elem nil)
         (car ,l) (cdar ,l)))

       

(defun polynomial-to-string (p)

   (declare (type polynomial p))

;;; This function computes a human readable representation of the polynomial <p> as a string

   (let ((v (poly-variables p))
         (l (length (poly-variables p)))
         (first-monomial t) 
         string-list)

     (setf string-list (cons nil nil)
           (car string-list) string-list)

     (map 
       nil                                                       ; for each monomial do lambda-expression
       #'(lambda (m)                                             ; i.e. print monomial

            (let ((first-var t)
                  (a (monom-exponent m))
                  (c (monom-coeff m)))
                 (declare (type fixnum c))

               (unless first-monomial (app-elem string-list "  +  "))
               (when (or (/= c 1)                                ; print coefficient of monomial
                         (zerop (reduce #'+ a)))
                     (app-elem string-list (if (< c 0)
                                               (format nil "(~d)" c)
                                               (prin1-to-string c)))
                     (app-elem string-list " "))
               (when first-monomial (setq first-monomial nil))

               (dotimes (i l)                                    ; print exponent of monomial
                   (let ((expo (aref a i)))
                     (when (plusp expo)
                           (unless first-var (app-elem string-list " ")) ; "*" also possible
                           (app-elem string-list (string-downcase (elt v i)))
                           (when first-var (setq first-var nil))
                           (when (> expo 1)
                                 (app-elem string-list "^")
                                 (app-elem string-list (prin1-to-string expo))))))))

       (poly-monomials p))
               
     (apply #'concatenate 'string (cdr string-list))))



(defun print-polynomial (p)

   (declare (type polynomial p))

;;; This function prints a readable representation of the polynomial <p> to *standard-output*.

   (write-string (polynomial-to-string p))
   (terpri))


(defun vc-print-polynomial (p)

  (declare (type vc-polynomial p))

;;; This function prints a readable representation of the polynomial with variable coefficients <p>
;;; to *standard-output*.

   (let* ((v (vc-poly-variables p))
          (l (length v))
          (first-monomial t) 
          string-list)

     (setf string-list (cons nil nil)
           (car string-list) string-list)

     (map 
       nil                                                       ; for each monomial do lambda-expression
       #'(lambda (m)                                             ; i.e. print monomial
                 (declare (type vc-monomial m))
            (let ((first-var t)
                  (a (vc-monom-exponent m))
                  (coeff-poly (vc-monom-coeff-poly m)))
                 (declare (type polynomial coeff-poly))

               (unless first-monomial (app-elem string-list "  +  "))
               (if (/= (length (poly-monomials coeff-poly)) 1)   ; print coefficient which is a polynomial
                   (progn
                      (app-elem string-list "(")
                      (app-elem string-list (polynomial-to-string coeff-poly))
                      (app-elem string-list ")"))
                   (app-elem string-list (polynomial-to-string coeff-poly)))
               (app-elem string-list " ")

               (when first-monomial (setq first-monomial nil))

               (dotimes (i l)                                    ; print exponent of monomial
                   (let ((expo (aref a i)))
                     (when (plusp expo)
                           (unless first-var (app-elem string-list " ")) ; "*" also possible
                           (app-elem string-list (string-downcase (elt v i)))
                           (when first-var (setq first-var nil))
                           (when (> expo 1)
                                 (app-elem string-list "^")
                                 (app-elem string-list (prin1-to-string expo))))))))

       (vc-poly-monomials p))
               
     (write-string (apply #'concatenate 'string (cdr string-list)))
     (terpri)))
     

 
;--------------------------------------------------------------------------------------------------------------------------------
;   Polynomial Algebra
;--------------------------------------------------------------------------------------------------------------------------------

(defun add-polynomials (p1 p2)

  (declare (type polynomial p1 p2))

;;; This function computes the sum of two polynomials without destroying the arguments.
;;; Notice that the function strongly depends on the fact that the lists of monomials
;;; are sorted.
;;; PRECONDITION :  (equal (poly-variables p1) (poly-variables p2))

  (let ((h (merge 'list 
                   (copy-list (poly-monomials p1))
                   (copy-list (poly-monomials p2))
                   #'monom-lex-order)))

   (make-polynomial :variables (poly-variables p1)
                    :monomials (join-monomials h))))


(defun join-monomials (m-list)

  (declare (type cons m-list))

;;; <m-list> is a list containing at least two monomials.
;;; This function joins two succeeding monomials in <m-list> with equal exponents
;;; by - destructively - adding the coefficient of the second monomial to that
;;; of the first one.
  

  (do ((x m-list (cdr x)))
      ((null (cdr x)) m-list)                                ; abort if length(x) = 1
                               
   (let ((m1 (car x))
         (m2 (cadr x)))
     (when (equalp (monom-exponent m1) (monom-exponent m2))  ; monomials "joinable"
           (setf (car x)
                 (make-monomial :coeff (+ (monom-coeff m1) (monom-coeff m2))
                                :exponent (monom-exponent m1)))
           (setf (cdr x) (cddr x))))))



(defun subtract-polynomials (p1 p2)

   (declare (type polynomial p1 p2))

;;; This function computes the difference of the two polynomials <p1> and <p2> by adding -<p2> to <p1>.
;;; The arguments are not destroyed.
;;; PRECONDITION :  (equal (poly-variables p1) (poly-variables p2))

  (let ((h (merge 'list
                  (copy-list (poly-monomials p1))
                  (mapcar
                          #'(lambda (m) (make-monomial :coeff (- (monom-coeff m)) :exponent (monom-exponent m)))
                          (poly-monomials p2))
                  #'monom-lex-order)))
  
      (setq h (join-monomials h))

;;; delete monomials with zero-coefficient

      (setq h (delete 0 h :test #'eql :key #'monom-coeff))

      (if (null h)       ; p1 = p2
          (make-constant 0 (poly-variables p1))
          (make-polynomial
             :variables (poly-variables p1)
             :monomials h))))



(defun mult-polynomials (p1 p2)

  (declare (type polynomial p1 p2))

;;; This function computes the product of two polynomials without destroying the arguments.
;;; Each monomial of the shorter polynomial is multiplied with the longer polynomial.
;;; The resulting polynomials are "merged" using a greedy algorithm to determine an optimal merge
;;; pattern.
;;; Notice that the function makes use of the fact that the ordering (monom-lex-order) is multiplicative,
;;; i.e. compatible with the multiplication of monomials.
;;; PRECONDITION :  (equal (poly-variables p1) (poly-variables p2))

  (let (short long opnds-array merge-opnds h n
        (i 0))

;;; determine the shorter polynomial

    (if (<= (length (poly-monomials p1)) (length (poly-monomials p2)))
        (setq short (poly-monomials p1)
              long  (poly-monomials p2))
        (setq short (poly-monomials p2)
              long  (poly-monomials p1)))

;;; initialize important variables

    (setq n (length short)
          opnds-array (make-array n :initial-element nil))     ; <opnds-array> used to store the operands of the <n>-1
                                                               ;  merge operations

;;; compute and store the <n> initial operands (i.e. lists of monomials) by multiplying each of the <n> monomials
;;; in <short> with <long>

    (dolist (m1 short)

      (setf (aref opnds-array i)
            (mapcar
              #'(lambda (m2) (make-monomial :coeff (* (monom-coeff m1) (monom-coeff m2))
                                            :exponent  (map '(vector fixnum) #'+ 
                                                            (monom-exponent m1) (monom-exponent m2))))
              long))
      (incf i))

;;; initialize the list of merge operands, a list of dotted pairs
;;; ( car : length of operand  cdr : storage position in <opnds-array> )

    (let ((len-long (length long)))

      (dotimes (j n) (setq merge-opnds (acons len-long j merge-opnds))))

;;; merge the <n> lists of monomials using an optimal merge pattern to obtain the result of  p1*p2

    (dotimes (dmy (1- n))                                             ; <n>-1 merge operations
      #+symbolics(declare (ignore dmy))
       (let ((a-pos (cdr (first merge-opnds))))

         (setq h (merge 'list (aref opnds-array a-pos)                ; merge the shortest operands
                        (aref opnds-array (cdr (second merge-opnds))) 
                        #'monom-lex-order))

         (setq merge-opnds (cddr merge-opnds))                        ; remove them from the list

         (setf (aref opnds-array a-pos) (join-monomials h))           ; store result in array after
                                                                      ;  joining equal monomials

         (setq merge-opnds                                            ; insert new operand into sorted
               (merge 'list                                           ;  list of merge operands
                      merge-opnds
                      (acons (length (aref opnds-array a-pos))
                             a-pos nil)
                      #'< :key #'car))))

;;; return the result of the multiplication

    (make-polynomial :variables (poly-variables p1)
                     :monomials (aref opnds-array (cdr (first merge-opnds))))))



(defun r-mult-polynomials (p1 p2)

   (declare (type polynomial p1 p2))

;;; This function computes the product of two polynomials without destroying the arguments. First, the shorter of 
;;; the operands is determined. Then the recursive function  rec-mult-pol  is applied to the lists of monomials of
;;; <p1> and <p2>.
;;; PRECONDITION : (equal (poly-variables p1) (poly-variables p2))

  (make-polynomial
     :variables (poly-variables p1)
     :monomials
                (if (<= (length (poly-monomials p1)) (length (poly-monomials p2)))
                    (rec-mult-pol (copy-list (poly-monomials p1)) (poly-monomials p2))
                    (rec-mult-pol (copy-list (poly-monomials p2)) (poly-monomials p1)))))

(defun rec-mult-pol (monom-list-1 monom-list-2)

   (declare (type list monom-list-1 monom-list-2))

;;; This function recursively computes the monomials of the two polynomials represented by  <monom-list-1>  and
;;;  <monom-list-2>  using a divide-and-concquer strategy.
;;; Notice that  r-mult-polynomials  and  rec-mult-pol  usually consume considerably less memory space than the
;;; equivalent function  mult-polynomials.

  (cond

;;; if <monom-list-1> is trivial - i.e. the list contains just one monomial m1 - then multiply m1 with each
;;; monomial in <monom-list-2>

       ((null (cdr monom-list-1))

          (let ((m1 (first monom-list-1)))
               (declare (type monomial m1))

            (mapcar #'(lambda (m2)
                              (declare (type monomial m2))
                          (make-monomial :coeff (* (monom-coeff m1) (monom-coeff m2))
                                         :exponent (map '(vector fixnum) #'+ (monom-exponent m1) (monom-exponent m2))))
                    monom-list-2)))

;;; otherwise divide <monom-list-1> - destructively - into two lists of (almost) equal lengths

       (t

          (let (m-l-11 m-l-12 middle-pos)

            (setf middle-pos
                  (do ((m-l monom-list-1 (cdr m-l))
                       (even nil (not even))
                       (pos monom-list-1))
                      ((null (cddr m-l)) pos)
                    (when even (setq pos (cdr pos))))

                  m-l-11 monom-list-1
                  m-l-12 (cdr middle-pos)
                  (cdr middle-pos) nil)

;;; merge the results of the recursive calls for <m-l-11> and <m-l-12>, then join equal monomials

          (join-monomials (merge 'list
                                 (rec-mult-pol m-l-11 monom-list-2)
                                 (rec-mult-pol m-l-12 monom-list-2)
                                 #'monom-lex-order))))))

;--------------------------------------------------------------------------------------------------------------------------------
;    Parsing Polynomials
;--------------------------------------------------------------------------------------------------------------------------------
              

 

(defun parse-exponent (s)

;;; This function determines the exponent for the variable before <s-pos> in <s>
;;; and the first position to the right of the exponent
;;; (two cases :  (1) x^<number> ...  --> return(<number>, pos)
;;;               (2) x ...           --> return(1, pos) )
;;; PRECONDITION : (<= s-pos s-len)

  (if (and (consp s) (eql '^ (first s)))
      (values (second s) (third s))
      (values s 1)))        
                   



(defun parse-monomial (s variables)

;;; This function parses the monomial beginning at <s-pos> in <s>. It returns the monomial
;;; and the first position in <s> to the right of the '+' following the monomial or <s-len>
;;; if <s> was completely parsed.
;;; PRECONDITION : (< s-pos s-len)

  (let (var-pos m)

    (setq m (make-monomial :exponent 
			   (make-array (length variables) :element-type 'fixnum :initial-element 0)))

    (if (and (consp s) (eql (first s) '*))
	(setq s (rest s))
	(setq s (list s)))

    (when (integerp (first s))			; number parsed
      (setf (monom-coeff m) (first s)
	    s (rest s)))

    (dolist (exp s)

      (multiple-value-bind (var num) (parse-exponent exp)

	(setq var-pos (position var variables))
	(setf (aref (monom-exponent m) var-pos)	; update monomial with exponent parsed
	      (+ num (aref (monom-exponent m) var-pos)))))
    m))



(defun read-polynomial (pol &optional (no-of-var-coeffs 0))

;;; This function provides a parser for polynomials. It returns a polynomial in <arity> variables and <no-of-var-coeffs>
;;; variable coefficients or :error in case of an illegal input

  (let* ((variables (append (first pol) (create-var-coeffs no-of-var-coeffs)))
         (stop nil)
         (p (make-constant 0 variables)))

     (setq stop nil)

     (do ((cur-pos (if (and (consp (second pol)) (eql (first (second pol)) '+))
		       (rest (second pol))
		       (list (second pol))))
	  m)
         (stop
          (when (> (length (poly-monomials p)) 1)
                (setf (poly-monomials p)                                       ; return p with redundand monomial 0 removed
                      (remove                                                  ;  if necessary
                         (first (poly-monomials (make-constant 0 variables)))
                         (poly-monomials p)
                         :test #'equalp)))
           p)
                                                          

       (setq m (parse-monomial (first cur-pos) variables)
	     cur-pos (rest cur-pos))
       (when (equal m :error) (return :error))
                                                                               ; add non-zero monomial to p
       (unless (zerop (monom-coeff m))
               (setq p
                     (add-polynomials p (make-polynomial :variables variables  :monomials (list m)))))

       (setq stop (null cur-pos)))))


(defun create-var-coeffs (n)

   (declare (type fixnum n))

;;; This function returns a list of <n> variable coefficients.
;;; PRECONDITION  (< <n> 21)

   (let ((vars '(A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A14 A15 A16 A17 A18 A19 A20)))
     (firstn vars n)))

;--------------------------------------------------------------------------------------------------------------------------------
;   Evaluating Polynomials in Several Variables
;--------------------------------------------------------------------------------------------------------------------------------
                                                                                                 

;;;  Polynomials are evaluated using an "iterated version" of Horner's rule.
;;;  To achieve fairly efficient evaluations of a polynomial <p> a sequence of polynomials in ONE variable,
;;; the so-called "eval-sequence" for <p>, is generated from <p> (only once for an arbitrary number of evaluations).
;;;  To obtain the value for <p> applied to some arguments (numbers or polynomials) the polynomials in the "eval-
;;; sequence" are successively evaluated (with Horner's rule). Each result is stored in an "eval-array" as it will
;;; be used as the value for a coefficient of a subsequent polynomial in the "eval-sequence". The value resulting
;;; from the last polynomial in the "eval-sequence" is the value for <p>.

;;;  A polynomial <c-p> in the "eval-sequence" for <p> is represented by a structure of type <eval-struct>.  <res-addr>
;;; indicates the address of the component in the "eval-array" where the result of the evaluation of <c-p> is to be
;;; stored.  <coeff-array>  contains the coefficients of <c-p> w.r.t. the variable <var> at position <var-pos> in
;;; (poly-variables <p>). A coefficient is either a natural number or the address of a component in the "eval-array"
;;; where the value for the coefficient must be stored before evaluating <c-p> with <coeff-array>.

(defstruct (eval-struct (:copier nil) (:predicate nil))
           (res-addr 0 :type fixnum)
           (var-pos  0 :type fixnum)
           (var "" :type string)
           (coeff-array #(0) :type (simple-vector *)))




(defun decomp-polynomial (p var-pos eval-array-offset)

   (declare (type polynomial p)
            (type fixnum var-pos eval-array-offset))

;;; This function returns the array of coefficients for <p> w.r.t. the variable at position <var-pos> in
;;; (poly-variables <p>) (see comments above).
;;; If there is more than one variable in <p> the list of the corresponding "coefficient polynomials" is also returned.
;;; If a coefficient is a polynomial (i.e. not a constant) then only a reference to the value of the coefficient
;;; (instead of the coefficient itself) is stored in the array of coefficients for <p>.
;;; This reference is of the form  '(<coeff-addr>) meaning that the value for the coefficient is stored in the "eval-array",
;;; more precicely in  (aref "eval-array" <coeff-addr>) .

;;; PRECONDITION :  (and "0 <= <var-pos> < (length (poly-variables <p>))"
;;;                      "<var> occurs in <p> at least once")

   (let* ((var-deg-of-p (max-degree var-pos p))
          (coeff-array (make-array (1+ var-deg-of-p) :element-type t
                                                     :initial-element nil)))
         (declare (type fixnum var-deg-of-p))

;;; compute the "coefficient polynomials" and store them in <coeff-array>

     (dolist (m (poly-monomials p))
             (declare (type monomial m))

        (let ((var-deg-of-m (aref (monom-exponent m) var-pos))
              (m1 (copy-monomial m)))
             (declare (type fixnum var-deg-of-m)
                      (type monomial m1))

          (setf (aref (monom-exponent m1) var-pos) 0            ; "delete" occurence of <var> in <m1>
                (aref coeff-array var-deg-of-m)                 ; store <m1> in the "coeff. polynomial"
                (merge 'list (aref coeff-array var-deg-of-m)    ;  for <var>^<var-deg-of-m>
                             (list m1) #'monom-lex-order))))

;;; compute the coefficient array for <p> (i.e. <coeff-array>) and
;;; the list of the "coefficient polynomials" (i.e. <coeff-poly-list>)

     (let ((j eval-array-offset)                                ; <j> is used to determine the "eval-array" address for the
           (coeff-poly-list nil))                               ;  value of a "coeff. polynomial"
          (declare (type fixnum j)
                   (type list coeff-poly-list))

       (dotimes (i (1+ var-deg-of-p) (values coeff-array coeff-poly-list))
                (declare (type fixnum i))

           (let ((c-list (aref coeff-array i)))

             (if (null c-list)                                  ; no coefficient for <var>^<i>
                 (setf (aref coeff-array i) 0)                  ;  --> coefficient becomes 0

                 (let ((coeff-poly
                        (min-var-polynomial (make-polynomial :variables (poly-variables p)
                                                             :monomials c-list))))
                      (declare (type polynomial coeff-poly))

                   (if (integerp coeff-poly)                    ; <coeff-poly> is a constant
                       (setf (aref coeff-array i) coeff-poly)
                       (progn
                          (setf (aref coeff-array i)            ; store address for value of <coeff-poly> in <coeff-array>
                                (cons j nil))
                          (incf j)
                          (setq coeff-poly-list
                                (append coeff-poly-list (list coeff-poly))))))))))))



(defun no-of-mult (eval-seq)

   (declare (type list eval-seq))

;;; This function determines the number of multiplications needed to process the "eval-sequence" <eval-seq>
;;; with Horner's rule.
;;; (<n> multiplications for a polynomial with a degree of <n>)

   (let ((res 0))
        (declare (type fixnum res))

     (dolist (e-strct eval-seq res)
             (declare (type eval-struct e-strct))
        (incf res (1- (length (eval-struct-coeff-array e-strct)))))))



(defun opt-eval-seq (p eval-array-offset result-eval-addr)

   (declare (type polynomial p)
            (type fixnum eval-array-offset result-eval-addr))

;;; This function computes a sequence of polynomials in one variable to evaluate <p>, the so-called "eval-sequence"
;;; for <p>. The sequence is optimal w.r.t. the number of multiplications.
;;; The "eval-sequence" is determined with a "brute force" algorithm which recursively enumerates all possible
;;; "eval-sequences" for <p> and returns the one with the least number of multiplications necessary to evaluate each of
;;; the polynomials in the "eval-sequence" with Horner's rule.
;;; PRECONDITION   :  (not (null (poly-variables <p>)))
;;; RECOMMENDATION :  (equalp <p> (min-var-polynomial <p>))

   (let* ((len-vars (length (poly-variables p)))
          (eval-seq-array   (make-array len-vars :element-type t :initial-element nil))
          (no-of-mult-array (make-array len-vars :element-type 'fixnum :initial-element 0)))
         (declare (type fixnum len-vars))

;;; for each variable in (poly-variables <p>) determine an "eval-sequence" with optimal subsequences (for
;;; the coefficient polynomials) and store it in <eval-seq-array>

     (dolist (var (poly-variables p))
             (declare (type string var))

        (let ((var-pos (position var (poly-variables p)))
              (new-offset eval-array-offset)
              (new-res-addr eval-array-offset)
              c-a c-l)
             (declare (type fixnum var-pos new-offset new-res-addr))

         (multiple-value-setq (c-a c-l)
            (decomp-polynomial p var-pos eval-array-offset))

         (setf (aref eval-seq-array var-pos)
               (list (make-eval-struct :res-addr result-eval-addr :var var
                                       :coeff-array c-a)))

         (incf new-offset (length c-l))

         (dolist (c-poly c-l)                          ; for each coeff. polynomial determine an optimal "eval-sequence"
                 (declare (type polynomial c-poly))    ;  which becomes part of the "eval-seq" for <p> w.r.t. <var>

            (let ((c-opt-seq (opt-eval-seq c-poly new-offset new-res-addr)))

              (setf (aref eval-seq-array var-pos)
                    (append c-opt-seq (aref eval-seq-array var-pos)))

              (incf new-offset (1- (length c-opt-seq)))
              (incf new-res-addr)))

;;; for each "eval-sequence" in <eval-seq-array> determine the number of multiplications needed with Horner's rule
;;; and store it in <no-of-mult-array>

         (setf (aref no-of-mult-array var-pos)
               (no-of-mult (aref eval-seq-array var-pos)))))

;;; determine the index of a sequence with minimal value in <no-of-mult-array>

      (let ((min-no most-positive-fixnum)
            (min-index 0))
           (declare (type fixnum min-no min-index))

       (dotimes (i len-vars)
                (declare (type fixnum i))
          (when (< (aref no-of-mult-array i) min-no)
                (setq min-no (aref no-of-mult-array i)
                      min-index i)))

;;; set the values for the :var-pos-components in the "eval-structures" (in the optimal "eval-sequence" only)

       (dolist (e-strct (aref eval-seq-array min-index))
               (declare (type eval-struct e-strct))
          (setf (eval-struct-var-pos e-strct)
                (position (eval-struct-var e-strct) (poly-variables p))))

       (aref eval-seq-array min-index))))              ; the result of  opt-eval-seq



(defun eval-polynomial (eval-seq eval-array args-array res-vars)

    (declare (type list eval-seq res-vars))

;;; This function evaluates a polynomial <p> with Horner's rule by means of its "eval-sequence" <eval-seq>,
;;; an "eval-array" <eval-array>, and an array of the arguments <args-array> (polynomials) to be applied to <p>.

   (dolist (e-strct eval-seq (aref eval-array 0))
           (declare (type eval-struct e-strct))

      (let* ((a (eval-struct-coeff-array e-strct))
             (n (1- (length a)))
             (a-n (aref a n)) res)
            (declare (type fixnum n)
                     (type polynomial res))

        (setq res
              (if (atom a-n)                       ; coefficient is a number
                  (make-constant a-n res-vars)
                  (aref eval-array (car a-n))))    ; else coefficient is found with the reference in <a-n>

        (setf (aref eval-array (eval-struct-res-addr e-strct))

              (dotimes (i n res)
		#+symbolics(declare (ignore i))

                 (setq res (r-mult-polynomials res (aref args-array (eval-struct-var-pos e-strct))))
                 (decf n)
                 (setq a-n (aref a n))
                 (if (atom a-n)
                     (when (> a-n 0)
                           (setq res (add-polynomials res (make-constant a-n res-vars))))
                     (setq res (add-polynomials res (aref eval-array (car a-n))))))))))

;--------------------------------------------------------------------------------------------------------------------------------
;   Interpreting Terms
;--------------------------------------------------------------------------------------------------------------------------------

;;; The interpretation of terms  [.] : T(F,V) --> POLY  is an extension of a mapping from the function symbols defined in
;;; the given signature to the set of polynomials. This mapping is represented using an association list stored in the
;;; global variable  *weight* . For each function symbol <f> there is an entry in the a-list of type <interpretation-entry>,
;;; which can be accessed via <f>.

(defstruct (interpretation-entry
                 (:copier nil) (:predicate nil) (:conc-name interp-))

     (arity 0 :type fixnum)
     (assoc-constant 0 :type fixnum)                            ; relevant only if arity = 0
     (assoc-polynomial (make-constant 0 nil) :type polynomial)  ; relevant only if arity > 0

     (eval-seq nil :type list)              ; the "eval-sequence" for [<f>] used for efficient evaluations
                                            ;  (relevant only if arity > 0)

     (eval-array #() :type simple-vector))  ; used to store intermediate results during the evaluation of [<f>]
                                            ;  (relevant only if arity > 0)

(defun ord=pol_assign (function pol)
						; edited:  23-NOV-1991 17:41
						; authors: PRCKLN
						; input:   FUNCTION is an MKRP function symbol.
						;          VARIABLES is a list of symbols occurring free in EXPRESSION.
						;          EXPRESSION is an arithmetic expression.
						; example: (X Y) for VARIABLES, (* (* 2 x) (+ 4 y)) for BODY is legal input.
						; effect:  Stores the input such that (ORD=POL_GET FUNCTION)
						;          Gets a Lisp function (LAMBDA (VARIABLES) changed EXPRESSION),
						;          may be compiled.
						; value:   This lisp function.
  (assert (= (dt-arity function) (length (first pol))) nil "Wrong arity in polynomial ordering: ~A" (first pol))
  (assert (or (not (dt-constant.is function)) (and (integerp (second pol)) (> (second pol) 0)))
	  nil "An interpretation of a constant operator must be a positive integer.")
  (when (dt-constant.is function)
    (setq ord*pol_limit (min ord*pol_limit (second pol))))
  (let* ((polynomial (read-polynomial pol (dt-arity function)))
	 (eval-seq (UNLESS (dt-constant.is function) (opt-eval-seq polynomial 1 0))))
    (dt-putprop function 'ord*weight (make-interpretation-entry
				       :arity (dt-arity function)
				       :assoc-constant (if (dt-constant.is function) (second pol) nil)
				       :assoc-polynomial polynomial
				       :eval-seq eval-seq
				       :eval-array (make-array (length eval-seq) :element-type 'polynomial)))))

(defun ord=pol_get.fct.entry (function)
						; edited:  23-NOV-1991 17:39
						; authors: PRCKLN
						; input:   A function symbol.
						; effect:  -
						; value:   The polynomial expansion function of this symbol.
  (or (dt-getprop function 'ord*weight)
      (let ((pol.opt (rest (assoc (intern (dt-pname function) (find-package "MKRP"))
				  (opt-get.option er_polynomial.weight)
				  :test #'(lambda (x y)
					    (string= (princ-to-string x) (princ-to-string y)))))))
	(unless pol.opt 
	  (setq pol.opt (ord=pol_default (dt-arity function))))
	(ord=pol_assign function pol.opt))))


(defun interprete (term variables)

  (declare (type list variables))

;;; This function recursively computes the polynomial [<term>] associated with the term <term> using the interpretation
;;; of the function symbols stored in the car of the global variable  *weight* .
;;; A variable x occuring in <term> is interpreted by the polynomial X.
;;; The resulting polynomial is a polynomial in the variables <variables> which requires

;;; PRECONDITION :  "Var(<term>) contained in <variables>"

  (cond ((dt-variable.is term)
	 

         (let ((res (make-constant 1 variables)))
              (declare (type polynomial res))

           (setf (aref (monom-exponent (first (poly-monomials res)))
                       (position term variables))
                 1)
           res))

        (t                                     ; <term> is not a variable
           (let* ((fct-entry (ord=pol_get.fct.entry (dt-term_topsymbol term)))
		  ; (cdr (assoc (dt-term_topsymbol term) (car *weight*)))
                  (ar (interp-arity fct-entry)))
                 (declare (type interpretation-entry fct-entry)
                          (type fixnum ar))

             (if (zerop ar)                    ; <term> is a constant
                 (make-constant (interp-assoc-constant fct-entry) variables)

                 (let ((args-array (make-array ar :element-type 'polynomial))
                       (i 0))
                      (declare (type fixnum i))

                   (dolist (arg (DT-TERM_ARGUMENTS term))

                      (setf (aref args-array i)
                            (interprete arg variables))
                      (incf i))

                   (eval-polynomial (interp-eval-seq fct-entry) (interp-eval-array fct-entry) args-array variables)))))))



(defun ac-oper-interpretation-p (p)

   (declare (type polynomial p))

;;; This predicate determines whether <p> is a legal interpretation for an AC-operator, i.e. <p> is a polynomial of the form
;;;  a x y  +  b x  +  c x  +  d  where  b = c  and  ad + b = bb
;;; PRECONDITION :  (= (length (poly-variables <p>)) 2)

   (let ((a 0) (b 0) (c 0) (d 0))
        (declare (type fixnum a b c d))

     (dolist (m (poly-monomials p))
             (declare (type monomial m))

        (let ((m-expt (monom-exponent m))
              (m-coeff (monom-coeff m)))

          (cond ((equalp m-expt #(1 1)) (setq a m-coeff))
                ((equalp m-expt #(1 0)) (setq b m-coeff))
                ((equalp m-expt #(0 1)) (setq c m-coeff))
                ((equalp m-expt #(0 0)) (setq d m-coeff))
                (t (return-from ac-oper-interpretation-p nil)))))

     (and (= b c) (= (+ (* a d) b) (* b b)))))










(defstruct (vc-monomial (:conc-name vc-monom-))

;;; the definition for the type of monomials used in (normalized) polynomials with variable coefficients

    (coeff-poly (make-constant 1 '("A1")) :type polynomial)
    (exponent (make-array 0 :element-type 'fixnum) :type (vector fixnum)))


(defstruct (vc-polynomial (:conc-name vc-poly-))

;;; the definition of the type of (normalized) polynomials with variable coefficients

    (variables nil :type list)
    (monomials nil :type list))

;;; actually a non-empty list of vc-monomials

                                 

(defun normalize-to-vc-polynomial (p n)

   (declare (type polynomial p)
            (type fixnum n))

;;; This function computes the representation of the polynomial <p> which is a polynomial in the variables X1, ... , Xm
;;;  and the variable coefficients A1 , ... , A<n> (e.g. the result of vc-interprete) as a vc-polynomial in X1 , .. , Xm.

;;; PRECONDITION :  (>= (length (poly-variables <p>)) <n>)

   (let ((no-of-vars (- (length (poly-variables p)) n)))

     (make-vc-polynomial

        :variables (subseq (poly-variables p) 0 no-of-vars)

        :monomials

          (let* ((p-m-list (poly-monomials p))
                 (vc-p-m-list nil)
                 (last-expo (subseq (monom-exponent (first p-m-list)) 0 no-of-vars))
                 (var-coeffs (subseq (poly-variables p) no-of-vars))
                 (coeff-poly-m-list (list (make-monomial
                                              :coeff (monom-coeff (first p-m-list))
                                              :exponent (subseq (monom-exponent (first p-m-list)) no-of-vars)))))

            (do ((m-list (cdr p-m-list) (cdr m-list))
                 act-expo coeff-poly-m)

                ((null m-list)

                 (append vc-p-m-list (list (make-vc-monomial
                                               :coeff-poly (make-polynomial :variables var-coeffs
                                                                            :monomials coeff-poly-m-list)
                                               :exponent last-expo))))

             (setq act-expo (subseq (monom-exponent (first m-list)) 0 no-of-vars)
                   coeff-poly-m (make-monomial :coeff (monom-coeff (first m-list))
                                               :exponent (subseq (monom-exponent (first m-list)) no-of-vars)))

             (if (equalp act-expo last-expo)

                 (setq coeff-poly-m-list (append coeff-poly-m-list (list coeff-poly-m)))

                 (setq vc-p-m-list (append vc-p-m-list (list (make-vc-monomial
                                                                :coeff-poly (make-polynomial :variables var-coeffs
                                                                                             :monomials coeff-poly-m-list)
                                                                :exponent last-expo)))
                       last-expo act-expo
                       coeff-poly-m-list (list coeff-poly-m))))))))





;--------------------------------------------------------------------------------------------------------------------------------
;   Orienting equations with polynomial orderings
;--------------------------------------------------------------------------------------------------------------------------------

(defun positive (p no-of-pos-mon no-of-neg-mon)

   (declare (type polynomial p)
            (type fixnum no-of-pos-mon no-of-neg-mon))

;;; This predicate returns true iff the positiveness of <p> can be proven with the procedure POSITIVE of Ben Cherifa/
;;; Lescanne.
;;; The heuristics used to implement the procedure CHOOSE require that all possible pairings of negative and "coering"
;;; positive monomials be computed. For this purpose, information about all possible pairings is provided, kept in
;;; various arrays and constantly updated.
;;; Notice the use of a flexible minimum value for the variables in the realization of the procedure CHANGE (not neces-
;;; sarily 1 or 2).

;;; PRECONDITION : <no-of-pos-mon> = "# of positive monomials in <p>" > 0
;;;                <no-of-neg-mon> = "# of negative monomials in <p>" > 0

   (let ((pos-coeff (make-array no-of-pos-mon :element-type 'rational))
         (neg-coeff (make-array no-of-neg-mon :element-type 'rational))
         (pos-expo  (make-array no-of-pos-mon :element-type 'simple-vector))
         (neg-expo  (make-array no-of-neg-mon :element-type 'simple-vector))
         (pos-expo-sum  (make-array no-of-pos-mon :element-type 'fixnum))
         (neg-expo-sum  (make-array no-of-neg-mon :element-type 'fixnum))
         (pos-covers (make-array no-of-pos-mon :element-type 'list :initial-element nil))
         (neg-covered-by (make-array no-of-neg-mon :element-type 'list :initial-element nil))

         (one-pos-for-neg nil)
         (one-neg-for-pos nil)
         (others nil)
         pair)

;;; initialize the various arrays for the monomials in <p>

    (let ((p-pos 0) (p-neg 0))
         (declare (type fixnum p-pos p-neg))

      (dolist (m (poly-monomials p))

         (cond ((plusp (monom-coeff m))  (setf (aref pos-coeff p-pos) (monom-coeff m)
                                               (aref pos-expo p-pos) (monom-exponent m)
                                               (aref pos-expo-sum p-pos) (reduce #'+ (monom-exponent m)))
                                         (incf p-pos))

               (t                        (setf (aref neg-coeff p-neg) (- (monom-coeff m))
                                               (aref neg-expo p-neg) (monom-exponent m)
                                               (aref neg-expo-sum p-neg) (reduce #'+ (monom-exponent m)))
                                         (incf p-neg)))))

;;; compute the relations "covered-by" and "covers"

    (dotimes (i no-of-neg-mon)

       (let ((n-cov-by_i (aref neg-covered-by i)))

         (dotimes (j no-of-pos-mon)

            (when (exponent-nat-order (aref pos-expo j) (aref neg-expo i))
                  (setf n-cov-by_i (cons j n-cov-by_i)
                        (aref pos-covers j) (cons i (aref pos-covers j)))))

         (if (null n-cov-by_i)
             (return-from positive nil)
             (setf (aref neg-covered-by i) n-cov-by_i))))

;;; compute all possible pairings of negative and covering positive monomials and store each of them in one of three
;;; different lists (= priority classes) :
;;;
;;; The pair (i,j) is stored in
;;;
;;; - one-pos-for-neg     if pos-expo[j] is the only positive monomial covering neg-expo[i]
;;; - one-neg-for-pos     if (i,j) is not in  one-pos-for-neg  and neg-expo[i] is the only monomial covered by pos-expo[j]
;;; - others              if (i,j) is not in the first nor in the second list.

    (dotimes (i no-of-neg-mon)

       (let ((n-cov-by_i (aref neg-covered-by i)))

          (cond ((= (length n-cov-by_i) 1)   (let ((j (first n-cov-by_i)))

                                                (setq pair (cons (cons i j) 
                                                           (- (aref pos-expo-sum j) (aref neg-expo-sum i)))
                                                      one-pos-for-neg (cons pair one-pos-for-neg))))

                (t                           (dolist (j n-cov-by_i)

                                                (setq pair (cons (cons i j)
                                                           (- (aref pos-expo-sum j) (aref neg-expo-sum i))))
                                                (if (/= (length (aref pos-covers j)) 1)
                                                    (setq others (cons pair others))
                                                    (setq one-neg-for-pos (cons pair one-neg-for-pos))))))))

    (setq one-pos-for-neg (sort one-pos-for-neg #'< :key #'cdr)
          one-neg-for-pos (sort one-neg-for-pos #'> :key #'cdr)
          others (sort others #'< :key #'cdr))

;;; so far the preparations ...

    (let (i j)
         (declare (type fixnum i j))

     (loop

;;; the procedure CHOOSE :

        (setq pair (if one-pos-for-neg
                       (first one-pos-for-neg)
                       (if one-neg-for-pos
                           (first one-neg-for-pos)
                           (first others)))
              i (caar pair)
              j (cdar pair))

;;; the procedure CHANGE :

        (let* ((n-i (aref neg-coeff i))
               (p-j (aref pos-coeff j))
               (h (expt ord*pol_limit (cdr pair))))
              (declare (type rational n-i p-j)
                       (type fixnum h))

          (if (> p-j (/ n-i h))
              (setf (aref neg-coeff i) 0
                    (aref pos-coeff j) (- p-j (/ n-i h)))
              (setf (aref pos-coeff j) 0
                    (aref neg-coeff i) (- n-i (* p-j h)))))

;;; updating the lists and arrays : delete pointers to monomials whose coefficients have been set to zero

        (when (zerop (aref neg-coeff i))

              (setq one-pos-for-neg (delete i one-pos-for-neg :test #'= :key #'caar)    ; delete all pairs of 
                    one-neg-for-pos (delete i one-neg-for-pos :test #'= :key #'caar)    ;  the form (i,*)
                    others (delete i others :test #'= :key #'caar))

              (dolist (k (aref neg-covered-by i))

                 (setf (aref pos-covers k) (delete i (aref pos-covers k) :test #'=))

                 (when (= (length (aref pos-covers k)) 1)
                    (let* ((l (first (aref pos-covers k)))
                           (pair-1 (cons (cons l k)
                                         (- (aref pos-expo-sum k) (aref neg-expo-sum l)))))
                      (setq one-neg-for-pos
                            (merge 'list (list pair-1) one-neg-for-pos #'> :key #'cdr))))))

        (when (zerop (aref pos-coeff j))

              (setq one-neg-for-pos (delete j one-neg-for-pos :test #'= :key #'cdar)    ; delete all pairs of
                    others (delete j others :test #'= :key #'cdar))                     ;  of the form (*,j)

              (dolist (k (aref pos-covers j))

                 (setf (aref neg-covered-by k) (delete j (aref neg-covered-by k) :test #'=))

                 (when (null (aref neg-covered-by k))
                       (return-from positive nil))

                 (when (= (length (aref neg-covered-by k)) 1)
                       (let* ((l (first (aref neg-covered-by k)))
                              (pair-1 (cons (cons k l)
                                            (- (aref pos-expo-sum l) (aref neg-expo-sum k)))))
                        (setq one-pos-for-neg
                              (merge 'list (list pair-1) one-pos-for-neg #'< :key #'cdr))))))

        (when (and (null others)
                   (null one-neg-for-pos)
                   (null one-pos-for-neg))
              (if (find-if #'plusp pos-coeff)
                  (return-from positive t)
                  (return-from positive nil)))))))



(defvar *print-poly* nil)

(defvar *print-poly-length* nil)

(setq *print-array* t)

(defun TR=pol (t1 t2)

;;; This function tries to orient the equation  <t1> = <t2>  using a polynomial ordering.
;;; It is assumed that the interpretations for the given operators are stored in  (car *weight*)  and the minimum value
;;; for the variables in  (cdr *weight*) .
;;; The function uses the procedure POSITIVE as described by Ben Cherifa/Lescanne to prove the positiveness of polynomials.
;;; If  <t1> = <t2>  can be oriented to a rule  l -> r  then  (l r)  is returned, otherwise nil.

  (let (vars
        p p1 p2
        (no-of-pos-mon 0)
        (no-of-neg-mon 0)
        max-pos-mon max-neg-mon
        (p1=p2 nil))
    (declare (type polynomial p p1 p2)
	     (type fixnum no-of-pos-mon no-of-neg-mon)
	     (type monomial max-pos-mon max-neg-mon))

    ;; compute and compare the sets of variables V(<t1>) and V(<t2>)

    (let ((v1 (dt-term.variables t1))
	  (v2 (dt-term.variables t2)))

      (if (subsetp v1 v2)
	  (setq vars v2)
	  (if (subsetp v2 v1)
	      (setq vars v1)
	      (return-from TR=pol (values nil :vars-uncomp)))))

    ;; interprete both terms <t1> and <t2> and analyse the "difference-polynomial" [<t1>] - [<t2>]

    (setq p1 (interprete t1 vars)
          p2 (interprete t2 vars)
          p  (subtract-polynomials p1 p2))

    (dolist (m (poly-monomials p))

      (cond ((plusp (monom-coeff m))   (when (zerop no-of-pos-mon)
					 (setq max-pos-mon m))
	     (incf no-of-pos-mon))

	    ((zerop (monom-coeff m))   (setq p1=p2 t))

	    (t                         (when (zerop no-of-neg-mon)
					 (setq max-neg-mon m))
				       (incf no-of-neg-mon))))

    (cond (p1=p2                        (values nil :p1=p2))

          ((zerop no-of-neg-mon)        (list t1 t2))

          ((zerop no-of-pos-mon)        nil) ;(list t2 t1))

          (t                            (if (monom-lex-order max-pos-mon max-neg-mon)
                                            (if (positive p no-of-pos-mon no-of-neg-mon)
                                                (list t1 t2)
                                                nil)
					    (progn
					      (dolist (m (poly-monomials p))	; p := -p
						(setf (monom-coeff m) (- (monom-coeff m))))
					      (if (positive p no-of-neg-mon no-of-pos-mon)
						  nil ; (list t2 t1)
						  nil)))))))
       
