;f,g,h
;'atom
;(rop arg rop)
;(lop lop arg)
;(bin arg [+ arg]*)

;term = (fnsym term*) | symbol | number | (rop term rop) | (lop op term) | (bin term [ op term ]*)

(defun a_gstring (obj)
  (typecase obj
    (symbol (string obj))
    (number (write-to-string obj)) 
    (string obj)
  ) 
)

(defun parse-term (ext-term &aux erg)
  (setq ext-term (unstringify-term ext-term))
  ; inserted 10.3.1992
  (setq erg (make-iterm))
  (if (or (symbolp ext-term) (numberp ext-term))
    (parse-tterm ext-term nil erg)
    (case (car ext-term)
      (quant (unless (check-quantifier (second ext-term))
               (error "parse-term>> unknown quantifier ~a" (second ext-term))
             )  
             (setf (iterm-quant-lists erg) (checked-quant-lists (third ext-term))
                   (iterm-quantifier erg) (second ext-term)
                   (iterm-typ erg) 'quant 
                   (iterm-args erg) (list (parse-tterm (fourth ext-term) erg))
             )
      ) 
      (t (parse-tterm ext-term nil erg))
    ) 
  ) 
  erg
)
; totally new version 26.2.1992

(defun unstringify-term (ext-term) 
  (typecase ext-term
    ((or symbol number) ext-term)
    (string (intern ext-term))
    (list (mapcar #'unstringify-term ext-term))
    (t (error "unstringify-term>> found unknown type in extern-term ~a." ext-term))
  ) 
) 
; new 10.3.1992

(defun check-quantifier (quantifier)
  (cdr (assoc quantifier *allowed-quantifiers-a-list))
)
; new 26.2.1992

(defun checked-quant-lists (quant-lists)
  (mapcar #'checked-quant-list quant-lists) 
)
; new 26.2.1992
; new version 10.3.1992

(defun checked-quant-list (quant-list)
  (unless quant-list (error "check-quant-list>> There is an empty quant-list"))
  (unless (symbolp (car quant-list)) 
    (error "~a is not a correct formal variable." (car quant-list))  
  )
  (if (symbolp (second quant-list))
    (if (every #'symbolp (cdr quant-list))
        quant-list
        (error "check-quant-list>> ~a is not a correct sort_quantification" quant-list)
    )
    (if (and (every #'symbolp (second quant-list)) (null (cddr quant-list)))
        (cons (car quant-list) (second quant-list))
        (error "check-quant-list>> ~a is not a correct sort_quantification" quant-list)
    )
  ) 
) 
; new 10.3.1993

(defun parse-tterm (ext-term &optional predecessor erg)
  (unless erg (setq erg (make-iterm)))
  (if (or (symbolp ext-term) (numberp ext-term))
    (setf (iterm-fn erg) (a_gstring ext-term) (iterm-typ erg) 'lit)
    (case (car ext-term)
      (rop (setf (iterm-fn erg) (a_gstring (third ext-term)) (iterm-typ erg) 'rightunaer
                 (iterm-args erg) (list (parse-tterm (second ext-term) erg))
           ) 
      ) 
      (lop (setf (iterm-fn erg) (a_gstring (second ext-term)) (iterm-typ erg) 'leftunaer
                 (iterm-args erg) (list (parse-tterm (third ext-term) erg))
           ) 
      )
      (kl (setf (iterm-typ erg) 'kl (iterm-args erg) (list (parse-tterm (second ext-term) erg))))
      ; inserted 10.3.1992
      (bins (let (ops args (isop nil)) 
              (dolist (ai (cdr ext-term))
                (if isop  
                  (progn (push (a_gstring ai) ops) (setq isop nil))
                  (progn (push (parse-tterm ai erg) args) (setq isop t))
                )
              ) 
              (setf (iterm-typ erg) 'bins (iterm-args erg) (nreverse args) 
                    (iterm-ops erg) (nreverse ops)
              )
            ) 
      ) 
      (t (if (consp (car ext-term))
           (progn (warn "Incorrect term-form! Trying to correct.")
                  (parse-tterm (car ext-term) predecessor erg)
           ) 
           (let (args) 
            (setf (iterm-fn erg) (a_gstring (car ext-term)))
            (dolist (ai (cdr ext-term))
              (push (parse-tterm ai erg) args)
            )
            (setf (iterm-typ erg) 'fnterm (iterm-args erg) (nreverse args))
           ) 
         )
         ; if and then inserted 10.3.1992
      ) 
    ) 
  ) 
  ; (setf (iterm-predecessor erg) predecessor)
  erg
)
; new 26.2.1992 

(defun next-local-length (iterm)
  (case (iterm-typ iterm)
    (lit (next-local-length-lit-term iterm))
    (fnterm (next-local-length-fn-term iterm))
    (bins (next-local-length-bins-term iterm))
    (leftunaer (next-local-length-leftunaer-term iterm)) 
    (rightunaer (next-local-length-rightunaer-term iterm))
    (quant (next-local-length-quant-term iterm))
    ; inserted 26.2.1992
    (kl (next-local-length-kl-term iterm))
    ; inserted 10.3.1992
    ;(leftunaerk (calc-local-next-step-length-leftunaerk-term iterm))
    ;(rightunaerk (calc-local-next-step-length-rightunaerk-term iterm))
  )
)

; Rueckgabe lokale Laenge letzter Zeile, lokale Laenge laengster Zeile  

(defun next-local-length-lit-term (iterm)
  (cons (iterm-end-length iterm) (iterm-local-length iterm))
)

(defun next-local-length-quant-term (iterm &aux (el 0) (ll 0))
  (case (iterm-format-nr iterm)
    (0 (setq ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (dolist (aj (cdr ai))
           (incf ll (+ (length (symbol-name aj)) 2))
         )
         (incf ll (+ (length-in) (length (symbol-name (car ai))) 1))
       ) 
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max ll (iterm-local-length (car (iterm-args iterm))))
       ) 
    )
    (1 (if (cdr (iterm-quant-lists iterm))
       (progn
       (setq old-ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (setq ll (quantifier-length (iterm-quantifier iterm)))
         (dolist (aj (cdr ai))
           (incf ll (+ (length (symbol-name aj)) 2))
         )
         (incf ll (+ (length-in) (length (symbol-name (car ai)))))
         (setq old-ll (max old-ll ll))
       ) 
       (setq old-ll (max (1+ ll) old-ll))
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max old-ll (iterm-local-length (car (iterm-args iterm))))
       )
       )
       (progn
       (setq old-ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (setq ll (quantifier-length (iterm-quantifier iterm)))
         (dolist (aj (cdr ai))
           (incf ll (+ (length (symbol-name aj)) 2))
         )
         (setq old-ll (max old-ll (- ll 2)))
         (setq ll (+ (quantifier-length (iterm-quantifier iterm)) (length-in) 1 (length (symbol-name (car ai)))))
         (setq old-ll (max old-ll ll))
       ) 
       (setq old-ll (max (1+ ll) old-ll))
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max old-ll (iterm-local-length (car (iterm-args iterm))))
       ) 
       )
       )
    )
    (2 (setq old-ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (setq ll (quantifier-length (iterm-quantifier iterm)))
         (dolist (aj (cdr ai))
           (incf ll (+ (length (symbol-name aj)) 2))
         )
         (setq old-ll (max old-ll (- ll 2)))
         (setq ll (+ (quantifier-length (iterm-quantifier iterm)) (length-in) 1 (length (symbol-name (car ai)))))
         (setq old-ll (max old-ll ll))
       ) 
       (setq old-ll (max (1+ ll) old-ll))
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max old-ll (iterm-local-length (car (iterm-args iterm))))
       ) 
    )
    (3 (setq old-ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (setq ll 0)
         (dolist (aj (cdr ai)) 
           (setq old-ll (max old-ll ll))
           (setq ll (1+ (length (symbol-name aj))))
         )
         (setq old-ll (max old-ll (- ll 1) (length-in) (length (symbol-name (car ai)))))
       ) 
       (setq old-ll (max old-ll 1))
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max old-ll (iterm-local-length (car (iterm-args iterm))))
       ) 
    )  
    (4 (cons (iterm-end-length iterm) (iterm-local-length iterm)))
  )
)
; new 26.2.1992

(defun next-local-length-kl-term (iterm)
  (case (iterm-format-nr iterm)
    (0 (cons 1 (1+ (iterm-local-length (car (iterm-args iterm))))))
    (1 (cons 1 (max 1 (iterm-local-length (car (iterm-args iterm))))))
    (2 (cons (iterm-end-length iterm) (iterm-local-length iterm))) 
  )
)
; new 10.3.1992

(defun next-local-length-leftunaer-term (iterm)
  (case (iterm-format-nr iterm)
    (0 (cons (iterm-end-length (car (iterm-args iterm)))
             (max (length (iterm-fn iterm)) (iterm-local-length (car (iterm-args iterm))))
       )
    )
    (1 (cons (iterm-end-length iterm) (iterm-local-length iterm)))
  )
)

(defun next-local-length-rightunaer-term (iterm)
  (case (iterm-format-nr iterm)
    (0 (cons (length (iterm-fn iterm))
             (max (length (iterm-fn iterm)) (iterm-local-length (car (iterm-args iterm))))
       )
    )
    (1 (cons (iterm-end-length iterm) (iterm-local-length iterm)))
  )
)


(defun next-local-length-fn-term (iterm &aux (maxtab 0) (ltab 0))
  (case (iterm-format-nr iterm)
    ;(0  (setq maxtab (setq ltab (1+ (length (iterm-fn iterm)))))
    ;    (dolist (ai (iterm-args iterm))
    ;      (setq maxtab (max maxtab (+ ltab 1 (iterm-end-length ai)) (+ ltab (iterm-local-length ai))))
    ;    )
    ;    (cons ltab maxtab)
    ;)
    ((0 1)  (setq maxtab (length (iterm-fn iterm))) (setq ltab 2)
        (dolist (ai (iterm-args iterm))
          (setq maxtab (max maxtab (+ ltab 1 (iterm-end-length ai)) (+ ltab (iterm-local-length ai))))
        )
        (cons 2 (max maxtab 2))
    ) 
   ; probeweise
    (2  (setq maxtab (1+ (length (iterm-fn iterm)))) (setq ltab 0)
        (dolist (ai (iterm-args iterm))
          (setq maxtab (max maxtab (+ ltab 1 (iterm-end-length ai)) (+ ltab (iterm-local-length ai))))
        )
        (cons 1 (max maxtab 1))
    )
    (3  (cons (iterm-end-length iterm) (iterm-local-length iterm)))
  )
)

(defun next-local-length-bins-term (iterm)
  (case (iterm-format-nr iterm)
    (0 (let ((maxtab 0) (dummy 0)
             (args (iterm-args iterm)) (ops (iterm-ops iterm))
            )
         (when args 
          (loop
           (setq dummy (iterm-end-length (car args)))
           (setq maxtab (max (iterm-local-length (pop args)) maxtab)) 
           (unless args (return))
           (setq maxtab (max (op-length (pop ops)) maxtab))
          )
         ) 
         (cons dummy maxtab)
       )
    )
    (2  (cons (iterm-end-length iterm) (iterm-local-length iterm)))
  )
) 
; vereinfachter bins-part

(defun actual-local-length (iterm)
  (case (iterm-typ iterm)
    (lit (actual-local-length-lit-term iterm))
    (fnterm (actual-local-length-fn-term iterm))
    (bins (actual-local-length-bins-term iterm))
    (leftunaer (actual-local-length-leftunaer-term iterm)) 
    (rightunaer (actual-local-length-rightunaer-term iterm))
    (quant (actual-local-length-quant-term iterm))
    ; inserted 26.2.1992
    (kl (actual-local-length-kl-term iterm))
    ; inserted 10.3.1992
    ;(leftunaerk (calc-local-next-step-length-leftunaerk-term iterm))
    ;(rightunaerk (calc-local-next-step-length-rightunaerk-term iterm))
  )
)

; Rueckgabe lokale Laenge letzter Zeile, lokale Laenge laengster Zeile  

(defun actual-local-length-lit-term (iterm)
  (cons (iterm-end-length iterm) (iterm-local-length iterm))
)

(defun actual-local-length-quant-term (iterm &aux (el 0) (ll 0))
  (case (iterm-format-nr iterm)
    (0 (setq ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (dolist (aj (cdr ai))
           (incf ll (+ (length (symbol-name aj)) 2))
         )
         (incf ll (+ (length-in) (length (symbol-name (car ai))) 1))
       ) 
       (cons (+ ll (iterm-end-length (car (iterm-args iterm))))
             (+ ll (iterm-local-length (car (iterm-args iterm))))
       ) 
    )
    (1 (setq ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (dolist (aj (cdr ai))
           (incf ll (+ (length (symbol-name aj)) 2))
         )
         (incf ll (+ (length-in) (length (symbol-name (car ai))) 1))
       ) 
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max ll (iterm-local-length (car (iterm-args iterm))))
       ) 
    )
    (2 (setq old-ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (setq ll (quantifier-length (iterm-quantifier iterm)))
         (dolist (aj (cdr ai))
           (incf ll (+ (length (symbol-name aj)) 2))
         )
         (incf ll (+ (length-in) (length (symbol-name (car ai)))))
         (setq old-ll (max old-ll ll))
       ) 
       (setq old-ll (max (1+ ll) old-ll))
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max old-ll (iterm-local-length (car (iterm-args iterm))))
       ) 
    )
    (3 (setq old-ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (setq ll (quantifier-length (iterm-quantifier iterm)))
         (dolist (aj (cdr ai))
           (incf ll (+ (length (symbol-name aj)) 2))
         )
         (setq old-ll (max old-ll (- ll 2)))
         (setq ll (+ (quantifier-length (iterm-quantifier iterm)) (length-in) 1 (length (symbol-name (car ai)))))
         (setq old-ll (max old-ll ll))
       ) 
       (setq old-ll (max (1+ ll) old-ll))
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max old-ll (iterm-local-length (car (iterm-args iterm))))
       ) 
    )
    (4 (setq old-ll (quantifier-length (iterm-quantifier iterm)))
       (dolist (ai (iterm-quant-lists iterm))
         (setq ll 0)
         (dolist (aj (cdr ai)) 
           (setq old-ll (max old-ll ll))
           (setq ll (1+ (length (symbol-name aj))))
         )
         (setq old-ll (max old-ll (- ll 1) (length-in) (length (symbol-name (car ai)))))
       ) 
       (setq old-ll (max old-ll 1))
       (cons (iterm-end-length (car (iterm-args iterm)))
             (max old-ll (iterm-local-length (car (iterm-args iterm))))
       ) 
    )   
  )
)
; new 26.2.1992

(defun actual-local-length-kl-term (iterm)
  (case (iterm-format-nr iterm)
    (0 (let ((end-length (+ 2 (iterm-end-length (car (iterm-args iterm))))))
         (cons end-length (max end-length (1+ (iterm-local-length (car (iterm-args iterm))))))
       ) 
    ) 
    (1 (cons 1 (1+ (iterm-local-length (car (iterm-args iterm))))))
    (2 (cons 1 (max 1 (iterm-local-length (car (iterm-args iterm))))))
  )
)
; new 10.3.1992

(defun actual-local-length-leftunaer-term (iterm)
  (case (iterm-format-nr iterm)
    (0 (cons (+ (length (iterm-fn iterm)) (iterm-end-length (car (iterm-args iterm))))
             (+ (length (iterm-fn iterm)) (iterm-local-length (car (iterm-args iterm))))
       ) 
    ) 
    (1 (cons (iterm-end-length (car (iterm-args iterm)))
             (max (length (iterm-fn iterm)) (iterm-local-length (car (iterm-args iterm))))
       )
    )
  )
)

(defun actual-local-length-rightunaer-term (iterm)
  (case (iterm-format-nr iterm)
    (0 (let ((end-length (+ (length (iterm-fn iterm)) (iterm-end-length (car (iterm-args iterm))))))
         (cons end-length (max (iterm-local-length (car (iterm-args iterm))) end-length))
       )
    ) 
    (1 (cons (length (iterm-fn iterm))
             (max (length (iterm-fn iterm)) (iterm-local-length (car (iterm-args iterm))))
       )
    )
  )
)

(defun actual-local-length-fn-term (iterm &aux (maxtab 0) (ltab 0))
  (case (iterm-format-nr iterm)
    (0  (setq maxtab (setq ltab (1+ (length (iterm-fn iterm)))))
        (dolist (ai (iterm-args iterm)) 
          (setq maxtab 
            (max maxtab (+ 1 ltab (iterm-end-length ai)) (+ ltab (iterm-local-length ai)))
          )
          (incf ltab (+ 1 (iterm-end-length ai)))
        )
        (cons ltab maxtab)
    ) 
    (1  (setq maxtab (setq ltab (1+ (length (iterm-fn iterm)))))
        (dolist (ai (iterm-args iterm))
          (setq maxtab (max maxtab (+ ltab 1 (iterm-end-length ai)) (+ ltab (iterm-local-length ai))))
        )
        (cons ltab maxtab)
    )
    (2  (setq maxtab (length (iterm-fn iterm))) (setq ltab 2)
        (dolist (ai (iterm-args iterm))
          (setq maxtab (max maxtab (+ ltab 1 (iterm-end-length ai)) (+ ltab (iterm-local-length ai))))
        )
        (cons 2 (max maxtab 2))
    )
    (3  (setq maxtab (1+ (length (iterm-fn iterm)))) (setq ltab 0)
        (dolist (ai (iterm-args iterm))
          (setq maxtab (max maxtab (+ ltab 1 (iterm-end-length ai)) (+ ltab (iterm-local-length ai))))
        )
        (cons 1 (max maxtab 1))
    )
  )
) 

(defun actual-local-length-bins-term (iterm)
  (case (iterm-format-nr iterm) 
    (0 (let ((maxtab 0) (ltab 0))
         (when (iterm-args iterm) 
           (let ((args (iterm-args iterm))
                 (ops  (iterm-ops iterm))
                ) 
             (loop
               (setq maxtab (max maxtab (+ ltab (iterm-local-length (car args)))))
               (incf ltab (iterm-end-length (pop args)))
               (unless args (return))
               (incf ltab (op-length (pop ops)))
               (setq maxtab (max maxtab ltab))
             )
           )
         )
         (cons ltab maxtab)
       )
    ) 
    (2 (let ((maxtab 0) (dummy 0)
             (args (iterm-args iterm)) (ops (iterm-ops iterm))
            )
         (when args 
          (loop
           (setq dummy (iterm-end-length (car args)))
           (setq maxtab (max (iterm-local-length (pop args)) maxtab)) 
           (unless args (return))
           (setq maxtab (max (op-length (pop ops)) maxtab))
          )
         ) 
         (cons dummy maxtab)
       )
    )
  )
) 
; vereinfachter bins-part

(defun find-longest-term-path (iterm)
  (let (erg max-len-bisher succ)
    (push iterm erg)
    (loop 
     (setq max-len-bisher 0)
     (dolist (ai (iterm-args iterm))
      (when (>= (iterm-local-length ai) max-len-bisher)  
                       ; statt der actual-length
        (setq succ ai) (setq max-len-bisher (iterm-local-length ai)) 
      )
     )                 
     (unless succ (return erg))
     (push succ erg) (setq iterm succ) (setq succ nil)
    )
  ) 
)

(defun toplpr (list)
  (dolist (ai list) 
    (princ ai) (terpri)
  ) 
) 

(defun get-zielterm (path-list)
  (let (zielterm neu-l neu-e alt-l alt-e act-term)
    (setq zielterm (car path-list))
    (setq neu-l (iterm-local-length zielterm) neu-e (iterm-end-length zielterm))
    (loop 
      (setq act-term (pop path-list))
      (unless path-list (return (values zielterm neu-e neu-l)))
      (setq next-local-length (next-local-length (car path-list)))
      (setq alt-l (iterm-local-length act-term) alt-e (iterm-end-length act-term))
      (setf (iterm-local-length act-term) neu-l (iterm-end-length act-term) neu-e)
      (setq actual-local-length (actual-local-length (car path-list)))
      (setf (iterm-local-length act-term) alt-l (iterm-end-length act-term) alt-e)
      (if (or (< (cdr next-local-length) (cdr actual-local-length)) 
              (and (= (cdr next-local-length) (cdr actual-local-length)) 
                   (int-term-at-max? zielterm)
              )
          )
        (setq zielterm (car path-list) neu-e (car next-local-length) neu-l (cdr next-local-length)) 
        (setq neu-e (car actual-local-length) neu-l (cdr actual-local-length))
      )
    )
  ) 
)

(defun optimize-iterm (iterm aim-length &aux zielterm)
  (loop 
    (pr-int-term iterm) (terpri) (terpri)
    (when (<= (iterm-local-length iterm) aim-length) (return))
    (setq zielterm (get-zielterm (find-longest-term-path iterm))) 
    ; (terpri) (princ zielterm)
    (terpri)
    (when (int-term-at-max? zielterm) 
      (warn "Could'nt reach aim") (return)
    )
    (step-int-term zielterm)
  ) 
)  

(defun step-int-term (iterm &optional (tab 0))
  (case (iterm-typ iterm)
    (lit)
    (fnterm 
      (case (iterm-format-nr iterm) 
        (0 (setf (iterm-format-nr iterm) 2))
        ; probeweise, sonst 0 zur anderen Gruppe
        ((1 2) (incf (iterm-format-nr iterm)))
      )
    )
    ;(bins (step-bins-term iterm tab))
    (bins 
      (case (iterm-format-nr iterm)
        (0 (setf (iterm-format-nr iterm) 2))
      ) 
    )
    ; vereinfachter bins-part
    ((leftunaer rightunaer)
      (case (iterm-format-nr iterm)
        (0 (incf (iterm-format-nr iterm)))
      ) 
    )
    (quant 
      (case (iterm-format-nr iterm) 
        ((0 2 3) (incf (iterm-format-nr iterm)))
        (1 (setf (iterm-format-nr iterm) 
                 (if (cdr (iterm-quant-lists iterm)) 2 3)
           )
        )
      ) 
    )
    ; inserted 26.2.1992
    (kl
      (case (iterm-format-nr iterm)
        ((0 1) (incf (iterm-format-nr iterm)))
      ) 
    )
    ; inserted 10.3.1992
    ;(leftunaerk (step-leftunaerk-term iterm tab))
    ;(rightunaerk (step-rightunaerk-term iterm tab))
  )
  iterm
)

(defun int-term-at-max? (iterm &optional (tab 0))
  (case (iterm-typ iterm)
    (lit t)
    (fnterm (= (iterm-format-nr iterm) 3))
    (bins (= (iterm-format-nr iterm) 2))
    ((leftunaer rightunaer) (= (iterm-format-nr iterm) 1))
    (quant (= (iterm-format-nr iterm) 4))
    ; inserted 26.2.1992
    (kl (= (iterm-format-nr iterm) 2))
    ; inserted 10.3.1992
    ;(leftunaerk (step-leftunaerk-term iterm tab))
    ;(rightunaerk (step-rightunaerk-term iterm tab))
  )
)
