(defun latex-out-axiom-clauses (list &optional (stream t) (title "Set of Axioms Clauses") &aux (first-time t))
  (when title (format stream "\\section*{~a}~%" title))
  (dolist (ai list)
    (unless first-time (format stream "\\vspace{-0.5cm}"))
    (format stream "\\makebox[0.5cm][l]{~a}\\makebox[1cm][l]{~a\:}~%" (first (first ai)) (second (first ai)))
    (format stream "\\parbox[t]{14.8cm}{~%\\vspace{-0.65cm}~%")
    (top-latex-out-int-term (silent-optimize-iterm (parse-term (second ai)) 30) 0 stream)
    (format stream "}~%~%")
    (setq first-time nil)
  ) 
)

(defun latex-out-splitted-theorems (list &optional (stream t) &aux (first-time t))
  (format stream "\\section*{Set of Theorem Clauses}~%")
  ;;(setq title (first list))
  (pop list)
  (dolist (ai list)
    (format stream "~a ~a\\\\[15pt]~%" (first (first ai)) (second (first ai)))
    (latex-out-splitpart-theorems (cdr ai) stream)
    (terpri stream)
  )
) 

(defun latex-out-splitpart-theorems (list &optional (stream t))
  (latex-out-axiom-clauses list stream nil)
)  

(defun latex-top-out (file list)
  (with-open-file (stream file :direction :output)
    (latex-out-start-surround stream)
    ;(latex-out-splitted-theorems list stream)
    (latex-out-initial-operations list stream)
    (latex-out-end-surround stream)
  )
)
; vorlaeufige Version

(defun latex-out-start-surround (stream)
  (format stream "\\documentstyle[german]{article}\
\
\\setlength{\\evensidemargin}{0.2cm}\
\\setlength{\\oddsidemargin}{0.5cm}\
\\setlength{\\marginparwidth}{1.5cm}\
\\setlength{\\marginparsep}{0.5cm}\
\
\\setlength{\\topmargin}{-1in}\
\\addtolength{\\topmargin}{2cm}\
\\setlength{\\textwidth}{16.3cm}\
\\setlength{\\textheight}{24cm}\
\
\\setlength{\\parindent}{0pt}\
\\setlength{\\parskip}{15pt}\
\
\\begin{document}~%")
) 

(defun latex-out-end-surround (stream)
  (format stream "\\end{document}~%")
)

(defun latex-out-initial-operations (list &optional (stream t))
  (format stream "\\section*{Initial Operations on Theorems}~%")
  (dolist (ai list)
    (latex-out-initial-operations-splitpart ai stream)
  )
)

(defun latex-out-initial-operations-splitpart (list &optional (stream t) &aux (first-time t))
  (format stream "~a ~a\\\\[15pt]~%" (first (first list)) (second (first list)))
  (pop list)
  (dolist (ai list)
    (unless first-time (format stream "\\vspace{-0.5cm}"))
    (format stream "\\makebox[2.3cm][l]{~a}\\makebox[1.0cm]{$\\longrightarrow$}~%"
      (string-subst-all-par "\\land" (first ai) "$\\land$")
    )
    (format stream "\\makebox[0.5cm][l]{~a}\\makebox[1cm][l]{~a\:}~%"
      (first (first (second ai))) (second (first (second ai)))
    )
    (format stream "\\parbox[t]{11.5cm}{~%\\vspace{-0.65cm}~%")
    (top-latex-out-int-term (silent-optimize-iterm (parse-term (second (second ai))) 30) 0 stream)
    (format stream "}~%~%")
    (setq first-time nil)
  )
)

(defun string-subst-first (from in to)
 (let ((l-from (length from))
       (l-in (length in))
       (start 0)
      )
  (loop (when (> l-from l-in) (return in))
        (when (string= from in :start2 start :end2 l-from)
          (return (concatenate 'string (subseq in 0 start) to (subseq in l-from l-in)))
        )
        (incf l-from) (incf start)
  )
 )
)

(defun string-subst-all-par (from in to)
 (let ((l-from (length from))
       (l-in (length in))
       (start 0)
      )
  (loop (when (> l-from l-in) (return in))
        (when (string= from in :start2 start :end2 l-from)
          (return (concatenate 'string (subseq in 0 start) 
                                       to (string-subst-all-par from (subseq in l-from l-in) to)
                  )
          )
        )
        (incf l-from) (incf start)
  )
 )
) 

(defun string-subst-all-seq (from in to &aux hv)
  (loop (setq hv (string-subst-first from in to))
        (when (string= hv in) (return hv))
        (setq in hv)
  )
)

