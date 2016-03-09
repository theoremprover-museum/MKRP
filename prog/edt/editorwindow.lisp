;;; -*- Package: Mkrp; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(IN-PACKAGE "MKRP" :use '("CL"))

;;; Common Variables

#+(or symbolics explorer)
(defconstant EDT*END.CHARACTER #\END)
                        ;;; these constants are used to define a key to leave the editor window
                        ;;; returning a value to the calling function


(defvar edt*run.window *terminal-io*)

(defvar edt*contents nil) 

#+(or symbolics explorer)  
(defun edt=READ.REGION ()
  ;; returning a marked region 
  (cond ((testeval (zwei:with-editor-stream (stream :buffer-name (global:send zwei:*interval* :name) 
						    :start :point
						    :end :mark)
		     (let ((*readtable* edt*standard.readtable)
			   (list nil)
			   elem)
		       (while (neq 'edt*eof (setq elem (read stream nil 'edt*eof)))
			 (setq list (append (if (or (symbolp elem)
						    (and (consp elem)
							 (symbolp (first elem))
							 (char= #\~ (char (symbol-name (first elem)) 0))))
						(reverse (eval elem))
						(list elem))
					    list)))
		       (cons :list (nreverse list))))
		   t))
	(t (list :list))))

#+(or symbolics explorer)
(defun edt=read.buffer (file)
  (zwei:with-editor-stream (stream :pathname file
				   :start :beginning)
    (let ((*readtable* edt*standard.readtable)
	  (list nil)
	  elem)
      (while (neq 'edt*eof (setq elem (read stream nil 'edt*eof)))
	(setq list (append (if (or (symbolp elem)
				   (and (consp elem)
					(symbolp (first elem))
					(char= #\~ (char (symbol-name (first elem)) 0))))
			       (reverse (eval elem))
			       (list elem))
			   list)))
      (nreverse list))))

#+(or symbolics explorer)
(defun edt=write.on.stream (expressions file)
  (zwei:with-editor-stream (stream :pathname file)
    (dolist (expr expressions)
      (pprint expr stream))))

#+(or symbolics explorer)
(zwei:DEFCOM edt=COM.quit.end "" ()
  ;; Definition of the command to get formulas from a marked region of zmacs.
  (when (eq *package* (find-package "MKRP"))
    (setq edt*contents (edt=read.region))
    (global:send edt*run.window :select))
  zwei:DIS-TEXT)

#+(or symbolics explorer)
(zwei:set-comtab zwei:*zmacs-comtab* (list EDT*END.CHARACTER 'edt=COM.quit.end))
  ;; Inserting command into zmacs command table

#+(or symbolics explorer)
(defun edt=EDIT.expression (expressions)
  (declare (ignore expressions))
  (setq edt*run.window *terminal-io*)
  (unless (probe-file (mkrp-make.pathname t "FORMULAE" "LISP" nil))
    (with-open-file (stream (mkrp-make.pathname t "FORMULAE" "LISP" nil) :direction :output)
      (format stream ";;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-~%")))
  (makunbound 'edt*contents)
  (ed (mkrp-make.pathname t "FORMULAE" "LISP" nil))
  ;(edt=write.on.stream expressions (mkrp-make.pathname t "FORMULAE" "LISP" nil))
  (global:process-wait "Editing formulae" #'boundp 'edt*contents)
  (setq EDT*SAVing.INDICATOR 'do.not.SAVE)
  edt*contents)

#-(or symbolics explorer)
(defun edt=edit.expression (expression)
  (declare (ignore expression))
  (warn "Not possible on this machine."))

#+(or symbolics explorer)
(defun edt=edit.changed (expression)
  (let ((file (mkrp-make.pathname t "FORMULAE" "LISP" nil)))
    (unless (probe-file (mkrp-make.pathname t "FORMULAE" "LISP" nil))
      (with-open-file (stream (mkrp-make.pathname t "FORMULAE" "LISP" nil) :direction :output)
	(format stream ";;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-~%")))
    (unless (member expression (edt=read.buffer file) :test #'equal)
      (edt=write.on.stream (list expression) file))
    (setq edt*run.window *terminal-io*)
    (makunbound 'edt*contents)
    (ed file)
    (global:process-wait "Editing formulae" #'boundp 'edt*contents)
    (setq EDT*SAVing.INDICATOR 'do.not.SAVE)
    edt*contents))

#-(or symbolics explorer)
(defun edt=edit.changed (expression) 
  (declare (ignore expression))
  (warn "Not possible on this machine."))

#+(or symbolics explorer)
(defun edt=EDIT.FILE (file)
  ;; putting contents of a file into a buffer and wait for result
  (setq edt*run.window *terminal-io*)
  (unless (probe-file file)
    (with-open-file (stream file :direction :output)
      (princ ";;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-" stream)))
  (makunbound 'edt*contents)
  (ed file)
  (global:process-wait "Editing formulae" #'boundp 'edt*contents)
  (setq EDT*SAVing.INDICATOR 'do.not.SAVE)
  edt*contents)

#-(or symbolics explorer)
(defun edt=edit.file (expression)
  (declare (ignore expression))
   (warn "Not possible on this machine."))
