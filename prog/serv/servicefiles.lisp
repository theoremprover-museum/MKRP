;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))

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


; Just for macs #-allegro
(defun mkrp-make.pathname (directory.example.name.flag name type odefaults)
						      ; Edited:  07-FEB-1989 21:15
						      ; Authors: PRCKLN
						      ; Input:   DIRECTORY.EXAMPLE.NAME.FLAG is a flag,
						      ;          NAME and TYPE are two strings, and
						      ;          ODEFAULTS is a pathname, a string, or a list.
						      ; Effect:  -
						      ; Value:   A pathname
  (let* ((defaults (if odefaults
		       (if (pathnamep odefaults)
			   odefaults
			   (funcall (mkrp-converse.filename) (if (symbolp odefaults)
								 (symbol-name odefaults)
								 odefaults)))
		       ""))
	 (pathname (pathname defaults)))
    (make-pathname :directory (if directory.example.name.flag
				  (or (and (not (equal (pathname-directory pathname) '(:relative)))
					   (pathname-directory pathname))				      
				      (append (pathname-directory (pathname (mkrp=get.default.directory)))
					      (list (mkrp=get.default.name))))
				  (pathname-directory pathname))
		   :name (funcall (mkrp-converse.filename) (or (pathname-name defaults)
							       name))
		   :type (funcall (mkrp-converse.filename)
				  (or (and (not (eq :unspecific (pathname-type defaults))) (pathname-type defaults))
				      type))
		   :host (or (and #-symbolics(string/= "" (namestring defaults))
				  #+symbolics t	      ;nil ??????????
				  (pathname-host defaults))
			     (pathname-host (pathname (mkrp=get.default.directory))))
		   :defaults pathname)))

#| just for macs #+allegro
(defun mkrp-make.pathname (directory.example.name.flag name type odefaults)
						; Edited:  07-FEB-1989 21:15
						; Authors: PRCKLN
						; Input:   DIRECTORY.EXAMPLE.NAME.FLAG is a flag,
						;          NAME and TYPE are two strings, and
						;          ODEFAULTS is a pathname, a string, or a list.
						; Effect:  -
						; Value:   A pathname
  (let* ((defaults (if odefaults odefaults ""))
	 (pathname (pathname (if (symbolp defaults)
				 (symbol-name defaults)
				 defaults)))
	 (directory nil))
    (when (and odefaults (pathname-type pathname))
      (setq type (pathname-type pathname)))
    (when (and odefaults (pathname-name pathname))
      (setq name (pathname-name pathname)))
    (when (and odefaults (pathname-directory pathname))
      (setq directory (pathname-directory pathname)))
    (setq pathname *default-pathname-defaults*)
    (when type (setq pathname (make-pathname :type type :defaults pathname)))
    (when name (setq pathname (make-pathname :name name :defaults pathname)))
    (setq pathname
          (make-pathname :directory
                         (if directory.example.name.flag
			     (if directory
				 directory
				 (concatenate `string (pathname-directory
							(pathname (mkrp=get.default.directory)))
					      (mkrp=get.default.name)))
			     directory)
                         :defaults pathname))
    pathname))|#

(defmacro mkrp-with.pathname (pathname &body body)
  `(let ((pathname ,pathname))
     (if pathname
	 (let* ((pathname (pathname (string ,pathname)))
		(mkrp*default.name (first (last (pathname-directory pathname))))
		(mkrp*default.directory (make-pathname :host (pathname-host pathname)
						       :directory (butlast (pathname-directory pathname) 1))))
	   ,@body)
	 (progn ,@body))))

(defun mkrp-linelength (&optional (stream *terminal-io*))
  (declare (ignore stream))
  #+(or symbolics explorer) 117
  #-(or symbolics explorer) 71)

(defvar mkrp*default.directory nil)

(defun mkrp=get.default.directory ()
  (unless mkrp*default.directory
    (let ((dir (pathname-directory *default-pathname-defaults*)))
      (setq mkrp*default.directory (make-pathname :directory (butlast dir) :defaults *default-pathname-defaults*))))
  mkrp*default.directory)

(defvar mkrp*default.name nil)

(defun mkrp=get.default.name ()
  (unless mkrp*default.name
    (setq mkrp*default.name (first (last (pathname-directory *default-pathname-defaults*)))))
  mkrp*default.name)

(defun mkrp-set.default.directory (dir)
  #+symbolics
  (progn (setq dir (string-downcase (string dir)))
	 (unless (let ((number -1))
		   (every #'(lambda (subdir)
			      (declare (ignore subdir))
			      (directory (make-pathname :directory (or (firstn (pathname-directory (pathname dir)) (incf number))
								       :root)
							:type :wild
							:name (nth number (pathname-directory (pathname dir)))
							:defaults (pathname dir))))
			  (pathname-directory (pathname dir))))
	   (warn "Directory ~A does not exist" dir)))
  (setq mkrp*default.directory dir))

(defun mkrp-set.default.name (name)
  #+symbolics
  (unless (and (directory (make-pathname :directory (or (butlast (pathname-directory (pathname (mkrp=get.default.directory))))
							:root)
					 :type :wild
					 :name (first (last (pathname-directory (pathname (mkrp=get.default.directory)))))
					 :defaults (pathname (mkrp=get.default.directory))))
	       (directory (make-pathname :type :wild
					 :name name
					 :defaults (pathname (mkrp=get.default.directory)))))
    (warn "Directory ~A does not exist" (make-pathname :defaults (pathname (mkrp=get.default.directory))
						       :directory (nconc1 (pathname-directory
									    (pathname (mkrp=get.default.directory)))
									  name))))
  (setq mkrp*default.name name))

#-(or symbolics explorer lucid)
(defun mkrp-OPENOUT (pathname full.name.variable &optional (mode "LISP") (latex nil))
  (let* ((stream  (open pathname :direction :output
                        :if-exists :new-version :if-does-not-exist :create)))
    (unless latex
      (format stream ";;; -*- Package: MKRP; Base: 10; Mode: ~A; Syntax: Common-lisp -*-~%" mode))
    (mkrp-add.stream  stream)
    (when full.name.variable (setf (symbol-value full.name.variable) pathname))
    stream))

#+(or symbolics explorer lucid)
(defun mkrp-OPENOUT (pathname full.name.variable &optional (mode "LISP") (latex nil))
  (let* ((stream  (open pathname :direction :output)))
    (unless latex
      (format stream ";;; -*- Package: MKRP; Base: 10; Mode: ~A; Syntax: Common-lisp -*-~%" mode))
    (mkrp-add.stream  stream)
    (when full.name.variable (setf (symbol-value full.name.variable) pathname))
    stream))

(defun mkrp-OPENIN (file)
  (let ((stream   (open file)))
    (mkrp-add.stream stream)
    stream))


(defun mkrp-LOAD.FILE (file &optional (readtable *readtable*))
  (let ((*readtable* readtable))
    (mapc #'eval (read-file file))
    file))

(defvar mkrp*all.system.streams nil)

(defun mkrp-add.stream (stream)
  (setq mkrp*all.system.streams (cons stream mkrp*all.system.streams)))

(defun mkrp-remove.stream (stream)
  (setq mkrp*all.system.streams (delete stream mkrp*all.system.streams)))

(defun mkrp-close.all.streams () (mapc #'closefile mkrp*all.system.streams))

(defun CLOSEFILE (stream)
  (when (member stream mkrp*all.system.streams)
    (let* ((truename (truename stream)))
      (mkrp-remove.stream stream)
      (close-stream stream)
      truename)))
    
(defun CLOSE-STREAM (stream)
  (testeval (close stream) nil))





(defvar breakmacros nil)


(defun mkrp-OUTSTREAMP (stream)
  (and (streamp stream) (output-stream-p stream)))

(defun mkrp-INSTREAMP (stream)
  (and (streamp stream) (input-stream-p stream)))
    
    

(defparameter *standard-readtable* (copy-readtable nil))

(set-syntax-from-char #\: #\a *standard-readtable*)
(set-syntax-from-char #\, #\a *standard-readtable*)
(set-macro-character #\: #'(lambda (stream char)
			     (declare (ignore stream char))
			     '\:)
		     nil *standard-readtable*)

(defun READFILE (pathname &optional (readtable *standard-readtable*))
  (let ((*readtable* readtable))
    (with-open-file (stream pathname)
      (let ((result (list nil))
	    expr)
	(while (neq 'error (setq expr (testeval (read stream nil 'error) t)))
	  (qconc1 result expr))
	(car result)))))

(defun mkrp-file.functions (file)
  (delete nil (mapcar #'(lambda (obj)
			  (if (and (consp obj)
				   (eq 'defun (first obj)))
			      (second obj)))
		      (readfile file))))



(DEFUN FILE.EXISTS (FILE &optional (TYPE (mkrp-default.lisp)))
						; INPUT: ANY ATOM
						; VALUE: T, IF THERE IS A FILE WITH THIS NAME,
						;        NIL, IF THERE IS NO SUCH FILE.
  (PROBE-file (make-pathname :type TYPE :defaults (pathname (princ-to-string FILE)))))

(DEFUN FILENAME.CHECK (NAME &REST IGNORE)
  (declare (ignore ignore))
  (or (streamp name) (pathnamep name) (pathnamep (pathname (string name)))))


(defun mkrp-view.file (file)
  #-symbolics
  (with-open-file (stream file)
    (labels ((readuntil (stream)
	       (let ((line (read-line stream nil :eof)))
		 (unless (eq :eof line)
		   (format t "~A~%" line)
		   (readuntil stream)))))
      (readuntil stream)))
  #+symbolics(GLOBAL:VIEWF file))

(defun mkrp-converse.filename ()
  #+symbolics #'string-upcase #-symbolics #'string-downcase)

(defun mkrp-default.problem ()
  (funcall (mkrp-converse.filename) "PROBLEM"))

(defun mkrp-default.lisp ()
  (funcall (mkrp-converse.filename) "LISP"))

(defun mkrp-default.graph ()
  (funcall (mkrp-converse.filename) "GRAPH"))

(defun mkrp-default.code ()
  (funcall (mkrp-converse.filename) "CODE"))

(defun mkrp-default.list ()
  (funcall (mkrp-converse.filename) "LIST"))

(defun mkrp-default.text ()
  (funcall (mkrp-converse.filename) "TEXT"))

(defun mkrp-default.options ()
  (funcall (mkrp-converse.filename) "OPTIONS"))

(defun mkrp-default.ax ()
  (funcall (mkrp-converse.filename) "AX"))

(defun mkrp-default.th ()
  (funcall (mkrp-converse.filename) "TH"))

(defun mkrp-default.formulae ()
  (funcall (mkrp-converse.filename) "FORMULAE"))

(defun mkrp-default.tempprepaxioms ()
  (funcall (mkrp-converse.filename) "TEMPPREPAXIOMS"))