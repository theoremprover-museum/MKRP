;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-
;;; Don't use this file.  Use the mkrp.system in ~omega/sys

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

#+allegro-v4.1
(setq excl:*enable-package-locked-errors* nil)
#+allegro-v4.1
(setq excl:*cltl1-in-package-compatibility-p* t)

#+omega(push :mkrp-basic *features*)
#-omega(setq *features* (delete :mkrp-basic *features*))

(in-package "LISP" :nicknames '("CL"))

(in-package "MARKGRAF-KARL" :use '("LISP") :nicknames '("MKRP"))
#-mkrp-basic(unless (find-package "TH-AC-MKRP") (make-package "TH-AC-MKRP"))

#+lucid(import '(user::quit) (find-package "MKRP"))

(defparameter mkrp*directory '(#-symbolics :relative "prog"))

(proclaim '(optimize (speed 3) (safety 3) (compilation-speed 0) (space 0)))
#+CLTL2(declaim (declaration edited authors input effect value))
#-CLTL2(proclaim '(declaration edited authors input effect value))

(defparameter mkrp*boot_files
  (mapcan #'(lambda (list)
	      (mapcar #'(lambda (name)
			  (make-pathname :directory
					 `(,@mkrp*directory ,(string-downcase (symbol-name (first list))))
					 :name (string-downcase (if (symbolp name)
								    (symbol-name name)
								  name))))
		      (rest list)))
	  '((serv service serviceqlists servicefiles atpservice)
	    (opt options)
	    (ds memory dataterm dt-term)
	    (ord ord-poly orderings)
	    #-:mkrp-basic (hd denz hades)
	    (pp pprint pprint-latex)
	    #-:mkrp-basic (edt symboltable formulatable editorwindow compile edit)
	    (init init)
	    (ds datastructure)
	    ;;(sort uds upp upr)
	    (uni #-:mkrp-basic uni-commutative
		 #-:mkrp-basic uni-ac-match
		 #-:mkrp-basic uni-thu
		 unification)
	    #-:mkrp-basic(norm normalization presimplification)
	    (cg connectiongraph)
	    (prot prot-datastructure
		  #-:mkrp-basic prot-print
		  #-:mkrp-basic prot-latex
		  #-:mkrp-basic prot-prepare
		  #-:mkrp-basic prot-execute
		  post
		  interface 
		  protocol)
	    (op op-service op-inherit op-create operation)
	    (red two red-service red-datastructure red-rw red-crr
		 #-:mkrp-basic narrow
		 red-linkcondition
		 reduction)
	    (op construct)
	    #-:mkrp-basic (ctl preparation)
	    #-:mkrp-basic (c preparation)
	    #-:mkrp-basic (e preparation)
	    (term mergeinst terminator)
	    (sel selection sel-mark sel-manual)
	    (ctl control)
	    #-:mkrp-basic (os os-explanation operatingsystem)
	    #+:mkrp-basic (omega operatingsystem preparation mkrp)
	    )))

(defvar mkrp*compile-type #+lcl4.0 "sbin" #+kcl "o" #+allegro "fasl" #-(or lcl4.0 kcl allegro) "lbin")

(defun mkrp=boot=print-menue ()
  (format *standard-output* "~%~%~%     1. Laden von .lbin oder .lisp~%")
  (format *standard-output* "     2. Laden compiliert~%")
  (format *standard-output* "     3. Laden interpretiert~%")
  (format *standard-output* "     4. Alle Dateien neu compilieren~%")
  (format *standard-output* "     5. Compilieren ab der ersten nicht compilierten Datei~%")
  (format *standard-output* "     6. Compilieren von nicht compilierten Dateien~%")
  (format *standard-output* "~%~%  Bitte waehlen Sie: ")
  (read *standard-input*))

(defparameter mkrp*boot_compilerlisting  (make-pathname :name "compilerlisting" :type "text"))

(defun mkrp=boot=run-boot (number)  
  (unless (y-or-n-p "Do you want to make a protocol? ")
    (setq mkrp*boot_compilerlisting nil))
  (case number
    (1 (mkrp=boot=load))
    (2 (mkrp=boot=load-fas))
    (3 (mkrp=boot=load-lsp))
    (4 (mkrp=boot=compile-all-new))
    (5 (mkrp=boot=load-and-compile-after))
    (6 (mkrp=boot=comp-not-comp))
    (otherwise (error "unbekannte Auswahl")))
  (mapc (function fmakunbound) '(mkrp-boot mkrp=boot=print-menue mkrp=boot=run-boot mkrp=boot=comp-not-comp
					   mkrp=boot=load mkrp=boot=load-fas mkrp=boot=compile-all
					   mkrp=boot=load-lsp mkrp=boot=compile-all-new
					   mkrp=boot=load-and-compile mkrp=make.path.out.comp
					   mkrp=boot=load-and-compile-after mkrp=make.path.load.comp))
  (mapc #'makunbound '(mkrp*boot_compilerlisting mkrp*compile-type mkrp*boot_files mkrp*directory)))

(defun mkrp=boot=load ()
  (mapc #'load mkrp*boot_files))

(defun mkrp=boot=load-fas ()
  (mapc (function (lambda (f) (load (mkrp=make.path.load.comp f))))
         mkrp*boot_files))

(defun mkrp=boot=load-lsp ()
  (mapc (function (lambda (f) (load (make-pathname :type "lisp" :defaults f))))
        mkrp*boot_files))

(defun mkrp=boot=compile-all-new ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (mapc (function (lambda (f)
                    (mkrp=boot=load-and-compile f)
                    (load (mkrp=make.path.load.comp f))))
        mkrp*boot_files))

(defun mkrp=make.path.out.comp (f)
  (make-pathname :type mkrp*compile-type
		 :defaults
		 #+lcl4.0 (make-pathname :directory nil :defaults f)
		 #-lcl4.0 f))

(defun mkrp=make.path.load.comp (f)
  (make-pathname :type mkrp*compile-type
		 :defaults f))

(defun mkrp=boot=load-and-compile (f)
  ;#+lucid(system:clear-undef)
  (let ((path (make-pathname :type "lisp" :defaults f))
	(outpath (mkrp=make.path.out.comp f)))
    (load path :print t)
    (compile-file path :output-file outpath)))

(defun mkrp=boot=compile-all ()
  (mapc (function (lambda (f)
                    (compile-file (make-pathname :type "lisp" :defaults f)
				   :output-file (mkrp=make.path.out.comp f)
				   #+lucid :messages #+lucid *standard-output*)))
        mkrp*boot_files))

(defun mkrp=boot=load-and-compile-after ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (let ((nocompile t))
    (declare (special nocompile))
    (mapc (function (lambda (f)
		      (declare (special nocompile))
		      (if nocompile
			  (if (probe-file (mkrp=make.path.load.comp f))
			      (load (mkrp=make.path.load.comp f))
			      (let ((path (make-pathname :type "lisp" :defaults f))
				    (outpath (mkrp=make.path.out.comp f)))
				(setq nocompile nil)
				;#+lucid(system:clear-undef)
				(load path :print t)
				(format t "~&Compilation starts ...")
				(compile-file path #+lucid :messages #+lucid *standard-output* :output-file outpath)
				(format t "~& ... Compilation ended~%")
				(load (mkrp=make.path.load.comp f))))
			  (let ((path (make-pathname :type "lisp" :defaults f))
				(outpath (mkrp=make.path.out.comp f)))
			    ;#+lucid(system:clear-undef)				 
			    (load path :print t)
			    (format t "~&Compilation starts ...")
			    (compile-file path #+lucid :messages #+lucid *standard-output* :output-file outpath)
			    (format t "~& ... Compilation ended~%")
			    (load f)))))
	  mkrp*boot_files))
  (dribble))

(defun mkrp=boot=comp-not-comp ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (let ()
    (declare (special nocompile))
    (mapc (function (lambda (f)
		      (declare (special nocompile))
		      (if (probe-file (mkrp=make.path.load.comp f))
			  (load f)
			  (let ((path (make-pathname :type "lisp" :defaults f))
				(outpath (mkrp=make.path.out.comp f)))
			    ;#+lucid(system:clear-undef)
			    (load path :print t)
			    (format t "~&Compilation starts ...")
			    (compile-file path #+lucid :messages #+lucid *standard-output* :output-file outpath)
			    (format t "~& ... Compilation ended~%")
			    (load f)))))
	  mkrp*boot_files))
  (dribble))

(defun mkrp-boot ()
  (proclaim '(optimize (compilation-speed 3) (space 0) (speed 0) (safety 3))) ;(optimize (speed 3) (safety 3)))
  (let ((*compile-verbose* t))
    (mkrp=boot=run-boot (mkrp=boot=print-menue)))) 


(defvar mkrp*restart.pathname (make-pathname :name "repair" :defaults *default-pathname-defaults*))

(compile 'mkrp-start
	 `(lambda ()
	    (in-package "MKRP")
	    (fresh-line)
	    (format t "Loading repair file ~S" "repair")
	    (load ,mkrp*restart.pathname)
	    (values)))

(defun mkrp-dumper ()
  (mkrp::mkrp-init)
  #+lucid(user::disksave "mkrp" :restart-function #'mkrp-start :full-gc t)			 
  #+kcl(system:save-system "mkrp")
  #+coral(user::disksave "Scuzzy C:mkrp:mkrp" :restart-function #'mkrp-start :full-gc t)
  #+poplog(lisp::savelisp "mkrp")
  #-(or kcl lucid coral)(format t "System dependent, implement yourself."))


; Repair for Kyoto and poplog:

#+(or poplog kcl)(defun ignore (&rest nix) nil)

#+(or poplog kcl)(compile 'ignore)

#+poplog(require "storeutils")
#+poplog(setq lisp::*max-store-size* 2000000)

(in-package "MKRP")

(mkrp-boot)


(mem-initialize 10000)
