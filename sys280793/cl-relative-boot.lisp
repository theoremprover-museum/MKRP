;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

(in-package "LISP" :nicknames '("CL"))

(in-package "MARKGRAF-KARL" :use '("LISP") :nicknames '("MKRP"))

(defparameter mkrp*directory '(#+lucid :relative "prog"))



(defparameter mkrp*boot_files (mapcar #'(lambda (name)
					 (make-pathname :directory mkrp*directory 
							:name (string-downcase (symbol-name name))))
				     '(service serviceqlists servicefiles atpservice
					       os-explanation options memory dataterm dt-term ord-poly orderings
					       HADES pprint pprint-latex
					       symboltable formulatable editorwindow compile edit init
					       datastructure uni-associative UNI-COMMUTATIVE uni-thu unification
					       normalization presimplification connectiongraph 
					       prot-datastructure prot-print prot-latex prot-prepare prot-execute protocol
					       op-service op-inherit op-create operation
					       two red-service red-datastructure RED-RW RED-CRR NARROW red-linkcondition
					       reduction construct preparation mergeinst terminator selection control
					       operatingsystem)))
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
  (mapc (function fmakunbound) '(mkrp-boot mkrp=boot=print-menue mkrp=boot=run-boot mkrp=boot=load mkrp=boot=load-fas
					   mkrp=boot=load-lsp mkrp=boot=compile-all-new mkrp=boot=load-and-compile
					  mkrp=boot=load-and-compile-after)))

(defun mkrp=boot=load ()
  (mapc #'load mkrp*boot_files))

(defun mkrp=boot=load-fas ()
  (mapc (function (lambda (f) (load f)))
         mkrp*boot_files))

(defun mkrp=boot=load-lsp ()
  (mapc (function (lambda (f) (load (make-pathname :type "lisp" :defaults f))))
        mkrp*boot_files))

(defun mkrp=boot=compile-all-new ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (mapc (function (lambda (f)
                    (mkrp=boot=load-and-compile f)
                    (load f)))
        mkrp*boot_files))

(defun mkrp=boot=load-and-compile (f)
  #+lucid(system:clear-undef)
  (let ((path (make-pathname :type "lisp" :defaults f))
	(outpath (make-pathname :type #+kcl"o"#-kcl"lbin" :defaults f)))  ; Our KCL version doesn't accept other types than "o"
    (load path :print t)
    (compile-file path :output-file outpath)))

(defun mkrp=boot=compile-all ()
  (mapc (function (lambda (f)
                    (compile-file (make-pathname :type "lisp" :defaults f)
				   :output-file (make-pathname :type #+kcl"o"#-kcl"lbin" :defaults f)
				   #+lucid :messages #+lucid *standard-output*)))
        mkrp*boot_files))

(defun mkrp=boot=load-and-compile-after ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (let ((nocompile t))
    (declare (special nocompile))
    (mapc (function (lambda (f)
		      (declare (special nocompile))
		      (if nocompile
			  (if (probe-file (make-pathname :type #+kcl"o"#-kcl"lbin" :defaults f))
			      (load f)
			      (let ((path (make-pathname :type "lisp" :defaults f))
				    (outpath (make-pathname :type #+kcl"o"#-kcl"lbin" :defaults f)))
				(setq nocompile nil)
				#+lucid(system:clear-undef)
				(load path :print t)
				(format t "~&Compilation starts ...")
				(compile-file path #+lucid :messages #+lucid *standard-output* :output-file outpath)
				(format t "~& ... Compilation ended~%")
				(load f)))
			  (let ((path (make-pathname :type "lisp" :defaults f))
				(outpath (make-pathname :type #+kcl"o"#-kcl"lbin" :defaults f)))
			    #+lucid(system:clear-undef)				 
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
		      (if (probe-file (make-pathname :type #+kcl"o"#-kcl"lbin" :defaults f))
			  (load f)
			  (let ((path (make-pathname :type "lisp" :defaults f))
				(outpath (make-pathname :type #+kcl"o"#-kcl"lbin" :defaults f)))
			    #+lucid(system:clear-undef)
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

(defun mkrp-start ()
  (in-package "MKRP")
  (fresh-line)
  (format t "Loading repair file ~S" "repair")
  (load "repair")
  (values))

(defun mkrp-dumper ()
  (mkrp::mkrp-init)
  #+lucid(user::disksave "mkrp" :restart-function #'mkrp-start :full-gc t)
  #+kcl(system:save-system "mkrp")
  #+coral(user::disksave "Scuzzy C:mkrp:mkrp" :restart-function #'mkrp-start :full-gc t)
  #-(or kcl lucid coral)(format t "System dependent, implement yourself."))
