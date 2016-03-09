(in-package "LISP" :nicknames '("CL"))
(in-package "MARKGRAF-KARL" :use '("LISP") :nicknames '("MKRP"))


(defparameter mkrp*directory "Scuzzy C:mkrp:prog:")

(defparameter mkrp*boot_files.ep (mapcar #'(lambda (name)
					 (make-pathname :directory mkrp*directory 
							:name (string-downcase (symbol-name name))))
				     '(repair-shadow repair-def service serviceqlists servicefiles atpservice
					       os-explanation options memory dataterm pprint
					       symboltable formulatable editorwindow compile edit
					       prot-datastructure prot-print prot-prepare prot-execute protocol
                                               operatingsystem)))

(defparameter mkrp*boot_files.cr (mapcar #'(lambda (name)
					 (make-pathname :directory mkrp*directory 
							:name (string-downcase (symbol-name name))))
				     '(repair-shadow repair-def service serviceqlists servicefiles atpservice
					       os-explanation options memory dataterm
					       datastructure unification uni-associative normalization presimplification
					       connectiongraph protocol
					       operation two red-service red-datastructure red-linkcondition
					       reduction construct preparation mergeinst terminator selection control
					       operatingsystem)))

(defparameter mkrp*boot_files (mapcar #'(lambda (name)
					 (make-pathname :directory mkrp*directory 
							:name (string-downcase (symbol-name name))))
				     '(repair-shadow repair-def service serviceqlists servicefiles atpservice
					       os-explanation options memory dataterm pprint
					       symboltable formulatable editorwindow compile edit
					       datastructure unification uni-associative normalization presimplification
					       connectiongraph 
					       prot-datastructure prot-print prot-prepare prot-execute protocol
					       operation two red-service red-datastructure red-linkcondition
					       reduction construct preparation mergeinst terminator selection control
					       operatingsystem)))
(defun mkrp=boot=print-menue ()
  (format *standard-output* "~%~%~%     1. Laden von .fasl oder .lisp~%")
  (format *standard-output* "     2. Laden compiliert~%")
  (format *standard-output* "     3. Laden interpretiert~%")
  (format *standard-output* "     4. Alle Dateien neu compilieren~%")
  (format *standard-output* "     5. Compilieren ab der ersten nicht compilierten Datei~%")
  (format *standard-output* "     6. Compilieren von nicht compilierten Dateien~%")
  (format *standard-output* "     7. Compilieren von nicht compilierten Dateien in ep~%")
  (format *standard-output* "     8. Compilieren von nicht compilierten Dateien in cr~%")
  (format *standard-output* "~%~%  Bitte waehlen Sie: ")
  (read *standard-input*))

(defparameter mkrp*boot_compilerlisting  (make-pathname :name "compilerlisting" :type "text" :directory mkrp*directory))

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
    (7 (mkrp=boot=comp-not-comp-ep))
    (8 (mkrp=boot=comp-not-comp-cr))
    (otherwise (error "unbekannte Auswahl")))
  (mapc (function fmakunbound) '(mkrp-boot mkrp=boot=print-menue mkrp=boot=run-boot mkrp=boot=load mkrp=boot=load-fas
					   mkrp=boot=load-lsp mkrp=boot=compile-all-new mkrp=boot=load-and-compile
					  mkrp=boot=load-and-compile-after)))

(defun mkrp=boot=load ()
  (mapc #'load mkrp*boot_files))

(defun mkrp=boot=load-fas ()
  (mapc (function (lambda (f) (load (make-pathname :type "fasl" :defaults f))))
         mkrp*boot_files))

(defun mkrp=boot=load-lsp ()
  (mapc (function (lambda (f) (load (make-pathname :type "lisp" :defaults f)
                                    :verbose t)))
        mkrp*boot_files))

(defun mkrp=boot=compile-all-new ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (mapc (function (lambda (f)
                    (mkrp=boot=load-and-compile f)
                    (load (make-pathname :type "fasl" :defaults f))))
        mkrp*boot_files))

(defun mkrp=boot=load-and-compile (f)
  (load (make-pathname :type "lisp" :defaults f) :print t)
  (compile-file f))

(defun mkrp=boot=compile-all ()
  (mapc (function (lambda (f)
                    (compile-file f :messages *standard-output*)))
        mkrp*boot_files))

(defun mkrp=boot=load-and-compile-after ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (let ((nocompile t))
    (declare (special nocompile))
    (mapc (function (lambda (f)
		      (declare (special nocompile))
		      (if nocompile
			  (if (probe-file (make-pathname :type "fasl" :defaults f))
			      (load f :verbose t)
			      (progn (setq nocompile nil)
				     (load (make-pathname :type "lisp" :defaults f) :verbose t :print t)
				     (format t "~&Compilation starts ...")
				     (compile-file f)
				     (format t "~& ... Compilation ended~%")
				     (load f)))
			  (progn (load (make-pathname :type "lisp" :defaults f) :print t :verbose t)
				 (format t "~&Compilation starts ...")
				 (compile-file f)
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
			  (if (probe-file (make-pathname :type "fasl" :defaults f))
			      (load f)
			      (progn (load (make-pathname :type "lisp" :defaults f) :print t)
				     (format t "~&Compilation starts ...")
				     (compile-file f)
				     (format t "~& ... Compilation ended~%")
				     (load f)))))
	  mkrp*boot_files))
  (dribble))

(defun mkrp=boot=comp-not-comp-ep ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (let ()
    (declare (special nocompile))
    (mapc (function (lambda (f)
		      (declare (special nocompile))
			  (if (probe-file (make-pathname :type "fasl" :defaults f))
			      (load f)
			      (progn (load (make-pathname :type "lisp" :defaults f) :print t)
				     (format t "~&Compilation starts ...")
				     (compile-file f)
				     (format t "~& ... Compilation ended~%")
				     (load f)))))
	  mkrp*boot_files.ep))
  (dribble))


(defun mkrp=boot=comp-not-comp-cr ()
  (when mkrp*boot_compilerlisting (dribble mkrp*boot_compilerlisting))
  (let ()
    (declare (special nocompile))
    (mapc (function (lambda (f)
		      (declare (special nocompile))
			  (if (probe-file (make-pathname :type "fasl" :defaults f))
			      (load f)
			      (progn (load (make-pathname :type "lisp" :defaults f) :print t)
				     (format t "~&Compilation starts ...")
				     (compile-file f)
				     (format t "~& ... Compilation ended~%")
				     (load f)))))
	  mkrp*boot_files.cr))
  (dribble))

(proclaim '(optimize (space 3) (speed 0) (safety 0)))
  

(defun mkrp-boot ()
  (mkrp=boot=run-boot (mkrp=boot=print-menue))
  (mkrp-rem.file "Scuzzy C:mkrp:sys:mac-boot.lisp")
  (fmakunbound 'ds=clause.get)
  (fmakunbound 'ds=clause.put)
  (fmakunbound 'ds=link.get)
  (fmakunbound 'ds=link.put))

(defun mkrp-start ()
  (in-package "MKRP")
  (fresh-line)
  (format t "Loading repair file ~S" "Scuzzy C:mkrp:repair")
  (load "Scuzzy C:mkrp:repair")
  (values))

(defun mkrp-dumper ()
  (mkrp::mkrp-init)
  (user::disksave "Scuzzy C:mkrp:mkrp" :restart-function #'mkrp-start :full-gc t))

(defun mkrp-rem.file (pathname)
  (with-open-file (st pathname)
    (let (ex)
      (while (not (eq (setq ex (read st nil :eof)) :eof))
             (cond ((consp ex)
                    (case (first ex)
                      (defvar (makunbound (second ex)))
                      (defun (fmakunbound (second ex))))))))))

