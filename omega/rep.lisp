;;; -*- syntax: common-lisp; package: omega; base: 10; mode: lisp -*-

(in-package "OMEGA")

(setq mkrp*dir "/home/omega/examples/deussen/4-8-2/")

(defun omega~top (&optional (interface (asi~create)))
  (declare 
   (authors nesmith)
   (input "An interface (creates one of type ASI~CREATE by default.")
   (value "Undefined")
   (effect "After initialization, runs a read-eval-print loop, reading a
command, getting its arguments and applying the command."))
  (let ((omega*current-interface interface)
	(omega*current-ndproof nil)
	(omega*current-db nil)
	(omega*current-databases nil)
	(omega*current-theory nil)
	(omega*current-ndstyle nil)
	(omega*current-command nil))
    (labels ((loop-for-arg (name type help default)
	       (let ((prompt (if name (format nil "~A (~A) ~A: " 
					      name type help)
			       "OMEGATOP: ")))
		 (block nil
		   (loop
		     (sys~handler-case
		      (progn
			(inter~terpri omega*current-interface)
			(if (com~specified-arg-p default)
			    (inter~prompt-for-input-with-default omega*current-interface prompt type default)
			  (inter~prompt-for-input omega*current-interface prompt type)))
		      (arg+input-error (c) (inter~print-error 
					    omega*current-interface c)
				       (inter~terpri omega*current-interface))
		      (:no-error (obj) (return obj)))))))
	     (loop-for-args (names types helps command input-args)
	       (let ((real-args nil))
		 (setq real-args
		   (mapcar #'(lambda (name type help arg)
			       (if (com~specified-arg-p arg)
				   (sys~handler-case
				    (arg~read-type type arg)
				    (arg+input-error (c) (inter~print-error 
							  omega*current-interface c)
						     (inter~terpri omega*current-interface)
						     (loop-for-arg name type help (com~unspecified))))
				 arg))
			   names types helps input-args))
		 (loop
		   (let* ((poss-args (com~apply-defaults command real-args))
			  (n (or (position-if-not #'com~specified-arg-p real-args)
				 (mismatch poss-args real-args
					   :test-not 
					   #'(lambda (x y) (and (com~specified-arg-p x)
								(not (com~specified-arg-p y))))))))
		     (if n
			 (setf (nth n real-args)
			   (loop-for-arg (nth n names) (nth n types) (nth n helps) (nth n poss-args)))
		       (return-from loop-for-args real-args))))))
	     (main-loop ()
	       (let ((input-list nil))
		 (inter~terpri omega*current-interface)
		 (inter~output-object omega*current-interface "OMEGATOP: ")
		 (setq input-list (sys~handler-case
				   (asi~linereadp (asi~input omega*current-interface))
				   (asi+no-input-error () nil)))
		 (when input-list
		   (setq omega*current-command
		     (sys~handler-case
		      (arg~read-type 'command (car input-list))
		      (arg+input-error (c) (inter~print-error 
					    omega*current-interface c)
				       (inter~terpri omega*current-interface)
				       (return-from main-loop))))
		   (let* ((argnames (com~argnames omega*current-command))
			  (argtypes (com~argtypes omega*current-command))
			  (arghelps (com~arghelps omega*current-command))
			  (input-args (append (subseq (cdr input-list) 0 
						      (min (length argtypes) (length input-list)))
					      (make-list (max 0 
							      (- (length argtypes) 
								 (length (cdr input-list))))
							 :initial-element (com~unspecified))))
			  (comhelp (com~help omega*current-command))
			  (fun (com~function omega*current-command))
			  (real-args nil))
		     (setq real-args
		       (loop-for-args argnames argtypes arghelps omega*current-command input-args))
		     (apply (symbol-function fun) real-args))))))

      (with-simple-restart (abort "Exit OMEGA top level.")
	(loop
	 (with-simple-restart (abort "Return to OMEGA top level." )
	   (sys~handler-case
		(main-loop)
		(omega+leave-omega (c) 
				   (declare (ignore c)) 
				   (return-from omega~top nil)))
	   #|(if omega*debugger-on-break
	       (sys~handler-case
		(main-loop)
		(omega+leave-omega (c) 
				   (declare (ignore c)) 
				   (return-from omega~top nil))
		(com+abort-command (c) (inter~print-warning interface c)))
	       (sys~handler-case
		(main-loop)
		(omega+leave-omega (c) (declare (ignore c)) 
				   (return-from omega~top nil))
		(com+abort-command (c) (inter~print-warning interface c))
		(break (c) 
		       (inter~print-warning 
			omega*current-interface c))
		(simple-error (c)
			      (inter~print-warning 
			       omega*current-interface c))
		(sys+error (c) (inter~terpri omega*current-interface)
			   (inter~print-warning omega*current-interface c)
			   (inter~terpri omega*current-interface)))
	       )|#))))))




  
