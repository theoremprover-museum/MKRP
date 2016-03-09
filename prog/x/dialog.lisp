;;---------------------------------------------------------------------------------------------
;;                                    dialog module
;;
;; edited:      6-JUL-92
;; Authors:     Michael Seyfried (email: Michael_Seyfried@lu.maus.de)
;; Description: This module provides a generic dialog mechanism for X using the CLX Common
;;              Lisp X Interface.
;;---------------------------------------------------------------------------------------------



;; global setups


(proclaim '(declaration edited authors input effect value))



;; common constants


(defconstant dial*cursor.font.name "cursor")
(defconstant item.edit.id 'item.edit)


;; common variables:


(defvar dial*debug.stream nil)
(defvar dial*exit.handler.list nil)
(defvar dial*display nil)
(defvar dial*screen)
(defvar dial*screen.black.pixel)
(defvar dial*screen.white.pixel)
(defvar dial*screen.colormap)
(defvar dial*screen.black.color)
(defvar dial*screen.white.color)
(defvar dial*cursor.font)



;; initialisation and deinitialisation of the dialog interface: 


(defun dial-init (&optional (host.name ""))
  (declare (edited "10-OCT-91")
	   (authors SEYFRIED)
	   (input   "HOST.NAME is the optional name of the host machine on which the X server    "
		    "executes. If no host name is specified, the dialog module is connected to   "
		    "the server listed in the UNIX environment DISPLAY variable.                 ")
	   (effect  "Initializes the dialog iterface. Permanently used resources are allocated   "
                    "by this function. A call to this function is required before usage of any   "
		    "other dialog interface function. To free allocated resources a call to the  "
		    "function DIAL-EXIT is required after usage of the dialog interface.         "
		    "The following modules belonging to the dialog interface are initialized:    "
		    "menu.                                                                       ")
	   (value   "t if the connection to the X server was successful, nil otherwise.          "))
  (setq dial*display (XLIB:open-display host.name))
  (cond (dial*display
	 (setq dial*screen (first (XLIB:DISPLAY-ROOTS dial*display)))
	 (setq dial*screen.black.pixel (XLIB:SCREEN-BLACK-PIXEL dial*screen))
	 (setq dial*screen.white.pixel (XLIB:SCREEN-WHITE-PIXEL dial*screen))
	 (setq dial*screen.colormap (XLIB:SCREEN-DEFAULT-COLORMAP dial*screen))
	 (multiple-value-setq (dial*screen.black.color dial*screen.white.color)
			      (values-list (XLIB:QUERY-COLORS dial*screen.colormap
							      (list dial*screen.black.pixel
								    dial*screen.white.pixel))))
	 (setq dial*cursor.font (XLIB:OPEN-FONT dial*display dial*cursor.font.name))
	 (menu-init dial*display 
		    dial*screen
		    :menu.fg.pixel dial*screen.black.pixel
		    :menu.bg.pixel dial*screen.white.pixel)
	 t)
	(t nil)))


(defun dial-exit ()
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "-                                                                           ")
	   (effect  "Frees all resources allocated by a call to DIAL-INIT. After a call to this  "
		    "function a call to DIAL-INIT is required again before calling any other     "
                    "dialog interface function. This function calls all exit handlers in order   "
		    "stored in the exit dialog handler list before deallocation of the resources.")
	   (value   "Undefined.                                                                  "))
  (dial-ctrl_call.exit.handlers dial*exit.handler.list)
  (menu-exit)
  (XLIB:CLOSE-DISPLAY dial*display)
  t)


(defun dial-add.exit.handler (exit.handler)
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "EXIT.HANDLER is the quoted exit dialog handler function name.              ")
	   (effect  "Adds an exit dialog handler in front of the exit dialog handler list if it "
		    "is not already a member of that list. The exit dialog handlers are called  "
		    "by DIAL-EXIT before deallocation of the common resources.                  ")
	   (value   "Undefined.                                                                 "))
  (dial-ctrl_add.exit.handler dial*exit.handler.list exit.handler))


(defun dial-remove.exit.handler (exit.handler)
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "EXIT.HANDLER is the quoted exit dialog handler function name.              ")
	   (effect  "Removes an exit dialog handler from the exit dialog handler list.          ")
	   (value   "Undefined.                                                                 "))
  (dial-ctrl_remove.exit.handler dial*exit.handler.list exit.handler))


(defmacro dial-ctrl_add.exit.handler (exit.handler.list exit.handler)
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "EXIT.HANDLER.LIST is a list of exit handlers. EXIT.HANDLER is the quoted   "
		    "exit dialog handler function name.                                         ")
	   (effect  "Adds an exit dialog handler in front of the exit dialog handler list if it "
		    "is not already a member of that list.                                      ")
	   (value   "Undefined.                                                                 "))
  `(pushnew ,exit.handler ,exit.handler.list))


(defmacro dial-ctrl_remove.exit.handler (exit.handler.list exit.handler)
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "EXIT.HANDLER.LIST is a list of exit handlers. EXIT.HANDLER is the quoted   "
		    "exit dialog handler function name.                                         ")
	   (effect  "Removes an exit dialog handler from the exit dialog handler list.          ")
	   (value   "Undefined.                                                                 "))
  `(setq ,exit.handler.list (delete ,exit.handler ,exit.handler.list)))


(defun dial-ctrl_call.exit.handlers (exit.handler.list)
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "EXIT.HANDLER.LIST is a list of exit handlers.                              ")
	   (effect  "Calls all exit handlers stored in EXIT.HANDLER.LIST in order of ocurrance. ")
	   (value   "Undefined.                                                                 "))
  (mapc #'funcall exit.handler.list))


(defun dial-cursor_create (cursor.shape)  
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "CURSOR.SHAPE is the index of the char in the cursor font used as cursor.   ")
	   (effect  "Creates an cursor. The source mask is taken from the char with the index   "
		    "CURSOR.SHAPE in the cursor font. See documentation for the XLIB function   "
		    "XCreateFontCursor.                                                         ")
	   (value   "a clx cursor                                                               "))
  (XLIB:CREATE-GLYPH-CURSOR :source-font dial*cursor.font
			    :source-char cursor.shape
			    :mask-font   dial*cursor.font
			    :mask-char   (1+ cursor.shape)
			    :foreground  dial*screen.black.color
			    :background  dial*screen.white.color))

(defstruct (dial-area
	    (:conc-name dial-area_)
	    (:constructor dial-area_make)
	    (:predicate dial-area_p))
  "data structure for a drawable bounding box"
  x                                           ; outside x
  y                                           ; outside y
  w                                           ; outside w
  h)                                          ; outside h

(defun dial-drawable_get.area (drawable &optional &key (hor.offset 0) (vert.offset 0))
  (let* ((border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH drawable))))
    (let ((x            (XLIB:DRAWABLE-X drawable))
	  (y            (XLIB:DRAWABLE-Y drawable))
	  (width        (+ (XLIB:DRAWABLE-WIDTH drawable) border.offset*2 hor.offset))
	  (height       (+ (XLIB:DRAWABLE-HEIGHT drawable) border.offset*2 vert.offset)))
      (dial-area_make :x x :y y :w width :h height))))

(defun dial-drawable_set.area (drawable area)
;;  (format t "set.area: ~A~%" area)
  (let* ((border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH drawable))))
    (setf (XLIB:DRAWABLE-X drawable) (dial-area_x area))
    (setf (XLIB:DRAWABLE-Y drawable) (dial-area_y area))
    (setf (XLIB:DRAWABLE-WIDTH drawable) (- (dial-area_w area) border.offset*2))
    (setf (XLIB:DRAWABLE-HEIGHT drawable) (- (dial-area_h area) border.offset*2))))


(defun dial-struct_print (dial-struct stream level)
  (if (or (null *print-level*) (<= level *print-level*))
      (format stream
	      "dialog structure:  type: ~S  window: ~S  childs:~%~S"
	      (dial-struct_type dial-struct)
	      (dial-struct_window dial-struct)
	      (dial-struct_childs dial-struct))))

(defstruct (dial-struct
	    (:conc-name dial-struct_)
	    (:constructor dial-struct_make)
	    (:predicate dial-struct_p)
	    (:print-function dial-struct_print))
  "A generic dialog structure"
  type                                    ; type of dialog
  data                                    ; private dialog data
  window                                  ; dialog window
  width                                   ; width of dialog; if nil message 'compute.width is sent
  height                                  ; height of dialog; if nil message 'compute.height is sent
  parent                                  ; parent object of the dialog
  childs                                  ; A recursive list of columns, where columns are lists of
                                          ; lines. The atoms are all structures of type dial-struct.
  event.handler                           ; event handler of the dialog
  dispatch)                               ; the dialog dispatch function

;; event.handler dial.struct &rest event-slots &allow-other-keys              [Function]
;; dispatch      dial.struct message-id &rest message-parameters              [Function]
;; Common messages to the dispatch function:
;;   'dispose  disposes itself including all childs, the dialog window and all subwindows
;;   'compute.width     computes the (minimum) width of the dialog window
;;   'compute.height    computes the (minimum) height of the dialog window
;;   'set.area          sets the area of the dialog window
;;   'get.area          returns the area of the dialog window
;;   'reset             resets the dialog to the state before mapped


(defun dial-struct_call.dispatch (dial.struct &rest message)
  (apply (dial-struct_dispatch dial.struct) dial.struct message))

;--------------------------------------------------------------------------------------------------------

(defun dial-struct_compute.width (dial.struct &optional (reset nil))
  (let ((width (dial-struct_width dial.struct)))
    (cond ((or (not width)
	       reset)
	   (setq width (dial-struct_call.dispatch dial.struct 'compute.width))
	   (setf (dial-struct_width dial.struct) width)))
    width))

(defun dial-struct_compute.height (dial.struct &optional (reset nil))
  (let ((height (dial-struct_height dial.struct)))
    (cond ((or (not height)
	       reset)
	   (setq height (dial-struct_call.dispatch dial.struct 'compute.height))
	   (setf (dial-struct_height dial.struct) height)))
    height))

(defun dial-struct_compute.columns.height (columns)
  (cond ((null columns)
	 0)
	(t (apply #'max (mapcar #'dial-struct_compute.column.height columns)))))

(defun dial-struct_compute.row.height (row)
  (cond ((listp row)
	 (dial-struct_compute.columns.height row))
	(t (dial-struct_compute.height row))))

(defun dial-struct_compute.rows.height (rows)
  (let ((height 0))
    (dolist (row rows)
	    (setq height
		  (+ (dial-struct_compute.row.height row)
		     height)))
    height))

(defun dial-struct_compute.column.height (column)
  (cond ((listp column)
	 (dial-struct_compute.rows.height column))
	(t (dial-struct_compute.height column))))

(defun dial-struct_compute.columns.heights (columns)
  ; returns the list of heights of the columns
  (mapcar #'dial-struct_compute.column.height columns))

(defun dial-struct_compute.rows.width (rows)
  (cond ((null rows)
	 0)
	(t (apply #'max (mapcar #'dial-struct_compute.row.width rows)))))

(defun dial-struct_compute.column.width (column)
  (cond ((listp column)
	 (dial-struct_compute.rows.width column))
	(t (dial-struct_compute.width column))))

(defun dial-struct_compute.columns.width (columns)
  (let ((width 0))
    (dolist (column columns)
	    (setq width
		  (+ (dial-struct_compute.column.width column)
		     width)))
    width))

(defun dial-struct_compute.row.width (row)
  (cond ((listp row)
	 (dial-struct_compute.columns.width row))
	(t (dial-struct_compute.width row))))

(defun dial-struct_compute.rows.widths (rows)
  (mapcar #'dial-struct_compute.row.width rows))

(defun dial-struct_compute.width.of.childs (dial-struct)
  (dial-struct_compute.columns.width (dial-struct_childs dial-struct)))

(defun dial-struct_compute.height.of.childs (dial-struct)
  (dial-struct_compute.columns.height (dial-struct_childs dial-struct)))

;--------------------------------------------------------------------------------------------------------

(defun dial-struct_set.area.of.row (row x y
					&key
					(hor-offset 0)
					(vert-offset 0))
;;  (format t "set.area.of.row: ~A x: ~A y: ~A hor-offset: ~A~%" row x y hor-offset)
  (cond ((listp row)
	 (dial-struct_set.area.of.columns row x y :hor-offset hor-offset))
	(t (let ((width (+ (dial-struct_compute.width row) hor-offset))
		 (height (+ (dial-struct_compute.height row) vert-offset)))
	     (dial-struct_set.area row
				   (dial-area_make :x x
						   :y y
						   :w width
						   :h height))
	     (values-list (list width height))))))

(defun dial-struct_set.area.of.rows (rows x y
					  &key
					  (widths (dial-struct_compute.rows.widths rows))
					  (max-width (cond ((null rows)
							    0)
							   (t (apply #'max widths))))
					  (vert-offset 0)
					  (no-of-rows (length rows)))
;;  (format t
;;	  "set.area.of.rows:~%x: ~A~%y: ~A~%widths: ~A~%max-width: ~A~%vert-offset: ~A~%no-of-rows: ~A~%"
;;	  x
;;	  y
;;	  widths
;;	  max-width
;;	  vert-offset
;;	  no-of-rows)
  (cond ((null rows)
	 (values-list '(0 0)))
	(t
	 (let ((first.row (first rows))
	       (first.width (first widths))
	       (offset (cond ((> no-of-rows 1)
			      (floor vert-offset
				     (1- no-of-rows)))
			     (t vert-offset))))
	   (multiple-value-bind (w h)
	       (dial-struct_set.area.of.row first.row x y
					    :hor-offset (- max-width
							first.width)
					    :vert-offset (cond ((= 1 no-of-rows)
								vert-offset)
							       (t 0)))
	       (multiple-value-bind (rest.w rest.h)
	           (dial-struct_set.area.of.rows (rest rows)
						 x (+ y h offset)
						 :widths (rest widths)
						 :max-width max-width
						 :vert-offset (- vert-offset offset)
						 :no-of-rows (1- no-of-rows))
		   (values-list (list max-width
				      (+ h rest.h offset)))))))))

(defun dial-struct_set.area.of.column (column x y
					      &key
					      (vert-offset 0)
					      (hor-offset 0))
;;  (format t "set.area.of.column: ~A x: ~A y: ~A vert-offset: ~A~%" column x y vert-offset)
  (cond ((listp column)
	 (dial-struct_set.area.of.rows column x y :vert-offset vert-offset))
	(t (let ((width (+ (dial-struct_compute.width column) hor-offset))
		 (height (+ (dial-struct_compute.height column) vert-offset)))
	     (dial-struct_set.area column
				   (dial-area_make :x x
						   :y y
						   :w width
						   :h height))
	     (values-list (list width height))))))

(defun dial-struct_set.area.of.columns (columns x y
						&key
						(heights (dial-struct_compute.columns.heights columns))
						(max-height (cond ((null columns)
								   0)
								  (t (apply #'max heights))))
						(hor-offset 0)
						(no-of-columns (length columns))
						(last-is-edit-item (let ((dial-struct (nth (1- no-of-columns)
											   columns)))
								     (cond ((dial-struct_p dial-struct)
									    (dial-struct_type.eq dial-struct
												 item.edit.id))))))
  ; sets the window size and position of the columns
  ; returns width
  ;         height
;;  (format t
;;	  "set.area.of.columns:~%x: ~A~%y: ~A~%heights: ~A~%max-height: ~A~%hor-offset: ~A~%no-of-columns: ~A~%"
;;	  x
;;	  y
;;	  heights
;;	  max-height
;;	  hor-offset
;;	  no-of-columns)
  (cond ((null columns)
	 (values-list '(0 0)))
	(t
	 (let ((first.column (first columns))
	       (first.height (first heights))
	       (offset (cond (last-is-edit-item
			      (cond ((> no-of-columns 1)
				     0)
				    (t hor-offset)))
			     ((> no-of-columns 1)
			      (floor hor-offset
				     (1- no-of-columns)))
			     (t hor-offset))))
	   (multiple-value-bind (w h)
	       (dial-struct_set.area.of.column first.column x y
					       :vert-offset (- max-height
							    first.height)
					       :hor-offset (cond ((= 1 no-of-columns)
								  hor-offset)
								 (t 0)))
	       (multiple-value-bind (rest.w rest.h)
	           (dial-struct_set.area.of.columns (rest columns)
						    (+ x w offset) y
						    :heights (rest heights)
						    :max-height max-height
						    :hor-offset (- hor-offset offset)
						    :no-of-columns (1- no-of-columns)
						    :last-is-edit-item last-is-edit-item)
		   (values-list (list (+ w rest.w offset)
				      max-height))))))))

(defun dial-struct_set.area.of.childs (dial-struct &optional (x 0) (y 0))
  ; sets the window size and position of the childs of dial.struct
  ; returns width
  ;         height
  (dial-struct_set.area.of.columns (dial-struct_childs dial-struct) x y))
	   				  
(defun dial-struct_standard.dispatch (dial.struct message &rest parameters)
  (let ((window (dial-struct_window dial.struct)))
    (cond ((eq message 'dispose)
	   (dial-struct_send.message.to.childs dial.struct (cons 'dispose parameters))
	   (XLIB:DESTROY-WINDOW window)
	   ; dispatch of subclass must dispose additional resources
	   )
	  ((eq message 'get.area)
	   (dial-drawable_get.area window))
	  ((eq message 'set.area)
	   (let ((area (first parameters)))
	     (dial-drawable_set.area window area))
	   ; dispatch of subclass must set the areas of the childs
	   )
	  ((eq message 'compute.width)
	   ; computes the width of the childs + the width of the window border
	   (let ((childs.width (dial-struct_compute.width.of.childs dial.struct))
		 (border.offset (* 2 
				   (XLIB:DRAWABLE-BORDER-WIDTH (dial-struct_window dial.struct)))))
	     (+ border.offset childs.width))
	   ; dispatch of subclass must compute the width of the whole object
	   )
	  ((eq message 'compute.height)
	   ; computes the height of the childs + the height of the window border
	   (let ((childs.height (dial-struct_compute.height.of.childs dial.struct))
		 (border.offset (* 2 
				   (XLIB:DRAWABLE-BORDER-WIDTH (dial-struct_window dial.struct)))))
	     (+ border.offset childs.height))
	   ; dispatch of subclass must compute the height of the whole object
	   )
	  ((eq message 'reset)
	   )
	  (t
	   (error "unknown message ~S ~S~%" message parameters))
	  )
    )
  )

(defun dial-struct_dispose (dial.struct)
  (dial-struct_call.dispatch dial.struct 'dispose))

(defun dial-struct_get.area (dial.struct)
  (dial-struct_call.dispatch dial.struct 'get.area))

(defun dial-struct_set.area (dial.struct area)
  (dial-struct_call.dispatch dial.struct 'set.area area))

(defun dial-struct_type.eq (dial.struct type)
  (eq (dial-struct_type dial.struct) type))

(defun dial-struct_apply.to.dialogs (dialogs
				     function
				     args
				     &key (type :all) predicate (pred.rest.args nil))
  (cond ((null dialogs))
	(t (let ((dialog (first dialogs)))
	     (cond ((dial-struct_p dialog)
		    (cond ((and (or (eq :all type)
				    (dial-struct_type.eq dialog type))
				(or (null predicate)
				    (not (functionp predicate))
				    (apply predicate dialog pred.rest.args)))
			   (apply function dialog args))))
		   (t (dial-struct_apply.to.dialogs dialog
						    function
						    args
						    :type type
						    :predicate predicate
						    :pred.rest.args pred.rest.args))))
	   (dial-struct_apply.to.dialogs (rest dialogs)
					 function
					 args
					 :type type
					 :predicate predicate
					 :pred.rest.args pred.rest.args))))

(defun dial-struct_apply.to.childs (dial.struct
				    function
				    args
				    &key (type :all) predicate (pred.rest.args nil))
  (dial-struct_apply.to.dialogs (dial-struct_childs dial.struct)
				function
				args
				:type type
				:predicate predicate
				:pred.rest.args pred.rest.args))

(defun dial-struct_send.message.to.dialogs (dialogs
					    message
					    &key (type :all) predicate (pred.rest.args nil))
  (dial-struct_apply.to.dialogs dialogs
				#'dial-struct_call.dispatch
				message
				:type type
				:predicate predicate
				:pred.rest.args pred.rest.args))

(defun dial-struct_send.message.to.childs (dial.struct
					   message
					   &key (type :all) predicate (pred.rest.args nil))
  (dial-struct_send.message.to.dialogs (dial-struct_childs dial.struct)
				       message
				       :type type
				       :predicate predicate
				       :pred.rest.args pred.rest.args))

(defun dial-struct_find.dialog (dialogs predicate &key (pred.rest.args nil) (type :all))
  (cond ((null dialogs) nil)
	(t (let ((dialog (first dialogs)))
	     (cond ((dial-struct_p dialog)
		    (cond ((and (or (eq :all type)
				    (dial-struct_type.eq dialog type))
				(apply predicate dialog pred.rest.args))
			   dialog)
			  (t (dial-struct_find.dialog (rest dialogs)
						      predicate
						      :pred.rest.args pred.rest.args
						      :type type))))
		   (t (let ((result (dial-struct_find.dialog dialog
							     predicate
							     :pred.rest.args pred.rest.args
							     :type type)))
			(cond (result result)
			      (t (dial-struct_find.dialog (rest dialogs)
							  predicate
							  :pred.rest.args pred.rest.args
							  :type type))))))))))

(defun dial-struct_find.child.by.window (dial-struct window)
  (labels ((child.window.eq (child window)
			    (eq (dial-struct_window child) window)))
	  (dial-struct_find.dialog (dial-struct_childs dial-struct)
				   #'child.window.eq
				   :pred.rest.args (list window))))

(defun dial-struct_call.event.handler (dial-struct &rest parameters &key &allow-other-keys)
  (apply (dial-struct_event.handler dial-struct) dial-struct parameters))

(defparameter dial*struct.actual nil)
(defparameter dial*struct.event.keys nil)

(defun dial=struct_event.handler (&rest event-slots &key &allow-other-keys)
  (apply #'dial-struct_call.event.handler dial*struct.actual event-slots))

(defconstant dial-struct_event.result.symbol :result)

(defun dial-struct_process.event (dial-struct)
  (let ((old.dial.struct dial*struct.actual))
    (setq dial*struct.actual dial-struct)
    (let ((result (XLIB:PROCESS-EVENT dial*display
				      :handler #'dial=struct_event.handler
				      :force-output-p t)))
      (setq dial*struct.actual old.dial.struct)
      result)))

(defun dial-struct_process.events (dial-struct)
  (do ((result (dial-struct_process.event dial-struct) (dial-struct_process.event dial-struct)))
      ((and (listp result) (eq (first result) dial-struct_event.result.symbol))
       (second result))))
  
(defun dial=struct_discard.event.handler (&rest event-slots &key event-window event-key &allow-other-keys)
;;  (format dial*debug.stream "dial-struct_discard.event.handler: ~S~%" event-window)
  (cond ((eq dial*struct.actual :all)
	 t)
	(t
	 (let ((dialog.window (dial-struct_window dial*struct.actual)))
	   (and (or (eq event-window dialog.window)
		    (dial-struct_find.child.by.window dial*struct.actual event-window))
		(or (null dial*struct.event.keys)
		    (member event-key dial*struct.event.keys)))))))

(defun dial-struct_discard.event (dial-struct &optional event-keys)
  (let ((old.dial.struct dial*struct.actual)
	(old.event.keys  dial*struct.event.keys))
    (setq dial*struct.actual dial-struct)
    (setq dial*struct.event.keys event-keys)
    (let ((result (XLIB:PROCESS-EVENT dial*display
				      :handler #'dial=struct_discard.event.handler
				      :force-output-p t
				      :timeout 200))) ; works properly if menu*debug.stream is nil
      (setq dial*struct.actual old.dial.struct)       ; otherwise higher numbers should be used (1000)
      (setq dial*struct.event.keys old.event.keys)
      result)))
				
(defun dial-struct_discard.events (dial-struct &optional event-keys)
  (do ((result (dial-struct_discard.event dial-struct event-keys)
	       (dial-struct_discard.event dial-struct event-keys)))
      ((null result))))

(defun dial-struct_set.childs (dial-struct &rest columns)
  (setf (dial-struct_childs dial-struct) columns))

(defun dial-struct_make.child.column (&rest lines)
  lines)

(defun dial-struct_make.child.line (&rest columns)
  columns)

(defun dial-misc_make.pair.lists (l1 l2)
  (cond ((or (null l1) (null l2)) nil)
	(t (cons (list (first l1)
		       (first l2))
		 (dial-misc_make.pair.lists (rest l1)
					    (rest l2))))))

(defun dial-misc_pointer.in.window.p (window &optional x y)
  (cond ((or (null x)
	     (null y))
	 (multiple-value-setq (x y)
	     (XLIB:POINTER-position window))))
  (let ((window.w (XLIB:DRAWABLE-WIDTH window))
	(window.h (XLIB:DRAWABLE-HEIGHT window)))
    (and (>= x 0)
	 (>= y 0)
	 (<  x window.w)
	 (<  y window.h))))