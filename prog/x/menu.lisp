;;---------------------------------------------------------------------------------------------
;;                                            menu module
;;
;; edited:      8-JUL-92
;; Authors:     Michael Seyfried (email: Michael_Seyfried@lu.maus.de)
;; Description: This module implements the functions described in sections 20.1 and 20.2 of
;;              the KK-Lisp Manual including menus and choose-variable-values under X using the
;;              CLX Common Lisp X Interface and the dialog module.
;;              For more information see sections 20.1 and 20.2 in the KK-Lisp Manual,
;;              AG-Siekmann, SEKI-Working-Paper 1991, Fachbereich Informatik, Universitaet
;;              Kaiserslautern.
;;              Implementation restrictions: Menus of type :choice are not implemented.
;;---------------------------------------------------------------------------------------------


;; global setups


(proclaim '(declaration edited authors input effect value))



;; common constants


(defconstant menu*default.font.name "9x15")           ; unproportional fonts only!!
(defconstant menu*default.bold.font.name "9x15bold")
(defconstant menu*cursor.arrow.index  68)
(defconstant menu*cursor.text.index 152)
(defconstant menu.single.id 'menu.single)
(defconstant title.id 'title)
(defconstant item.single.id 'item.single)
(defconstant item.check.box.id 'item.check.box)
(defconstant item.multiple.id 'item.multiple)
(defconstant item.choose.id 'item.choose)
(defconstant item.filler.id 'item.filler)
(defconstant menu.multiple.id 'menu.multiple)
(defconstant menu.choose.id 'choose.menu)



;; common variables:


(defvar menu*debug.stream nil)
(defvar menu*exit.handler.list nil)
(defvar menu*root.window)
(defvar menu*fg.pixel)
(defvar menu*bg.pixel)
(defvar menu*gcontext)
(defvar menu*font)
(defvar menu*bold.font)
(defvar menu*cursor)
(defvar menu*text.cursor)
(defvar menu*title.hor.offset 2)
(defvar menu*title.vert.offset 2)
(defvar menu*item.single.hor.offset 2)
(defvar menu*item.single.vert.offset 2)
(defvar menu*item.multiple.hor.offset 2)
(defvar menu*item.multiple.vert.offset 2)
(defvar menu*item.choose.hor.offset 16)
(defvar menu*item.choose.vert.offset 2)
(defvar menu*item.edit.hor.offset 2)
(defvar menu*item.edit.vert.offset 2)
(defvar menu*check.box.hor.offset 2)
(defvar menu*check.box.vert.offset 2)
(defvar menu*exit.button.hor.offset 16)
(defvar menu*exit.button.vert.offset 2)
(defvar menu*title.filler.width 2)
(defvar menu*title.filler.height 5)
(defvar menu*buttons.filler.width 1)
(defvar menu*buttons.filler.height 5)
(defvar menu*text.button.border.offset 4)

(defvar menu*unmapped nil)
(defvar menu*mapped nil)


;;--------------------------------------------------------------------------------------------------------
;; initialisation and deinitialisation of the menu module:
;;--------------------------------------------------------------------------------------------------------

(defun menu-init (display screen &optional &key (menu.font.name menu*default.font.name)
			                        (menu.bold.font.name menu*default.bold.font.name)
			                        (menu.fg.pixel (XLIB:SCREEN-BLACK-PIXEL screen))
						(menu.bg.pixel (XLIB:SCREEN-WHITE-PIXEL screen))
						(cursor.source.char menu*cursor.arrow.index))
  (declare (edited "10-OCT-91")
	   (authors SEYFRIED)
	   (input   "DISPLAY is a CLX display structure and SCREEN is a CLX screen structure.    "
		    "The following optional keyword parameters may be specified: MENU.FONT.NAME  "
		    "is the name of the font used in menus, MENU.FG.PIXEL is the pixel value for "
		    "the menu foreground, MENU.BG.PIXEL is the pixel value for the menu back-    "
		    "ground and CURSOR.SOURCE.CHAR is the char of cursor font used as the source "
		    "for the cursor. The value for CURSOR.SOURCE.CHAR should be one of the       "
		    "values listed in the include file 'X11/cursorfont.h'.                       ")
	   (effect  "Initializes the menu module. Permanently used resources are allocated by    "
                    "this function. A call to this function is required before usage of any othe "
		    "function in the menu module. To free allocated resources a call to the      "
		    "function MENU-EXIT is required.                                             ")
	   (value   "Undefined.                                                                  "))
  (setq menu*root.window (XLIB:SCREEN-ROOT screen))
  (setq menu*fg.pixel menu.fg.pixel)
  (setq menu*bg.pixel menu.bg.pixel)
  (setq menu*font (XLIB:OPEN-FONT display menu.font.name))
  (setq menu*bold.font (XLIB:OPEN-FONT display menu.bold.font.name))
  (setq menu*gcontext (XLIB:CREATE-GCONTEXT
		       :drawable       menu*root.window
		       :foreground     menu*fg.pixel
		       :background     menu*bg.pixel
		       :line-width     1
		       :cap-style      :round
		       :font           menu*font))
  (setq menu*cursor (dial-cursor_create cursor.source.char))
  (setq menu*text.cursor (dial-cursor_create menu*cursor.text.index))
  (setq menu*unmapped nil)
  (setq menu*mapped nil)
  t)


(defun menu-exit ()
  (declare (edited "09-OCT-91")
	   (authors SEYFRIED)
	   (input   "-                                                                           ")
	   (effect  "Frees all resources allocated by a call to MENU-INIT. After a call to this  "
		    "function a call to MENU-INIT is required again before calling any other     "
                    "function of the menu module. This function calls all exit menu handlers in  "
		    "order stored in the exit menu handler list before freeing the resources.    ")
	   (value   "Undefined.                                                                  "))
  (dial-ctrl_call.exit.handlers menu*exit.handler.list)
  (XLIB:FREE-CURSOR menu*cursor)
  (XLIB:FREE-CURSOR menu*text.cursor)
  (XLIB:FREE-GCONTEXT menu*gcontext)
  (XLIB:CLOSE-FONT menu*font)
  (setq menu*unmapped nil)
  (setq menu*mapped nil)
  t)


(defun menu-add.exit.handler (exit.handler)
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "EXIT.HANDLER is the quoted exit menu handler function name.                ")
	   (effect  "Adds an exit menu handler in front of the exit menu handler list if it     "
		    "is not already a member of that list. The exit menu handlers are called    "
		    "by MENU-EXIT before deallocation of the common resources.                  ")
	   (value   "Undefined.                                                                 "))
  (dial-ctrl_add.exit.handler menu*exit.handler.list exit.handler))


(defun menu-remove.exit.handler (exit.handler)
  (declare (edited "25-SEP-91")
	   (authors SEYFRIED)
	   (input   "EXIT.HANDLER is the quoted exit menu handler function name.                ")
	   (effect  "Removes an exit menu handler from the exit menu handler list.              ")
	   (value   "Undefined.                                                                 "))
  (dial-ctrl_remove.exit.handler menu*exit.handler.list exit.handler))



;;--------------------------------------------------------------------------------------------------------
;; menu functions:
;;--------------------------------------------------------------------------------------------------------


(defun menu-create (menu-type item-list &key top-line default-item special-choices (help t) (do-it t) (abort t) (momentary t) static no-scroll)
  ;; see section 20.2 in the KK-Lisp manual
  ;; the following parameters are ignored: no-scroll, static (menues are all not static)
  ;; Implementation restriction: menus of type :choice are not implemented
    (cond ((eq :single menu-type)
	   (menu=dial_single.create nil                           ; parent dialog
				    item-list                     ; parameters for single menus
				    :top-line top-line
				    :default-item default-item
				    :help help
				    :momentary momentary))
	  ((eq :multiple menu-type)
	   (menu=dial_multiple.create nil
				      item-list
				      :top-line top-line
				      :default-item default-item
				      :special-choices special-choices
				      :help help
				      :do-it do-it
				      :abort abort
				      :momentary momentary))
	  ))


(defun menu-dispose (menu)
  (dial-struct_dispose menu))


(defun menu-choose (menu &key
			 (unmap-current nil)
			 (map-unmapped t)
			 (near-mode nil)
			 (mouse-cursor nil))
;  (format t "menu-choose:~%unmap-current: ~A  map-unmapped: ~A  near-mode: ~A~%"
;	  unmap-current
;	  map-unmapped
;	  near-mode)
  (cond ((and unmap-current menu*mapped)
	 (menu=dial_unmap.aux menu*mapped)
	 (setq menu*mapped nil)))
  (menu=dial_choose menu near-mode map-unmapped))

(defun menu-unmap ()
  (let ((menu menu*mapped))
    (cond (menu
	   (menu=dial_unmap.aux menu)
	   (setq menu*mapped nil)))
    menu))

(defun menu-map (menu)
  (multiple-value-bind (x y)
		       (menu=dial_calc.orgin menu)
		       (menu=dial_map menu x y)))

(defun menu-deactivate (menu)
  (dial-struct_call.dispatch menu 'deactivate))

(defun menu-get-title (menu)
  (dial-struct_call.dispatch menu 'get.title.string))

(defun menu-get-item-list (menu)
  (dial-struct_call.dispatch menu 'get.item.list))

(defun menu-set-item-list (menu item-list)
  (dial-struct_call.dispatch menu 'set.item.list item-list))

(defun menu-get-selected-items (menu)
  (dial-struct_call.dispatch menu 'get.selected.items))

(defun menu-get-selected-values (menu)
  (dial-struct_call.dispatch menu 'get.selected.values))


;;--------------------------------------------------------------------------------------------------------
;; Choosing Values for Variables (see section 20.1 of the KK-Lisp manual)
;;--------------------------------------------------------------------------------------------------------

(defun choose-variable-values (header variables &optional margin-choices near-mode)
  (let ((choose-menu (menu=dial_choose.create nil header variables margin-choices)))
    (menu-choose choose-menu :near-mode near-mode)
    (menu-dispose choose-menu)))

(defun notify (list-of-items)
  (let ((menu (menu=dial_notify.create list-of-items)))
    (menu-choose menu)
    (menu-dispose menu)))

;;========================================================================================================

;;--------------------------------------------------------------------------------------------------------
;; non common functions
;;--------------------------------------------------------------------------------------------------------

(defun menu=dial_map (menu x y)
  (cond (menu*mapped
	 (push menu*mapped menu*unmapped)
	 (menu=dial_unmap.aux menu*mapped)))
  (setq menu*mapped menu)
  (menu=dial_map.aux menu x y))

(defun menu=dial_map.aux (menu x y)
;  (format t "map.aux:~%")
  (menu=dial_update.geometry menu)
  (dial-struct_send.message.to.childs menu '(reset))
  (let* ((window (dial-struct_window menu))
	 (window.area (dial-drawable_get.area window)))
    (setf (dial-area_x window.area) x)
    (setf (dial-area_y window.area) y)
    (dial-drawable_set.area window window.area)
    (XLIB:MAP-WINDOW window)
    #|(XLIB:GRAB-POINTER window
		       (XLIB:WINDOW-EVENT-MASK window)
		       :owner-p t
		       :cursor menu*cursor)|#
    (XLIB:MAP-SUBWINDOWS window)))

(defun menu=dial_choose (menu &optional near-mode (map-unmapped t))
  (dial-struct_call.dispatch menu 'choose near-mode map-unmapped))

(defun menu=dial_unmap (menu &optional (map-unmapped t))
  (menu=dial_unmap.aux menu)
  (cond (map-unmapped
	 (setq menu*mapped (pop menu*unmapped))
	 (cond (menu*mapped
		(let ((window.area (dial-drawable_get.area (dial-struct_window menu*mapped))))
		  (menu=dial_map.aux menu*mapped
				     (dial-area_x window.area)
				     (dial-area_y window.area))
		  ))))
	(t
	 (setq menu*mapped nil))))

(defun menu=dial_unmap.aux (menu)
  (cond (menu
	 (let ((window (dial-struct_window menu)))
	   (XLIB:UNMAP-WINDOW window))
	 ;(XLIB:UNGRAB-POINTER dial*display)
	 (XLIB:DISPLAY-FORCE-OUTPUT dial*display)
	 (dial-struct_discard.events menu)
	 )))

(defun menu=dial_calc.orgin (menu)
;;  (format t "menu=dial_calc.orgin: window: ~S~%" (dial-struct_window menu))
  (multiple-value-bind (pointer.x pointer.y)
		       (XLIB:POINTER-position menu*root.window)
		       (let ((item (menu=dial_last.exit.button menu))
			     (menu.area (dial-drawable_get.area (dial-struct_window menu)))
			     (x.offset 0)
			     (y.offset 0))
			 (cond ((null item)
				(setq x.offset (floor (dial-area_w menu.area) 2))
				(setq y.offset (floor (dial-area_h menu.area) 2)))
			       (t (let ((area (dial-drawable_get.area (dial-struct_window item))))
				    (setq x.offset (+ (floor (dial-area_w area) 2)
						      (dial-area_x area)))
				    (setq y.offset (+ (dial-area_y area)
						      (floor (dial-area_h area) 2))))))
			 (setq pointer.x (- pointer.x x.offset))
			 (setq pointer.y (- pointer.y y.offset))
			 (cond ((< pointer.x 0)
				(XLIB:WARP-POINTER-RELATIVE dial*display (- pointer.x) 0)
				(setq pointer.x 0)))
			 (cond ((< pointer.y 0)
				(XLIB:WARP-POINTER-RELATIVE dial*display 0 (- pointer.y))
				(setq pointer.y 0)))
			 (let* ((screen.area (dial-drawable_get.area menu*root.window))
				(x2 (+ pointer.x (dial-area_w menu.area)))
				(y2 (+ pointer.y (dial-area_h menu.area))))
			   (cond ((> x2 (dial-area_w screen.area))
				  (let ((offset (- x2 (dial-area_w screen.area))))
				    (XLIB:WARP-POINTER-RELATIVE dial*display (- offset) 0)
				    (setq pointer.x (- pointer.x offset)))))
			   (cond ((> y2 (dial-area_h screen.area))
				  (let ((offset (- y2 (dial-area_h screen.area))))
				    (XLIB:WARP-POINTER-RELATIVE dial*display 0 (- offset))
				    (setq pointer.y (- pointer.y offset))))))
			 (values-list (list pointer.x pointer.y)))))

(defun menu=dial_update.geometry (menu)
  (dial-struct_call.dispatch menu 'update.geometry))

(defun menu=dial_last.exit.button (menu)
  (dial-struct_call.dispatch menu 'last.exit.button))

;; methods:

(defun menu=dial_set.area (menu)
  (dial-struct_set.area menu nil))


;;----------------------------------------- single menu -------------------------------------------------

(defun menu=dial_single.deactivate (single.menu)
  (setf (menu=dial_single.data.last.exit.button (dial-struct_data single.menu)) nil))

(defun menu=dial_single.choose (single.menu &optional near-mode (map-unmapped t))
  (menu=dial_single.update.geometry single.menu)
  (let* ((single.data (dial-struct_data single.menu))
	 (momentary (menu=dial_single.data.momentary single.data)))
    (cond (momentary
	   (menu=dial_single.deactivate single.menu)))
    (let ((default.item (menu=dial_single.data.default.item single.data)))
      (cond ((and (not (null default.item))
		  (or momentary
		      (null (menu=dial_single.data.last.exit.button single.data))))
	     (setf (menu=dial_single.data.last.exit.button single.data)
		   (nth (menu=dial_single.data.default.item single.data)
			(menu=dial_single.data.items single.data))))))
    (multiple-value-bind (x y)
			 (cond (near-mode
				(cond ((eq (first near-mode) :point)
				       (values-list (rest near-mode)))
				      (t
				       (menu=dial_calc.orgin single.menu))))
			       (t
				(menu=dial_calc.orgin single.menu)))
			 (menu=dial_map single.menu x y))
    (let ((result (dial-struct_process.events single.menu)))
      (setf (menu=dial_single.data.selected.values single.data) result)
      (menu=dial_unmap single.menu map-unmapped)
      result)))


;;------------------------------------ single menu definition--------------------------------------------


;; constants:


;; data data structure:

(defstruct (menu=dial_single.data
	    (:conc-name menu=dial_single.data.)
	    (:constructor menu=dial_single.data.make)
	    (:predicate menu=dial_single.data.p))
  "menu data"
  item.list            ; item list (string (item-type argument {font info}:)*
  title.separator      ; (menu_title title_separator)
  items                ; menu items (of type dial-struct)
  default.item         ; no of default item
  help                 ; is help available?
  help.separator       ; separator before help-Button
  help.item            ; Help-item
  geometry.changed.p   ; true if the menu geometry was changed
  childs.changed.p     ; true if the menu must be reconstructed
  last.exit.button     ; the last selected item (selected.items)
  momentary            ; momentary-flag
  selected.values      ; selected values
)


;; create & dispose:

(defun menu=dial_single.create (parent-dialog item-list &key top-line default-item help momentary)
  (let* ((parent.window (cond ((null parent-dialog) menu*root.window)
			      (t (dial-struct_window parent-dialog))))
	 (menu.window   (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :class		      :input-output
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1 			; temporary value
			 :height	      1 			; temporary value
			 :border-width	      1
			 :border	      menu*fg.pixel
			 :background          menu*bg.pixel
			 :save-under 	      :on
			 :event-mask 	      (XLIB:MAKE-EVENT-MASK :button-release :button-press)
			 :override-redirect   :on)) ;override window mgr when positioning
	 (menu.data      (menu=dial_single.data.make
			  :item.list nil                                ; temporary value
			  :items nil                                    ; temporary value
			  :default.item default-item
			  :help help
			  :help.separator nil                           ; temporary value
			  :geometry.changed.p t                         ; geometry changed
			  :childs.changed.p nil                         ; no childs at this moment
			  :last.exit.button nil
			  :momentary momentary
			  :selected.values nil
			  ))
	 (menu           (dial-struct_make
			  :type               menu.single.id
			  :data               menu.data
			  :window             menu.window
			  :parent             parent-dialog
			  :childs             nil                       ; temporary value
			  :event.handler      #'menu=dial_single.event.handler
			  :dispatch           #'menu=dial_single.dispatch
			  )))
    (cond (help
	   (menu=dial_single.set.help menu "Help")))
    (menu=dial_single.set.title menu top-line)
    (menu=dial_single.add.items menu item-list)
    menu))


(defun menu=dial_single.get.title.separator (menu)
  (menu=dial_single.data.title.separator (dial-struct_data menu)))

(defun menu=dial_single.get.title.string (menu)
  (let ((title (menu=dial_single.data.title.separator (dial-struct_data menu))))
    (cond (title
	   (menu=dial_text.button.text (menu=dial_title.data.button (dial-struct_data (first title)))))
	  (t ""))))

(defun menu=dial_single.set.title (menu top-line)
;; sets (changes or adds) a new menu title
  (dial-struct_send.message.to.dialogs (menu=dial_single.get.title.separator menu)
				       '(dispose))
  (cond ((null top-line) (setf (menu=dial_single.data.title.separator (dial-struct_data menu)) nil))
	(t (setf (menu=dial_single.data.title.separator (dial-struct_data menu))
		 (list (menu=dial_title.create menu top-line)
		       (menu=dial_filler.item.create menu menu*title.filler.width
						          menu*title.filler.height
							  :separator :hor)))))
  (menu=dial_single.set.childs.changed menu t)
  (menu=dial_single.set.geometry.changed menu t))

(defun menu=dial_single.get.help.separator (menu)
  (menu=dial_single.data.help.separator (dial-struct_data menu)))

(defun menu=dial_single.set.help (menu help-string)
  (dial-struct_send.message.to.dialogs (menu=dial_single.get.title.separator menu)
				       '(dispose))
  (cond ((null help-string) (setf (menu=dial_single.data.help.separator (dial-struct_data menu)) nil))
	(t (setf (menu=dial_single.data.help.separator (dial-struct_data menu))
		 (list (menu=dial_filler.item.create menu menu*buttons.filler.width
						          menu*buttons.filler.height
							  :separator :hor)
		       (list (menu=dial_single.item.create menu
							   (list help-string :help :help)
							   :text.centered t))))))
  (menu=dial_single.set.childs.changed menu t)
  (menu=dial_single.set.geometry.changed menu t))

(defun menu=dial_single.get.item.list (menu)
  (menu=dial_single.data.item.list (dial-struct_data menu)))

(defun menu=dial_single.get.items (menu)
  (menu=dial_single.data.items (dial-struct_data menu)))

(defun menu=dial_single.delete.items (menu)
;; disposes the menu items
  (dial-struct_send.message.to.dialogs (menu=dial_single.get.items menu)
				       (list 'dispose)
				       :type item.single.id)
  (let ((menu.data (dial-struct_data menu)))
    (setf (menu=dial_single.data.item.list menu.data) nil)
    (setf (menu=dial_single.data.items menu.data) nil)
    (setf (menu=dial_single.data.last.exit.button menu.data) nil)
    )
  (menu=dial_single.set.childs.changed menu t)
  (menu=dial_single.set.geometry.changed menu t))

(defun menu=dial_single.add.items (menu item-list)
  (let ((menu.data (dial-struct_data menu)))
    (labels ((create.item (item)
			  (menu=dial_single.item.create menu item)))
	    (let ((item.childs (mapcar #'create.item item-list)))
	      (setf (menu=dial_single.data.items menu.data)
		    (nconc (menu=dial_single.get.items menu) item.childs))))
    (setf (menu=dial_single.data.item.list menu.data)
	  (nconc (menu=dial_single.get.item.list menu)
		 item-list)))
  (menu=dial_single.set.childs.changed menu t)
  (menu=dial_single.set.geometry.changed menu t))

(defun menu=dial_single.set.item.list (menu item-list)
  (menu=dial_single.delete.items menu)
  (menu=dial_single.add.items menu item-list))

(defun menu=dial_single.get.selected.items (menu)
  (let ((exit.item (menu=dial_single.data.last.exit.button (dial-struct_data menu))))
    (cond (exit.item
	   (menu=dial_single.item.data.item (dial-struct_data exit.item)))
	  (t nil))))

(defun menu=dial_single.get.selected.values (menu)
  (menu=dial_single.data.selected.values (dial-struct_data menu)))
	    

;; event handler & dispatch function:

(defun menu=dial_single.event.handler (menu &rest event-slots &key event-key event-window &allow-other-keys)
;; handles the events for a menu
;; events are processed by the childs of the menu
;;  (format t
;;	  "menu=dial_single.event.handler: event-key: ~S event-window: ~S~%" event-key event-window)
  (let* ((event-child (dial-struct_find.child.by.window menu event-window))
	 (result (cond (event-child
			; set the menu area if changed
			(menu=dial_single.update.geometry menu)
			; call the event handle of the child with window event-window
			(apply #'dial-struct_call.event.handler event-child event-slots))
		       ((eq :button-release event-key)
			(list dial-struct_event.result.symbol nil))
		       ((eq (dial-struct_window menu) event-window)
			t))))
    (cond ((and (listp result)
		(eq (first result) dial-struct_event.result.symbol))
	   (setf (menu=dial_single.data.last.exit.button (dial-struct_data menu)) (third result))))
    result))

(defun menu=dial_single.dispatch (menu message &rest parameters)
;; handles messages for a menu
;; messages: 'compute.width
;;           'compute.height
;;           'set-area            sets the area of all childs of the menu and then calls standard.dispatch
;;           'get-area            calls (the inherited) standard.dispatch
;;           'dispose               "     "      "              "
;;           'update.geometry     updates the geometry of the menu
;;           'last.exit.button    returns the last selected exit button
;;           'choose              maps the menu, does the dialog and unmap it again
;;           'deactivate          forget any previous settings made by the user
;;           'get.title.string    returns the title string
;;           'get.item.list       returns the item list
;;           'set.item.list       sets the item list
;;           'get.selected.items  returns the selected items
;;           'get.selected.values returns the values for the selected items
;;  (format menu*debug.stream
;;	  "menu=dial_single.dispatch: message: ~S parameters ~S~%" message parameters)
  (cond ((or (eq message 'compute.width)
	     (eq message 'compute.height))
	 (menu=dial_single.update.childs menu)
	 (dial-struct_standard.dispatch menu message))
	((eq message 'set.area)
	 (menu=dial_single.update.childs menu)
	 (multiple-value-bind (width height)
	     (dial-struct_set.area.of.childs menu)
	     ; call inherited dispatch:
	     (let ((offset (* 2 (XLIB:DRAWABLE-BORDER-WIDTH (dial-struct_window menu)))))
	       (dial-struct_standard.dispatch menu
					      'set.area
					      (dial-area_make :x 0
							      :y 0
							      :w (+ width offset)
							      :h (+ height offset))))))
	((eq message 'update.geometry)
	 (menu=dial_single.update.geometry menu))
	((eq message 'last.exit.button)
	 (menu=dial_single.data.last.exit.button (dial-struct_data menu)))
	((eq message 'choose)
	 (apply #'menu=dial_single.choose menu parameters))
	((eq message 'deactivate)
	 (menu=dial_single.deactivate menu))
	((eq message 'get.title.string)
	 (menu=dial_single.get.title.string menu))
	((eq message 'get.item.list)
	 (menu=dial_single.get.item.list menu))
	((eq message 'set.item.list)
	 (menu=dial_single.set.item.list menu (first parameters)))
	((eq message 'get.selected.items)
	 (menu=dial_single.get.selected.items menu))
	((eq message 'get.selected.values)
	 (menu=dial_single.get.selected.values menu))
	(t
	 (apply #'dial-struct_standard.dispatch menu message parameters)))) ; call inherited dispatch


;; geometry changed flag:

(defun menu=dial_single.set.geometry.changed (menu geometry.changed)
;; sets the geometry changed flag
  (setf (menu=dial_single.data.geometry.changed.p (dial-struct_data menu)) geometry.changed))

(defun menu=dial_single.geometry.changed.p (menu)
;; returns the geometry changed flag
  (menu=dial_single.data.geometry.changed.p (dial-struct_data menu)))

(defun menu=dial_single.childs.changed.p (menu)
  (menu=dial_single.data.childs.changed.p (dial-struct_data menu)))

(defun menu=dial_single.set.childs.changed (menu childs.changed)
  (setf (menu=dial_single.data.childs.changed.p (dial-struct_data menu)) childs.changed))

(defun menu=dial_single.update.childs (menu)
;; sets the childs slot in the menu structure
;;  (format t "menu=dial_single.update.childs ~A~%" (menu=dial_single.childs.changed.p menu))
  (cond ((menu=dial_single.childs.changed.p menu)
	 (let ((title.separator (menu=dial_single.get.title.separator menu))
	       (items (menu=dial_single.get.items menu))
	       (help.separator (menu=dial_single.get.help.separator menu)))
	   (cond (title.separator
		  (setq items (append title.separator items))))
	   (cond (help.separator
		  (setq items (append items help.separator))))
	   (setf (dial-struct_childs menu) (list items)))
	 (menu=dial_single.set.childs.changed menu nil))))

(defun menu=dial_single.update.geometry (menu)
;; updates the geometry of the menu and sets the window sizes including all menu childs if necessary
  (cond ((menu=dial_single.geometry.changed.p menu)
	 (menu=dial_single.update.childs menu)
	 (menu=dial_set.area menu)
	 (menu=dial_single.set.geometry.changed menu nil))))


;;---------------------------------- single menu title definition ---------------------------------------


;; constants:



;; title data:

(defstruct (menu=dial_title.data
	    (:conc-name menu=dial_title.data.)
	    (:constructor menu=dial_title.data.make)
	    (:predicate menu=dial_title.data.p))
  "Title data"
  button      ; title button
  )


;; create and dispose:

(defun menu=dial_title.create (menu top-line &key (text.centered t)
				                  (hor.offset menu*title.hor.offset)
						  (vert.offset menu*title.vert.offset))
  (let* ((parent.window (dial-struct_window menu))
	 (title.window  (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1 			; temporary value
			 :height	      1 			; temporary value
			 :border-width        0
			 :background          menu*bg.pixel
			 :event-mask 	      (XLIB:MAKE-EVENT-MASK
					       :button-press
					       :button-release
					       :exposure)))
	 (item.font    (cond ((listp top-line)
			      (second (member :font top-line)))))
	 (gcontext     (cond ((and item.font (XLIB:LIST-FONT-NAMES dial*display item.font))
			      (let ((new.gcontext (XLIB:CREATE-GCONTEXT :drawable menu*root.window)))
				(XLIB:COPY-GCONTEXT menu*gcontext new.gcontext)
				(setf (XLIB:GCONTEXT-FONT new.gcontext)
				      (XLIB:OPEN-FONT dial*display item.font))
				new.gcontext))
			     (t menu*gcontext)))
	 (title.data    (menu=dial_title.data.make
			 :button (menu=dial_text.button.create title.window
							       (cond ((listp top-line)
								      (second (member :string top-line)))
								     (t top-line))
							       gcontext
							       :text.centered text.centered
							       :hor.offset hor.offset
							       :vert.offset vert.offset))))
    (dial-struct_make
     :type               title.id
     :data               title.data
     :window             title.window
     :parent             menu
     :childs             nil                                            ; a menu title has no childs
     :event.handler      #'menu=dial_title.event.handler
     :dispatch           #'menu=dial_title.dispatch)))


;; methods:


;; event & dispatch function:

(defun menu=dial_title.event.handler (title
					     &rest event-slots
					     &key display event-key send-event-p        ; common slots
					          event-window x y width height count   ; exposure slots
					     &allow-other-keys)
;;  (format t
;;	  "menu=dial_title.event.handler: event-key: ~S event-window: ~S~%"
;;	  event-key
;;	  event-window)
  (cond ((eq :exposure event-key)
	 (cond ((= 0 count)
		(let ((button (menu=dial_title.data.button (dial-struct_data title))))
		  (menu=dial_text.button.draw button))))
	 t)
	(t t)
	)
  )

(defun menu=dial_title.dispatch (title message &rest parameters)
;; handles messages for a menu
;;  (format menu*debug.stream
;;	  "menu=dial_title.dispatch: message: ~S parameters ~S~%" message parameters)
  (let ((button (menu=dial_title.data.button (dial-struct_data title))))
    (cond ((eq message 'compute.width)
	   (menu=dial_text.button.compute.width button))
	  ((eq message 'compute.height)
	   (menu=dial_text.button.compute.height button))
	  ((eq message 'set.area)
	   (let ((area (car parameters)))
	     (menu=dial_text.button.set.area button area)))
	  (t
	   (apply #'dial-struct_standard.dispatch title message parameters))))) ; call inherited dispatch


;;------------------------------- single menu items definition-------------------------------------------


;; constants:



;; item data:

(defstruct (menu=dial_single.item.data
	    (:conc-name menu=dial_single.item.data.)
	    (:constructor menu=dial_single.item.data.make)
	    (:predicate menu=dial_single.item.data.p))
  "Item data"
  item                   ; item data of type (string [item-type argument {font info}:])
  button                 ; item button
  mouse.buttons.pressed  ; mouse buttons pressed
  )


;; create and dispose:

(defun menu=dial_single.item.create (menu item &key (text.centered nil)
					            (hor.offset menu*item.single.hor.offset)
						    (vert.offset menu*item.single.vert.offset))
  ;; item is (string [item-type argument {font info centered selectable exit}:]) (see section 20.2 in the KK-Lisp manual)
  (let* ((parent.window (dial-struct_window menu))
	 (item.font    (second (member :font item)))
	 (gcontext     (cond ((and item.font (XLIB:LIST-FONT-NAMES dial*display item.font))
			      (let ((new.gcontext (XLIB:CREATE-GCONTEXT :drawable menu*root.window)))
				(XLIB:COPY-GCONTEXT menu*gcontext new.gcontext)
				(setf (XLIB:GCONTEXT-FONT new.gcontext)
				      (XLIB:OPEN-FONT dial*display item.font))
				new.gcontext))
			     (t menu*gcontext)))
	 (centered     (cond ((not text.centered)           ; text.centered has priority
			      (second (member :centered item)))
			     (t text.centered)))
	 (selectable   (let ((found (member :selectable item)))
			 (and (cond (found
				     (second found))
				    (t t))                      ; default is true
			      (not (member :no-select item)))))
	 (exit   (let ((found (member :exit item)))
			 (cond (found
				(second found))
			       (t t))))                      ; default is true
	 (item.window  (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1 			; temporary value
			 :height	      1 			; temporary value
			 :border-width        0
			 :background          menu*bg.pixel
			 :cursor              menu*cursor
			 :event-mask 	      (cond (selectable
						     (XLIB:MAKE-EVENT-MASK :enter-window
									   :leave-window
									   :button-press
									   :button-release
									   :exposure))
						    (t
						     (XLIB:MAKE-EVENT-MASK :button-press
									   :button-release
									   :exposure)))))
	 (item.data    (menu=dial_single.item.data.make
			:item item
			:mouse.buttons.pressed nil
			:button (menu=dial_text.button.create item.window
							      (first item)
							      gcontext
							      :text.centered centered
							      :selectable selectable
							      :exit exit
							      :hor.offset hor.offset
							      :vert.offset vert.offset))))
    (dial-struct_make
     :type               item.single.id
     :data               item.data
     :window             item.window
     :parent             menu
     :childs             nil                                            ; a menu item has no childs
     :event.handler      #'menu=dial_single.item.event.handler
     :dispatch           #'menu=dial_single.item.dispatch)))


;; methods:


;; event & dispatch function:

(defun menu=dial_single.item.event.handler (item
					    &rest event-slots
					    &key display event-key send-event-p        ; common slots
					         event-window x y width height count   ; exposure slots
						 code                                  ; button press
					    &allow-other-keys)
;;  (format t
;;	  "menu=dial_single.item.event.handler: event-key: ~S event-window: ~S~%"
;;	  event-key
;;	  event-window)
  (let* ((item.data (dial-struct_data item))
	 (window (dial-struct_window item))
	 (button (menu=dial_single.item.data.button item.data))
	 (item.item (menu=dial_single.item.data.item item.data))
	 (mouse.buttons.pressed (menu=dial_single.item.data.mouse.buttons.pressed item.data)))
    (cond ((eq :exposure event-key)
	   (cond ((= 0 count)
		  (menu=dial_text.button.draw button)))
	   t)
	  ((eq :button-press event-key)
	   (setf (menu=dial_single.item.data.mouse.buttons.pressed item.data)
		 (union (list code) mouse.buttons.pressed))
	   t)
	  ((and (eq :button-release event-key)
		(member code mouse.buttons.pressed))
	   (setf (menu=dial_single.item.data.mouse.buttons.pressed item.data)
		 (delete code mouse.buttons.pressed))
	   (let ((result (apply #'menu=dial_handle.item
				(dial-struct_parent item)
				code
				item.item))
		 (item-type (second item.item)))
	     (cond ((and (not (or (eq :help item-type)           ; terminate if not help or no-select item
				  (eq :no-select item-type)))
			 (menu=dial_text.button.exit button))    ; exit button?
		    (menu=dial_text.button.set.selected button nil nil)
		    (list dial-struct_event.result.symbol result item))
		   (t t))))
	  (t (cond ((menu=dial_text.button.selectable button)
		    (cond ((and (eq :enter-notify event-key)
				(dial-misc_pointer.in.window.p window))
			   (menu=dial_text.button.set.selected button t t))
			  ((and (eq :leave-notify event-key)
				(not (dial-misc_pointer.in.window.p window)))
			   (setf (menu=dial_single.item.data.mouse.buttons.pressed item.data) nil)
			   (menu=dial_text.button.set.selected button nil t))
			 )
		    )
		  )
	     t))))

(defun menu=dial_single.item.dispatch (item message &rest parameters)
;; handles messages for a menu
;;  (format t
;;	  "menu=dial_single.item.dispatch: item: ~S message: ~S parameters ~S~%"
;;	  (menu=dial_text.button.text (menu=dial_single.item.data.button (dial-struct_data item)))
;;	  message
;;	  parameters)
  (let ((button (menu=dial_single.item.data.button (dial-struct_data item))))
    (cond ((eq message 'compute.width)
	   (menu=dial_text.button.compute.width button))
	  ((eq message 'compute.height)
	   (menu=dial_text.button.compute.height button))
	  ((eq message 'set.area)
	   (let ((area (car parameters)))
	     (menu=dial_text.button.set.area button area)))
	  ((eq message 'dispose)
	   (menu=dial_text.button.dispose button)                              ; dispose text button
	   (apply #'dial-struct_standard.dispatch item message parameters)    ; call inherited dispatch
	   )
	  ((eq message 'reset)
	   (menu=dial_text.button.set.selected button nil nil))
	   (t
	   (apply #'dial-struct_standard.dispatch item message parameters))))) ; call inherited dispatch


;;----------------------------------------- multiple menu ------------------------------------------------

(defun menu=dial_multiple.deactivate (multiple.menu)
  (let ((multiple.data (dial-struct_data multiple.menu)))
    (setf (menu=dial_multiple.data.last.exit.button multiple.data) nil)
    (labels ((deselect.check.box.item (item)
				      (menu=dial_check.box.item.set.selected item nil)))
	    (mapc #'deselect.check.box.item (menu=dial_multiple.data.check.boxes multiple.data)))))

(defun menu=dial_multiple.choose (multiple.menu &optional near-mode (map-unmapped t))
  (menu=dial_multiple.update.geometry multiple.menu)
  (let* ((multiple.data (dial-struct_data multiple.menu))
	 (default.item (menu=dial_multiple.data.default.item multiple.data)))
    (cond ((menu=dial_multiple.data.momentary multiple.data)
	   (menu=dial_multiple.deactivate multiple.menu)
	   (menu=dial_check.boxes.select (menu=dial_multiple.data.check.boxes multiple.data)
					 default.item))
	  ((null (menu=dial_multiple.data.last.exit.button multiple.data))
	   (menu=dial_check.boxes.select (menu=dial_multiple.data.check.boxes multiple.data)
					 default.item)))
    (multiple-value-bind (x y)
			 (cond (near-mode
				(cond ((eq (first near-mode) :point)
				       (values-list (rest near-mode)))
				      (t
				       (menu=dial_calc.orgin multiple.menu))))
			       (t
				(menu=dial_calc.orgin multiple.menu)))
			 (menu=dial_map multiple.menu x y))
    (let ((result (dial-struct_process.events multiple.menu)))
      (setf (menu=dial_multiple.data.selected.values multiple.data) result)
      (menu=dial_unmap multiple.menu map-unmapped)
      result)))


;;------------------------------------ multiple menu definition-------------------------------------------


;; constants:


;; data data structure:

(defstruct (menu=dial_multiple.data
	    (:conc-name menu=dial_multiple.data.)
	    (:constructor menu=dial_multiple.data.make)
	    (:predicate menu=dial_multiple.data.p))
  "menu data"
  item.list            ; item-list: string item-type argument {font info}:
  title                ; menu title (of type dial-struct)
  separators           ; list of separators (of type dial-struct)
  items                ; menu items (of type dial-struct)
  check.boxes          ; menu check boxes (of type dial-struct)
  exit.buttons         ; menu exit buttons (of type dial-struct)
  default.item         ; default items, which are checked.
  momentary            ; momentary-flag
  last.exit.button     ; last selected button
  geometry.changed.p   ; true if the menu geometry was changed
  childs.changed.p     ; true if the menu must be reconstructed
  selected.values      ; selected values
  )

;; create & dispose:

(defun menu=dial_multiple.create (parent-dialog item-list &key top-line default-item special-choices help do-it abort momentary)
  (let* ((parent.window (cond ((null parent-dialog) menu*root.window)
			      (t (dial-struct_window parent-dialog))))
	 (menu.window   (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :class		      :input-output
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1 			; temporary value
			 :height	      1 			; temporary value
			 :border-width	      1
			 :border	      menu*fg.pixel
			 :background          menu*bg.pixel
			 :save-under 	      :on
			 :event-mask 	      (XLIB:MAKE-EVENT-MASK :button-release :button-press)
			 :override-redirect   :on)) ;override window mgr when positioning
	 (menu.data      (menu=dial_multiple.data.make
			  :item.list nil                                ; temporary value
			  :title nil                                    ; temporary value
			  :separators nil                               ; temporary value
			  :items nil                                    ; temporary value
			  :exit.buttons nil                             ; temporary value
			  :check.boxes nil                              ; temporary value
			  :default.item default-item
			  :momentary momentary
			  :geometry.changed.p t                         ; geometry changed
			  :childs.changed.p nil
			  :selected.values nil
			  ))                       ; no childs at this moment
	 (menu           (dial-struct_make
			  :type               menu.multiple.id
			  :data               menu.data
			  :window             menu.window
			  :parent             parent-dialog
			  :childs             nil                       ; temporary value
			  :event.handler      #'menu=dial_multiple.event.handler
			  :dispatch           #'menu=dial_multiple.dispatch
			  )))
    (menu=dial_multiple.set.title menu top-line)
    (menu=dial_multiple.add.items menu item-list)
    (cond (help
	   (setq special-choices (append special-choices (list (list "Help" :help :help))))))
    (cond (do-it
	   (setq special-choices (append special-choices (list (list "Do it" :do-it :do-it))))))
    (cond (abort
	   (setq special-choices (append special-choices (list (list "Abort" :abort :abort))))))
    (menu=dial_multiple.add.exit.buttons menu special-choices)
    menu))


(defun menu=dial_multiple.get.title (menu)
  (menu=dial_multiple.data.title (dial-struct_data menu)))

(defun menu=dial_multiple.get.title.string (menu)
  (let ((title (menu=dial_multiple.data.title (dial-struct_data menu))))
    (cond (title
	   (menu=dial_text.button.text (menu=dial_title.data.button (dial-struct_data title))))
	  (t ""))))

(defun menu=dial_multiple.set.title (menu top-line)
  ;; sets (changes or adds) a new menu title
  (cond ((null top-line) (setf (menu=dial_multiple.data.title (dial-struct_data menu)) nil))
	(t
	 (let ((old.title (menu=dial_multiple.get.title menu)))
	   (cond (old.title
		  (menu=dial_text.button.set.text old.title top-line))
		 (t (setf (menu=dial_multiple.data.title (dial-struct_data menu))
			  (menu=dial_title.create menu top-line)))))))
  (menu=dial_multiple.set.childs.changed menu t)
  (menu=dial_multiple.set.geometry.changed menu t))

(defun menu=dial_multiple.get.item.list (menu)
  (menu=dial_multiple.data.item.list (dial-struct_data menu)))

(defun menu=dial_multiple.get.items (menu)
  (menu=dial_multiple.data.items (dial-struct_data menu)))

(defun menu=dial_multiple.get.check.boxes (menu)
  (menu=dial_multiple.data.check.boxes (dial-struct_data menu)))

(defun menu=dial_multiple.delete.items (menu)
  ;; disposes the menu items
  (dial-struct_send.message.to.dialogs (menu=dial_multiple.get.items menu)
				       (list 'dispose))
  (dial-struct_send.message.to.dialogs (menu=dial_multiple.get.check.boxes menu)
				       (list 'dispose))
  (let ((menu.data (dial-struct_data menu)))
    (setf (menu=dial_multiple.data.check.boxes menu.data) nil)
    (setf (menu=dial_multiple.data.item.list menu.data) nil)
    (setf (menu=dial_multiple.data.items menu.data) nil)
    (setf (menu=dial_multiple.data.last.exit.button menu.data) nil)
    )
  (menu=dial_multiple.set.childs.changed menu t)
  (menu=dial_multiple.set.geometry.changed menu t))

(defun menu=dial_multiple.add.items (menu item-list)
  (let ((item.childs nil)
	(check.box.items nil)
	(menu.data (dial-struct_data menu)))
    (labels ((create.check.box.and.item (item)
					(let* ((check.box (menu=dial_check.box.item.create menu))
					       (item (menu=dial_multiple.item.create menu item check.box)))
					  (setq item.childs (nconc item.childs (list item)))
					  (setq check.box.items (nconc check.box.items (list check.box))))))
	    (mapcar #'create.check.box.and.item item-list))
    (setf (menu=dial_multiple.data.items menu.data)
	  (nconc (menu=dial_multiple.get.items menu) item.childs))
    (setf (menu=dial_multiple.data.item.list menu.data)
	  (nconc (menu=dial_multiple.get.item.list menu) item-list))
    (setf (menu=dial_multiple.data.check.boxes menu.data)
	  (nconc (menu=dial_multiple.get.check.boxes menu) check.box.items)))
  (menu=dial_multiple.set.childs.changed menu t)
  (menu=dial_multiple.set.geometry.changed menu t))

(defun menu=dial_multiple.set.item.list (menu item-list)
  (menu=dial_multiple.delete.items menu)
  (menu=dial_multiple.add.items menu item-list))

(defun menu=dial_multiple.get.exit.buttons (menu)
  (menu=dial_multiple.data.exit.buttons (dial-struct_data menu)))

(defun menu=dial_multiple.delete.exit.buttons (menu)
  ;; disposes the menu exit.buttons
  (dial-struct_send.message.to.dialogs (menu=dial_multiple.get.exit.buttons menu)
				       (list 'dispose))
  (setf (menu=dial_multiple.data.exit.buttons (dial-struct_data menu)) nil)
  (menu=dial_multiple.set.childs.changed menu t)
  (menu=dial_multiple.set.geometry.changed menu t))

(defun menu=dial_multiple.add.exit.buttons (menu item-list)
  (labels ((create.item (item)
			(menu=dial_single.item.create menu
						      item
						      :text.centered t
						      :hor.offset menu*exit.button.hor.offset
						      :vert.offset menu*exit.button.vert.offset)))
	  (let ((item.childs (mapcar #'create.item item-list)))
	    (setf (menu=dial_multiple.data.exit.buttons (dial-struct_data menu))
		  (nconc (menu=dial_multiple.get.exit.buttons menu) item.childs))))
  (menu=dial_multiple.set.childs.changed menu t)
  (menu=dial_multiple.set.geometry.changed menu t))

(defun menu=dial_multiple.get.separators (menu)
  (menu=dial_multiple.data.separators (dial-struct_data menu)))

(defun menu=dial_multiple.delete.separators (menu)
  ;; disposes the menu exit.buttons
  (dial-struct_send.message.to.dialogs (menu=dial_multiple.get.separators menu)
				       (list 'dispose))
  (setf (menu=dial_multiple.data.separators (dial-struct_data menu)) nil)
  (menu=dial_multiple.set.childs.changed menu t)
  (menu=dial_multiple.set.geometry.changed menu t))

(defun menu=dial_multiple.add.separators (menu &rest separators.parameters)
  ;; adds separators to the menu
  (labels ((create.filler.item (separator.parameters)
			       (apply #'menu=dial_filler.item.create menu separator.parameters)))
	  (let ((separator.childs (mapcar #'create.filler.item separators.parameters)))
	    (setf (menu=dial_multiple.data.separators (dial-struct_data menu))
		  (nconc (menu=dial_multiple.get.separators menu) separator.childs))))
  (menu=dial_multiple.set.childs.changed menu t)
  (menu=dial_multiple.set.geometry.changed menu t))

(defun menu=dial_multiple.get.selected.items (menu)
  (labels ((get.selected.items (items)
			       (cond ((not (null items))
				      (let ((first.item (car items)))
					(cond ((menu=dial_multiple.item.selected.p first.item)
					       (cons (menu=dial_multiple.item.data.item (dial-struct_data first.item))
						     (get.selected.items (cdr items))))
					      (t (get.selected.items (cdr items))))))
				     (t nil))))
	  (get.selected.items (menu=dial_multiple.data.items (dial-struct_data menu)))))

(defun menu=dial_multiple.get.selected.values (menu)
  (menu=dial_multiple.data.selected.values (dial-struct_data menu)))

;; event handler & dispatch function:

(defun menu=dial_multiple.event.handler (menu &rest event-slots &key event-key event-window &allow-other-keys)
  ;; handles the events for a menu
  ;; events are processed by the childs of the menu
;;  (format t
;;  	  "menu=dial_multiple.event.handler: event-key: ~S event-window: ~S~%" event-key event-window)
  (let* ((event-child (dial-struct_find.child.by.window menu event-window))
	 (result (cond (event-child
					; set the menu area if changed
			(menu=dial_multiple.update.geometry menu)
					; call the event handle of the child with window event-window
			(apply #'dial-struct_call.event.handler event-child event-slots))
		       ((eq (dial-struct_window menu) event-window)
			t))))
    (cond ((and (listp result)
		(eq (first result) dial-struct_event.result.symbol))
	   (setf (menu=dial_multiple.data.last.exit.button (dial-struct_data menu)) (third result))))
    result))

(defun menu=dial_multiple.dispatch (menu message &rest parameters)
  ;; handles messages for a menu
  ;; messages: 'compute.width       computes and returns the (minimum) width of the menu
  ;;           'compute.height        "       "    "     "     "       height " "    "
  ;;           'set-area            sets the area of all childs of the menu and then calls standard.dispatch
  ;;           'get-area            calls (the inherited) standard.dispatch
  ;;           'dispose               "     "      "              "
  ;;           'update.geometry     updates the geometry of the menu
  ;;           'last.exit.button    returns the last selected exit button
  ;;           'choose              maps the menu, does the dialog and unmap it again
  ;;           'deactivate          forget any previous settings made by the user
  ;;           'get.title.string    returns the title string
  ;;           'get.item.list       returns the item list
  ;;           'set.item.list       sets the item list
  ;;           'get.selected.items  returns the selected items
  ;;           'get.selected.values returns the values for the selected items
  ;;  (format menu*debug.stream
  ;;	  "menu=dial_multiple.dispatch: message: ~S parameters ~S~%" message parameters)
  (cond ((or (eq message 'compute.width)
	     (eq message 'compute.height))
	 (menu=dial_multiple.update.childs menu)
	 (dial-struct_standard.dispatch menu message))                      ; call inherited dispatch
	((eq message 'set.area)
	 (menu=dial_multiple.update.childs menu)
	 (multiple-value-bind (width height)
	     (dial-struct_set.area.of.childs menu)
	     ; call inherited dispatch:
	     (let ((offset (* 2 (XLIB:DRAWABLE-BORDER-WIDTH (dial-struct_window menu)))))
	       (dial-struct_standard.dispatch menu
					      'set.area
					      (dial-area_make :x 0
							      :y 0
							      :w (+ width offset)
							      :h (+ offset height))))))
	((eq message 'update.geometry)
	 (menu=dial_multiple.update.geometry menu))
	((eq message 'last.exit.button)
	 (menu=dial_multiple.data.last.exit.button (dial-struct_data menu)))
	((eq message 'choose)
	 (apply #'menu=dial_multiple.choose menu parameters))
	((eq message 'deactivate)
	 (menu=dial_multiple.deactivate menu))
	((eq message 'get.title.string)
	 (menu=dial_multiple.get.title.string menu))
	((eq message 'get.item.list)
	 (menu=dial_multiple.get.item.list menu))
	((eq message 'set.item.list)
	 (menu=dial_multiple.set.item.list menu (first parameters)))
	((eq message 'get.selected.items)
	 (menu=dial_multiple.get.selected.items menu))
	((eq message 'get.selected.values)
	 (menu=dial_multiple.get.selected.values menu))
	(t
	 (apply #'dial-struct_standard.dispatch menu message parameters)))) ; call inherited dispatch


;; geometry changed flag:

(defun menu=dial_multiple.set.geometry.changed (menu geometry.changed)
  ;; sets the geometry changed flag
  (setf (menu=dial_multiple.data.geometry.changed.p (dial-struct_data menu)) geometry.changed))

(defun menu=dial_multiple.geometry.changed.p (menu)
  ;; returns the geometry changed flag
  (menu=dial_multiple.data.geometry.changed.p (dial-struct_data menu)))

(defun menu=dial_multiple.childs.changed.p (menu)
  (menu=dial_multiple.data.childs.changed.p (dial-struct_data menu)))

(defun menu=dial_multiple.set.childs.changed (menu childs.changed)
  (setf (menu=dial_multiple.data.childs.changed.p (dial-struct_data menu)) childs.changed))

(defun menu=dial_multiple.update.childs (menu)
  ;; sets the childs slot in the menu structure
  (cond ((menu=dial_multiple.childs.changed.p menu)
	 (menu=dial_multiple.delete.separators menu)
	 (let ((title (menu=dial_multiple.get.title menu))
	       (items (list (list (menu=dial_multiple.get.check.boxes menu)
				  (menu=dial_multiple.get.items menu))))
	       (exit.buttons (menu=dial_multiple.get.exit.buttons menu)))
	   (menu=dial_multiple.add.separators menu
					      (list menu*buttons.filler.width
						    menu*buttons.filler.height
						    :separator :hor)) ; button separator
	   (let ((buttons.separator (first (menu=dial_multiple.get.separators menu))))
	     (cond (title
		    (menu=dial_multiple.add.separators menu
						       (list menu*title.filler.width
							     menu*title.filler.height
							     :separator :hor))  ; title separator
		    (let ((title.separator (second (menu=dial_multiple.get.separators menu))))
		      (setq items (list* title title.separator items)))))
	     (setq items (nconc items (list buttons.separator) (list exit.buttons))))
	   (setf (dial-struct_childs menu) (list items)))
	 (menu=dial_multiple.set.childs.changed menu nil))))

(defun menu=dial_multiple.update.geometry (menu)
  ;; updates the geometry of the menu and sets the window sizes including all menu childs if necessary
  (cond ((menu=dial_multiple.geometry.changed.p menu)
	 (menu=dial_multiple.update.childs menu)
	 (menu=dial_set.area menu)
	 (menu=dial_multiple.set.geometry.changed menu nil))))


;;------------------------------- multiple menu items definition------------------------------------------


;; constants:



;; item data:

(defstruct (menu=dial_multiple.item.data
	    (:conc-name menu=dial_multiple.item.data.)
	    (:constructor menu=dial_multiple.item.data.make)
	    (:predicate menu=dial_multiple.item.data.p))
  "Item data"
  item            ; item is (string [item-type argument {font info}:]) (see section 20.2, KK-Lisp manual)
  button          ; item button
  check.box.item  ; reference to the correspondig check box item
  )

;; create and dispose:

(defun menu=dial_multiple.item.create (menu item check.box.item
					    &key (text.centered nil)
					    (hor.offset menu*item.multiple.hor.offset)
					    (vert.offset menu*item.multiple.vert.offset))
  ;; item is (string [item-type argument {font info centered selectable}:]) (see section 20.2 in the KK-Lisp manual)
  (let* ((parent.window (dial-struct_window menu))
	 (item.window  (XLIB:CREATE-WINDOW
			:parent	      parent.window
			:x		      0			        ; temporary value
			:y		      0			        ; temporary value
			:width		      1 			; temporary value
			:height	              1 			; temporary value
			:border-width         0
			:cursor               menu*cursor
			:background           menu*bg.pixel
			:event-mask 	      (XLIB:MAKE-EVENT-MASK :exposure
								    :button-press
								    :button-release
								    :leave-window)))
	 (item.font    (second (member :font item)))
	 (gcontext     (cond ((and item.font (XLIB:LIST-FONT-NAMES dial*display item.font))
			      (let ((new.gcontext (XLIB:CREATE-GCONTEXT :drawable menu*root.window)))
				(XLIB:COPY-GCONTEXT menu*gcontext new.gcontext)
				(setf (XLIB:GCONTEXT-FONT new.gcontext)
				      (XLIB:OPEN-FONT dial*display item.font))
				new.gcontext))
			     (t menu*gcontext)))
	 (centered     (cond ((not text.centered)
			      (second (member :centered item)))
			     (t text.centered)))
	 (selectable   (let ((found (member :selectable item)))
			 (and (cond (found
				     (second found))
				    (t t))                      ; default is true
			      (not (member :no-select item)))))
	 (item.data    (menu=dial_multiple.item.data.make
			:check.box.item check.box.item
			:item item
			:button (menu=dial_text.button.create item.window
							      (first item)
							      gcontext
							      :text.centered centered
							      :selectable selectable
							      :hor.offset hor.offset
							      :vert.offset vert.offset))))
    (dial-struct_make
     :type               item.multiple.id
     :data               item.data
     :window             item.window
     :parent             menu
     :childs             nil                                            ; a menu item has no childs
     :event.handler      #'menu=dial_multiple.item.event.handler
     :dispatch           #'menu=dial_multiple.item.dispatch)))


;; methods:

(defun menu=dial_multiple.item.selected.p (item)
  (menu=dial_check.box.item.selected.p (menu=dial_multiple.item.data.check.box.item (dial-struct_data item))))

;; event & dispatch function:

(defun menu=dial_multiple.item.event.handler (item
					      &rest event-slots
					      &key display event-key send-event-p  ; common slots
					      event-window x y width height count   ; exposure slots
					      &allow-other-keys)
;;  (format t
;;  	  "menu=dial_multiple.item.event.handler: event-key: ~S event-window: ~S~%"
;;  	  event-key
;;  	  event-window)
  (let ((button (menu=dial_multiple.item.data.button (dial-struct_data item))))
    (cond ((eq :exposure event-key)
	   (cond ((= 0 count)
		  (menu=dial_text.button.draw button)))
	   t)
	  (t
	   (let ((check.box.item (menu=dial_multiple.item.data.check.box.item (dial-struct_data item))))
	     (apply #'dial-struct_call.event.handler check.box.item event-slots))
	   )
	  )
    )
  )

(defun menu=dial_multiple.item.dispatch (item message &rest parameters)
  ;; handles messages for a menu
  ;;  (format menu*debug.stream
  ;;	  "menu=dial_multiple.item.dispatch: message: ~S parameters ~S~%" message parameters)
  (let ((button (menu=dial_multiple.item.data.button (dial-struct_data item))))
    (cond ((eq message 'compute.width)
	   (menu=dial_text.button.compute.width button))
	  ((eq message 'compute.height)
	   (menu=dial_text.button.compute.height button))
	  ((eq message 'set.area)
	   (let ((area (car parameters)))
	     (menu=dial_text.button.set.area button area)))
	  ((eq message 'dispose)
	   (menu=dial_text.button.dispose button)                              ; dispose text button
	   (apply #'dial-struct_standard.dispatch item message parameters))    ; call inherited dispatch
	  (t
	   (apply #'dial-struct_standard.dispatch item message parameters))))) ; call inherited dispatch


;;------------------------------------- choose-variable-values -------------------------------------------

(defstruct (menu=dial_choose.data
	    (:conc-name menu=dial_choose.data.)
	    (:constructor menu=dial_choose.data.make)
	    (:predicate menu=dial_choose.data.p))
  "Choose Values for Variables Menu Data"
  variables
  )

(defun menu=dial_choose.create (parent-dialog header variables &optional margin-choices)
  (let* ((parent.window (cond ((null parent-dialog) menu*root.window)
			      (t (dial-struct_window parent-dialog))))
	 (menu.window   (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :class		      :input-output
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1  			; temporary value
			 :height	      1 			; temporary value
			 :border-width	      1
			 :border	      menu*fg.pixel
			 :background          menu*bg.pixel
			 :save-under 	      :on
			 :event-mask 	      (XLIB:MAKE-EVENT-MASK :button-release :button-press)
			 :override-redirect   :on)) ;override window mgr when positioning
	 (menu.data      (menu=dial_choose.data.make
			  :variables variables
			  ))                       ; no childs at this moment
	 (menu           (dial-struct_make
			  :type               menu.choose.id
			  :data               menu.data
			  :window             menu.window
			  :parent             parent-dialog
			  :childs             nil       ; temporary value 
			  :event.handler      #'menu=dial_choose.event.handler
			  :dispatch           #'menu=dial_choose.dispatch
			  )))
    (setf (dial-struct_childs menu)
	  (menu=dial_choose.create.childs menu header variables margin-choices))
    (menu=dial_set.area menu)
    menu))

(defun menu=dial_choose.create.childs (menu header variables margin-choices)
  (list (append (menu=dial_choose.create.header menu header)
		(menu=dial_choose.create.variables menu variables)
		(menu=dial_choose.create.margin.choices menu margin-choices))))

(defun menu=dial_choose.create.header (menu header)
  (list (menu=dial_title.create menu header)
	(menu=dial_filler.item.create menu menu*title.filler.width
				      menu*title.filler.height
				      :separator :hor)))

(defun menu=dial_choose.create.category.aux (menu variable-desc type value-set)
  ; variable-desc is (variable namestring info category [function])
  (cond ((not (null value-set))
	 (let* ((value (first value-set))
		(printstring (cond ((listp value)
				    (first value))
				   (t (symbol-name value)))))
	   (cons (menu=dial_choose.item.create menu
					       variable-desc
					       value)
		 (menu=dial_choose.create.category.aux menu variable-desc type (rest value-set)))))))

(defun menu=dial_choose.set.category.items (items category-items)
  (cond ((not (null items))
	 (let ((item (first items)))
	   (menu=dial_choose.item.set.category.items item category-items))
	 (menu=dial_choose.set.category.items (rest items) category-items))))

(defun menu=dial_choose.create.category (menu variable-desc)
  ; variable-desc is (variable namestring info category [function])
  (let ((variable (first variable-desc))
	(category (fourth variable-desc)))
    (let ((type (first category)))
      (cond ((eq type :infinite)
	     (list (menu=dial_edit.item.create menu variable-desc nil)))
	    (t (let ((result (menu=dial_choose.create.category.aux menu variable-desc type (rest category))))
		 (menu=dial_choose.set.category.items result result)
		 result))))))

(defun menu=dial_choose.create.variable.aux (menu variable-desc)
  ; variable-desc is (variable namestring info category [function])
  (let* ((variable (first variable-desc))
	 (info (third variable-desc))
	 (namestring (second variable-desc))
	 (variable.name (cond ((null namestring)
			       (symbol-name variable))
			      (t namestring))))
    (append (list (append (list (menu=dial_single.item.create menu
							      (list (format nil "~A: " variable.name)
								    :no-select)))
			  (menu=dial_choose.create.category menu variable-desc)))
	    (cond (info
		   (list (menu=dial_single.item.create menu
						       (list (format nil "~A Information" variable.name)
							     :eval `(notify (quote ,info))
							     :exit nil))))))))

(defun menu=dial_choose.create.variable (menu variable)
  ;; variable is string or (variable namestring info category [function])
  (cond ((stringp variable)
	 (list (menu=dial_single.item.create menu
					     (list variable :no-select))))
	(t (menu=dial_choose.create.variable.aux menu variable))))

(defun menu=dial_choose.create.variables (menu variables)
  (cond ((not (null variables))
	 (append (menu=dial_choose.create.variable menu (first variables))
		 (menu=dial_choose.create.variables menu (rest variables))))))

(defun menu=dial_choose.create.margin.choices (menu margin-choices)
  (let ((exit nil))
    (labels ((create.margin.choices (margin-choices)
				    (cond ((not (null margin-choices))
					   (let ((margin-choice (first margin-choices)))
					     (cond ((stringp margin-choice)
						    (setq exit t)
						    (setq margin-choice (list margin-choice
									      '(throw 'choose-variable-values nil)))))
					     (cons (menu=dial_single.item.create menu
										 (list (first margin-choice)
										       :eval
										       (second margin-choice)
										       :exit nil)
										 :text.centered t
										 :hor.offset menu*exit.button.hor.offset
										 :vert.offset menu*exit.button.vert.offset)
						   (create.margin.choices (rest margin-choices)))))))
				 )
	    (let ((filler (menu=dial_filler.item.create menu menu*title.filler.width
							menu*title.filler.height
							:separator :hor))
		  (choices (create.margin.choices margin-choices)))
	      (cond ((not exit)
		     (setq choices (append choices
					   (list (menu=dial_single.item.create menu
									       (list "exit"
										     :eval
										     '(throw 'choose-variable-values nil)
										     :exit nil)
									       :text.centered t
									       :hor.offset menu*exit.button.hor.offset
									       :vert.offset menu*exit.button.vert.offset))))))
	      (list filler choices)))))

(defun menu=dial_choose.dispose (choose-menu)
  (dial-struct_dispose choose-menu))


;; event handler & dispatch function:

(defun menu=dial_choose.event.handler (menu &rest event-slots &key event-key event-window &allow-other-keys)
  ;; handles the events for a menu
  ;; events are processed by the childs of the menu
;;  (format t
;;	  "menu=dial_choose.event.handler: event-key: ~S event-window: ~S~%" event-key event-window)
  (let* ((event-child (dial-struct_find.child.by.window menu event-window))
	 (result (cond (event-child
					; call the event handle of the child with window event-window
			(apply #'dial-struct_call.event.handler event-child event-slots))
		       ((eq (dial-struct_window menu) event-window)
			t))))
    result))

(defun menu=dial_choose.dispatch (menu message &rest parameters)
  ;; handles messages for a menu
  ;; messages: 'compute.width       computes and returns the (minimum) width of the menu
  ;;           'compute.height      computes and returns the (minimum) height of the menu
  ;;           'set-area            sets the area of all childs of the menu and then calls standard.dispatch
  ;;           'get-area            calls (the inherited) standard.dispatch
  ;;           'dispose               "     "      "              "
  ;;           'update.geometry     here: has no effect
  ;;           'last.exit.button    here: always returns nil
  ;;           'choose              maps the menu, does the dialog and unmap it again
  ;;  (format menu*debug.stream
  ;;	  "menu=dial_choose.dispatch: message: ~S parameters ~S~%" message parameters)
  (cond ((eq message 'set.area)
	 (multiple-value-bind (width height)
	     (dial-struct_set.area.of.childs menu)
	     ; call inherited dispatch:
	     (let ((offset (* 2 (XLIB:DRAWABLE-BORDER-WIDTH (dial-struct_window menu)))))
	       (dial-struct_standard.dispatch menu
					      'set.area
					      (dial-area_make :x 0
							      :y 0
							      :w (+ width offset)
							      :h (+ height offset))))))
	((eq message 'update.geometry)
	 )
	((eq message 'last.exit.button)
	 nil)
	((eq message 'choose)
	 (apply #'menu=dial_choose.choose menu parameters))
	((eq message 'variables.to.dialog)
	 (let ((message (cons 'variable.to.dialog parameters)))
	   (dial-struct_send.message.to.childs menu message :type item.choose.id)
	   (dial-struct_send.message.to.childs menu message :type item.edit.id))
	 )
	((eq message 'dialog.to.variables)
	 (menu=dial_choose.reset.variables menu)
	 (dial-struct_send.message.to.childs menu '(dialog.to.variable) :type item.choose.id)
	 )
	(t
	 (apply #'dial-struct_standard.dispatch menu message parameters)))) ; call inherited dispatch

(defun menu=dial_choose.reset.variables (menu)
  (menu=dial_choose.reset.variables.aux (menu=dial_choose.data.variables (dial-struct_data menu))))

(defun menu=dial_choose.reset.variables.aux (variables)
  (cond ((not (null variables))
	 (let ((variable (first variables)))
	   (cond ((listp variable)
		  (set (first variable) nil))))
	 (menu=dial_choose.reset.variables.aux (rest variables)))))

(defun menu=dial_choose.choose (choose.menu &optional near-mode (map-unmapped t))
  (dial-struct_call.dispatch choose.menu 'variables.to.dialog nil)
  (multiple-value-bind (x y)
		       (cond (near-mode
			      (cond ((eq (first near-mode) :point)
				     (values-list (rest near-mode)))
				    (t
				     (menu=dial_calc.orgin choose.menu))))
			     (t
			      (menu=dial_calc.orgin choose.menu)))
		       (menu=dial_map choose.menu x y))
  (let ((result (catch 'choose-variable-values (dial-struct_process.events choose.menu))))
    (menu=dial_unmap choose.menu map-unmapped)
;    (dial-struct_call.dispatch choose.menu 'dialog.to.variables)
    result
    )
  )

;;---------------------------- choose items (derived form single items) ----------------------------------

(defstruct (menu=dial_choose.item.data
	    (:conc-name menu=dial_choose.item.data.)
	    (:constructor menu=dial_choose.item.data.make)
	    (:predicate menu=dial_choose.item.data.p))
  "Choose Item Data"
  variable.desc          ; variable.desc is (variable namestring info category function)
  value                  ; value of item
  button                 ; item button
  category.items         ; items to be deselected, if this item bekomes selected
  mouse.buttons.pressed  ; mouse buttons pressed
  )

(defun menu=dial_choose.item.set.category.items (item category-items)
  (setf (menu=dial_choose.item.data.category.items (dial-struct_data item)) category-items))

(defun menu=dial_choose.item.create (menu variable-desc item-value
					  &key (hor.offset menu*item.choose.hor.offset)
					       (vert.offset menu*item.choose.vert.offset))
  ;; variable-desc is (variable namestring info category [function])
  ;; item-value is value | (printstring value)
  (let* ((parent.window (dial-struct_window menu))
	 (item.window  (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1 			; temporary value
			 :height	      1 			; temporary value
			 :border-width        0
			 :background          menu*bg.pixel
			 :cursor              menu*cursor
			 :event-mask 	      (XLIB:MAKE-EVENT-MASK :enter-window
								    :leave-window
								    :button-press
								    :button-release
								    :exposure)))
	 (type         (first (fourth variable-desc)))
	 (printstring  (cond ((eq :infinite type)
			      (format nil "~A" (eval (first variable-desc))))
			     ((listp item-value)
			      (first item-value))
			     (t (symbol-name item-value))))
	 (item.data    (menu=dial_choose.item.data.make
			:variable.desc variable-desc
			:value item-value
			:mouse.buttons.pressed nil
			:button (menu=dial_text.button.create item.window
							      printstring
							      menu*gcontext
							      :text.centered t
							      :selectable t
							      :exit nil
							      :hor.offset hor.offset
							      :vert.offset vert.offset))))
    (dial-struct_make
     :type               item.choose.id
     :data               item.data
     :window             item.window
     :parent             menu
     :childs             nil                                            ; a menu item has no childs
     :event.handler      #'menu=dial_choose.item.event.handler
     :dispatch           #'menu=dial_choose.item.dispatch)))


(defun menu=dial_choose.item.event.handler (item
					    &rest event-slots
					    &key display event-key send-event-p        ; common slots
					         event-window x y width height count   ; exposure slots
						 code                                  ; button press
					    &allow-other-keys)
;;  (format t
;;	  "menu=dial_choose.item.event.handler: event-key: ~S event-window: ~S~%"
;;	  event-key
;;	  event-window)
  (let* ((item.data (dial-struct_data item))
	 (window (dial-struct_window item))
	 (button (menu=dial_choose.item.data.button item.data))
	 (mouse.buttons.pressed (menu=dial_choose.item.data.mouse.buttons.pressed item.data)))
    (cond ((eq :exposure event-key)
	   (cond ((= 0 count)
		  (menu=dial_text.button.draw button)))
	   t)
	  ((eq :button-press event-key)
	   (setf (menu=dial_choose.item.data.mouse.buttons.pressed item.data)
		 (union (list code) mouse.buttons.pressed))
	   t)
	  ((and (eq :button-release event-key)
		(member code mouse.buttons.pressed))
	   (let* ((variable.desc (menu=dial_choose.item.data.variable.desc item.data))
		  (type (first (fourth variable.desc)))
		  (variable (first variable.desc))
		  (old.value (eval variable))
		  (function (fifth variable.desc))
		  (category.items (menu=dial_choose.item.data.category.items item.data)))
	     (setf (menu=dial_choose.item.data.mouse.buttons.pressed item.data)
		   (delete code mouse.buttons.pressed))
	     (menu=dial_text.button.set.selected button nil t)
	     (cond ((eq :finite-single type)
		    (dial-struct_send.message.to.dialogs (remove item category.items)
							 '(uncheck t))))
	     (menu=dial_text.button.set.attribute button
						  'checked
						  (cond ((eq type :finite-multiple)
							 (not (menu=dial_text.button.attribute.set.p button 'checked)))
							(t t))
						  t)
	     (menu=dial_text.button.set.selected button t t)
	     (set variable nil)
	     (dial-struct_send.message.to.dialogs category.items '(dialog.to.variable))
	     (cond (function
		    (dial-struct_call.dispatch (dial-struct_parent item)
					       'variables.to.dialog
					       (funcall function old.value (eval variable)))))
	     )
	   t)
	  (t (cond ((menu=dial_text.button.selectable button)
		    (cond ((and (eq :enter-notify event-key)
				(dial-misc_pointer.in.window.p window))
			   (menu=dial_text.button.set.selected button t t))
			  ((and (eq :leave-notify event-key)
				(not (dial-misc_pointer.in.window.p window)))
			   (setf (menu=dial_choose.item.data.mouse.buttons.pressed item.data) nil)
			   (menu=dial_text.button.set.selected button nil t))
			 )
		    )
		  )
	     t))))

(defun menu=dial_choose.item.dispatch (item message &rest parameters)
;; handles messages for a menu:
;;(format t
;;	  "menu=dial_choose.item.dispatch: item: ~S message: ~S parameters ~S~%"
;;	  (menu=dial_text.button.text (menu=dial_choose.item.data.button (dial-struct_data item)))
;;	  message
;;	  parameters)
  (let* ((item.data (dial-struct_data item))
	 (button (menu=dial_choose.item.data.button item.data)))
    (cond ((eq message 'compute.width)
	   (menu=dial_text.button.compute.width button))
	  ((eq message 'compute.height)
	   (menu=dial_text.button.compute.height button))
	  ((eq message 'set.area)
	   (let ((area (car parameters)))
	     (menu=dial_text.button.set.area button area)))
	  ((eq message 'dispose)
	   (menu=dial_text.button.dispose button)                              ; dispose text button
	   (apply #'dial-struct_standard.dispatch item message parameters)    ; call inherited dispatch
	   )
	  ((eq message 'uncheck)
	   (menu=dial_text.button.set.attribute button
						'checked
						nil
						(car parameters)))
	  ((eq message 'reset)
	   (menu=dial_text.button.set.selected button nil nil))
	  (t
	   (let* ((variable.desc (menu=dial_choose.item.data.variable.desc item.data))
		  (variable (first variable.desc))
		  (type (first (fourth variable.desc)))
		  (value (menu=dial_choose.item.data.value item.data)))
	     (cond ((eq message 'variable.to.dialog)
		    (menu=dial_text.button.set.attribute button
							 'checked
							 (menu=dial_choose.variable.eq.value.p variable
											       type
											       value)
							 (car parameters))
		    )
		   ((eq message 'dialog.to.variable)
		    (cond ((menu=dial_text.button.attribute.set.p button 'checked)
			   (menu=dial_choose.variable.set variable type value)))
		    )
		   (t
		    (apply #'dial-struct_standard.dispatch item message parameters)))))))) ; call inherited dispatch


(defun menu=dial_choose.variable.eq.value.p (variable type value)
  (let ((item.value (cond ((listp value)
			  (second value))
			 (t value)))
	(curr.value (eval variable)))
    (cond ((eq type :finite-multiple)
	   (member item.value curr.value))
	(t (eq item.value curr.value)))))

(defun menu=dial_choose.variable.set (variable type value)
  (let ((new.value (cond ((listp value)
			  (second value))
			 (t value))))
    (cond ((eq type :finite-multiple)
	   (let ((curr.value (eval variable)))
	     (cond ((member value curr.value)
		    (set variable (delete value curr.value)))
		   (t (set variable (append curr.value (list new.value)))))))
	  (t (set variable new.value)))))

;;----------------------------------------- edit item ----------------------------------------------------

(defstruct (menu=dial_edit.item.data
	    (:conc-name menu=dial_edit.item.data.)
	    (:constructor menu=dial_edit.item.data.make)
	    (:predicate menu=dial_edit.item.data.p))
  "Choose Item Data"
  variable.desc          ; variable.desc is (variable namestring info category function)
  value                  ; value of item
  button                 ; item button
  category.items         ; items to be deselected, if this item bekomes selected
  mouse.buttons.pressed  ; mouse buttons pressed
  )

(defun menu=dial_edit.item.create (menu variable-desc item-value
					&key (hor.offset menu*item.edit.hor.offset)
					(vert.offset menu*item.edit.vert.offset))
  ;; variable-desc is (variable namestring info category [function] {min-width max-width}:)
  ;; item-value is value | (printstring value)
  (let* ((parent.window (dial-struct_window menu))
	 (item.window  (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1 			; temporary value
			 :height	      1 			; temporary value
			 :border-width        1
			 :background          menu*bg.pixel
			 :cursor              menu*cursor
			 :event-mask 	      (XLIB:MAKE-EVENT-MASK :enter-window
								    :leave-window
								    :key-release
								    :button-press
								    :button-release
								    :exposure)))
	 (type         (first (fourth variable-desc)))
	 (printstring  (cond ((eq :infinite type)
			      (format nil "~A" (eval (first variable-desc))))
			     ((listp item-value)
			      (first item-value))
			     (t (symbol-name item-value))))
	 (rest-parms   (cdr (cddddr variable-desc)))
	 (min-width    (second (member :min-width rest-parms)))
	 (max-width    (second (member :max-width rest-parms)))
	 (item.data    (menu=dial_edit.item.data.make
			:variable.desc variable-desc
			:value item-value
			:mouse.buttons.pressed nil
			:button (menu=dial_text.button.create item.window
							      printstring
							      menu*gcontext
							      :text.centered nil
							      :selectable t
							      :exit nil
							      :min-width (cond (min-width
										min-width)
									       (t 10))
							      :max-width max-width
							      :hor.offset hor.offset
							      :vert.offset vert.offset))))
    (dial-struct_make
     :type               item.edit.id
     :data               item.data
     :window             item.window
     :parent             menu
     :childs             nil                                            ; a menu item has no childs
     :event.handler      #'menu=dial_edit.item.event.handler
     :dispatch           #'menu=dial_edit.item.dispatch)))


(defun menu=dial_edit.item.event.handler (item
					    &rest event-slots
					    &key display event-key send-event-p        ; common slots
					         event-window x y width height count   ; exposure slots
						 code state                            ; button press
					    &allow-other-keys)
;;  (format t
;;	  "menu=dial_edit.item.event.handler: event-key: ~S event-window: ~S~%"
;;	  event-key
;;	  event-window)
  (let* ((item.data (dial-struct_data item))
	 (window (dial-struct_window item))
	 (button (menu=dial_edit.item.data.button item.data))
	 (mouse.buttons.pressed (menu=dial_edit.item.data.mouse.buttons.pressed item.data)))
    (cond ((eq :exposure event-key)
	   (cond ((= 0 count)
		  (menu=dial_text.button.draw button)))
	   t)
	  ((eq :button-press event-key)
	   (setf (menu=dial_edit.item.data.mouse.buttons.pressed item.data)
		 (union (list code) mouse.buttons.pressed))
	   t)
	  ((and (eq :button-release event-key)
		(member code mouse.buttons.pressed))
	   (setf (menu=dial_edit.item.data.mouse.buttons.pressed item.data)
		 (delete code mouse.buttons.pressed))
	   (menu=dial_text.button.set.selected button nil t)
	   (let* ((pos (length (menu=dial_text.button.text button)))
		  (keysym 0)
		  (variable.desc (menu=dial_choose.item.data.variable.desc item.data))
		  (variable (first variable.desc))
		  (old.value (eval variable))
		  (function (fifth variable.desc))
		  (category (fourth variable.desc))
		  (predicate (second category))
		  (old-focus (multiple-value-list (XLIB:INPUT-FOCUS dial*display)))
		  (type-info (third category)))
	     (menu=dial_text.button.set.cursor button pos)
	     #|(XLIB:GRAB-POINTER window
				(XLIB:MAKE-EVENT-MASK :enter-window :leave-window)
				:cursor menu*text.cursor
				:sync-pointer-p t)|#
	     (XLIB:SET-INPUT-FOCUS dial*display window :none)
	     (labels ((exit ()
			    (let ((exit.result (XK-is_end.of.input keysym)))
			      (cond (exit.result
				     (let* ((result (read-from-string (menu=dial_text.button.text button)
								      nil :__EOF__))
					    (reset (or (cond (predicate
							      (cond ((not (funcall predicate result))
								     (notify type-info)
								     t))))
						       (eq :__EOF__ result))))
				       (cond (reset
					      (menu=dial_text.button.set.text button
									      (format nil
										      "~A"
										      old.value)
									      t))))))
			      exit.result)))
		     (do () ((exit))
			 (XLIB:EVENT-CASE (dial*display :force-output t)
					  (:key-release
					   (code state)
					   (setq keysym (XLIB:KEYCODE->KEYSYM dial*display
									      code
									      (XK-keysym_index dial*display
											       code
											       state)))
					   (cond ((XK-is_left keysym state)
						  (cond ((> pos 0)
							 (menu=dial_text.button.set.cursor button
											   (1- pos)
											   pos)
							 (setq pos (1- pos))))
						  )
						 ((XK-is_right keysym state)
						  (cond ((< pos
							    (length (menu=dial_text.button.text button)))
							 (menu=dial_text.button.set.cursor button
											   (1+ pos)
											   pos)
							 (setq pos (1+ pos))))
						  )
						 ((XK-is_printable keysym state)
						  (let ((character (XLIB:KEYSYM->CHARACTER dial*display
											   keysym)))
;;						    (format t
;;							    "key-release:~%code: ~A~%state: ~A~%keysym: ~A~%character: ~A~%"
;;							    code
;;							    state
;;							    keysym
;;							    character)
						    (cond (character
							   (menu=dial_text.button.insert.character button character pos t)
							   (menu=dial_text.button.set.cursor button (1+ pos) pos)
							   (setq pos (1+ pos))))))
						 ((and (> pos 0)
						       (or (= keysym XK_BackSpace)
							   (= keysym XK_Delete)))
						  (setq pos (1- pos))
						  (menu=dial_text.button.delete.character button pos t)
						  (menu=dial_text.button.set.cursor button pos (1+ pos)))
						 )
					   t)
					  (otherwise
					   ()
					   t)))
		     )
	     (apply #'XLIB:SET-INPUT-FOCUS dial*display old-focus)
	     ;(XLIB:UNGRAB-POINTER dial*display)
	     (let ((window (dial-struct_window menu*mapped)))
	       #|(XLIB:GRAB-POINTER window
				  (XLIB:WINDOW-EVENT-MASK window)
				  :owner-p t
				  :cursor menu*cursor)|#)
	     (set variable (read-from-string (menu=dial_text.button.text button)))
	     (menu=dial_edit.item.dispatch item 'variable.to.dialog t)
	     (cond (function
		    (dial-struct_call.dispatch (dial-struct_parent item)
					       'variables.to.dialog
					       (funcall function old.value (eval variable)))))
	     )
	   (menu=dial_text.button.set.origin.pos button 0 t)
	   (cond ((and (menu=dial_text.button.selectable button)
		       (dial-misc_pointer.in.window.p window))
		  (menu=dial_text.button.set.selected button t t)))
	   t)
	  ((eq :key-release event-key)
;;	   (let* ((keysym1 (XLIB:KEYCODE->KEYSYM dial*display code state))
;;		  (character1 (XLIB:KEYSYM->CHARACTER dial*display keysym1))
;;		  (keysym2 (XLIB:KEYCODE->KEYSYM dial*display code 0))
;;		  (character2 (XLIB:KEYSYM->CHARACTER dial*display keysym2)))
;;	     (format t
;;		   "key-release:~%code: ~A~%state: ~A~%keysym1: ~A~%character1: ~A~%keysym2: ~A~%character2: ~A~%"
;;		   code
;;		   state
;;		   keysym1
;;		   character1
;;		   keysym2
;;		   character2)
;;	   )
	   t)
	  (t (cond ((menu=dial_text.button.selectable button)
		    (cond ((and (eq :enter-notify event-key)
				(dial-misc_pointer.in.window.p window))
			   (menu=dial_text.button.set.selected button t t))
			  ((and (eq :leave-notify event-key)
				(not (dial-misc_pointer.in.window.p window)))
			   (setf (menu=dial_edit.item.data.mouse.buttons.pressed item.data) nil)
			   (menu=dial_text.button.set.selected button nil t))
			 )
		    )
		   )
	     t))))

(defun menu=dial_edit.item.dispatch (item message &rest parameters)
;; handles messages for a menu:
;;(format t
;;	  "menu=dial_edit.item.dispatch: item: ~S message: ~S parameters ~S~%"
;;	  (menu=dial_text.button.text (menu=dial_edit.item.data.button (dial-struct_data item)))
;;	  message
;;	  parameters)
  (let* ((item.data (dial-struct_data item))
	 (button (menu=dial_edit.item.data.button item.data)))
    (cond ((eq message 'compute.width)
	   (menu=dial_text.button.compute.width button))
	  ((eq message 'compute.height)
	   (menu=dial_text.button.compute.height button))
	  ((eq message 'set.area)
	   (let ((area (car parameters)))
	     (menu=dial_text.button.set.area button area)))
	  ((eq message 'dispose)
	   (menu=dial_text.button.dispose button)                              ; dispose text button
	   (apply #'dial-struct_standard.dispatch item message parameters)    ; call inherited dispatch
	   )
	  ((eq message 'variable.to.dialog)
	   (menu=dial_text.button.set.text button
					   (format nil "~A" (eval (first (menu=dial_choose.item.data.variable.desc item.data))))
					   (car parameters))
	   )
	  ((eq message 'uncheck)
	   (menu=dial_text.button.set.attribute button
						'checked
						nil
						(car parameters)))
	  ((eq message 'reset)
	   (menu=dial_text.button.set.selected button nil nil))
	  (t
	   (apply #'dial-struct_standard.dispatch item message parameters))))) ; call inherited dispatch


;;------------------------------------------- notify function --------------------------------------------

(defun menu=dial_notify.create (list-of-items)
  (labels ((make.item (item)
		      (list (eval item)
			    :value nil
			    :selectable nil)))
	  (menu-create :single
		       (mapcar #'make.item list-of-items)
		       :top-line "Information"
		       :help nil)))

;;========================================================================================================

;;------------------------------- check box items definition----------------------------------------------


;; constants:



;; check box data:

(defstruct (menu=dial_check.box.item.data
	    (:conc-name menu=dial_check.box.item.data.)
	    (:constructor menu=dial_check.box.item.data.make)
	    (:predicate menu=dial_check.box.item.data.p))
  "Check Box Item data"
  check.box
  mouse.buttons.pressed  ; mouse buttons pressed
  )

;; create and dispose:

(defun menu=dial_check.box.item.create (menu &key (hor.offset menu*check.box.hor.offset)
					          (vert.offset menu*check.box.vert.offset))
  (let* ((parent.window (dial-struct_window menu))
	 (item.window  (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1 			; temporary value
			 :height	      1 			; temporary value
			 :border-width        0
			 :background          menu*bg.pixel
			 :cursor              menu*cursor
			 :event-mask 	      (XLIB:MAKE-EVENT-MASK :button-release
								    :leave-window
								    :button-press
								    :exposure)))
	 (item.data    (menu=dial_check.box.item.data.make
			:mouse.buttons.pressed nil
			:check.box (menu=dial_check.box.create item.window
							    menu*gcontext
							    :selectable t
							    :hor.offset hor.offset
							    :vert.offset vert.offset))))
    (dial-struct_make
     :type               item.check.box.id
     :data               item.data
     :window             item.window
     :parent             menu
     :childs             nil                                            ; a menu item has no childs
     :event.handler      #'menu=dial_check.box.item.event.handler
     :dispatch           #'menu=dial_check.box.item.dispatch)))


;; methods:

(defun menu=dial_check.box.item.set.selected (item select &optional (redraw nil))
  (menu=dial_check.box.set.selected (menu=dial_check.box.item.data.check.box (dial-struct_data item))
				    select
				    redraw))

(defun menu=dial_check.boxes.select (check.boxes which)
  (cond ((not (null which))
	 (let ((check.box (nth (car which) check.boxes)))
	   (cond (check.box
		  (menu=dial_check.box.item.set.selected check.box t))))
	 (menu=dial_check.boxes.select check.boxes (cdr which)))))

(defun menu=dial_check.box.item.selected.p (item)
  (menu=dial_check.box.selected (menu=dial_check.box.item.data.check.box (dial-struct_data item))))

;; event & dispatch function:


(defun menu=dial_check.box.item.event.handler (item
					       &rest event-slots
					       &key display event-key send-event-p   ; common slots
					       event-window x y width height count   ; exposure slots
					       code
					       &allow-other-keys)
;;  (format t
;;	  "menu=dial_check.box.item.event.handler: event-key: ~S event-window: ~S~%"
;;	  event-key
;;	  event-window)
  (let* ((item.data (dial-struct_data item))
	 (mouse.buttons.pressed (menu=dial_check.box.item.data.mouse.buttons.pressed item.data))
	 (check.box (menu=dial_check.box.item.data.check.box item.data)))
    (cond ((eq :exposure event-key)
	   (cond ((= 0 count)
		  (menu=dial_check.box.draw check.box)))
	   t)
	  (t (cond ((menu=dial_check.box.selectable check.box)
		    (cond ((eq :leave-notify event-key)
			   (setf (menu=dial_check.box.item.data.mouse.buttons.pressed item.data) nil)
			   t)
			  ((eq :button-press event-key)
			   (setf (menu=dial_check.box.item.data.mouse.buttons.pressed item.data)
				 (union (list code) mouse.buttons.pressed))
			   t)
			  ((and (eq :button-release event-key)
				(member code mouse.buttons.pressed))
			   (setf (menu=dial_check.box.item.data.mouse.buttons.pressed item.data)
				 (delete code mouse.buttons.pressed))
			   (menu=dial_check.box.set.selected check.box (not (menu=dial_check.box.selected check.box)) t)
			   t)
			  (t t)
			 ))
		   (t t)
		  )))))

(defun menu=dial_check.box.item.dispatch (item message &rest parameters)
;; handles messages for a menu
;;  (format menu*debug.stream
;;	  "menu=dial_check.box.item.dispatch: message: ~S parameters ~S~%" message parameters)
  (let ((check.box (menu=dial_check.box.item.data.check.box (dial-struct_data item))))
    (cond ((eq message 'compute.width)
	   (menu=dial_check.box.get.width check.box))
	  ((eq message 'compute.height)
	   (menu=dial_check.box.get.height check.box))
	  ((eq message 'set.area)
	   (let ((area (car parameters)))
	     (menu=dial_check.box.set.area check.box area)))
	  (t
	   (apply #'dial-struct_standard.dispatch item message parameters))))) ; call inherited dispatch


;;------------------------------- filler items definition-------------------------------------------


;; constants:



;; check box data:

(defstruct (menu=dial_filler.item.data
	    (:conc-name menu=dial_filler.item.data.)
	    (:constructor menu=dial_filler.item.data.make)
	    (:predicate menu=dial_filler.item.data.p))
  "Filler Item data"
  filler
  )


;; create and dispose:

(defun menu=dial_filler.item.create (menu width height &key (separator :none))
;;  (format t "filler.create: width:~A  height:~A  separator:~A~%" width height separator)
  (let* ((parent.window (dial-struct_window menu))
	 (filler.window  (XLIB:CREATE-WINDOW
			 :parent	      parent.window
			 :x		      0			        ; temporary value
			 :y		      0			        ; temporary value
			 :width		      1           		; temporary value
			 :height	      1 			; temporary value
			 :border-width        0
			 :background          menu*bg.pixel
			 :event-mask 	      (XLIB:MAKE-EVENT-MASK :exposure)))
	 (filler.data    (menu=dial_filler.item.data.make
			:filler (menu=dial_filler.create filler.window
							 menu*gcontext
							 width
							 height
							 :separator separator))))
    (dial-struct_make
     :type               item.filler.id
     :data               filler.data
     :window             filler.window
     :parent             menu
     :childs             nil                                            ; a menu item has no childs
     :event.handler      #'menu=dial_filler.item.event.handler
     :dispatch           #'menu=dial_filler.item.dispatch)))


;; methods:


;; event & dispatch function:

(defun menu=dial_filler.item.event.handler (filler.item
					       &rest event-slots
					       &key display event-key send-event-p   ; common slots
					       event-window x y width height count   ; exposure slots
					       &allow-other-keys)
;;  (format t
;;	  "menu=dial_filler.item.event.handler: event-key: ~S event-window: ~S~%"
;;	  event-key
;;	  event-window)
  (let ((filler (menu=dial_filler.item.data.filler (dial-struct_data filler.item))))
    (cond ((eq :exposure event-key)
	   (cond ((= 0 count)
		  (menu=dial_filler.draw filler)))
	   t)
	  (t t)
	  )
    )
  )

(defun menu=dial_filler.item.dispatch (filler.item message &rest parameters)
;; handles messages for a menu
;;  (format menu*debug.stream
;;	  "menu=dial_filler.item.dispatch: message: ~S parameters ~S~%" message parameters)
  (let ((filler (menu=dial_filler.item.data.filler (dial-struct_data filler.item))))
    (cond ((eq message 'compute.width)
	   (menu=dial_filler.compute.width filler))
	  ((eq message 'compute.height)
	   (menu=dial_filler.compute.height filler))
	  ((eq message 'set.area)
	   (let ((area (car parameters)))
	     (menu=dial_filler.set.area filler area)))
	  (t
	   (apply #'dial-struct_standard.dispatch
		  filler.item message parameters))))) ; call inherited dispatch


;;------------------------------------ text button windows ----------------------------------------------

(defstruct (menu=dial_text.button
	    (:conc-name menu=dial_text.button.)
	    (:constructor menu=dial_text.button.make)
	    (:predicate menu=dial_text.button.p))
  "data structure for a text button"
  window              ; button window
  gcontext            ; graphic context
  text                ; button text string
  min.width           ; minimum pixel width
  max.width           ; maximum pixel width
  text.origin.x       ; text x-origin
  text.origin.y       ; text y-origin (baseline)
  text.origin.pos     ; number of first character
  text.width          ; text width
  text.height         ; text height
  text.centered       ; is the text centered in the button window
  hor.offset          ; horizontal margin offset
  vert.offset         ; vertical margin offset
  attributes          ; attributes
  selectable          ; is the button selectable?
  exit                ; exit button?
  selected)           ; is the button selected?

(defun menu=dial_text.button.create (window
				     button-text
				     gcontext
				     &key (text.centered nil)
				          (selectable t)
					  (exit t)
					  (attributes nil)
					  (min-width nil)
					  (max-width nil)
					  (hor.offset 0)
					  (vert.offset 0))
  (let* ((max-char-width (XLIB:MAX-CHAR-WIDTH (XLIB:GCONTEXT-FONT gcontext)))
	 (pixel.min.width (cond (min-width
				 (* min-width max-char-width))
				(t 0)))
	 (pixel.max.width (cond (max-width
				 (* max-width max-char-width))
				(t (XLIB:DRAWABLE-WIDTH menu*root.window)))))
    (menu=dial_text.button.make :window          window
				:gcontext        gcontext
				:text            button-text
				:min.width       pixel.min.width
				:max.width       pixel.max.width
				:text.origin.x   0  ; temporary value (set by set.area)
				:text.origin.y   0  ; temporary value (set by set.area)
				:text.origin.pos 0
				:text.width      (menu=dial_text.compute.width gcontext button-text)
				:text.height     (menu=dial_text.compute.height gcontext button-text)
				:text.centered   text.centered
				:hor.offset      hor.offset
				:vert.offset     vert.offset
				:attributes      attributes
				:selectable      selectable
				:exit            exit
				:selected        nil))
  )

(defun menu=dial_text.button.dispose (button)
  (let ((gcontext (menu=dial_text.button.gcontext button)))
    (cond ((not (eq gcontext menu*gcontext))
	   (XLIB:FREE-GCONTEXT gcontext)))))

(defun menu=dial_text.button.set.centered (button centered &optional (redraw nil))
  (let* ((text.width (menu=dial_text.button.text.width button))
	 (window (menu=dial_text.button.window button))
	 (window.width (XLIB:DRAWABLE-WIDTH window))
	 (origin.x (if (and centered
			    (<= text.width window.width))
		       (floor (- window.width text.width) 2)
		     0)))
    (setf (menu=dial_text.button.text.centered button) centered)
    (setf (menu=dial_text.button.text.origin.x button) origin.x))
  (if redraw (menu=dial_text.button.draw button)))
    
(defun menu=dial_text.button.set.area (button button.area &optional (redraw nil))
;;  (format t "text.button.set.area: ~A" button.area)
  (let* ((text.width (menu=dial_text.button.text.width button))
	 (text.height (menu=dial_text.button.text.height button))
	 (text.ascent (XLIB:MAX-CHAR-ASCENT (XLIB:GCONTEXT-FONT (menu=dial_text.button.gcontext button))))
	 (button.area.w (dial-area_w button.area))
	 (button.area.h (dial-area_h button.area))
	 (window (menu=dial_text.button.window button))
	 (window.border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH window)))
	 (offset*2 (cond ((> window.border.offset*2 0)
			  menu*text.button.border.offset)
			 (t 0)))
	 (offset (floor offset*2 2))
	 (window.outside.width (if (= button.area.w 0)
				   (+ text.width
				      window.border.offset*2
				      (menu=dial_text.button.hor.offset button))
				 (- button.area.w offset*2)))
	 (window.outside.height (if (= button.area.h 0)
				    (+ text.height
				       window.border.offset*2
				       (menu=dial_text.button.vert.offset button))
				  (- button.area.h offset*2)))
	 (window.inside.height (- window.outside.height window.border.offset*2))
	 (text.origin.y (max text.ascent (+ (floor (- window.inside.height text.height) 2) text.ascent)))
	 (window.area (dial-area_make :x (+ (dial-area_x button.area) offset)
				      :y (+ (dial-area_y button.area) offset)
				      :w window.outside.width
				      :h window.outside.height)))
    (setf (menu=dial_text.button.text.origin.y button) text.origin.y)
    (dial-drawable_set.area window window.area)
    )
  (menu=dial_text.button.set.centered button (menu=dial_text.button.text.centered button) redraw))

(defun menu=dial_text.button.compute.width (button)
  (let* ((text.width (min (max (menu=dial_text.button.text.width button)
			       (menu=dial_text.button.min.width button))
			  (menu=dial_text.button.max.width button)))
	 (window (menu=dial_text.button.window button))
	 (window.border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH window)))
	 (window.outside.width (+ text.width
				  window.border.offset*2
				  (cond ((> window.border.offset*2 0)
					 menu*text.button.border.offset)
					(t 0))
				  (menu=dial_text.button.hor.offset button))))
    window.outside.width))

(defun menu=dial_text.button.compute.height (button)
  (let* ((text.height (menu=dial_text.button.text.height button))
	 (window (menu=dial_text.button.window button))
	 (window.border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH window)))
	 (window.outside.height (+ text.height
				   window.border.offset*2
				   (cond ((> window.border.offset*2 0)
					  menu*text.button.border.offset)
					 (t 0))
				   (menu=dial_text.button.vert.offset button))))
    window.outside.height))

(defun menu=dial_text.button.set.origin.pos (button pos &optional (redraw nil))
  (let ((old.pos (menu=dial_text.button.text.origin.pos button)))
    (cond ((not (= pos old.pos))
	   (setf (menu=dial_text.button.text.origin.pos button) pos)
	   (cond (redraw
		  (menu=dial_text.button.draw button)))))))

(defun menu=dial_text.button.set.text (button button-text &optional (redraw nil) (start 0) (end nil))
  (let ((gcontext (menu=dial_text.button.gcontext button)))
    (setf (menu=dial_text.button.text button) button-text)
    (setf (menu=dial_text.button.text.width button) (menu=dial_text.compute.width gcontext button-text))
    (setf (menu=dial_text.button.text.height button) (menu=dial_text.compute.height gcontext button-text))
    (cond (redraw
	   (menu=dial_text.button.draw button :start start :end end)))
    ))

(defun menu=dial_text.button.insert.character (button character pos &optional (redraw nil))
  (let ((text (menu=dial_text.button.text button))
	(char.text (make-sequence 'string 1 :initial-element character)))
    (menu=dial_text.button.set.text button
				    (cond ((>= pos (length text))
					   (concatenate 'string text char.text))
					  ((<= pos 0)
					   (concatenate 'string char.text text))
					  (t (concatenate 'string
							  (subseq text 0 pos)
							  char.text
							  (subseq text pos))))
				    redraw
				    pos
				    )))

(defun menu=dial_text.button.delete.character (button pos &optional (redraw nil))
  (let ((text (menu=dial_text.button.text button)))
    (cond ((< pos (length text))
	   (menu=dial_text.button.set.text button
					   (delete (elt text pos) text :start pos :end (1+ pos))
					   redraw
					   pos)))))

(defun menu=dial_text.button.set.cursor (button pos &optional (old-pos 0))
  (let* ((window (menu=dial_text.button.window button))
	 (max.width (XLIB:DRAWABLE-WIDTH window))
	 (origin.x (menu=dial_text.button.text.origin.x button))
	 (origin.y (menu=dial_text.button.text.origin.y button))
	 (origin.pos  (menu=dial_text.button.text.origin.pos button))
	 (text (menu=dial_text.button.text button))
	 (text.length (length text))
	 (gcontext (menu=dial_text.button.gcontext button))
	 (font (XLIB:GCONTEXT-FONT gcontext))
	 (char.width (XLIB:MAX-CHAR-WIDTH font))
	 (ascent (XLIB:MAX-CHAR-ASCENT font))
	 (char.height (+ ascent
			 (XLIB:MAX-CHAR-DESCENT font)))
	 (offset (max 1 (floor max.width char.width)))
	 (fg (XLIB:GCONTEXT-BACKGROUND gcontext))
	 (bg (XLIB:GCONTEXT-FOREGROUND gcontext)))
    (cond ((< pos origin.pos)
	   (do ((org.pos origin.pos (menu=dial_text.button.text.origin.pos button)))
	       ((not (< pos org.pos)))
	       (menu=dial_text.button.set.origin.pos button (max 0 (- org.pos offset)) t)))
	  ((> (* (1+ (- pos origin.pos))
		 char.width)
	      max.width)
	   (do ((org.pos origin.pos (menu=dial_text.button.text.origin.pos button)))
	       ((not (> (* (1+ (- pos org.pos))
			   char.width)
			max.width)))
	       (menu=dial_text.button.set.origin.pos button (min (+ org.pos offset) text.length) t)))
	  ((= old-pos 0)
	   (menu=dial_text.button.draw button))
	  (t
	   (menu=dial_text.button.draw button
				       :start old-pos
				       :end (cond ((>= old-pos text.length) nil)
						  (t (1+ old-pos))))))
    (let ((x (+ origin.x
		(* (- pos
		      (menu=dial_text.button.text.origin.pos button))
		   char.width)))
	  (sequence (cond ((>= pos text.length)
			   nil)
			  (t (subseq text pos (1+ pos))))))
      (setf (XLIB:WINDOW-BACKGROUND window) bg)
      (XLIB:CLEAR-AREA window
		       :x (1+ x)
		       :y (- origin.y ascent)
		       :width char.width
		       :height char.height
		       :exposures-p nil)
      (cond (sequence
	     (XLIB:WITH-GCONTEXT (gcontext :foreground fg :background bg)
				 (XLIB:DRAW-GLYPHS window gcontext x origin.y sequence))))
      (setf (XLIB:WINDOW-BACKGROUND window) fg)
      )
    )
  )

(defun menu=dial_text.button.draw (button &key (start 0) (end nil))
  (let* ((window (menu=dial_text.button.window button))
	 (selected (menu=dial_text.button.selected button))
	 (gcontext (menu=dial_text.button.gcontext button))
	 (gcontext.font (XLIB:GCONTEXT-FONT gcontext))
	 (font (cond ((menu=dial_text.button.attribute.set.p button 'checked)
		      menu*bold.font)
		     (t gcontext.font)))
	 (fg (if selected 
		 (XLIB:GCONTEXT-BACKGROUND gcontext)
	       (XLIB:GCONTEXT-FOREGROUND gcontext)))
	 (bg (if selected
		 (XLIB:GCONTEXT-FOREGROUND gcontext)
	       (XLIB:GCONTEXT-BACKGROUND gcontext)))
	 (pos (menu=dial_text.button.text.origin.pos button))
	 (first.to.draw (max pos start))
	 (char.width (XLIB:MAX-CHAR-WIDTH font))
	 (x (+ (menu=dial_text.button.text.origin.x button)
	       (* char.width (- first.to.draw pos))))
	 (y (menu=dial_text.button.text.origin.y button))
	 (text (menu=dial_text.button.text button))
	 (sequence (cond ((<= first.to.draw (length text))
			  (subseq text first.to.draw end))
			 (t ""))))
;;    (format t "button.area: ~A~%" (dial-drawable_get.area window))
;;    (format t "text.button.draw: sequence: ~A  start: ~A  end: ~A~%" sequence start end)
    (setf (XLIB:WINDOW-BACKGROUND window) bg)
    (let ((clear.x (cond ((> start 0)
			  x)
			 (t 0)))
	  (clear.width (cond (end
			      (1+ (* char.width (length sequence)))))))
      (cond (clear.width
	     (XLIB:CLEAR-AREA window
			      :x clear.x
			      :width clear.width
			      :exposures-p nil))
	    (t (XLIB:CLEAR-AREA window
				:x clear.x
				:exposures-p nil))))
    (XLIB:WITH-GCONTEXT (gcontext :font font :foreground fg :background bg)
			(XLIB:DRAW-GLYPHS window gcontext x y sequence))
    (setf (XLIB:GCONTEXT-FONT gcontext) gcontext.font)  ; with-gcontext does not save the font!!
    ))

(defun menu=dial_text.button.set.attribute (button attribute set &optional (redraw nil))
  (cond (set
	 (setf (menu=dial_text.button.attributes button)
	       (union (list attribute)
		      (menu=dial_text.button.attributes button)))
	 (cond (redraw
		(menu=dial_text.button.draw button))))
	(t (menu=dial_text.button.disable.attribute button attribute redraw))))

(defun menu=dial_text.button.attribute.set.p (button attribute)
  (member attribute (menu=dial_text.button.attributes button)))

(defun menu=dial_text.button.disable.attribute (button attribute &optional (redraw nil))
  (setf (menu=dial_text.button.attributes button)
	(delete attribute
		(menu=dial_text.button.attributes button)))
  (cond (redraw
	 (menu=dial_text.button.draw button))))

(defun menu=dial_text.button.set.selected (button select &optional (redraw nil))
  (let ((old.selected (menu=dial_text.button.selected button)))
    (cond ((not (eq old.selected select))
	   (setf (menu=dial_text.button.selected button) select)
	   (cond (redraw
		  (menu=dial_text.button.draw button)
		  (XLIB:DISPLAY-FORCE-OUTPUT dial*display))
		 (t
		  (let* ((gcontext (menu=dial_text.button.gcontext button))
			 (bg (if select
				 (XLIB:GCONTEXT-FOREGROUND gcontext)
			       (XLIB:GCONTEXT-BACKGROUND gcontext)))
			 (window (menu=dial_text.button.window button)))
		    (setf (XLIB:WINDOW-BACKGROUND window) bg))))))))

(defun menu=dial_text.compute.width (font text)
  (multiple-value-bind (width ascent descent left right font-ascent font-descent direction)
		       (XLIB:TEXT-EXTENTS font text)
		       (+ (- right left) (* 2 (max left (- width right))))))

(defun menu=dial_text.compute.height (font text)
  (multiple-value-bind (width ascent descent left right font-ascent font-descent direction)
		       (XLIB:TEXT-EXTENTS font text)
		       (+ font-ascent font-descent)))

;;------------------------------------ check box windows -------------------------------------------------

(defstruct (menu=dial_check.box
	    (:conc-name menu=dial_check.box.)
	    (:constructor menu=dial_check.box.make)
	    (:predicate menu=dial_check.box.p))
  "data structure for a text check.box"
  window              ; check.box window
  gcontext            ; graphic context
  box.area            ; check box area
  hor.offset          ; horizontal margin offset
  vert.offset         ; vertical margin offset
  selectable          ; is the check box selectable?
  selected)           ; is the check box selected?

(defun menu=dial_check.box.compute.min.width.height (gcontext)
  (let ((font (XLIB:GCONTEXT-FONT gcontext)))
    (max (XLIB:MAX-CHAR-WIDTH font)
	 (+ (XLIB:MAX-CHAR-DESCENT font)
	    (XLIB:MAX-CHAR-ASCENT  font)))))

(defun menu=dial_check.box.compute.min.area (gcontext)
  (let ((x (menu=dial_check.box.compute.min.width.height gcontext)))
    (dial-area_make :x 0
		    :y 0
		    :w x
		    :h x)))

(defun menu=dial_check.box.create (window
				   gcontext
				   &key (selectable t)
				        (hor.offset 0)
					(vert.offset 0))
  (let ((check.box
	 (menu=dial_check.box.make :window         window
				   :gcontext       gcontext
				   :box.area       (menu=dial_check.box.compute.min.area gcontext)
				   :hor.offset     hor.offset
				   :vert.offset    vert.offset
				   :selectable     selectable
				   :selected       nil))
	(window.area (dial-drawable_get.area window)))
;;    (menu=dial_check.box.set.area check.box (dial-area_make :x (dial-area_x window.area)
;;							    :y (dial-area_y window.area)
;;							    :w 0        ; compute minimum
;;							    :h 0))      ; compute minimum
    check.box)
  )

(defun menu=dial_check.box.get.width (check.box)
  (+ (menu=dial_check.box.compute.min.width.height (menu=dial_check.box.gcontext check.box))
     (menu=dial_check.box.hor.offset check.box)))

(defun menu=dial_check.box.get.height (check.box)
  (+ (menu=dial_check.box.compute.min.width.height (menu=dial_check.box.gcontext check.box))
     (menu=dial_check.box.vert.offset check.box)))

(defun menu=dial_check.box.set.area (check.box check.box.area &optional (redraw nil))
  (let* ((box.area (menu=dial_check.box.box.area check.box))
	 (box.width (dial-area_w box.area))
	 (box.height (dial-area_h box.area))
	 (check.box.area.w (dial-area_w check.box.area))
	 (check.box.area.h (dial-area_h check.box.area))
	 (window (menu=dial_check.box.window check.box))
	 (window.border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH window)))
	 (window.outside.width (if (= check.box.area.w 0)
				   (+ box.width
				      window.border.offset*2
				      (menu=dial_check.box.hor.offset check.box))
				 check.box.area.w))
	 (window.outside.height (if (= check.box.area.h 0)
				    (+ box.height
				       window.border.offset*2
				       (menu=dial_check.box.vert.offset check.box))
				  check.box.area.h))
	 (window.inside.height (- window.outside.height window.border.offset*2))
	 (window.inside.width  (- window.outside.width  window.border.offset*2))
	 (box.x (if (<= box.width window.inside.width)
		    (floor (- window.inside.width box.width) 2)
		  0))
	 (box.y (if (<= box.height window.inside.height)
		    (floor (- window.inside.height box.height) 2)
		  0))
	 (window.area (dial-area_make :x (dial-area_x check.box.area)
				      :y (dial-area_y check.box.area)
				      :w window.outside.width
				      :h window.outside.height)))
    (setf (dial-area_x (menu=dial_check.box.box.area check.box)) box.x)
    (setf (dial-area_y (menu=dial_check.box.box.area check.box)) box.y)
    (dial-drawable_set.area window window.area)
    (if redraw (menu=dial_check.box.draw check.box))))

(defun menu=dial_check.box.draw (check.box)
  (let* ((window (menu=dial_check.box.window check.box))
	 (selected (menu=dial_check.box.selected check.box))
	 (gcontext (menu=dial_check.box.gcontext check.box))
	 (box.area (menu=dial_check.box.box.area check.box))
	 (x1 (dial-area_x box.area))
	 (x2 (1- (+ x1 (dial-area_w box.area))))
	 (y1 (dial-area_y box.area))
	 (y2 (1- (+ y1 (dial-area_h box.area))))
	 (box.border (list x1 y1
			   x1 y2
			   x2 y2
			   x2 y1
			   x1 y1)))
    (XLIB:CLEAR-AREA window)
    (XLIB:DRAW-LINES window gcontext box.border)
    (cond (selected
	   (XLIB:DRAW-LINE window gcontext x1 y1 x2 y2)
	   (XLIB:DRAW-LINE window gcontext x1 y2 x2 y1)))))

(defun menu=dial_check.box.set.selected (check.box select &optional (redraw nil))
  (let ((old.selected (menu=dial_check.box.selected check.box)))
    (cond ((not (eq old.selected select))
	   (setf (menu=dial_check.box.selected check.box) select)
	   (cond (redraw
		  (menu=dial_check.box.draw check.box)
		  (XLIB:DISPLAY-FORCE-OUTPUT dial*display)))))))


;;------------------------------------ filler windows -------------------------------------------------

(defstruct (menu=dial_filler
	    (:conc-name menu=dial_filler.)
	    (:constructor menu=dial_filler.make)
	    (:predicate menu=dial_filler.p))
  "data structure for a text filler"
  window              ; filler window
  gcontext            ; graphic context
  separator           ; separator to draw? :none or :hor or :vert
  filler.min.width    ; minimum width
  filler.min.height   ; minimum height
  filler.area)        ; filler area

(defun menu=dial_filler.create (window gcontext width height &key (separator :none))
;;  (format t "filler.create: width:~A  height:~A~%" width height)
  (let ((filler
	 (menu=dial_filler.make :window            window
				:gcontext          gcontext
				:separator         separator
				:filler.area       (dial-area_make :x 0   ; temporary values
								   :y 0
								   :w width
								   :h height)
				:filler.min.width  (max 1 width)
				:filler.min.height (max 1 height)))
	(window.area (dial-drawable_get.area window)))
;;    (menu=dial_filler.set.area filler (dial-area_make :x (dial-area_x window.area)
;;						      :y (dial-area_y window.area)
;;						      :w 0        ; compute minimum
;;						      :h 0))      ; compute minimum
    filler)
  )

(defun menu=dial_filler.get.area (filler &key (hor.offset 0) (vert.offset 0))
  (dial-drawable_get.area (menu=dial_filler.window filler)
			  :hor.offset hor.offset
			  :vert.offset vert.offset))

(defun menu=dial_filler.compute.width (filler)
  (let* ((filler.width (menu=dial_filler.filler.min.width filler))
	 (window (menu=dial_filler.window filler))
	 (window.border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH window)))
	 (window.outside.width (+ filler.width
				  window.border.offset*2)))
    window.outside.width))

(defun menu=dial_filler.compute.height (filler)
  (let* ((filler.height (menu=dial_filler.filler.min.height filler))
	 (window (menu=dial_filler.window filler))
	 (window.border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH window)))
	 (window.outside.height (+ filler.height
				   window.border.offset*2)))
    window.outside.height))

(defun menu=dial_filler.set.area (filler area &optional (redraw nil))
  (let* ((filler.width (menu=dial_filler.filler.min.width filler))
	 (filler.height (menu=dial_filler.filler.min.height filler))
	 (filler.area.w (dial-area_w area))
	 (filler.area.h (dial-area_h area))
	 (separator (menu=dial_filler.separator filler))
	 (window (menu=dial_filler.window filler))
	 (window.border.offset*2 (* 2 (XLIB:DRAWABLE-BORDER-WIDTH window)))
	 (window.outside.width (if (= filler.area.w 0)
				   (+ filler.width
				      window.border.offset*2)
				 filler.area.w))
	 (window.outside.height (if (= filler.area.h 0)
				    (+ filler.height
				       window.border.offset*2)
				  filler.area.h))
	 (window.inside.height (- window.outside.height window.border.offset*2))
	 (window.inside.width  (- window.outside.width  window.border.offset*2))
	 (filler.x (if (and (not (eq :hor separator)) (<= filler.width window.inside.width))
		       (floor (- window.inside.width filler.width) 2)
		     0))
	 (filler.y (if (and (not (eq :vert separator)) (<= filler.height window.inside.height))
		       (floor (- window.inside.height filler.height) 2)
		     0))
	 (window.area (dial-area_make :x (dial-area_x area)
				      :y (dial-area_y area)
				      :w window.outside.width
				      :h window.outside.height)))
    (setf (dial-area_x (menu=dial_filler.filler.area filler)) filler.x)
    (setf (dial-area_y (menu=dial_filler.filler.area filler)) filler.y)
    (cond ((eq :hor separator)
	   (setf (dial-area_w (menu=dial_filler.filler.area filler)) window.inside.width))
	  ((eq :vert separator)
	   (setf (dial-area_h (menu=dial_filler.filler.area filler)) window.inside.height)))
    (dial-drawable_set.area window window.area)
    (if redraw (menu=dial_filler.draw filler))))

(defun menu=dial_filler.draw (filler)
  (let* ((window (menu=dial_filler.window filler))
	 (separator (menu=dial_filler.separator filler))
	 (gcontext (menu=dial_filler.gcontext filler))
	 (filler.area (menu=dial_filler.filler.area filler))
	 (filler.x (dial-area_x filler.area))
	 (filler.y (dial-area_y filler.area))
	 (filler.w (dial-area_w filler.area))
	 (filler.h (dial-area_h filler.area)))
    (XLIB:CLEAR-AREA window)
    (cond ((eq :hor separator)
	   (let ((separator.y (+ filler.y (floor filler.h 2))))
	     (XLIB:DRAW-LINE window
			     gcontext
			     filler.x
			     separator.y
			     (1- (+ filler.x filler.w))
			     separator.y)))
	  ((eq :vert separator)
	   (let ((separator.x (+ filler.x (floor filler.w 2))))
	     (XLIB:DRAW-LINE window
			     gcontext
			     separator.x
			     filler.y
			     separator.x
			     (1- (+ filler.y filler.h))))))))

;;--------------------------------------------------------------------------------------------------------

(defun menu=help_extract.help.items (item-list)
  (cond ((not (null item-list))
	 (let* ((first-item (first item-list))
		(item-string (first first-item))
		(info-item (member :info first-item)))
	   (cond (info-item
		  (cons (list item-string
			      :value
			      (cons item-string (second info-item)))
			(menu=help_extract.help.items (rest item-list))))
		 (t (menu=help_extract.help.items (rest item-list)))))))
  )

(defun menu=help_create (menu)
  (let ((item-list (menu=help_extract.help.items (menu-get-item-list menu)))
	(title (menu-get-title menu)))
    (menu-create :multiple
		 item-list
		 :top-line (format nil "Help: ~A" title)
		 :help nil)
  ))

(defun menu=help_make.item.list (help-text)
  (cond ((not (null help-text))
	 (let ((first-items (first help-text)))
	   (labels ((make-item (item)
			       (list item :value item :selectable nil)))
		   (append (cons (list (first first-items)
				       :value
				       nil
				       :font menu*default.bold.font.name
				       :selectable nil
				       :centered t)
				 (mapcar #'make-item (rest first-items)))
			   (menu=help_make.item.list (rest help-text))))))))

(defun menu=help_do (help-menu)
  (let ((help.result (menu-choose help-menu :map-unmapped nil)))
    (cond (help.result
	   (let* ((help-items (menu=help_make.item.list help.result))
		  (menu (menu-create :single
				     help-items
				     :top-line (menu-get-title help-menu)
				     :help nil)))
	     (menu-choose menu :unmap-current t) ; unmapped menu becomes mapped automaticaly
	     (menu-dispose menu)))
	  (t (menu=dial_unmap nil))))) ; map unmapped menu

(defun menu=help_dispose (help-menu)
  (menu-dispose help-menu)
  )

(defun menu=dial_handle.item (menu
			      button
			      string &optional (item-type :value) (argument string) &key font info (exit t) &allow-other-keys)
;;(format t "menu=dial_handle.item( string:~S~% item-type~S~% argument:~S~% font:~S~% info:~S~% exit:~S~%)~%" string item-type argument font info exit)
  (cond ((eq :value item-type)
	 argument)
	((eq :no-select item-type)
	 nil)
	((eq :eval item-type)
	 (eval argument))
	((eq :funcall item-type)
	 (funcall argument))
	((eq :funcall-with-self item-type)
	 (funcall argument menu))
	((eq :menu item-type)
	 (let ((unmap-current (or exit
				  (not (dial-struct_type.eq menu menu.single.id)))))
	   (menu-choose (eval argument)
			:unmap-current unmap-current
			:map-unmapped (not unmap-current))))
	((eq :buttons item-type)
	 (cond ((not (numberp button))
		(let* ((button.menu (menu-create :single
						 argument
						 :help nil))
		       (button.menu.result (menu-choose button.menu
							:unmap-current t
							:map-unmapped nil))) ; always multiple menu
		  (menu-dispose button.menu)
		  button.menu.result))
	       (t
		(apply #'menu=dial_handle.item menu button (nth (1- button) argument)))))
	((eq :choice item-type)
	 'not-implemented)
	 ;;extensions
	((eq :help item-type)
	 (let ((help-menu (menu=help_create menu)))
	   (menu=help_do help-menu)
	   (menu=help_dispose help-menu))
	 nil)
	((eq :do-it item-type)
	 (cond ((dial-struct_type.eq menu menu.multiple.id)
		(menu=dial_handle.selected.items (menu=dial_multiple.data.items (dial-struct_data menu))))))
	((eq :abort item-type)
	 nil)
	(t (error "unknown item type: ~A" item-type))))

(defun menu=dial_handle.selected.items (items)
  (cond ((null items) nil)
	(t (let ((first.item (car items)))
	     (cond ((menu=dial_multiple.item.selected.p first.item)
		    (cons (apply #'menu=dial_handle.item
				(dial-struct_parent first.item)
				nil
				(menu=dial_single.item.data.item (dial-struct_data first.item)))
			  (menu=dial_handle.selected.items (cdr items))))
		   (t (menu=dial_handle.selected.items (cdr items))))))))
