;;---------------------------------------------------------------------------------------------
; test module:  
;;---------------------------------------------------------------------------------------------



(defun test-menu.create.1 ()
  (menu-create :single '(("Hello,") ("i'm") ("your submenu")) :top-line "submenu"))

(defun test-menu.create.1a ()
  (menu-create :single '(("Hello,") ("i'm") ("your submenu"))
               :momentary nil
	       :top-line "Single Menu"
	       :help nil))

(defun test-menu.create.2 ()
  (menu-create :single '(("  3  ") ("  4  ")  ("Helpstring")) :top-line "m2"))

(defun test-function (&optional args)
  (format t "test-function ~A~%" args))

(defun test-menu.create.3 ()
  (menu-create :single
	       (list (list "This is a value item"
			   :value "Value of value item"
			   :font  "9x15bold"
			   :info '("Returns a string"))
		     (list "This is an eval item"
			   :eval '(+ 2 (* 2 3))
			   :info '("Returns the value" "for 2 + 2 * 3"))
		     (list "This is a funcall item"
			   :funcall #'test-function
			   :info '("Calls test-function" "with no arguments"))
		     (list "This is a funcall-with-self-item"
			   :funcall-with-self #'test-function
			   :info '("Calls test-function" "with the menu as argument"))
		     (list "This is a menu item"
			   :menu (test-menu.create.4)
			   :info '("Displays a submenu"))
		     (list "This is a buttons item"
			   :buttons '(("Left") ("Middle") ("Right"))
			   :font "9x15bold"
			   :info '("Distinguishes between" "serveral buttons")))
	       :top-line "Choose one item:"
	       :momentary nil
	       :default-item 0
	       ))

(defun test-menu.create.3a ()
  (menu-create :single
	       (list (list "This is a value item"
			   :value "Value of value item"
			   :font  "9x15bold"
			   :info '("Returns a string"))
		     (list "This is an eval item"
			   :eval '(+ 2 (* 2 3))
			   :info '("Returns the value" "for 2 + 2 * 3"))
		     (list "This is a funcall item"
			   :funcall #'test-function
			   :info '("Calls test-function" "with no arguments"))
		     (list "This is a funcall-with-self-item"
			   :funcall-with-self #'test-function
			   :info '("Calls test-function" "with the menu as argument"))
		     (list "This is a menu item"
			   :menu (test-menu.create.4a)
			   :exit nil
			   :info '("Displays a submenu"))
		     (list "This is a buttons item"
			   :buttons '(("Left") ("Middle") ("Right"))
			   :font "9x15bold"
			   :info '("Distinguishes between" "serveral buttons")))
	       :top-line "Choose one item:"
	       :momentary nil
	       :default-item 0
	       ))

(defun test-menu.create.4 ()
  (menu-create :multiple
	       (list (list "This is a value item"
			   :value "Value of value item"
			   :font  "9x15bold"
			   :info '("Returns a string"))
		     (list "This is an eval item"
			   :eval '(+ 2 (* 2 3))
			   :info '("Returns the value" "for 2 + 2 * 3"))
		     (list "This is a funcall item"
			   :funcall #'test-function
			   :info '("Calls test-function" "with no arguments"))
		     (list "This is a funcall-with-self-item"
			   :funcall-with-self #'test-function
			   :font  "9x15bold"
			   :info '("Calls test-function" "with the menu as argument"))
		     (list "This is a menu item"
			   :menu (test-menu.create.1)
			   :info '("Displays a submenu"))
		     (list "This is a buttons item"
			   :buttons '(("Left") ("Middle") ("Right"))
			   :info '("Distinguishes between" "serveral buttons")))
	       :special-choices (list (list "Button1" :menu (test-menu.create.1))
				     )
	       :top-line "Choose multiple items:"
	       :momentary nil
	       :default-item '(0 2 4)
	       ))

(defun test-menu.create.4a ()
  (menu-create :multiple
	       (list (list "This is a value item"
			   :value "Value of value item"
			   :font  "9x15bold"
			   :info '("Returns a string"))
		     (list "This is an eval item"
			   :eval '(+ 2 (* 2 3))
			   :info '("Returns the value" "for 2 + 2 * 3"))
		     (list "This is a funcall item"
			   :funcall #'test-function
			   :info '("Calls test-function" "with no arguments"))
		     (list "This is a funcall-with-self-item"
			   :funcall-with-self #'test-function
			   :font  "9x15bold"
			   :info '("Calls test-function" "with the menu as argument"))
		     (list "This is a menu item"
			   :menu (test-menu.create.1)
			   :exit nil
			   :info '("Displays a submenu"))
		     (list "This is a buttons item"
			   :buttons '(("Left") ("Middle") ("Right"))
			   :info '("Distinguishes between" "serveral buttons")))
	       :special-choices (list (list "Button1" :menu (test-menu.create.1))
				     )
	       :top-line "Choose multiple items:"
	       :momentary nil
	       :default-item '(0 2 4)
	       ))

(defun test-menu.create.4b ()
  (menu-create :multiple
	       nil
	       :special-choices (list (list "Button1" :menu (test-menu.create.1))
				     )
	       :top-line "Choose multiple items:"
	       :momentary nil
	       :default-item '(0 2 4)
	       ))

(setq fish '(salmon)
      crustaceans '(clams)
      lettuce 'boston
      apples '(pippin)
      coupon-value 0
      price 0
      reset 'reset)

(defun test-choose.create.1 ()
  (menu=dial_choose.create nil
			   "Todays's Food Selections"
			   '("FISH STORE"
			     (apples "Apples"
				     ("Info for Apples" "Line2" "Line3" (format nil "*~A*" "Last Line"))
				     (:finite-multiple macintosh jonathan pippin)
				     (lambda (old new) (format t "old: ~A~%new: ~A~%" old new))
				     )
			     (lettuce nil ("Info for Lettuce")
				      (:finite-single boston ("Red Iceberg" red-ice)))
			     (reset nil ("Resets the variables")
				    (:finite-single reset)
				    (lambda (old new)
				      (setq fish '(salmon)
					    crustaceans '(clams)
					    lettuce 'boston
					    apples '(pippin)
					    coupon-value 0
					    price 0)
				      t))
			     (coupon-value "Coupons" nil
					   (:infinite numberp ("type in a number")))
			     )
			   '(("XqgytpA" (format t "abort~%")) ("help" (format t "help~%")) "Exit Button")
			   ))

(defun test-choose ()
  (choose-variable-values
			   "Todays's Food Selections"
			   '("FISH STORE"
			     (apples "Apples"
				     ("Info for Apples" "Line2" "Line3" (format nil "*~A*" "Last Line"))
				     (:finite-multiple macintosh jonathan pippin)
				     (lambda (old new) (format t "old: ~A~%new: ~A~%" old new))
				     )
			     (lettuce nil ("Info for Lettuce")
				      (:finite-single boston ("Red Iceberg" red-ice)))
			     (reset nil ("Resets the variables")
				    (:finite-single reset)
				    (lambda (old new)
				      (setq fish '(salmon)
					    crustaceans '(clams)
					    lettuce 'boston
					    apples '(pippin)
					    coupon-value 0
					    price 0)
				      t))
			     (coupon-value "Coupons" nil
					   (:infinite numberp ("type in a number")))
			     )
			   '(("XqgytpA" (format t "abort~%")) ("help" (format t "help~%")) "Exit Button")
			   ))

(defun test-choose.create.2 ()
  (menu=dial_choose.create nil
			   "Todays's Food Selections"
			   '("FISH STORE"
			     (apples "Apples"
				     ("Info for Apples" "Line2" "Line3" (format nil "*~A*" "Last Line"))
				     (:finite-multiple macintosh jonathan pippin)
				     (lambda (old new) (format t "old: ~A~%new: ~A~%" old new))
				     )
			     (lettuce nil ("Info for Lettuce")
				      (:finite-single boston ("Red Iceberg" red-ice)))
			     (coupon-value "Coupons" nil
					   (:infinite numberp ("type in a number"))
					   nil
					   :min-width 5
					   :max-width 10)
			     )
			   '(("XqgytpA" (format t "abort~%")) ("help" (format t "help~%")) "Exit Button")
			   ))

(defun test-choose.create.3 ()
  (menu=dial_choose.create nil
			   "Todays's Food Selections"
			   '(
			     "Test"
;			     (coupon-value "Coupons" nil
;					   (:infinite numberp ("type in a number")))
			     )
			   '(("XqgytpA" (format t "abort~%")) ("help" (format t "help~%")) "Exit Button")
			   ))


(defun test-create.button.window ()
  (XLIB:CREATE-WINDOW
			 :parent	      menu*root.window
			 :class		      :input-output
			 :x		      100			; temporary value
			 :y		      100			; temporary value
			 :width		      16			; temporary value
			 :height	      16			; temporary value
			 :border-width	      1
			 :border	      menu*fg.pixel
			 :save-under 	      :on
			 :override-redirect   :on))

(defun test-create.button ()
  (menu=dial_text.button.create (test-create.button.window) "Michael" menu*gcontext))

(defun test-dispose.button (button)
  (XLIB:DESTROY-WINDOW (menu=dial_text.button.window button)))

(defun test-map.button (button)
  (XLIB:MAP-WINDOW (menu=dial_text.button.window button))
  (menu=dial_text.button.draw button)
  (XLIB:DISPLAY-FORCE-OUTPUT dial*display))

(defun test-unmap.button (button)
  (XLIB:UNMAP-WINDOW (menu=dial_text.button.window button)))

(defun test-create.check.box.window ()
  (XLIB:CREATE-WINDOW
			 :parent	      menu*root.window
			 :class		      :input-output
			 :x		      100
			 :y		      100
			 :width		      16		        ; temporary value
			 :height	      16  			; temporary value
			 :border-width	      1
			 :border	      menu*fg.pixel
			 :background          menu*bg.pixel
			 :save-under 	      :on
			 :override-redirect   :on))

(defun test-create.check.box ()
  (let ((check.box (menu=dial_check.box.create (test-create.check.box.window)
					       menu*gcontext
					       :selectable t)))
    (menu=dial_check.box.set.area check.box (dial-area_make :x 100
							    :y 100
							    :w 100
							    :h 50))
    check.box))

(defun test-dispose.check.box (check.box)
  (XLIB:DESTROY-WINDOW (menu=dial_check.box.window check.box)))

(defun test-map.check.box (check.box)
  (XLIB:MAP-WINDOW (menu=dial_check.box.window check.box))
  (menu=dial_check.box.draw check.box)
  (XLIB:DISPLAY-FORCE-OUTPUT dial*display))

(defun test-unmap.check.box (check.box)
  (XLIB:UNMAP-WINDOW (menu=dial_check.box.window check.box)))

(defun test-event.handler (&rest event-slots &key &allow-other-keys)
  (format t "test-event.handler: ~S~%" event-slots)
  nil)

(defun test-process.event ()
  (XLIB:PROCESS-EVENT dial*display :handler #'test-event.handler :timeout 0))

(defun test-process.events ()
  (do ((result (test-process.event) (test-process.event)))
      ((null result))))

(defun test-test ()
  (dial-init "js-sfbslc12")
  (menu-choose (test-menu.create.1)))
