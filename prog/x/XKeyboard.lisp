; see keysymdef.h for the follwing declarations

(defconstant ShiftMask     1)
(defconstant LockMask      2)
(defconstant ControlMask   4)
(defconstant Mod1Mask      8)

(defconstant XK_BackSpace  #xFF08)         ; controls
(defconstant XK_Delete     #xFFFF)
(defconstant XK_Left       #xFF51)
(defconstant XK_Right      #xFF53)

(defconstant XK_Return     #xFF0D)         ; end of input
(defconstant XK_Linefeed   #xFF0A)
(defconstant XK_KP_Enter   #xFF8D)

(defconstant XK_space      #x0020)         ; printable characters
(defconstant XK_asciitilde #x007e)
(defconstant XK_KP_Space   #xFF80)
(defconstant XK_KP_9       #xFFB9)

(defconstant XK_A          #x0041)
(defconstant XK_B          #x0042)
(defconstant XK_F          #x0046)
(defconstant XK_H          #x0048)
(defconstant XK_M          #x004D)
(defconstant XK_Z          #x005A)
(defconstant |XK_a|        #x0061)
(defconstant |XK_b|        #x0062)
(defconstant |XK_f|        #x0066)
(defconstant |XK_z|        #x007A)



(defun XK-is_end.of.input (keysym)
  (or (= keysym XK_Return)
      (= keysym XK_Linefeed)
      (= keysym XK_KP_Enter)))

(defun XK-is_printable (keysym &optional (state 0))
  (and (or (= state 0)
	   (> (logand ShiftMask state) 0)
	   (> (logand LockMask state) 0))
       (or (and (>= keysym XK_Space)
		(<= keysym XK_asciitilde))
	   (and (>= keysym XK_KP_Space)
		(<= keysym XK_KP_9)))))

(defun XK-is_left (keysym &optional (state 0))
  (or (= keysym XK_Left)
      (and (> (logand ControlMask state) 0)
	   (= keysym |XK_b|))))

(defun XK-is_right (keysym &optional (state 0))
  (or (= keysym XK_Right)
      (and (> (logand ControlMask state) 0)
	   (= keysym |XK_f|))))

(defun XK-state_is.control (state)
  (> (logand ControlMask state) 0))

(defun XK-keysym_index (display code state)
  (let ((keysym (XLIB:KEYCODE->KEYSYM display code 0)))
    (cond ((> (logand ShiftMask state) 0)
	   1)
	  ((> (logand LockMask state) 0)
	   (cond ((and (>= keysym |XK_a|)
		       (<= keysym |XK_z|))
		  1)
		 (t 0)))
	  (t 0))))





