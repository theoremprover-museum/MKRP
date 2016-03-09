;;; -*- Package: MKRP; Base:10.; Syntax: Common-lisp; Mode: LISP -*-

(Thief(BILLIE) or thief(Lucky) or thief(Jacky))
(not ((thief(Billie) and thief(Lucky)) or (thief(Billie) and thief(Jacky)) or (thief(Lucky) and thief(Jacky))))
(all x thief(x) impl hotel(x))
(not (not thief(Lucky) and not hotel(Lucky) and thief(Billie)))
(not (not thief(Billie) and ( thief(Lucky) and hotel(Lucky) and not thief(Billie)) and not thief(Jacky)))
(not (not thief(Jacky) and hotel(Lucky) and not( thief(Lucky) and hotel(Lucky) and not thief(Billie))))

(thief(Jacky))