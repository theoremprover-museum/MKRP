;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION 'NIL 'NIL '(((ALL U,V
    (((NOT ((ALL W,X
             ((((MODEL (A W)) OR
                (NOT ((ALL Y,Z
                       (((MODEL (A Y)) AND (R (STAR (W) Y Z))) IMPL
                        (NOT (NOT (MODEL (A STAR (STAR (Z)))))))))))
               AND (R (STAR (U) W X)))
              IMPL
              (NOT (((NOT ((ALL X0719,X0720
                            ((((MODEL (B X0719)) OR
                               (NOT ((ALL X0721,X0722
                                      (((MODEL (B X0721)) AND (R (STAR (X0719) X0721 X0722)))
                                       IMPL (NOT (NOT (MODEL (B STAR (STAR (X0722)))))))))))
                              AND (R (STAR (STAR (X)) X0719 X0720)))
                             IMPL
                             (NOT (((MODEL (C STAR (X0720))) OR
                                    (NOT (MODEL (C STAR (STAR (X0720))))))))))))
                     OR
                     (NOT (NOT ((ALL X0723,X0724
                                 ((((MODEL (B X0723)) OR
                                    (NOT ((ALL X0725,X0726
                                           (((MODEL (B X0725)) AND
                                             (R (STAR (X0723) X0725 X0726)))
                                            IMPL (NOT (NOT (MODEL (B STAR (STAR (X0726)))))))))
                                         ))
                                   AND (R (STAR (STAR (STAR (X))) X0723 X0724)))
                                  IMPL
                                  (NOT (((MODEL (C STAR (X0724))) OR
                                         (NOT (MODEL (C STAR (STAR (X0724))))))))))))))))))))
      AND (R (0 U V)))
     IMPL
     (NOT ((ALL X0727,X0728
            ((((NOT ((ALL X0729,X0730
                      ((((MODEL (A X0729)) OR
                         (NOT ((ALL X0731,X0732
                                (((MODEL (A X0731)) AND (R (STAR (X0729) X0731 X0732))) IMPL
                                 (NOT (NOT (MODEL (A STAR (STAR (X0732)))))))))))
                        AND (R (STAR (X0727) X0729 X0730)))
                       IMPL
                       (NOT (((MODEL (B STAR (X0730))) OR (NOT (MODEL (B STAR (STAR (X0730)))))
                              )))))))
               OR
               (NOT ((ALL X0733,X0734
                      (((NOT ((ALL X0735,X0736
                               ((((MODEL (A X0735)) OR
                                  (NOT ((ALL X0737,X0738
                                         (((MODEL (A X0737)) AND (R (STAR (X0735) X0737 X0738))
                                           )
                                          IMPL (NOT (NOT (MODEL (A STAR (STAR (X0738)))))))))))
                                 AND (R (STAR (X0733) X0735 X0736)))
                                IMPL
                                (NOT (((MODEL (B STAR (X0736))) OR
                                       (NOT (MODEL (B STAR (STAR (X0736))))))))))))
                        AND (R (STAR (X0727) X0733 X0734)))
                       IMPL
                       (NOT (NOT (NOT ((ALL X0739,X0740
                                        ((((MODEL (A X0739)) OR
                                           (NOT ((ALL X0741,X0742
                                                  (((MODEL (A X0741)) AND
                                                    (R (STAR (X0739) X0741 X0742)))
                                                   IMPL
                                                   (NOT (NOT (MODEL (A STAR (STAR (X0742)))))))
                                                  ))))
                                          AND (R (STAR (STAR (STAR (X0734))) X0739 X0740)))
                                         IMPL
                                         (NOT (((MODEL (B STAR (X0740))) OR
                                                (NOT (MODEL (B STAR (STAR (X0740)))))))))))))))
                      ))))
              AND (R (STAR (V) X0727 X0728)))
             IMPL (NOT (((MODEL (C STAR (X0728))) OR (NOT (MODEL (C STAR (STAR (X0728))))))))))
           ))))
   AND
   (ALL X0743,X0744
    (((NOT ((ALL X0745,X0746
             ((((NOT ((ALL X0747,X0748
                       ((((MODEL (A X0747)) OR
                          (NOT ((ALL X0749,X0750
                                 (((MODEL (A X0749)) AND (R (STAR (X0747) X0749 X0750))) IMPL
                                  (NOT (NOT (MODEL (A STAR (STAR (X0750)))))))))))
                         AND (R (STAR (X0745) X0747 X0748)))
                        IMPL
                        (NOT (((MODEL (B STAR (X0748))) OR
                               (NOT (MODEL (B STAR (STAR (X0748))))))))))))
                OR
                (NOT ((ALL X0751,X0752
                       (((NOT ((ALL X0753,X0754
                                ((((MODEL (A X0753)) OR
                                   (NOT ((ALL X0755,X0756
                                          (((MODEL (A X0755)) AND
                                            (R (STAR (X0753) X0755 X0756)))
                                           IMPL (NOT (NOT (MODEL (A STAR (STAR (X0756))))))))))
                                   )
                                  AND (R (STAR (X0751) X0753 X0754)))
                                 IMPL
                                 (NOT (((MODEL (B STAR (X0754))) OR
                                        (NOT (MODEL (B STAR (STAR (X0754))))))))))))
                         AND (R (STAR (X0745) X0751 X0752)))
                        IMPL
                        (NOT (NOT (NOT ((ALL X0757,X0758
                                         ((((MODEL (A X0757)) OR
                                            (NOT ((ALL X0759,X0760
                                                   (((MODEL (A X0759)) AND
                                                     (R (STAR (X0757) X0759 X0760)))
                                                    IMPL
                                                    (NOT (NOT (MODEL (A STAR (STAR (X0760))))))
                                                    )))))
                                           AND (R (STAR (STAR (STAR (X0752))) X0757 X0758)))
                                          IMPL
                                          (NOT (((MODEL (B STAR (X0758))) OR
                                                 (NOT (MODEL (B STAR (STAR (X0758))))))))))))))
                        )))))
               AND (R (STAR (X0743) X0745 X0746)))
              IMPL (NOT (((MODEL (C STAR (X0746))) OR (NOT (MODEL (C STAR (STAR (X0746)))))))))
             )))
      AND (R (0 X0743 X0744)))
     IMPL
     (NOT ((ALL X0761,X0762
            ((((MODEL (A X0761)) OR
               (NOT ((ALL X0763,X0764
                      (((MODEL (A X0763)) AND (R (STAR (X0761) X0763 X0764))) IMPL
                       (NOT (NOT (MODEL (A STAR (STAR (X0764)))))))))))
              AND (R (STAR (X0744) X0761 X0762)))
             IMPL
             (NOT (((NOT ((ALL X0765,X0766
                           ((((MODEL (B X0765)) OR
                              (NOT ((ALL X0767,X0768
                                     (((MODEL (B X0767)) AND (R (STAR (X0765) X0767 X0768)))
                                      IMPL (NOT (NOT (MODEL (B STAR (STAR (X0768)))))))))))
                             AND (R (STAR (STAR (X0762)) X0765 X0766)))
                            IMPL
                            (NOT (((MODEL (C STAR (X0766))) OR
                                   (NOT (MODEL (C STAR (STAR (X0766))))))))))))
                    OR
                    (NOT (NOT ((ALL X0769,X0770
                                ((((MODEL (B X0769)) OR
                                   (NOT ((ALL X0771,X0772
                                          (((MODEL (B X0771)) AND
                                            (R (STAR (X0769) X0771 X0772)))
                                           IMPL (NOT (NOT (MODEL (B STAR (STAR (X0772))))))))))
                                   )
                                  AND (R (STAR (STAR (STAR (X0762))) X0769 X0770)))
                                 IMPL
                                 (NOT (((MODEL (C STAR (X0770))) OR
                                        (NOT (MODEL (C STAR (STAR (X0770)))))))))))))))))))))))
  ) '((AND (ALL 3
        (ALL 2
         (IMPL
          (AND (NOT (ALL 5
                     (ALL 4
                      (IMPL
                       (AND (OR (+ 7 (6 4) NIL)
                                (NOT (ALL 9
                                      (ALL 8
                                       (IMPL
                                        (AND (+ 7 (6 8) NIL)
                                             (+ 11 ((10 4) 8 9) NIL))
                                        (NOT (NOT (+ 7 (6 (10 (10 9))) NIL))))))))
                            (+ 11 ((10 2) 4 5) NIL))
                       (NOT (OR (NOT (ALL 13
                                      (ALL 12
                                       (IMPL
                                        (AND (OR (+ 7 (14 12) NIL)
                                                 (NOT (ALL 16
                                                       (ALL 15
                                                        (IMPL
                                                         (AND
                                                          (+ 7 (14 15) NIL)
                                                          (+ 11 ((10 12) 15 16) NIL))
                                                         (NOT
                                                          (NOT (+ 7 (14 (10 (10 16))) NIL))))))
                                                      ))
                                             (+ 11 ((10 (10 5)) 12 13) NIL))
                                        (NOT (OR (+ 7 (17 (10 13)) NIL)
                                                 (NOT (+ 7 (17 (10 (10 13))) NIL))))))))
                                (NOT (NOT (ALL 19
                                           (ALL 18
                                            (IMPL
                                             (AND (OR (+ 7 (14 18) NIL)
                                                      (NOT (ALL 21
                                                            (ALL 20
                                                             (IMPL
                                                              (AND
                                                               (+ 7 (14 20) NIL)
                                                               (+ 11 ((10 18) 20 21) NIL))
                                                              (NOT
                                                               (NOT
                                                                (+ 7 (14 (10 (10 21))) NIL)))))
                                                            )))
                                                  (+ 11 ((10 (10 (10 5))) 18 19) NIL))
                                             (NOT (OR (+ 7 (17 (10 19)) NIL)
                                                      (NOT (+ 7 (17 (10 (10 19))) NIL))))))))))
                            )))))
               (+ 11 (22 2 3) NIL))
          (NOT (ALL 24
                (ALL 23
                 (IMPL
                  (AND (OR (NOT (ALL 26
                                 (ALL 25
                                  (IMPL
                                   (AND (OR (+ 7 (6 25) NIL)
                                            (NOT (ALL 28
                                                  (ALL 27
                                                   (IMPL
                                                    (AND (+ 7 (6 27) NIL)
                                                         (+ 11 ((10 25) 27 28) NIL))
                                                    (NOT (NOT (+ 7 (6 (10 (10 28))) NIL))))))))
                                        (+ 11 ((10 23) 25 26) NIL))
                                   (NOT (OR (+ 7 (14 (10 26)) NIL)
                                            (NOT (+ 7 (14 (10 (10 26))) NIL))))))))
                           (NOT (ALL 30
                                 (ALL 29
                                  (IMPL
                                   (AND (NOT (ALL 32
                                              (ALL 31
                                               (IMPL
                                                (AND (OR (+ 7 (6 31) NIL)
                                                         (NOT
                                                          (ALL 34
                                                           (ALL 33
                                                            (IMPL
                                                             (AND
                                                              (+ 7 (6 33) NIL)
                                                              (+ 11 ((10 31) 33 34) NIL))
                                                             (NOT
                                                              (NOT (+ 7 (6 (10 (10 34))) NIL)))
                                                             )))))
                                                     (+ 11 ((10 29) 31 32) NIL))
                                                (NOT (OR (+ 7 (14 (10 32)) NIL)
                                                         (NOT (+ 7 (14 (10 (10 32))) NIL)))))))
                                             )
                                        (+ 11 ((10 23) 29 30) NIL))
                                   (NOT (NOT (NOT (ALL 36
                                                   (ALL 35
                                                    (IMPL
                                                     (AND (OR
                                                           (+ 7 (6 35) NIL)
                                                           (NOT
                                                            (ALL 38
                                                             (ALL 37
                                                              (IMPL
                                                               (AND
                                                                (+ 7 (6 37) NIL)
                                                                (+ 11 ((10 35) 37 38) NIL))
                                                               (NOT
                                                                (NOT
                                                                 (+ 7 (6 (10 (10 38))) NIL)))))
                                                             )))
                                                          (+ 11 ((10 (10 (10 30))) 35 36) NIL))
                                                     (NOT (OR
                                                           (+ 7 (14 (10 36)) NIL)
                                                           (NOT (+ 7 (14 (10 (10 36))) NIL)))))
                                                    ))))))))))
                       (+ 11 ((10 3) 23 24) NIL))
                  (NOT (OR (+ 7 (17 (10 24)) NIL)
                           (NOT (+ 7 (17 (10 (10 24))) NIL)))))))))))
       (ALL 40
        (ALL 39
         (IMPL
          (AND (NOT (ALL 42
                     (ALL 41
                      (IMPL
                       (AND (OR (NOT (ALL 44
                                      (ALL 43
                                       (IMPL
                                        (AND (OR (+ 7 (6 43) NIL)
                                                 (NOT (ALL 46
                                                       (ALL 45
                                                        (IMPL
                                                         (AND
                                                          (+ 7 (6 45) NIL)
                                                          (+ 11 ((10 43) 45 46) NIL))
                                                         (NOT (NOT (+ 7 (6 (10 (10 46))) NIL)))
                                                         )))))
                                             (+ 11 ((10 41) 43 44) NIL))
                                        (NOT (OR (+ 7 (14 (10 44)) NIL)
                                                 (NOT (+ 7 (14 (10 (10 44))) NIL))))))))
                                (NOT (ALL 48
                                      (ALL 47
                                       (IMPL
                                        (AND (NOT (ALL 50
                                                   (ALL 49
                                                    (IMPL
                                                     (AND (OR
                                                           (+ 7 (6 49) NIL)
                                                           (NOT
                                                            (ALL 52
                                                             (ALL 51
                                                              (IMPL
                                                               (AND
                                                                (+ 7 (6 51) NIL)
                                                                (+ 11 ((10 49) 51 52) NIL))
                                                               (NOT
                                                                (NOT
                                                                 (+ 7 (6 (10 (10 52))) NIL)))))
                                                             )))
                                                          (+ 11 ((10 47) 49 50) NIL))
                                                     (NOT (OR
                                                           (+ 7 (14 (10 50)) NIL)
                                                           (NOT (+ 7 (14 (10 (10 50))) NIL)))))
                                                    )))
                                             (+ 11 ((10 41) 47 48) NIL))
                                        (NOT (NOT (NOT (ALL 54
                                                        (ALL 53
                                                         (IMPL
                                                          (AND
                                                           (OR
                                                            (+ 7 (6 53) NIL)
                                                            (NOT
                                                             (ALL 56
                                                              (ALL 55
                                                               (IMPL
                                                                (AND
                                                                 (+ 7 (6 55) NIL)
                                                                 (+ 11 ((10 53) 55 56) NIL))
                                                                (NOT
                                                                 (NOT
                                                                  (+ 7 (6 (10 (10 56))) NIL))))
                                                               ))))
                                                           (+ 11 ((10 (10 (10 48))) 53 54) NIL)
                                                           )
                                                          (NOT
                                                           (OR
                                                            (+ 7 (14 (10 54)) NIL)
                                                            (NOT (+ 7 (14 (10 (10 54))) NIL))))
                                                          )))))))))))
                            (+ 11 ((10 39) 41 42) NIL))
                       (NOT (OR (+ 7 (17 (10 42)) NIL)
                                (NOT (+ 7 (17 (10 (10 42))) NIL))))))))
               (+ 11 (22 39 40) NIL))
          (NOT (ALL 58
                (ALL 57
                 (IMPL
                  (AND (OR (+ 7 (6 57) NIL)
                           (NOT (ALL 60
                                 (ALL 59
                                  (IMPL
                                   (AND (+ 7 (6 59) NIL)
                                        (+ 11 ((10 57) 59 60) NIL))
                                   (NOT (NOT (+ 7 (6 (10 (10 60))) NIL))))))))
                       (+ 11 ((10 40) 57 58) NIL))
                  (NOT (OR (NOT (ALL 62
                                 (ALL 61
                                  (IMPL
                                   (AND (OR (+ 7 (14 61) NIL)
                                            (NOT (ALL 64
                                                  (ALL 63
                                                   (IMPL
                                                    (AND (+ 7 (14 63) NIL)
                                                         (+ 11 ((10 61) 63 64) NIL))
                                                    (NOT (NOT (+ 7 (14 (10 (10 64))) NIL)))))))
                                            )
                                        (+ 11 ((10 (10 58)) 61 62) NIL))
                                   (NOT (OR (+ 7 (17 (10 62)) NIL)
                                            (NOT (+ 7 (17 (10 (10 62))) NIL))))))))
                           (NOT (NOT (ALL 66
                                      (ALL 65
                                       (IMPL
                                        (AND (OR (+ 7 (14 65) NIL)
                                                 (NOT (ALL 68
                                                       (ALL 67
                                                        (IMPL
                                                         (AND
                                                          (+ 7 (14 67) NIL)
                                                          (+ 11 ((10 65) 67 68) NIL))
                                                         (NOT
                                                          (NOT (+ 7 (14 (10 (10 68))) NIL))))))
                                                      ))
                                             (+ 11 ((10 (10 (10 58))) 65 66) NIL))
                                        (NOT (OR (+ 7 (17 (10 66)) NIL)
                                                 (NOT (+ 7 (17 (10 (10 66))) NIL)))))))))))))))
          ))))) '("                                                                                                                     "
  "*********************************************************************************************************************"
  " EDIT:     Axioms and Theorems edited: 25-JUL,1989 14:08 "
  "*********************************************************************************************************************"
  ) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(89610 89615 89620 89625 89630 89635 89640 89645 89650 89655 89660 89665 89670 89675
          89680 89685 89690 89695 89700 89705 89710 89715 89720 89725 89730 89735 89740 89745
          89750 89755 89760 89765 89770 89775 89780 89785 89790 89795 89800 89805 89810 89815
          89820 89825 89830 89835 89840 89845 89850 89855 89860 89865 89870 89875 89880 89885
          89890 89905 89919 89924 89929 89944 89949 89954 89959 89964 89969 89984 89999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY "A" (DT*ST-KIND NIL) CONSTANT 15 "MODEL" NIL NIL (ANY ANY) NIL NIL
          ((- 7 (POSITIVE))) ((+ 7 (NEGATIVE))) ((+ 7 (NIL))) ((- 7 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 14 "STAR" NIL ANY (ANY) NIL NIL NIL
          NIL NIL 1 ANY (DT*ST-KIND NIL) FUNCTION 15 "R" NIL NIL (ANY ANY ANY) NIL NIL
          ((- 11 (POSITIVE))) ((+ 11 (NEGATIVE))) ((+ 11 (NIL))) ((- 11 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "B" (DT*ST-KIND NIL) CONSTANT 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "C" (DT*ST-KIND NIL) CONSTANT 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "0"
          (DT*ST-KIND NIL) CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY
          NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE))
       (INCREMENT (- MEM*SIZE 90000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 90000)
             (SETQ COUNTER1 69)
             (MAPC #'(LAMBDA (RADDR)
                       (SETF (AREF MEM*MEMORY (DECF COUNTER1))
                              (COND ((EQL RADDR 'ATP.MEMORY.NIL) RADDR)
                                    ((EQL 'END RADDR) RADDR)
                                    ((OR (EQL RADDR 0)
                                         (MINUSP RADDR))
                                     RADDR)
                                    (T (+ RADDR INCREMENT)))))
                   ADDRLIST)
             (SETQ COUNTER1 MEM*SIZE)
             (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            (T (UNLESS (= MEM*SIZE 90000)
                 (MEM-INITIALIZE 90000))
               (SETQ COUNTER1 69)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 90000) (+ 89605 INCREMENT) 89605))
      (SETQ MEM*NEXT.VADR 69
            MEM*REST (- MEM*NEXT.RADR MEM*NEXT.VADR -1)
            MEM*FIRST.REUSABLE.VADR NIL
            MEM*LAST.REUSABLE.VADR NIL))

(PROGN (SETQ DT*SORT.ALL '(ANY))
       (SETQ DT*SORT.NR '2)
       (SETQ DT*SORT.PROPERTIES
              '(DT*MAX.SUBSORTS DT*LEAST.SUPERSORTS DT*TRANSITIVE.CLOSURE
                DT*INVERSE.TRANSITIVE.CLOSURE DT*MINIMAL.SUBSORTS DT*DIRECT.SUBSORTS
                DT*DIRECT.SUPERSORTS DT*DISJOINT.SORTS DT*COMPLETION.SORT))
       (SETQ DT*SORT.COMMON.COMPUTE.FLAG 'NIL)
       (SETQ DT*VARIABLE.COUNTER '0)
       (SETQ DT*CONSTANT.COUNTER '0)
       (SETQ DT*CONSTANT.ALL '(22 17 14 6))
       (SETQ DT*ABBREVIATIONS 'NIL)
       (SETQ DT*FUNCTION.COUNTER '0)
       (SETQ DT*FUNCTION.ALL '(10))
       (SETQ DT*FUNCTION.ADMISSIBLE.THEORIES '(ASSOCIATIVE))
       (SETQ DT*FUNCTION.ACTUAL.THEORIES 'NIL)
       (SETQ DT*FUNCTION.WITH.ARGUMENT.SYMMETRIES 'NIL)
       (SETQ DT*FUNCTION.COMPONENTS
              '(PNAME ATTRIBUTES MAX.RANGE.SORT MAX.DOMAIN.SORTS MIN.RANGE.SORTS SORT.LIST
                SORT.LIST.INVERSE SORT.TREE.CALC ARGUMENT.SYMMETRIES ARITY G.L.B.OF.RANGES))
       (SETQ DT*PREDICATE.ADMISSABLE.ATTRIBUTES '(SYMMETRIC DEFINED REFLEXIVE))
       (SETQ DT*PREDICATE.COUNTER '0)
       (SETQ DT*EQUALITY.SYMBOLS '("=" ":=" "=:" ":=:"))
       (SETQ DT*EQUALITY.PREDICATES 'NIL)
       (SETQ DT*NONEQUALITY.PREDICATES '(11 7 1 0))
       (SETQ DT*PREDICATE.ALL '(11 7 1 0))
       (SETQ DT*PREDICATE.WITH.ATTRIBUTES 'NIL)
       (SETQ DT*PREDICATE.COMPONENTS
              '(PNAME POSITIVE.OCCURRENCES NEGATIVE.OCCURRENCES DOMAINSORTS ATTRIBUTES
                REFL.CLAUSE +ROTHERSIDES -ROTHERSIDES +SOTHERSIDES -SOTHERSIDES +TOTHERSIDES
                -TOTHERSIDES))
       (SETQ DT*TRUE.PREDICATE '0)
       (SETQ DT*FALSE.PREDICATE '1)
       (SETQ DT*UNI.CREATES.VARIABLES 'NIL)
       (SETQ DT*SIGN.MINUS.SYMBOLS '(- --))
       (SETQ DT*SIGN.PLUS.SYMBOLS '(+ ++))
       (SETQ DT*SYMBOL.KINDS '(CONSTANT.ALL FUNCTION.ALL PREDICATE.ALL))) 
(PROGN) 
(PROGN (PROGN)) 
(PROGN (PROGN (SETF (GET 'ANY 'DT*SORT.NUMBER) '1)
              (SETF (GET 'ANY 'DT*DIRECT.SUBSORTS) 'NIL)
              (SETF (GET 'ANY 'DT*TRANSITIVE.CLOSURE) '(ANY))
              (SETF (GET 'ANY 'DT*MAX.SUBSORTS) '((ANY ANY)))
              (SETF (GET 'ANY 'DT*MINIMAL.SUBSORTS) '(ANY))
              (SETF (GET 'ANY 'DT*DIRECT.SUPERSORTS) 'NIL)
              (SETF (GET 'ANY 'DT*INVERSE.TRANSITIVE.CLOSURE) '(ANY)))) ))