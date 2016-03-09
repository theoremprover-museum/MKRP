;;; -*- Package: MKRP; Base: 10; Mode: LISP; Syntax: Common-lisp -*-
(PREP-PROBLEM.SPECIFICATION 'NIL 'NIL '(((ALL X0087,X0088
    (((NOT ((ALL X0089,X0090
             (((MODEL (A X0089)) AND (R (STAR (X0087) X0089 X0090))) IMPL
              (NOT ((((NOT ((ALL X0091,X0092
                             (((MODEL (B X0091)) AND (R (STAR (STAR (X0090)) X0091 X0092)))
                              IMPL
                              (NOT ((((MODEL (C STAR (X0092))) OR
                                      (NOT (MODEL (C STAR (STAR (X0092))))))
                                     OR
                                     (NOT ((ALL X0093,X0094
                                            (((NOT (MODEL (B STAR (X0093)))) AND
                                              (R (STAR (STAR (X0092)) X0093 X0094)))
                                             IMPL (NOT (NOT (MODEL (B STAR (STAR (X0094))))))))
                                           )))))))))
                      OR
                      (NOT (NOT ((ALL X0095,X0096
                                  (((MODEL (B X0095)) AND
                                    (R (STAR (STAR (STAR (X0090))) X0095 X0096)))
                                   IMPL
                                   (NOT ((((MODEL (C STAR (X0096))) OR
                                           (NOT (MODEL (C STAR (STAR (X0096))))))
                                          OR
                                          (NOT ((ALL X0097,X0098
                                                 (((NOT (MODEL (B STAR (X0097)))) AND
                                                   (R (STAR (STAR (X0096)) X0097 X0098)))
                                                  IMPL
                                                  (NOT (NOT (MODEL (B STAR (STAR (X0098))))))))
                                                )))))))))))
                     OR
                     (NOT ((ALL X0099,X0100
                            (((NOT (MODEL (A STAR (X0099)))) AND
                              (R (STAR (STAR (X0090)) X0099 X0100)))
                             IMPL (NOT (NOT (MODEL (A STAR (STAR (X0100)))))))))))))))))
      AND (R (0 X0087 X0088)))
     IMPL
     (NOT ((ALL X0101,X0102
            (((NOT ((ALL X0103,X0104
                     (((MODEL (A X0103)) AND (R (STAR (X0101) X0103 X0104))) IMPL
                      (NOT ((((MODEL (B STAR (X0104))) OR (NOT (MODEL (B STAR (STAR (X0104)))))
                              )
                             OR
                             (NOT ((ALL X0105,X0106
                                    (((NOT (MODEL (A STAR (X0105)))) AND
                                      (R (STAR (STAR (X0104)) X0105 X0106)))
                                     IMPL (NOT (NOT (MODEL (A STAR (STAR (X0106))))))))))))))))
                   )
              AND (R (STAR (X0088) X0101 X0102)))
             IMPL
             (NOT ((((MODEL (C STAR (X0102))) OR (NOT (MODEL (C STAR (STAR (X0102)))))) OR
                    (NOT ((ALL X0107,X0108
                           (((NOT (NOT ((ALL X0109,X0110
                                         (((MODEL (A X0109)) AND
                                           (R (STAR (STAR (X0107)) X0109 X0110)))
                                          IMPL
                                          (NOT ((((MODEL (B STAR (X0110))) OR
                                                  (NOT (MODEL (B STAR (STAR (X0110))))))
                                                 OR
                                                 (NOT ((ALL X0111,X0112
                                                        (((NOT (MODEL (A STAR (X0111)))) AND
                                                          (R (STAR (STAR (X0110)) X0111 X0112))
                                                          )
                                                         IMPL
                                                         (NOT
                                                          (NOT (MODEL (A STAR (STAR (X0112)))))
                                                          )))))))))))))
                             AND (R (STAR (STAR (X0102)) X0107 X0108)))
                            IMPL
                            (NOT (NOT (NOT ((ALL X0113,X0114
                                             (((MODEL (A X0113)) AND
                                               (R (STAR (STAR (STAR (X0108))) X0113 X0114)))
                                              IMPL
                                              (NOT ((((MODEL (B STAR (X0114))) OR
                                                      (NOT (MODEL (B STAR (STAR (X0114))))))
                                                     OR
                                                     (NOT ((ALL X0115,X0116
                                                            (((NOT (MODEL (A STAR (X0115))))
                                                              AND
                                                              (R
                                                               (STAR (STAR (X0114)) X0115
                                                                X0116)))
                                                             IMPL
                                                             (NOT
                                                              (NOT
                                                               (MODEL (A STAR (STAR (X0116)))))
                                                              )))))))))))))))))))))))))))
   AND
   (ALL X0117,X0118
    (((NOT ((ALL X0119,X0120
             (((NOT ((ALL X0121,X0122
                      (((MODEL (A X0121)) AND (R (STAR (X0119) X0121 X0122))) IMPL
                       (NOT ((((MODEL (B STAR (X0122))) OR
                               (NOT (MODEL (B STAR (STAR (X0122))))))
                              OR
                              (NOT ((ALL X0123,X0124
                                     (((NOT (MODEL (A STAR (X0123)))) AND
                                       (R (STAR (STAR (X0122)) X0123 X0124)))
                                      IMPL (NOT (NOT (MODEL (A STAR (STAR (X0124)))))))))))))))
                     ))
               AND (R (STAR (X0117) X0119 X0120)))
              IMPL
              (NOT ((((MODEL (C STAR (X0120))) OR (NOT (MODEL (C STAR (STAR (X0120)))))) OR
                     (NOT ((ALL X0125,X0126
                            (((NOT (NOT ((ALL X0127,X0128
                                          (((MODEL (A X0127)) AND
                                            (R (STAR (STAR (X0125)) X0127 X0128)))
                                           IMPL
                                           (NOT ((((MODEL (B STAR (X0128))) OR
                                                   (NOT (MODEL (B STAR (STAR (X0128))))))
                                                  OR
                                                  (NOT ((ALL X0129,X0130
                                                         (((NOT (MODEL (A STAR (X0129)))) AND
                                                           (R (STAR (STAR (X0128)) X0129 X0130)
                                                            ))
                                                          IMPL
                                                          (NOT
                                                           (NOT
                                                            (MODEL (A STAR (STAR (X0130))))))))
                                                        ))))))))))
                              AND (R (STAR (STAR (X0120)) X0125 X0126)))
                             IMPL
                             (NOT (NOT (NOT ((ALL X0131,X0132
                                              (((MODEL (A X0131)) AND
                                                (R (STAR (STAR (STAR (X0126))) X0131 X0132)))
                                               IMPL
                                               (NOT ((((MODEL (B STAR (X0132))) OR
                                                       (NOT (MODEL (B STAR (STAR (X0132))))))
                                                      OR
                                                      (NOT ((ALL X0133,X0134
                                                             (((NOT (MODEL (A STAR (X0133))))
                                                               AND
                                                               (R
                                                                (STAR (STAR (X0132)) X0133
                                                                 X0134)))
                                                              IMPL
                                                              (NOT
                                                               (NOT
                                                                (MODEL (A STAR (STAR (X0134))))
                                                                ))))))))))))))))))))))))))
      AND (R (0 X0117 X0118)))
     IMPL
     (NOT ((ALL X0135,X0136
            (((MODEL (A X0135)) AND (R (STAR (X0118) X0135 X0136))) IMPL
             (NOT ((((NOT ((ALL X0137,X0138
                            (((MODEL (B X0137)) AND (R (STAR (STAR (X0136)) X0137 X0138)))
                             IMPL
                             (NOT ((((MODEL (C STAR (X0138))) OR
                                     (NOT (MODEL (C STAR (STAR (X0138))))))
                                    OR
                                    (NOT ((ALL X0139,X0140
                                           (((NOT (MODEL (B STAR (X0139)))) AND
                                             (R (STAR (STAR (X0138)) X0139 X0140)))
                                            IMPL (NOT (NOT (MODEL (B STAR (STAR (X0140)))))))))
                                         ))))))))
                     OR
                     (NOT (NOT ((ALL X0141,X0142
                                 (((MODEL (B X0141)) AND
                                   (R (STAR (STAR (STAR (X0136))) X0141 X0142)))
                                  IMPL
                                  (NOT ((((MODEL (C STAR (X0142))) OR
                                          (NOT (MODEL (C STAR (STAR (X0142))))))
                                         OR
                                         (NOT ((ALL X0143,X0144
                                                (((NOT (MODEL (B STAR (X0143)))) AND
                                                  (R (STAR (STAR (X0142)) X0143 X0144)))
                                                 IMPL
                                                 (NOT (NOT (MODEL (B STAR (STAR (X0144)))))))))
                                              ))))))))))
                    OR
                    (NOT ((ALL X0145,X0146
                           (((NOT (MODEL (A STAR (X0145)))) AND
                             (R (STAR (STAR (X0136)) X0145 X0146)))
                            IMPL (NOT (NOT (MODEL (A STAR (STAR (X0146))))))))))))))))))))) '((AND (ALL 3
        (ALL 2
         (IMPL
          (AND (NOT (ALL 5
                     (ALL 4
                      (IMPL
                       (AND (+ 7 (6 4) NIL)
                            (+ 9 ((8 2) 4 5) NIL))
                       (NOT (OR (OR (NOT (ALL 11
                                          (ALL 10
                                           (IMPL
                                            (AND (+ 7 (12 10) NIL)
                                                 (+ 9 ((8 (8 5)) 10 11) NIL))
                                            (NOT (OR (OR (+ 7 (13 (8 11)) NIL)
                                                         (NOT (+ 7 (13 (8 (8 11))) NIL)))
                                                     (NOT (ALL 15
                                                           (ALL 14
                                                            (IMPL
                                                             (AND
                                                              (NOT (+ 7 (12 (8 14)) NIL))
                                                              (+ 9 ((8 (8 11)) 14 15) NIL))
                                                             (NOT
                                                              (NOT (+ 7 (12 (8 (8 15))) NIL))))
                                                            )))))))))
                                    (NOT (NOT (ALL 17
                                               (ALL 16
                                                (IMPL
                                                 (AND (+ 7 (12 16) NIL)
                                                      (+ 9 ((8 (8 (8 5))) 16 17) NIL))
                                                 (NOT (OR (OR
                                                           (+ 7 (13 (8 17)) NIL)
                                                           (NOT (+ 7 (13 (8 (8 17))) NIL)))
                                                          (NOT
                                                           (ALL 19
                                                            (ALL 18
                                                             (IMPL
                                                              (AND
                                                               (NOT (+ 7 (12 (8 18)) NIL))
                                                               (+ 9 ((8 (8 17)) 18 19) NIL))
                                                              (NOT
                                                               (NOT (+ 7 (12 (8 (8 19))) NIL)))
                                                              ))))))))))))
                                (NOT (ALL 21
                                      (ALL 20
                                       (IMPL
                                        (AND (NOT (+ 7 (6 (8 20)) NIL))
                                             (+ 9 ((8 (8 5)) 20 21) NIL))
                                        (NOT (NOT (+ 7 (6 (8 (8 21))) NIL)))))))))))))
               (+ 9 (22 2 3) NIL))
          (NOT (ALL 24
                (ALL 23
                 (IMPL
                  (AND (NOT (ALL 26
                             (ALL 25
                              (IMPL
                               (AND (+ 7 (6 25) NIL)
                                    (+ 9 ((8 23) 25 26) NIL))
                               (NOT (OR (OR (+ 7 (12 (8 26)) NIL)
                                            (NOT (+ 7 (12 (8 (8 26))) NIL)))
                                        (NOT (ALL 28
                                              (ALL 27
                                               (IMPL
                                                (AND (NOT (+ 7 (6 (8 27)) NIL))
                                                     (+ 9 ((8 (8 26)) 27 28) NIL))
                                                (NOT (NOT (+ 7 (6 (8 (8 28))) NIL)))))))))))))
                       (+ 9 ((8 3) 23 24) NIL))
                  (NOT (OR (OR (+ 7 (13 (8 24)) NIL)
                               (NOT (+ 7 (13 (8 (8 24))) NIL)))
                           (NOT (ALL 30
                                 (ALL 29
                                  (IMPL
                                   (AND (NOT (NOT (ALL 32
                                                   (ALL 31
                                                    (IMPL
                                                     (AND (+ 7 (6 31) NIL)
                                                          (+ 9 ((8 (8 29)) 31 32) NIL))
                                                     (NOT (OR
                                                           (OR
                                                            (+ 7 (12 (8 32)) NIL)
                                                            (NOT (+ 7 (12 (8 (8 32))) NIL)))
                                                           (NOT
                                                            (ALL 34
                                                             (ALL 33
                                                              (IMPL
                                                               (AND
                                                                (NOT (+ 7 (6 (8 33)) NIL))
                                                                (+ 9 ((8 (8 32)) 33 34) NIL))
                                                               (NOT
                                                                (NOT (+ 7 (6 (8 (8 34))) NIL)))
                                                               )))))))))))
                                        (+ 9 ((8 (8 24)) 29 30) NIL))
                                   (NOT (NOT (NOT (ALL 36
                                                   (ALL 35
                                                    (IMPL
                                                     (AND (+ 7 (6 35) NIL)
                                                          (+ 9 ((8 (8 (8 30))) 35 36) NIL))
                                                     (NOT (OR
                                                           (OR
                                                            (+ 7 (12 (8 36)) NIL)
                                                            (NOT (+ 7 (12 (8 (8 36))) NIL)))
                                                           (NOT
                                                            (ALL 38
                                                             (ALL 37
                                                              (IMPL
                                                               (AND
                                                                (NOT (+ 7 (6 (8 37)) NIL))
                                                                (+ 9 ((8 (8 36)) 37 38) NIL))
                                                               (NOT
                                                                (NOT (+ 7 (6 (8 (8 38))) NIL)))
                                                               )))))))))))))))))))))))))
       (ALL 40
        (ALL 39
         (IMPL
          (AND (NOT (ALL 42
                     (ALL 41
                      (IMPL
                       (AND (NOT (ALL 44
                                  (ALL 43
                                   (IMPL
                                    (AND (+ 7 (6 43) NIL)
                                         (+ 9 ((8 41) 43 44) NIL))
                                    (NOT (OR (OR (+ 7 (12 (8 44)) NIL)
                                                 (NOT (+ 7 (12 (8 (8 44))) NIL)))
                                             (NOT (ALL 46
                                                   (ALL 45
                                                    (IMPL
                                                     (AND (NOT (+ 7 (6 (8 45)) NIL))
                                                          (+ 9 ((8 (8 44)) 45 46) NIL))
                                                     (NOT (NOT (+ 7 (6 (8 (8 46))) NIL)))))))))
                                    ))))
                            (+ 9 ((8 39) 41 42) NIL))
                       (NOT (OR (OR (+ 7 (13 (8 42)) NIL)
                                    (NOT (+ 7 (13 (8 (8 42))) NIL)))
                                (NOT (ALL 48
                                      (ALL 47
                                       (IMPL
                                        (AND (NOT (NOT (ALL 50
                                                        (ALL 49
                                                         (IMPL
                                                          (AND
                                                           (+ 7 (6 49) NIL)
                                                           (+ 9 ((8 (8 47)) 49 50) NIL))
                                                          (NOT
                                                           (OR
                                                            (OR
                                                             (+ 7 (12 (8 50)) NIL)
                                                             (NOT (+ 7 (12 (8 (8 50))) NIL)))
                                                            (NOT
                                                             (ALL 52
                                                              (ALL 51
                                                               (IMPL
                                                                (AND
                                                                 (NOT (+ 7 (6 (8 51)) NIL))
                                                                 (+ 9 ((8 (8 50)) 51 52) NIL))
                                                                (NOT
                                                                 (NOT (+ 7 (6 (8 (8 52))) NIL))
                                                                 ))))))))))))
                                             (+ 9 ((8 (8 42)) 47 48) NIL))
                                        (NOT (NOT (NOT (ALL 54
                                                        (ALL 53
                                                         (IMPL
                                                          (AND
                                                           (+ 7 (6 53) NIL)
                                                           (+ 9 ((8 (8 (8 48))) 53 54) NIL))
                                                          (NOT
                                                           (OR
                                                            (OR
                                                             (+ 7 (12 (8 54)) NIL)
                                                             (NOT (+ 7 (12 (8 (8 54))) NIL)))
                                                            (NOT
                                                             (ALL 56
                                                              (ALL 55
                                                               (IMPL
                                                                (AND
                                                                 (NOT (+ 7 (6 (8 55)) NIL))
                                                                 (+ 9 ((8 (8 54)) 55 56) NIL))
                                                                (NOT
                                                                 (NOT (+ 7 (6 (8 (8 56))) NIL))
                                                                 )))))))))))))))))))))))
               (+ 9 (22 39 40) NIL))
          (NOT (ALL 58
                (ALL 57
                 (IMPL
                  (AND (+ 7 (6 57) NIL)
                       (+ 9 ((8 40) 57 58) NIL))
                  (NOT (OR (OR (NOT (ALL 60
                                     (ALL 59
                                      (IMPL
                                       (AND (+ 7 (12 59) NIL)
                                            (+ 9 ((8 (8 58)) 59 60) NIL))
                                       (NOT (OR (OR (+ 7 (13 (8 60)) NIL)
                                                    (NOT (+ 7 (13 (8 (8 60))) NIL)))
                                                (NOT (ALL 62
                                                      (ALL 61
                                                       (IMPL
                                                        (AND
                                                         (NOT (+ 7 (12 (8 61)) NIL))
                                                         (+ 9 ((8 (8 60)) 61 62) NIL))
                                                        (NOT (NOT (+ 7 (12 (8 (8 62))) NIL)))))
                                                      ))))))))
                               (NOT (NOT (ALL 64
                                          (ALL 63
                                           (IMPL
                                            (AND (+ 7 (12 63) NIL)
                                                 (+ 9 ((8 (8 (8 58))) 63 64) NIL))
                                            (NOT (OR (OR (+ 7 (13 (8 64)) NIL)
                                                         (NOT (+ 7 (13 (8 (8 64))) NIL)))
                                                     (NOT (ALL 66
                                                           (ALL 65
                                                            (IMPL
                                                             (AND
                                                              (NOT (+ 7 (12 (8 65)) NIL))
                                                              (+ 9 ((8 (8 64)) 65 66) NIL))
                                                             (NOT
                                                              (NOT (+ 7 (12 (8 (8 66))) NIL))))
                                                            )))))))))))
                           (NOT (ALL 68
                                 (ALL 67
                                  (IMPL
                                   (AND (NOT (+ 7 (6 (8 67)) NIL))
                                        (+ 9 ((8 (8 58)) 67 68) NIL))
                                   (NOT (NOT (+ 7 (6 (8 (8 68))) NIL)))))))))))))))))) '("                                                                                                                     "
  "*********************************************************************************************************************"
  " EDIT:     Axioms and Theorems edited: 24-JUL,1989 19:09 "
  "*********************************************************************************************************************"
  ) (PROGN(PROGN (DT-RESET))

(PROG ((ADDRLIST
        '(9610 9615 9620 9625 9630 9635 9640 9645 9650 9655 9660 9665 9670 9675 9680 9685 9690
          9695 9700 9705 9710 9715 9720 9725 9730 9735 9740 9745 9750 9755 9760 9765 9770 9775
          9780 9785 9790 9795 9800 9805 9810 9815 9820 9825 9830 9835 9840 9845 9850 9855 9860
          9865 9870 9875 9880 9885 9890 9895 9900 9915 9929 9944 9949 9954 9959 9964 9969 9984
          9999))
       (DATALIST
        '(15 "TRUE" NIL NIL NIL (DEFINED) NIL ((- 0 (POSITIVE))) ((+ 0 (NEGATIVE)))
          ((+ 0 (NIL))) ((- 0 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 15 "FALSE" NIL NIL
          NIL (DEFINED) NIL ((- 1 (POSITIVE))) ((+ 1 (NEGATIVE))) ((+ 1 (NIL))) ((- 1 (NIL)))
          NIL NIL (DT*ST-KIND NIL) PREDICATE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY "A" (DT*ST-KIND NIL) CONSTANT 15 "MODEL" NIL NIL (ANY ANY) NIL NIL
          ((- 7 (POSITIVE))) ((+ 7 (NEGATIVE))) ((+ 7 (NIL))) ((- 7 (NIL))) NIL NIL
          (DT*ST-KIND NIL) PREDICATE 14 "STAR" NIL ANY (ANY) NIL NIL NIL NIL NIL 1 ANY
          (DT*ST-KIND NIL) FUNCTION 15 "R" NIL NIL (ANY ANY ANY) NIL NIL ((- 9 (POSITIVE)))
          ((+ 9 (NEGATIVE))) ((+ 9 (NIL))) ((- 9 (NIL))) NIL NIL (DT*ST-KIND NIL) PREDICATE 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY "B" (DT*ST-KIND NIL) CONSTANT 5 ANY
          "C" (DT*ST-KIND NIL) CONSTANT 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5
          ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
          (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR)
          VARIABLE 5 ANY NIL (ST*DATA NIL DT*ST-KIND SYS-VAR) VARIABLE 5 ANY NIL
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
       (INCREMENT (- MEM*SIZE 10000))
       COUNTER1)
      (MEM-RESET)
      (COND ((> MEM*SIZE 10000)
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
            (T (UNLESS (= MEM*SIZE 10000)
                 (MEM-INITIALIZE 10000))
               (SETQ COUNTER1 69)
               (MAPC #'(LAMBDA (RADDR) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) RADDR))
                     ADDRLIST)
               (SETQ COUNTER1 MEM*SIZE)
               (MAPC #'(LAMBDA (DATA) (SETF (AREF MEM*MEMORY (DECF COUNTER1)) DATA)) DATALIST))
            )
      (SETQ MEM*NEXT.RADR (IF (> MEM*SIZE 10000) (+ 9605 INCREMENT) 9605))
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
       (SETQ DT*CONSTANT.ALL '(22 13 12 6))
       (SETQ DT*ABBREVIATIONS 'NIL)
       (SETQ DT*FUNCTION.COUNTER '0)
       (SETQ DT*FUNCTION.ALL '(8))
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
       (SETQ DT*NONEQUALITY.PREDICATES '(9 7 1 0))
       (SETQ DT*PREDICATE.ALL '(9 7 1 0))
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