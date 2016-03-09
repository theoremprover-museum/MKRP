;;; -*- Mode: LISP; Package: MKRP; Syntax: Common-lisp -*-
(* THE SUITOR OF THE LAST TALE PASSED BOTH TESTS AND HAPPILY)
(* CLAIMED PORTIA II AS HIS BRIDE. THEY LIVED HAPPILY EVER AFTER)
(* AND HAD A LOVELY DAUGHTER PORTIA III - HENCEFORTH TO BE CALLED)
(* PORTIA . WHEN SHE GREW UP TO YOUNG WOMANHOOD, SHE WAS BORN)
(* SMART AND BEAUTIFUL - JUST LIKE HER MOMMY AND GRANDMOMMY. SHE)
(* ALSO DECIDED TO CHOOSE HER HUSBAND BY THE CASKET METHOD. THE)
(* SUITOR HAD TO PASS THREE TESTS IN ORDER TO WIN HER! THE TESTS)
(* WERE QUITE INGENIOUS. SHE WENT BACK TO HER GRANDMOTHER^S IDEA)
(* OF HAVING ONLY ONE STATEMENT INSCRIBED ON EACH CASKET RATHER)
(* THAN TWO. BUT SHE INTRODUCED THE FOLLOWING WRINKLE: SHE EX-)
(* PLAINED TO THE SUITOR THAT EACH CASKET WAS FASHIONED BY ONE OF)
(* TWO FAMOUS FLORENTINE CRAFTSMEN - CELLINI OR BELLINI. WHENEVER)
(* CELLINI FASHIONED A CASKET, HE ALWAYS PUT A FALSE INSCRIPTION)
(* ON IT, WHEREAS BELLINI PUT ONLY TRUE INSCRIPTIONS ON HIS)
(* CASKETS.)
(*)
(*)
(* 69A. THE FIRST TEST.)
(*)
(* IN THIS UNUSUAL TEST THE SUITOR (IF HE GUESSED BLINDLY) WOULD)
(* HAVE A TWO OUT OF THREE RATHER THAN A ONE OUT OF THREE CHANCE.)
(* INSTEAD OF USING A PORTRAIT, PORTIA USED A DAGGER WHICH WAS)
(* PLACED IN ONE OF THE THREE CASKETS THE OTHER TWO CASKETS WERE)
(* EMPTY. IF THE SUITOR COULD AVOID THE CASKET WITH THE DAGGER,)
(* THEN HE COULD TAKE THE NEXT TEST. THE INSCRIPTIONS ON THE)
(* CASKETS WERE AS FOLLOWS :)
(*)
(* GOLD: THE DAGGER IS IN THIS CASKET)
(* SILVER: THIS CASKET IS EMPTY)
(* LEAD: AT MOST ONE OF THESE THREE CASKETS WAS FASHIONED BY)
(* BELLINI)
(*)
(* WHICH CASKET SHOULD THE SUITOR CHOOSE? INSCRIPTION *)
(*)
(* WE
DEFINE THE FOLLOWING PREDICATES:)
(*)
(* BELLINI (X) - THE CASKET X WAS FASHIONED BY BELLINI)
(* CELLINI (X) - THE CASKET X WAS FASHIONED BY CELLINI)
(* DAGGER (X) - THE CASKET
X CONTAINS THE DAGGER)
(* INSCR.DAGGER (X Y) - THE INSCRIPTION X TELLS THAT THE DAGGER)
(* IS IN CASKET Y)
(* NEG.INSCR.DAGGER (X Y) - THE INSCRIPTION X TELLS THAT THE)
(*
DAGGER IS NOT IN CASKET Y)
(* INSCR.ONE.BELLINI (X U V Z) - INSCRIPTION X TELLS THAT AT MOST ONE INSCRIPTION * OF THE CASKETS U,V,Z WAS FASHIONED BY BELLINI)
(* ONE.BELLINI (U V Z) - AT MOST ONE OF THE CASKETS
U,V,Z WAS)
(* FASHIONED BY BELLINI)
(*)
(*)
(* WE DEFINE THE FOLLOWING FUNCTION:)
(*)
(* INSCRIPTION (X) - MAPS AN ELEMENT OF THE SORT
CASKET TO AN)
(* ELEMENT OF THE SORT INSCRIPT)
(*)
(TYPE GOLD,SILVER,LEAD : CASKET)
(TYPE INSCRIPTION (CASKET) :INSCRIPT)
(TYPE BELLINI (CASKET))
(TYPE CELLINI (CASKET))
(TYPE DAGGER (CASKET))
(TYPE TRUTH (INSCRIPT))
(TYPE INSCR.DAGGER (INSCRIPT CASKET))
(TYPE NEG.INSCR.DAGGER (INSCRIPT CASKET))
(TYPE
ONE.BELLINI (CASKET CASKET CASKET))
(TYPE INSCR.ONE.BELLINI (INSCRIPT CASKET CASKET CASKET))
(*)
(*)
(* THE DAGGER IS IN EXACTLY ONE OF THE THREE CASKETS:)
(DAGGER (GOLD) OR DAGGER (SILVER) OR DAGGER (LEAD))
(NOT (DAGGER (GOLD) AND DAGGER (SILVER) OR DAGGER (SILVER) AND DAGGER (LEAD) OR DAGGER (LEAD) AND DAGGER (GOLD)))
(*)
(* IMPLICATIONS TO BE DRAWN FROM INSCRIPTIONS:)
(ALL X:INSCRIPT
ALL Y:CASKET INSCR.DAGGER (X Y) AND TRUTH (X) IMPL DAGGER (Y))
(ALL X:INSCRIPT ALL Y:CASKET INSCR.DAGGER (X Y) AND NOT TRUTH (X) IMPL NOT DAGGER (Y))
(ALL X:INSCRIPT ALL Y:CASKET NEG.INSCR.DAGGER (X Y) AND TRUTH (X) IMPL NOT DAGGER (Y))
(ALL X:INSCRIPT ALL Y:CASKET NEG.INSCR.DAGGER (X Y) AND NOT TRUTH (X) IMPL DAGGER (Y))
(ALL X:INSCRIPT ALL U,V,W:CASKET INSCR.ONE.BELLINI (X U V W) AND TRUTH (X) IMPL ONE.BELLINI (U V W))
(ALL X:INSCRIPT
ALL U,V,W:CASKET INSCR.ONE.BELLINI (X U V W) AND NOT TRUTH (X) IMPL NOT ONE.BELLINI (U V W))
(*)
(* BELLINI PUT TRUE AND CELLINI FALSE INSCRITIONS ON THE CASKET:)
(ALL X:CASKET BELLINI (X) EQV TRUTH (INSCRIPTION (X)))
(ALL X:CASKET CELLINI (X) EQV NOT TRUTH (INSCRIPTION (X)))
(*)
(* AT MOST ONE CASKET WAS FASHIONED BY BELLINI:)
(ALL U,V,W:CASKET ONE.BELLINI (U V W) EQV NOT (BELLINI (U) AND BELLINI (V) OR BELLINI (V) AND BELLINI (W) OR BELLINI (W) AND BELLINI (U)))
(*)
(* ACTUAL INSCRIPTIONS:)
(INSCR.DAGGER (INSCRIPTION (GOLD) GOLD))
(NEG.INSCR.DAGGER (INSCRIPTION (SILVER) SILVER))
(INSCR.ONE.BELLINI (INSCRIPTION
(LEAD) GOLD SILVER LEAD))

(DAGGER (GOLD))