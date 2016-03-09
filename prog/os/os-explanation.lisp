;;; -*- Mode: LISP; Syntax: Common-lisp; Package: MARKGRAF-KARL; Base: 10 -*-

(IN-PACKAGE "MARKGRAF-KARL" :use '("CL") :nicknames '("MKRP"))


(DEFUN OS.E-GET.EXPLANATION (INDICATOR &optional first.arg second.arg third.arg)
						;INPUT:  -ATOM, AN INDICATOR FOR THE TEXT TO BE      
						;         SELECTED.
						;        -LIST, OF ITEMS (S-EXPRESSIONS) THAT ARE IN-	
						;         SERTED INTO THE TEXT AT THE SPECIFIED
						;         POSITIONS. --- MAY BE EMPTY ! ---    
						;VALUE:   LIST, OF STRINGS (TO BE PRINTED).    
						;EFFECT:  SELECTS ACCORDING TO <INDICATOR> THE APPRO- 
						;         PRIATE LIST OF TEXT STRINGS IN THE LANGUAGE 	
						;         SPECIFIED BY OS*LANGUAGE. THE ELEMENTS OF    
						;         <PRINT.ARGUMENTS> ARE INSERTED AT THE SPECI- 
						;         FIED POSITIONS INTO THE LIST OF TEXT STRINGS)) 
  (let ()  
    (CASE INDICATOR
      (ERROR.ILLEGAL.FILE (OS.E=ERROR.ILLEGAL.FILE FIRST.ARG second.arg third.arg))
      (ERROR.NO.FILE.EXISTS (OS.E=ERROR.NO.FILE.EXISTS FIRST.ARG second.arg third.arg))
      (CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
	(OS.E=CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
      (help_define.directory (os.e=help_define.directory first.arg))
      (help_define.example.name (os.e=help_define.example.name first.arg))
      (help_select.example (os.e=help_select.example first.arg))
      (help_vp  (os.e=help_vp))
      (help_hp  (os.e=help_hp))
      (EDIT.CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
	(OS.E=EDIT.CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
      (EDIT.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
	(OS.E=EDIT.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
      (COMMON.ERROR.MESSAGE_COMMAND.IGNORED (OS.E=COMMON.ERROR.MESSAGE_COMMAND.IGNORED FIRST.ARG))
      (\@ATP_SESSION.LANGUAGE (OS.E=\@ATP_SESSION.LANGUAGE))
      (\@ATP_INFO.AND.REPAIR.FILE (OS.E=\@ATP_INFO.AND.REPAIR.FILE))
      (\@ATP_EXPLAIN.HELP (OS.E=\@ATP_EXPLAIN.help))
      (READ&EXECUTE.ATP.COMMAND_START.EDIT (OS.E=READ&EXECUTE.ATP.COMMAND_START.EDIT))
      (READ&EXECUTE.ATP.COMMAND_ERROR=ILLEGAL.COMMAND
	(OS.E=READ&EXECUTE.ATP.COMMAND_ERROR=ILLEGAL.COMMAND FIRST.ARG))
      (VDT_ERROR=ILLEGAL.ARGUMENT (OS.E=VDT_ERROR=ILLEGAL.ARGUMENT FIRST.ARG second.arg))
      (HARDCOPY_ERROR=ILLEGAL.ARGUMENT (OS.E=HARDCOPY_ERROR=ILLEGAL.ARGUMENT FIRST.ARG))
      (HELP_V (OS.E=HELP_V))
      (HELP_HC (OS.E=HELP_HC))
      (HELP_HELP (OS.E=HELP_HELP))
      (HELP_EXIT (OS.E=HELP_EXIT))
      (HELP_LOGOFF (OS.E=HELP_LOGOFF))
      (HELP_LISP (OS.E=HELP_LISP))
      (HELP_OPTIONS (OS.E=HELP_OPTIONS))
      (HELP_INDUCTION (OS.E=HELP_INDUCTION))
      (HELP_CONSTRUCT (OS.E=HELP_CONSTRUCT))
      (HELP_CONSTRUCT.REFUTE (OS.E=HELP_CONSTRUCT.REFUTE))
      (HELP_CONSTRUCT.REFUTE.PROTOCOL (OS.E=HELP_CONSTRUCT.REFUTE.PROTOCOL))
      (HELP_EDIT (OS.E=HELP_EDIT))
      (HELP_EDIT.CONSTRUCT (OS.E=HELP_EDIT.CONSTRUCT))
      (HELP_EDIT.CONSTRUCT.REFUTE (OS.E=HELP_EDIT.CONSTRUCT.REFUTE))
      (HELP_EDIT.CONSTRUCT.REFUTE.PROTOCOL (OS.E=HELP_EDIT.CONSTRUCT.REFUTE.PROTOCOL))
      (HELP_FORMULA (OS.E=HELP_FORMULA))
      (HELP_FORMULA.CONSTRUCT (OS.E=HELP_FORMULA.CONSTRUCT))
      (HELP_FORMULA.CONSTRUCT.REFUTE (OS.E=HELP_FORMULA.CONSTRUCT.REFUTE))
      (HELP_FORMULA.CONSTRUCT.REFUTE.PROTOCOL
	(OS.E=HELP_FORMULA.CONSTRUCT.REFUTE.PROTOCOL))
      (HELP_REFUTE (OS.E=HELP_REFUTE))
      (HELP_REFUTE.PROTOCOL (OS.E=HELP_REFUTE.PROTOCOL))
      (HELP_PROTOCOL (OS.E=HELP_PROTOCOL))
      (HELP_COMMAND.NOT.IMPLEMENTED (OS.E=HELP_COMMAND.NOT.IMPLEMENTED FIRST.ARG))
      (HELP.EXPLANATION_EXPLAIN.HELP (OS.E=HELP.EXPLANATION_EXPLAIN.HELP))
      (INDUCTION_ENTER.THEORY (OS.E=INDUCTION_ENTER.THEORY))
      (error.ILLEGAL.ATP.VERSION
	(OS.E=ERROR.ILLEGAL.ATP.VERSION FIRST.ARG))
      (CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
	(OS.E=CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
      (CONSTRUCT.PROC_PROOF.COMMENT (OS.E=CONSTRUCT.PROC_PROOF.COMMENT FIRST.ARG))
      (CONSTRUCT.PROC_START.MESSAGE (OS.E=CONSTRUCT.PROC_START.MESSAGE))
      (CONSTRUCT.PROC_THEOREM.PROVED (OS.E=CONSTRUCT.PROC_THEOREM.PROVED))
      (CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
	(OS.E=CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
      (otherwise
	(os.e=help first.arg indicator second.arg third.arg)))))

(defun os.e=help (first.arg indicator second.arg third.arg)
  (case indicator	    
    (EDIT.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=EDIT.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (EDIT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=EDIT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (EDIT.PROC_PROOF.COMMENT (OS.E=EDIT.PROC_PROOF.COMMENT FIRST.ARG))
    (EDIT.PROC_ERROR=NOTHING.TO.PROVE (OS.E=EDIT.PROC_ERROR=NOTHING.TO.PROVE))
    (EDIT.PROC_START.EDIT.AXIOMS (OS.E=EDIT.PROC_START.EDIT.AXIOMS))
    (EDIT.PROC_START.EDIT.THEOREMS (OS.E=EDIT.PROC_START.EDIT.THEOREMS))
    (EDIT.PROC_WARNING=NO.AXIOMS (OS.E=EDIT.PROC_WARNING=NO.AXIOMS))
    (EDIT.PROC_WARNING=NO.THEOREMS (OS.E=EDIT.PROC_WARNING=NO.THEOREMS))
    (FORMULA.CONSTRUCT.INPUT.CONTROL_ERROR=MISSING.INPUT
      (OS.E=FORMULA.CONSTRUCT.INPUT.CONTROL_ERROR=MISSING.INPUT))
    (FORMULA.CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=FORMULA.CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=MISSING.INPUT
      (OS.E=FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=MISSING.INPUT))
    (FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=MISSING.INPUT
      (OS.E=FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=MISSING.INPUT))
    (FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (FORMULA.INPUT.CONTROL_ERROR=MISSING.INPUT
      (OS.E=FORMULA.INPUT.CONTROL_ERROR=MISSING.INPUT))
    (FORMULA.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=FORMULA.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (FORMULA.PROC_PROOF.COMMENT (OS.E=FORMULA.PROC_PROOF.COMMENT FIRST.ARG second.arg third.arg))
    (PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (PROTOCOL.PROC_LIST.FILE.MESSAGE (OS.E=PROTOCOL.PROC_LIST.FILE.MESSAGE FIRST.ARG))
    (REFUTE.INPUT.CONTROL_ERROR=ILLEGAL.SPLITPART.ID
      (OS.E=REFUTE.INPUT.CONTROL_ERROR=ILLEGAL.SPLITPART.ID FIRST.ARG))
    (REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (REFUTE.PROC_THEOREM.NOT.PROVED (OS.E=REFUTE.PROC_THEOREM.NOT.PROVED))
    (REFUTE.PROC_THEOREM.PROVED (OS.E=REFUTE.PROC_THEOREM.PROVED))
    (REFUTE.PROC_START.MESSAGE (OS.E=REFUTE.PROC_START.MESSAGE))
    (REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=ILLEGAL.SPLITPART.ID
      (OS.E=REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=ILLEGAL.SPLITPART.ID FIRST.ARG))
    (REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS
      (OS.E=REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS FIRST.ARG))
    (OPTION_ENTER.MODULE (OS.E=OPTION_ENTER.MODULE))
    (OPTION_ILLEGAL.OPTION.VALUE (OS.E=OPTION_ILLEGAL.OPTION.VALUE FIRST.ARG))
    (OPTION_GET.HELP (OS.E=OPTION_GET.HELP FIRST.ARG))
    (OPT.PRINT_GET.HELP (OS.E=OPT.PRINT_GET.HELP FIRST.ARG))
    (OPT.READ_FILENAME? (OS.E=OPT.READ_FILENAME?))
    (OPT.READ_OPTIONS.SET (OS.E=OPT.READ_OPTIONS.SET))
    (OPT.READ_INCONSISTENT.OPTIONS (OS.E=OPT.READ_INCONSISTENT.OPTIONS))
    (OPT.WRITE_FILENAME? (OS.E=OPT.WRITE_FILENAME?))
    (OPT.PRETTYPRINT_FILENAME? (OS.E=OPT.PRETTYPRINT_FILENAME?))
    (OPT.PRETTYPRINT_HEADLINE (OS.E=OPT.PRETTYPRINT_HEADLINE))
    (OPT.PRETTYPRINT_DEFAULTVAL (OS.E=OPT.PRETTYPRINT_DEFAULTVAL FIRST.ARG))
    (OPT_ERROR=ILLEGAL.FILENAME (OS.E=OPT_ERROR=ILLEGAL.FILENAME FIRST.ARG second.arg))
    (OPT_OUTPUT.END (OS.E=OPT_OUTPUT.END))
    (OPT.HELP_HELP (OS.E=OPT.HELP_HELP))
    (OPT.HELP_PRINT (OS.E=OPT.HELP_PRINT))
    (OPT.HELP_PPRINT (OS.E=OPT.HELP_PPRINT))
    (OPT.HELP_READ (OS.E=OPT.HELP_READ))
    (OPT.HELP_WRITE (OS.E=OPT.HELP_WRITE))
    (OPT.HELP_LISP (OS.E=OPT.HELP_LISP))
    (OPT.HELP_OK (OS.E=OPT.HELP_OK))
    (OPT.HELP_V (OS.E=OPT.HELP_V))
    (OPT.HELP_AREA.ABBREVIATION (OS.E=OPT.HELP_AREA.ABBREVIATION))
    (OPT.HELP_ALL.COMMANDS (OS.E=OPT.HELP_ALL.COMMANDS))
    (OTHERWISE (OS.E=OTHERWISE indicator))))

(DEFUN OS.E=COMMON.ERROR.MESSAGE_COMMAND.IGNORED (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " KOMMANDO ~A IGNORIERT." FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil " Command ~A ignored. " FIRST.ARG)))

(DEFUN OS.E=\@ATP_SESSION.LANGUAGE ()
  #+:mkrp.deutsch`("Die Dialogsprache waehrend dieser Sitzung ist deutsch. ")
  #-:mkrp.deutsch`("The dialogue language is English for this session. "))

(DEFUN OS.E=\@ATP_INFO.AND.REPAIR.FILE ()
  #+:mkrp.deutsch`("BITTE DIE INFO.AND.REPAIR-DATEI ANGEBEN. IM NORMALFALL DIESE:")
  #-:mkrp.deutsch`("Please enter the INFO.AND.REPAIR file name. The standard name is:"))

(DEFUN OS.E=\@ATP_EXPLAIN.HELP ()
  #+:mkrp.deutsch`(" H[ELP]        druckt eine Liste aller verfuegbaren Kommandos. "
		   " H[ELP] <KOM>  erklaert das Kommando <KOM>.")
  #-:mkrp.deutsch`(" H[ELP]        prints a list of all available commands."
		   " H[ELP] <COM>  explains the command <COM>. "))

(DEFUN OS.E=READ&EXECUTE.ATP.COMMAND_START.EDIT ()
  #+:mkrp.deutsch`("EDITOR FUER LOGISCHE FORMELN:"
		   " H[ELP]        DRUCKT EINE LISTE DER VERFUEGBAREN EDITOR-BEFEHLE."
		   " OK            VERLAESST DEN EDITOR WIEDER.")
  #-:mkrp.deutsch`("Editor for logical formulas: "
		   " H[ELP]        prints a list of all available editor-commands. "
		   " OK            returns to where you came from."))

(DEFUN OS.E=READ&EXECUTE.ATP.COMMAND_ERROR=ILLEGAL.COMMAND (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ~A  ist kein gueltiges Kommando."  FIRST.ARG)
		   ,(format nil " Kommando ~A ignoriert." first.ARG)
		   " H[ELP] hilft weiter.")
  #-:mkrp.deutsch`(,(format nil " ~A is an illegal command." FIRST.ARG)
		   ,(format nil " Command ~A ignored." FIRST.ARG)
		   " Try H[ELP]."))

(DEFUN OS.E=VDT_ERROR=ILLEGAL.ARGUMENT (FIRST.ARG second.arg)
  #+:mkrp.deutsch`(,(format nil "  ~A IST EIN UNZULAESSIGES ARGUMENT FUER V <JA/NEIN>." FIRST.ARG)
		   ,(format nil "  KOMMANDO ~A IGNORIERT." SECOND.ARG))
  #-:mkrp.deutsch`(,(format nil "  ~A IS AN ILLEGAL ARGUMENT FOR V <YES/NO>." FIRST.ARG)
		   ,(format nil "  COMMAND ~A IGNORED." SECOND.ARG)))

(DEFUN OS.E=HARDCOPY_ERROR=ILLEGAL.ARGUMENT (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ~A IST EIN UNZULAESSIGES ARGUMENT. " FIRST.ARG)
		   " H[ELP] HC   ERKLAERT DAS KOMMANDO HARDCOPY."
		   " KOMMANDO HARDCOPY IGNORIERT.")
  #-:mkrp.deutsch`(,(format nil " ~A is an illegal argument." FIRST.ARG)
		   " H[ELP] HC  explains the command HARDCOPY."
		   " Command HARDCOPY ignored."))

(DEFUN OS.E=HELP_V ()
  #+:mkrp.deutsch`(" V T/J/JA      SCHALTET DIE MANUELLE BILDSCHIRMSTEUERUNG EIN. "
		   " V NIL/N/NEIN  SCHALTET SIE AUS.")
  #-:mkrp.deutsch`(" V T/Y/YES     TURNS THE MANUAL TERMINAL CONTROL ON. "
		   " V NIL/N/NO    TURNS IT OFF. "))

(DEFUN OS.E=HELP_Vp ()
  #+:mkrp.deutsch`(" VP      Zeigt das Beweisprotokoll auf dem Bildschirm.")
  #-:mkrp.deutsch`(" VP      Shows the proof protocol on the screen."))

(DEFUN OS.E=HELP_Hp ()
  #+:mkrp.deutsch`(" HP      Druckt das Beweisprotokoll auf einen Drucker.")
  #-:mkrp.deutsch`(" HP      Hardcopies the proof protocol on a line printer."))

(DEFUN OS.E=HELP_HC ()
  #+:mkrp.deutsch`(" HC <DATEI>    HARDCOPY DER BILDSCHIRMSITZUNG AUF <DATEI>."
		   " HC T          HARDCOPY AUF SYSTEMDATEI. DRUCKAUSGABE NACH LOGOFF."
		   " HC NIL        SCHALTET HC AUS UND SCHLIESST <DATEI>.")
  #-:mkrp.deutsch`(" HC <FILE>     Hardcopy of the terminal-session on <FILE>."
		   " HC T          Hardcopy on system file. Automatic output on printer."
		   " HC NIL        HC is switched off and <FILE> is closed."))

(DEFUN OS.E=HELP_HELP ()
  #+:mkrp.deutsch`(" H[ELP]  <KOM>      ERKLAERT DAS KOMMANDO <KOM>."
		   " H[ELP]             DRUCKT EINE LISTE ALLER VERFUEGBAREN OS-KOMMANDOS.")
  #-:mkrp.deutsch`(" H[ELP] <COM>       EXPLAINS THE COMMAND <COM>. "
		   " H[ELP]             PRINTS A LIST OF ALL AVAILABLE OS-COMMANDS. "))

(DEFUN OS.E=HELP_EXIT ()
  #+:mkrp.deutsch`(" EX[IT]        BEENDET DIE LISP-SITZUNG UND KEHRT ZUM BS2000 ZURUECK. ")
  #-:mkrp.deutsch`(" EX[IT]        TERMINATES LISP AND RETURNS TO THE BS2000 ."))

(DEFUN OS.E=HELP_LOGOFF ()
  #+:mkrp.deutsch`(" LO[GOFF]      BEENDET DIESE BILDSCHIRMSITZUNG. ")
  #-:mkrp.deutsch`(" LO[GOFF]      TERMINATES THIS TERMINAL-SESSION."))

(DEFUN OS.E=HELP_LISP ()
  #+:mkrp.deutsch`(" L[ISP]        RUFT INTERLISP AUF. RUECKKEHR MIT 'OK'.")
  #-:mkrp.deutsch`(" L[ISP]        CALLS INTERLISP. RETURN WITH 'OK'. "))

(DEFUN OS.E=HELP_OPTIONS ()
  #+:mkrp.deutsch`(" O[PTIONS]     RUFT DEN OPTIONEN-MODUL DER BEWEISSTEUERUNG AUF. DIESER"
		   "               BESITZT EIN EIGENES SELBSTERKLAERENDES BEFEHLSSYSTEM."
		   "               'OK'  VERLAESST DEN OPTIONEN-MODUL WIEDER. ")
  #-:mkrp.deutsch`(" O[PTIONS]     CALLS THE OPTION-MODULE OF THE PROOFCONTROL. IT HAS ITS"
		   "               OWN SELF-EXPLANATORY COMMAND SYSTEM."
		   "               'OK'  RETURNS TO WHERE YOU CAME FROM. "))

(DEFUN OS.E=HELP_INDUCTION ()
  #+:mkrp.deutsch`(" I[ND[UCTION]]    NOCH NICHT VOLLSTAENDIG IMPLEMENTIERT."
		   "                  RUFT DEN KARLSRUHER INDUKTIONS-BEWEISER AUF.")
  #-:mkrp.deutsch`(" I[ND[UCTION]]    NOT YET FULLY IMPLEMENTED. "
		   "                  CALLS THE KARLSRUHE INDUCTION-THEOREMPROVER."))

(DEFUN OS.E=HELP_CONSTRUCT ()
  #+:mkrp.deutsch`(" C[ONSTRUCT] [<PROBLEMDATEI> [<GRAPHDATEI> [<CODEDATEI> [<KOMMENTAR>]]]]"
		   "                 ERZEUGT EINE MENGE VON INITIALEN GRAPHEN AUS EINER "
		   "                 PROBLEMBESCHREIBUNG."
		   "  <PROBLEMDATEI> DATEI, VON DER DIE PROBLEMBESCHREIBUNG GELESEN WIRD. "
		   "                 BEI FEHLENDER ANGABE WIRD DIE ZULETZT ERZEUGTE BE- "
		   "                 NUTZT, FALLS EINE SOLCHE EXISTIERT. "
		   "  <GRAPHDATEI>   DATEI, AUF DIE DIE INITIALEN GRAPHEN GESCHRIEBEN "
		   "                 WERDEN. BEI FEHLENDER ANGABE WIRD EINE DATEI MIT "
		   "                 STANDARDNAMEN BENUTZT. "
		   "  <CODEDATEI>    DATEI, AUF DIE DIE ROH-DATEN FUER DIE PROTOKOLLIERUNG"
		   "                 GESCHRIEBEN WERDEN. "
		   "  <KOMMENTAR>    WIRD IN DAS BEWEISPROTOKOLL EINGEFUEGT."
		   "                 BEI ANGABE EINER ")
  #-:mkrp.deutsch`(" C[ONSTRUCT] [<PROBLEM FILE> [<GRAPH FILE> [<CODE FILE> [<COMMENT>]]]]"
		   "                 CREATES A SET OF INITIAL GRAPHS FROM A PROBLEM "
		   "                 DESCRIPTION."
		   "  <PROBLEM FILE> FILE CONTAINING THE PROBLEM DESCRIPTION. IF OMITTED, "
		   "                 THE LAST CREATED ONE IS USED, IF SUCH A FILE EXISTS. "
		   "  <GRAPH FILE>   FILE THE INITIAL GRAPHS ARE WRITTEN ON. IF OMITTED,"
		   "                 A STANDARD FILE NAME IS USED."
		   "  <CODE FILE>    FILE, WHERE THE RAW-DATAS FOR THE PROTOCOL ARE "
		   "                 WRITTEN ON. "
		   "  <COMMENT>      IS INSERTED INTO THE PROOF PROTOCOL."))

(DEFUN OS.E=HELP_CONSTRUCT.REFUTE ()
  #+:mkrp.deutsch`(" C[ONSTRUCT.]R[EFUTE] [<PROBLEMDATEI> [<GRAPHDATEI> [<CODEDATEI> "
		   "                      [<KOMMENTAR>]]]]"
		   "   GLEICHER EFFEKT WIE DIE FOLGE"
		   "     CONSTRUCT  <PROBLEMDATEI> <GRAPHDATEI> <CODEDATEI> <KOMMENTAR> "
		   "     REFUTE     <GRAPHDATEI> NIL <CODEDATEI> "
		   "                 BEI ANGABE EINER ")
  #-:mkrp.deutsch`(" C[ONSTRUCT.]R[EFUTE]    [<PROBLEM FILE> [<GRAPH FILE> [<CODE FILE> "
		   "                         [<COMMENT>]]]]"
		   "   has the same effect as the sequence"
		   "   CONSTRUCT  <PROBLEM FILE> <GRAPH FILE> <CODE FILE> <COMMENT>"
		   "   REFUTE     <GRAPH FILE> NIL <CODE FILE>"))

(DEFUN OS.E=HELP_CONSTRUCT.REFUTE.PROTOCOL ()
  #+:mkrp.deutsch`(" C[ONSTRUCT.REFUTE.]P[ROTOCOL]    [<PROBLEMDATEI> [<GRAPHDATEI> "
		   "                     [<CODEDATEI> [<LISTDATEI>"
		   "                     [<KOMMENTAR> [<STAPELDATEI> [<ATP-VERSION>]]]]]]]"
		   "   GLEICHER EFFEKT WIE DIE FOLGE"
		   "      CONSTRUCT  <PROBLEMDATEI> <GRAPHDATEI> <CODEDATEI> <KOMMENTAR>"
		   "      REFUTE     <GRAPHDATEI> NIL <CODEDATEI>"
		   "      PROTOCOL   <CODEDATEI> <LISTDATEI>"
		   "                 BEI ANGABE EINER "
		   " <STAPELDATEI>   WIRD EIN STAPELPROZESS ERZEUGT, DER DIE ANGEGEBENE "
		   " <ATP-VERSION>   BZW. DIE STANDARDVERSION BENUTZT.")
  #-:mkrp.deutsch`(" C[ONSTRUCT.REFUTE.]P[ROTOCOL] [<PROBLEM FILE> [<GRAPH FILE> "
		   "                               [<CODE FILE> [<LIST FILE> [<COMMENT>]]]]] "
		   "   SAME EFFECT AS THE SEQUENCE"
		   "      CONSTRUCT  <PROBLEM FILE> <GRAPH FILE> <CODE FILE> <COMMENT>"
		   "      REFUTE     <GRAPH FILE> NIL <CODE FILE>"
		   "      PROTOCOL   <CODE FILE> <LIST FILE>"))

(DEFUN OS.E=HELP_EDIT ()
  #+:mkrp.deutsch`(" E[DIT]    [<PROBLEMDATEI>]"
		   "                 ERSTELLT EINE PROBLEMBESCHREIBUNG."
		   "                 DAZU WIRD DER EDITOR FUER LOGISCHE FORMELN ZWEIMAL "
		   "                 AUFGERUFEN, ZUERST FUER DIE AXIOMFORMELN, DANACH FUER"
		   "                 DIE THEOREMFORMELN. "
		   "                 DIE ERZEUGTE PROBLEMBESCHREIBUNG WIRD AUF"
		   "  <PROBLEMDATEI> GESCHRIEBEN, FALLS ANGEGEBEN, ANSONSTEN AUF EINE "
		   "                 DATEI MIT STANDARDNAMEN. ")
  #-:mkrp.deutsch`(" E[DIT]    [<PROBLEM FILE>]"
		   "        creates a problem description. "
		   "        To that end the formula editor is called twice,"
		   "        first for the axiom formulas, second for the theorem "
		   "        formulas."
		   "        The problem description is written on"
		   "        <PROBLEM FILE>, if given, otherwise a standard file name is used."))

(DEFUN OS.E=HELP_EDIT.CONSTRUCT ()
  #+:mkrp.deutsch`(" E[DIT.]C[ONSTRUCT]    [<PROBLEMDATEI> [<GRAPHDATEI> [<CODEDATEI> "
		   "                             [<KOMMENTAR>]]]]"
		   "   GLEICHE WIRKUNG WIE DIE FOLGE"
		   "      EDIT       <PROBLEMDATEI> "
		   "      CONSTRUCT  <PROBLEMDATEI> <GRAPHDATEI> <CODEDATEI> <KOMMENTAR>")
  #-:mkrp.deutsch`(" E[DIT.]C[ONSTRUCT]    [<PROBLEM FILE> [<GRAPH FILE> [<CODE FILE> "
		   "                             [<COMMENT>]]]]"
		   "   SAME EFFECT AS THE SEQUENCE"
		   "      EDIT       <PROBLEM FILE> "
		   "      CONSTRUCT  <PROBLEM FILE> <GRAPH FILE> <CODE FILE> <COMMENT>"))

(DEFUN OS.E=HELP_EDIT.CONSTRUCT.REFUTE ()
  #+:mkrp.deutsch`(" E[DIT.CONSTRUCT.]R[EFUTE]    [<PROBLEMDATEI> [<GRAPHDATEI> "
		   "                        [<CODEDATEI>[<KOMMENTAR>]]]] "
		   "   GLEICHE WIRKUNG WIE DIE FOLGE"
		   "      EDIT       <PROBLEMDATEI> "
		   "      CONSTRUCT  <PROBLEMDATEI> <GRAPHDATEI> <CODEDATEI> <KOMMENTAR>"
		   "      REFUTE     <GRAPHDATEI> NIL <CODEDATEI>")
  #-:mkrp.deutsch`(" E[DIT.CONSTRUCT.]R[EFUTE]    [<PROBLEM FILE> [<GRAPH FILE> "
		   "                        [<CODE FILE> [<COMMENT>]]]]"
		   "   SAME EFFECT AS THE SEQUENCE"
		   "      EDIT       <PROBLEM FILE> "
		   "      CONSTRUCT  <PROBLEM FILE> <GRAPH FILE> <CODE FILE> <COMMENT>"
		   "      REFUTE     <GRAPH FILE> NIL <CODE FILE>"))

(DEFUN OS.E=HELP_EDIT.CONSTRUCT.REFUTE.PROTOCOL ()
  #+:mkrp.deutsch`(" E[DIT.CONSTRUCT.REFUTE.]P[ROTOCOL]    [<PROBLEMDATEI> [<GRAPHDATEI>"
		   "                [<CODEDATEI> [<LISTDATEI> [<KOMMENTAR>]]]]] "
		   "   GLEICHE WIRKUNG WIE DIE FOLGE"
		   "      EDIT       <PROBLEMDATEI> "
		   "      CONSTRUCT  <PROBLEMDATEI> <GRAPHDATEI> <CODEDATEI> <KOMMENTAR>"
		   "      REFUTE     <GRAPHDATEI> NIL <CODEDATEI>"
		   "      PROTOCOL   <CODEDATEI> <LISTDATEI>")
  #-:mkrp.deutsch`(" E[DIT.CONSTRUCT.REFUTE.]P[ROTOCOL]    [<PROBLEM FILE> [<GRAPH FILE>"
		   "                [<CODE FILE> [<LIST FILE> [<COMMENT>]]]]] "
		   "   SAME EFFECT AS THE SEQUENCE"
		   "      EDIT       <PROBLEM FILE> "
		   "      CONSTRUCT  <PROBLEM FILE> <GRAPH FILE> <CODE FILE> <KOMMENTAR>"
		   "      REFUTE     <GRAPH FILE> NIL <CODE FILE>"
		   "      PROTOCOL   <CODE FILE> <LIST FILE>"))

(DEFUN OS.E=HELP_FORMULA ()
  #+:mkrp.deutsch`(" F[ORMULA]    <AXIOMDATEI> <THEOREMDATEI> [<PROBLEMDATEI>]"
		   "                 ERZEUGT EINE PROBLEMBESCHREIBUNG AUS COMPILIERTEN"
		   "                 FORMELDATEIEN. "
		   "  <AXIOMDATEI>   UND "
		   "  <THEOREMDATEI> ENTHALTEN DIE COMPILIERTEN AXIOM- UND THEOREMFORMELN."
		   "                 DIE ERZEUGTE PROBLEMBESCHREIBUNG WIRD AUF"
		   "  <PROBLEMDATEI> GESCHRIEBEN, FALLS ANGEGEBEN, ANSONSTEN AUF EINE "
		   "                 DATEI MIT STANDARDNAMEN. ")
  #-:mkrp.deutsch`(" F[ORMULA]    <AXIOM FILE> <THEOREM FILE> [<PROBLEM FILE>]"
		   "                 CREATES A PROBLEM DESCRIPTION FROM COMPILED FORMULA"
		   "                 FILES. "
		   "  <AXIOM FILE>   AND "
		   "  <THEOREM FILE> CONTAIN THE COMPILED AXIOM AND THEOREM FORMULAS. "
		   "                 THE PROBLEM DESCRIPTION IS WRITTEN ON"
		   "  <PROBLEM FILE> , IF GIVEN, OTHERWISE A STANDARD FILE NAME IS USED."))

(DEFUN OS.E=HELP_FORMULA.CONSTRUCT ()
  #+:mkrp.deutsch`(" F[ORMULA.]C[ONSTRUCT]    <AXIOMDATEI> <THEOREMDATEI> [<PROBLEMDATEI> "
		   "                     [<GRAPHDATEI> [<CODEDATEI> [<KOMMENTAR>]]]]"
		   "   GLEICHE WIRKUNG WIE DIE FOLGE"
		   "      FORMULA    <AXIOMDATEI> <THEOREMDATEI> <PROBLEMDATEI> "
		   "      CONSTRUCT  <PROBLEMDATEI> <GRAPHDATEI> <CODEDATEI> <KOMMENTAR>")
  #-:mkrp.deutsch`(" F[ORMULA.]C[ONSTRUCT]    <AXIOM FILE> <THEOREM FILE> [<PROBLEM FILE> "
		   "                     [<GRAPH FILE> [<CODE FILE> [<COMMENT>]]]]"
		   "   SAME EFFECT AS  THE SEQUENCE "
		   "      FORMULA    <AXIOM FILE> <THEOREM FILE> <PROBLEM FILE> "
		   "      CONSTRUCT  <PROBLEM FILE> <GRAPH FILE> <CODE FILE> <COMMENT>"))

(DEFUN OS.E=HELP_FORMULA.CONSTRUCT.REFUTE ()
  #+:mkrp.deutsch`(" F[ORMULA.CONSTRUCT.]R[EFUTE]    <AXIOMDATEI> <THEOREMDATEI>"
		   "               [<PROBLEMDATEI> [<GRAPHDATEI> [<CODEDATEI> [<KOMMENTAR>]]]] "
		   "   GLEICHE WIRKUNG WIE DIE FOLGE"
		   "      FORMULA    <AXIOMDATEI> <THEOREMDATEI> <PROBLEMDATEI> "
		   "      CONSTRUCT  <PROBLEMDATEI> <GRAPHDATEI> <CODEDATEI> <KOMMENTAR>"
		   "      REFUTE     <GRAPHDATEI> NIL <CODEDATEI>"
		   "                 BEI ANGABE EINER ")
  #-:mkrp.deutsch`(" F[ORMULA.CONSTRUCT.]R[EFUTE] <AXIOM FILE> <THEOREM FILE>"
		   "                              [<PROBLEM FILE> [<GRAPH FILE> [<CODE FILE> [<COMMENT>]]]]"
		   "   SAME EFFECT AS THE SEQUENCE"
		   "      FORMULA    <AXIOM FILE> <THEOREM FILE> <PROBLEM FILE> "
		   "      CONSTRUCT  <PROBLEM FILE> <GRAPH FILE> <CODE FILE> <COMMENT>"
		   "      REFUTE     <GRAPH FILE> NIL <CODE FILE>"))

(DEFUN OS.E=HELP_FORMULA.CONSTRUCT.REFUTE.PROTOCOL ()
  #+:mkrp.deutsch`(" F[ORMULA.CONSTRUCT.REFUTE.]P[ROTOCOL]    <AXIOMDATEI> <THEOREMDATEI> "
		   "                          [<PROBLEMDATEI> [<GRAPHDATEI> [<CODEDATEI>"
		   "                          [<LISTDATEI> [<KOMMENTAR>"
		   "                          [<STAPELDATEI> [<ATP-VERSION>]]]]]]]"
		   "   GLEICHE WIRKUNG WIE DIE FOLGE"
		   "      FORMULA    <AXIOMDATEI> <THEOREMDATEI> <PROBLEMDATEI> "
		   "      CONSTRUCT  <PROBLEMDATEI> <GRAPHDATEI> <CODEDATEI> <KOMMENTAR>"
		   "      REFUTE     <GRAPHDATEI> NIL <CODEDATEI>"
		   "      PROTOCOL   <CODEDATEI> <LISTDATEI>"
		   "                 BEI ANGABE EINER "
		   " <STAPELDATEI>   WIRD EIN STAPELPROZESS ERZEUGT, DER DIE ANGEGEBENE "
		   " <ATP-VERSION>   BZW. DIE STANDARDVERSION BENUTZT.")
  #-:mkrp.deutsch`(" F[ORMULA.CONSTRUCT.REFUTE.]P[ROTOCOL] <AXIOM FILE> <THEOREM FILE> "
		   "                                       [<PROBLEM FILE> [<GRAPH FILE> [<CODE FILE>"
		   "                                       [<LIST FILE> [<COMMENT>]]]]] "
		   "   SAME EFFECT AS THE SEQUENCE"
		   "      FORMULA    <AXIOM FILE> <THEOREM FILE> <PROBLEM FILE> "
		   "      CONSTRUCT  <PROBLEM FILE> <GRAPH FILE> <CODE FILE> <COMMENT>"
		   "      REFUTE     <GRAPH FILE> NIL <CODE FILE>"
		   "      PROTOCOL   <CODE FILE> <LIST FILE>"))

(DEFUN OS.E=HELP_REFUTE ()
  #+:mkrp.deutsch`(" R[EFUTE]    [<GRAPHDATEI> [<NUMMER> [<CODEDATEI> [<STAPELDATEI>"
		   "                   [<ATP-VERSION>]]]]]"
		   "                 WIDERLEGT EINE MENGE VON INITIALEN GRAPHEN UND "
		   "                 ERZEUGT ROHDATEN FUER DIE PROTOKOLLIERUNG. "
		   "  <GRAPHDATEI>   DATEI, VON DER DIE INITIALEN GRAPHEN GELESEN WERDEN. "
		   "                 BEI FEHLENDER ANGABE WIRD DIE ZULETZT ERZEUGTE BE- "
		   "                 NUTZT, FALLS EINE SOLCHE EXISTIERT. "
		   "                 BEI ANGEGEBENER"
		   "  <NUMMER>       WIRD NUR DER ENTSPRECHENDE GRAPH WIDERLEGT, ANSONSTEN"
		   "                 ALLE GRAPHEN AUF DER DATEI. "
		   "  <CODEDATEI>    DATEI, AUF DIE DIE ROH-DATEN FUER DIE PROTOKOLLIERUNG"
		   "                 GESCHRIEBEN WERDEN. "
		   "                 BEI ANGABE EINER "
		   "  <STAPELDATEI>  WIRD EIN STAPELPROZESS ERZEUGT, DER DIE ANGEGEBENE "
		   "  <ATP-VERSION>  BZW. DIE STANDARDVERSION BENUTZT.")
  #-:mkrp.deutsch`(" R[EFUTE] [<GRAPH FILE> [<NUMBER> [<CODE FILE>]]] "
		   "                 REFUTES A SET OF INITIAL GRAPHS AND CREATES RAW DATA "
		   "                 FOR THE PROTOCOL."
		   "  <GRAPH FILE>   FILE CONTAINING THE INITIAL GRAPHS. IF OMITTED, THE"
		   "                 LAST CREATED ONE IS USED, IF SUCH A FILE EXISTS. IF"
		   "  <NUMBER>       IS GIVEN, ONLY THE RESPECTIVE GRAPH IS REFUTED,"
		   "                 OTHERWISE ALL GRAPHS ON THE FILE."
		   "  <CODE FILE>    FILE, WHERE THE RAW-DATAS FOR THE PROTOCOL ARE "
		   "                 WRITTEN ON. "))

(DEFUN OS.E=HELP_REFUTE.PROTOCOL ()
  #+:mkrp.deutsch`(" R[EFUTE.]P[ROTOCOL]   [<GRAPHDATEI> [<NUMMER> [<CODEDATEI> "
		   "                       [<LISTDATEI> [<STAPELDAEI> [<ATP-VERSION>]]]]]]"
		   "   GLEICHE WIRKUNG WIE DIE FOLGE"
		   "      REFUTE     <GRAPHDATEI> <NUMMER> <CODEDATEI>"
		   "      PROTOCOL   <CODEDATEI> <LISTDATEI>"
		   "                 BEI ANGABE EINER "
		   " <STAPELDATEI>   WIRD EIN STAPELPROZESS ERZEUGT, DER DIE ANGEGEBENE "
		   " <ATP-VERSION>   BZW. DIE STANDARDVERSION BENUTZT.")
  #-:mkrp.deutsch`(" R[EFUTE.]P[ROTOCOL] [<GRAPH FILE> [<NUMBER> [<CODE FILE> "
		   "                     [<LIST FILE>]]]]"
		   "   SAME EFFECT AS THE SEQUENCE"
		   "      REFUTE     <GRAPH FILE> <NUMBER> <CODE FILE>"
		   "      PROTOCOL   <CODE FILE> <LIST FILE>"))

(DEFUN OS.E=HELP_PROTOCOL ()
  #+:mkrp.deutsch`(" P[ROTOCOL] [<CODEDATEI> [<LISTDATEI> [<STAPELDATEI> [<ATP-VERSION>]]]]"
		   "                 ERZEUGT EIN BEWEISPROTOKOLL AUS ROHDATEN."
		   "  <CODE-DATEI>   DATEI, VON DER DIE ROHDATEN GELESEN WERDEN."
		   "  <LIST-DATEI>   DATEI, AUF DIE DIE AUFBEREITETEN DATEN FUER DIE"
		   "                 PROTOKOLLIERUNG GESCHRIEBEN WERDEN. ")
  #-:mkrp.deutsch`(" P[ROTOCOL]  [<CODE FILE> [<LIST FILE> [<ATP VERSION>]]]]"
		   "        creates a proof protocol from raw data."
		   "        <CODE FILE> is the file containing the raw data."
		   "        <LIST FILE> is the file, where the processed datas for protocol are "
		   "        written on. "))

(DEFUN OS.E=HELP_COMMAND.NOT.IMPLEMENTED (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil "  KOMMANDO ~A IST NICHT VERFUEGBAR." FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil "  COMMAND ~A IS NOT AVAILABLE." FIRST.ARG)))

(defun os.e=help_define.directory (arg)
  (declare (ignore arg))
  #+:mkrp.deutsch`("D[EFINE.]D[IRECTORY] DIRECTORY"
		   " Setzen eines Defaultdirectories fuer alle anzulegenden Dateien."
		   " Beispiel: >benutzer>beispiele> .")
  #-:mkrp.deutsch`("D[EFINE.]D[IRECTORY] DIRECTORY"
		   " Defines a default directory for all files."
		   " Example: >user>examples> ."))

(defun os.e=help_define.example.name (arg)
  (declare (ignore arg))
  #+:mkrp.deutsch`("D[EFINE.]E[EXAMPLE.NAME] NAME"
		   " Setzen eines Defaultnamen fuer alle anzulegenden Dateien."
		   " Der Typ kann fuer die verschiedenen Dateien unterschiedlich sein.")
  #-:mkrp.deutsch`("D[EFINE.]E[XAMPLE.NAME] NAME"
		   " Defines a default name for all files."
		   " Example for a problem file: name.problem."))

(defun os.e=help_select.example (arg)
  (declare (ignore arg))
  #+:mkrp.deutsch`("S[ELECT.]E[EXAMPLE] NAME"
		   " Setzen eines Defaultnamen fuer alle anzulegenden Dateien."
		   " Der Typ kann fuer die verschiedenen Dateien unterschiedlich sein.")
  #-:mkrp.deutsch`("S[ELECT.]E[EXAMPLE] NAME"
		   " Defines a default name for all files."
		   " Example for a problem file: name.problem."))

(DEFUN OS.E=HELP.EXPLANATION_EXPLAIN.HELP ()
  #+:mkrp.deutsch`(" In diesem System stehen folgende Kommandos zur Verfuegung:"
		   ""
		   "   V     H[ELP]   EX[IT]    O[PTIONS]          D[EFINE.]E[XAMPLE.NAME]"
		   "   HC    L[ISP]   LO[GOFF]  V[IEW.]P[ROTOCOL]  D[EFINE.]D[IRECTORY] "
		   "   H[ARDCOPY.]P[ROTOCOL]"
		   "                                EP"
		   "                                FP"
		   "                        ER              CP"
		   "                        FR"
		   "                EC              CR              RP"
		   "                FC"
		   "     E[DIT]        C[ONSTRUCT]        R[EFUTE]         P[ROTOCOL] "
		   "     F[ORMULA]"
		   ""
		   " H[ELP]  <KOM>       DRUCKT EINE ERKLAERUNG DES KOMMANDOS <KOM>.")
  #-:mkrp.deutsch`(" The following commands are available in this system:"
		   ""
		   "   V     H[ELP]   EX[IT]    O[PTIONS]          D[EFINE.]E[XAMPLE.NAME]"
		   "   HC    L[ISP]   LO[GOFF]  V[IEW.]P[ROTOCOL]  D[EFINE.]D[IRECTORY] "
		   "   H[ARDCOPY.]P[ROTOCOL]"
		   "                                EP"
		   "                                FP"
		   "                        ER              CP"
		   "                        FR"
		   "                EC              CR              RP"
		   "                FC"
		   "     E[DIT]        C[ONSTRUCT]        R[EFUTE]         P[ROTOCOL] "
		   "     F[ORMULA]"
		   ""
		   " H[ELP]  <COM>      prints an explanation of the command <COM>. "))

(DEFUN OS.E=INDUCTION_ENTER.THEORY ()
  #+:mkrp.deutsch`("KARLSRUHER INDUKTIONSBEWEISER "
		   "BITTE THEORIE EINGEBEN:")
  #-:mkrp.deutsch`("THIS IS THE KARLSRUHE INDUCTION-THEOREMPROVER "
		   "PLEASE ENTER A THEORY:"))

(DEFUN OS.E=ERROR.illegal.FILE (FIRST.ARG second.arg third.arg)
  #+:mkrp.deutsch`(,(format nil "~A ist kein korrekter Dateiname fuer ~Adatei in Kommando ~A. " FIRST.ARG third.arg second.arg))
  #-:mkrp.deutsch`(,(format nil "~A is no correct file name for a ~A file in command ~A. " FIRST.ARG third.arg second.arg)))

(DEFUN OS.E=ERROR.NO.FILE.EXISTS (FIRST.ARG second.arg third.arg)
  #+:mkrp.deutsch`(,(format nil "~Adatei ~A existiert nicht in Kommando ~A. " third.arg FIRST.ARG second.arg))
  #-:mkrp.deutsch`(,(format nil "~A-file ~A does not exist in command ~A. " third.arg FIRST.ARG second.arg)))

(DEFUN OS.E=ERROR.ILLEGAL.ATP.VERSION (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ~A ist eine unzulaessige ATP-Version. " FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil " ~A is an illegal ATP-version. " FIRST.ARG)))

(DEFUN OS.E=CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 3 PARAMETER ZULAESSIG:"
		   " C[ONSTRUCT]     [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>]]] ")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 4 PARAMETERS ALLOWED AT MOST: "
		   " C[ONSTRUCT]     [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>]]]"))

(DEFUN OS.E=CONSTRUCT.PROC_PROOF.COMMENT (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil "~v,,,'*A" (mkrp-linelength) "")
		   ,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil " CONSTRUCT:  ~A" first.arg)
		   ,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil "~v,,,'*A" (mkrp-linelength) "")
		   ,(format nil "~vA"      (mkrp-linelength) ""))
  #-:mkrp.deutsch`(,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil "~v,,,'*A" (mkrp-linelength) "")
		   ,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil " CONSTRUCT:  ~A" first.arg)
		   ,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil "~v,,,'*A" (mkrp-linelength) "")
		   ,(format nil "~vA"      (mkrp-linelength) "")))

(DEFUN OS.E=CONSTRUCT.PROC_START.MESSAGE ()
  (list (os.e=full.line #\*
			#+:mkrp.deutsch"Construct: Aufbau der initialen Graphen."
			#-:mkrp.deutsch"Construct: Construction of the initial graphs.")))

(DEFUN OS.E=CONSTRUCT.PROC_THEOREM.PROVED ()
  (list (os.e=full.line #\*
			#+:mkrp.deutsch"Construct: Theorem bewiesen."
			#-:mkrp.deutsch"Construct: Theorem proved.")))

(DEFUN OS.E=CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 3 PARAMETER ZULAESSIG:"
		   " C[ONSTRUCT.]R[EFUTE]   [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>"
		   "                        [<BATCH.FILE> [<ATP.VERSION>]]]]] ")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 3 PARAMETERS ALLOWED AT MOST: "
		   " C[ONSTRUCT.]R[EFUTE]   [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>]]] "))

(DEFUN OS.E=CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=NO.PROBLEM.FILE.EXISTS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil "PROBLEM-DATEI ~A EXISTIERT NICHT. " FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil "PROBLEM-FILE ~A DOES NOT EXIST. " FIRST.ARG)))

(DEFUN OS.E=CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 4 PARAMETER ZULAESSIG: "
		   " C[ONSTRUCT.REFUTE.]P[ROTOCOL]     [<PROBLEM.FILE> [<GRAPH.FILE> "
		   "       [<CODE.FILE> [<LIST.FILE>]]]]")
  #-:mkrp.deutsch`(,(format nil " Too many arguments: ~A" FIRST.ARG)
		   " 4 parameters allowed at most:"
		   " C[ONSTRUCT.REFUTE.]P[ROTOCOL]     [<PROBLEM.FILE> [<GRAPH.FILE> "
		   "                                   [<CODE.FILE> [<LIST.FILE>]]]]"))

(DEFUN OS.E=EDIT.CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 3 PARAMETER ERLAUBT:"
		   " E [DIT.]C[ONSTRUCT]    [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>]]]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 3 PARAMETERS ALLOWED AT MOST:"
		   " E[DIT.]C[ONSTRUCT]   [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>]]]"))

(DEFUN OS.E=EDIT.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 3 PARAMETER ERLAUBT:"
		   " E[DIT.CONSTRUCT.]R[EFUTE]     [<PROBLEM.FILE> [<GRAPH.FILE> "
		   "                               [<CODE.FILE>]]]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 3 PARAMETERS ALLOWED AT MOST:"
		   " E[DIT.CONSTRUCT.]R[EFUTE]     [<PROBLEM.FILE> [<GRAPH.FILE> "
		   "                               [<CODE.FILE>]]]"))

(DEFUN OS.E=EDIT.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 4 PARAMETER ERLAUBT:"
		   " E[DIT.CONSTRUCT.REFUTE.]P[ROTOCOL]     [<PROBLEM.FILE> [<GRAPH.FILE>"
		   "                                        [<CODE.FILE> [<LIST.FILE>]]]]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 4 PARAMETERS ALLOWED AT MOST:"
		   " E[DIT.CONSTRUCT.REFUTE.]P[ROTOCOL]     [<PROBLEM.FILE> [<GRAPH.FILE>"
		   "                                        [<CODE.FILE> [<LIST.FILE>]]]]"))

(DEFUN OS.E=EDIT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS EIN PARAMETER ERLAUBT: "
		   " E[DIT]    [<PROBLEM.FILE>]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 1 PARAMETER ALLOWED AT MOST:"
		   " E[DIT]    [<PROBLEM.FILE>]"))

(DEFUN OS.E=EDIT.PROC_PROOF.COMMENT (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil "Edit:     Axiome und Theoreme editiert am ~A " first.arg))
  #-:mkrp.deutsch`(,(format nil "Edit:     Axioms and Theorems edited: ~A " first.arg)))

(DEFUN OS.E=EDIT.PROC_ERROR=NOTHING.TO.PROVE ()
  #+:mkrp.deutsch`("Edit:     Es sind weder Axiome noch Theoreme vorhanden."
		   "          Kommando EDIT ignoriert.")
  #-:mkrp.deutsch`("Edit:     There are neither axioms nor theorems."
		   "          Command EDIT ignored."))

(DEFUN OS.E=EDIT.PROC_START.EDIT.AXIOMS ()
  (list (os.e=full.line #\*
			#+:mkrp.deutsch"Edit: Aufruf des Formeleditors. Bitte Axiomformeln zusammenstellen."
			#-:mkrp.deutsch"Edit: Calling the formula editor. Please edit the axiom formulas.")))

(DEFUN OS.E=EDIT.PROC_START.EDIT.THEOREMS ()
  (list (os.e=full.line #\*
			#+:mkrp.deutsch"Edit: Aufruf des Formeleditors. Bitte Theoremformeln zusammenstellen."
			#-:mkrp.deutsch"Edit: Calling the formula editor. Please edit the theorem formulas.")))

(DEFUN OS.E=EDIT.PROC_WARNING=NO.AXIOMS ()
  (list (os.e=full.line #\*
			#+:mkrp.deutsch"Edit: Es sind keine Axiomformeln vorhanden!"
			#-:mkrp.deutsch"Edit: There are no axiom formulas!")))

(DEFUN OS.E=EDIT.PROC_WARNING=NO.THEOREMS ()
  (list (os.e=full.line #\*
			#+:mkrp.deutsch"Edit: Es sind keine Theoremformeln vorhanden!"
			#-:mkrp.deutsch"Edit: There are no theorem formulas!")))

(DEFUN OS.E=FORMULA.CONSTRUCT.INPUT.CONTROL_ERROR=MISSING.INPUT ()
  #+:mkrp.deutsch`(" MINDESTENS 2 PARAMETER NOTWENDIG: "
		   " F[ORMULA.]C[ONSTRUCT]     <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE> "
		   "                           [<GRAPH.FILE> [<CODE.FILE>]]]")
  #-:mkrp.deutsch`(" 2 PARAMETERS NECESSARY AT LEAST:"
		   " F[ORMULA.]C[ONSTRUCT]     <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE> "
		   "                           [<GRAPH.FILE> [<CODE.FILE>]]]"))

(DEFUN OS.E=FORMULA.CONSTRUCT.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 5 PARAMETER ERLAUBT:"
		   " F[ORMULA.]C[ONSTRUCT]     <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE> "
		   "                           [<GRAPH.FILE> [<CODE.FILE>]]]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 5 PARAMETERS ALLOWED AT MOST:"
		   " F[ORMULA.]C[ONSTRUCT]     <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE> "
		   "                           [<GRAPH.FILE> [<CODE.FILE>]]]"))

(DEFUN OS.E=FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=MISSING.INPUT ()
  #+:mkrp.deutsch`(" MINDESTENS 2 PARAMETER NOTWENDIG: "
		   " F[ORMULA.CONSTRUCT.]R[EFUTE]     <AXIOM.FILE> <THEOREM.FILE>"
		   "                                  [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>]]]")
  #-:mkrp.deutsch`(" 2 PARAMETERS NECESSARY AT LEAST:"
		   " F[ORMULA.CONSTRUCT.]R[EFUTE]     <AXIOM.FILE> <THEOREM.FILE>"
		   "                                  [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>]]]"))

(DEFUN OS.E=FORMULA.CONSTRUCT.REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 5 PARAMETER ERLAUBT:"
		   " F[ORMULA.CONSTRUCT.]R[EFUTE]     <AXIOM.FILE> <THEOREM.FILE>"
		   "                       [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>]]]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 5 PARAMETERS ALLOWED AT MOST:"
		   " F[ORMULA.CONSTRUCT.]R[EFUTE]     <AXIOM.FILE> <THEOREM.FILE>"
		   "                                  [<PROBLEM.FILE> [<GRAPH.FILE> [<PROOF.COMMENT>]]]"))

(DEFUN OS.E=FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=MISSING.INPUT ()
  #+:mkrp.deutsch`(" MINDESTENS 2 PARAMETER NOTWENDIG: "
		   " F[ORMULA.CONSTRUCT.REFUTE.]P[ROTOCOL]    <AXIOM.FILE> <THEOREM.FILE>"
		   "                       [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>"
		   "                       [<LIST.FILE>]]]] ")
  #-:mkrp.deutsch`(" 2 PARAMETERS NECESSARY AT LEAST:"
		   " F[ORMULA.CONSTRUCT.REFUTE.]P[ROTOCOL]    <AXIOM.FILE> <THEOREM.FILE>"
		   "                       [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>"
		   "                       [<LIST.FILE>]]]] "))

(DEFUN OS.E=FORMULA.CONSTRUCT.REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 6 PARAMETER ZULAESSIG: "
		   " F[ORMULA.CONSTRUCT.REFUTE.]P[ROTOCOL]    <AXIOM.FILE> <THEOREM.FILE>"
		   "                       [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>"
		   "                       [<LIST.FILE>]]]] ")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 6 PARAMETERS ALLOWED AT MOST:"
		   " F[ORMULA.CONSTRUCT.REFUTE.]P[ROTOCOL]    <AXIOM.FILE> <THEOREM.FILE>"
		   "                       [<PROBLEM.FILE> [<GRAPH.FILE> [<CODE.FILE>"
		   "                       [<LIST.FILE>]]]] "))

(DEFUN OS.E=FORMULA.INPUT.CONTROL_ERROR=MISSING.INPUT ()
  #+:mkrp.deutsch`(" MINDESTENS 2 PARAMETER NOTWENDIG: "
		   " F[ORMULA]     <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE>]")
  #-:mkrp.deutsch`(" 2 PARAMETERS ARE NECESSARY AT LEAST:"
		   " F[ORMULA]     <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE>]"))

(DEFUN OS.E=FORMULA.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 3 PARAMETER ZULAESSIG: "
		   " F[ORMULA]     <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE>]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 3 PARAMETERS ALLOWED AT MOST:"
		   " F[ORMULA]     <AXIOM.FILE> <THEOREM.FILE> [<PROBLEM.FILE>]"))

(DEFUN OS.E=FORMULA.PROC_PROOF.COMMENT (first.arg second.arg third.arg)
  #+:mkrp.deutsch`(,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil "~v,,,'*A" (mkrp-linelength) "")
		   ,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil " FORMULA:  Datum: ~A" first.arg)
		   "           Axiome und Theoreme fuer diesen Beweis wurden"
		   "           von folgenden Dateien gelesen:"
		   ,(format nil "              Axiom-Datei: ~A" second.arg)
		   ,(format nil "            Theorem-Datei: ~A" third.arg)
		   ,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil "~v,,,'*A" (mkrp-linelength) ""))
  #-:mkrp.deutsch`(,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil "~v,,,'*A" (mkrp-linelength) "")
		   ,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil " FORMULA:  Date: ~A" first.arg)
		   "           Axioms und Theorems for this proof have been read from"
		   ,(format nil "              Axiom-File: ~A" second.arg)
		   ,(format nil "            Theorem-File: ~A" third.arg)
		   ,(format nil "~vA"      (mkrp-linelength) "")
		   ,(format nil "~v,,,'*A" (mkrp-linelength) "")))

(DEFUN OS.E=PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 2 PARAMETER ERLAUBT: "
		   " P[ROTOCOL]     [<CODE.FILE> [<LIST.FILE>]]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 2 PARAMETERS ALLOWED AT MOST:"
		   " P[ROTOCOL]     [<CODE.FILE> [<LIST.FILE>]]"))

(DEFUN OS.E=PROTOCOL.PROC_LIST.FILE.MESSAGE (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil "PROTOCOL: DAS PROTOKOLL WURDE AUF DER DATEI ~A  ERZEUGT. " FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil "Protocol: The protocol was created on file ~A." FIRST.ARG)))

(DEFUN OS.E=REFUTE.INPUT.CONTROL_ERROR=ILLEGAL.SPLITPART.ID (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ~A IST EIN UNZULAESSIGER SPLITPART-BEZEICHNER. " FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil " ~A IS AN ILLEGAL SPLITPART IDENTIFIER. " FIRST.ARG)))

(DEFUN OS.E=REFUTE.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 3 PARAMETER ZULAEESIG:"
		   " R[EFUTE]     [<GRAPH.FILE> [<CODE.FILE> [<SPLITPART.ID>]]]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 3 PARAMETERS ALLOWED AT MOST: "
		   " R[EFUTE]     [<GRAPH.FILE> [<CODE.FILE> [<SPLITPART.ID>]]]"))

(DEFUN OS.E=REFUTE.PROC_THEOREM.NOT.PROVED ()
  (list (os.e=full.line #\#
			#+:mkrp.deutsch"Refute: Theorem nicht bewiesen."
			#-:mkrp.deutsch"Refute: Theorem not proved.")))

(DEFUN OS.E=REFUTE.PROC_THEOREM.PROVED ()
  (list (os.e=full.line #\*
			#+:mkrp.deutsch"Refute: Theorem bewiesen."
			#-:mkrp.deutsch"Refute: Theorem proved.")))

(DEFUN OS.E=REFUTE.PROC_START.MESSAGE ()
  (list (os.e=full.line #\*
			#+:mkrp.deutsch"Refute: Beginn der Widerlegung."
			#-:mkrp.deutsch"Refute: Start of refutation.")))

(defun os.e=full.line (char string)
  (replace (make-string (mkrp-linelength) :initial-element char) (format nil " ~A " STRING) :start1 5))

(defun os.e=full.lines (char &rest strings)
  (mapcar #'(lambda (string) (list (os.e=full.line char string))) strings))

(DEFUN OS.E=REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=ILLEGAL.SPLITPART.ID (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ~A ist ein unzulaessiger Splitpart-Bezeichner. " FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil " ~A is an illegal splitpart identifier. " FIRST.ARG)))

(DEFUN OS.E=REFUTE.PROTOCOL.INPUT.CONTROL_ERROR=TOO.MANY.ARGS (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " ZU VIELE ARGUMENTE: ~A" FIRST.ARG)
		   " HOECHSTENS 4 PARAMETER ZULAESSIG:"
		   " R[EFUTE.]P[ROTOCOL]  [<GRAPH.FILE> [<CODE.FILE> [<LIST.FILE>"
		   "                      [<SPLITPART.ID>]]]]")
  #-:mkrp.deutsch`(,(format nil " TOO MANY ARGUMENTS: ~A" FIRST.ARG)
		   " 4 PARAMETERS ALLOWED AT MOST: "
		   " R[EFUTE.]P[ROTOCOL]  [<GRAPH.FILE> [<CODE.FILE> [<LIST.FILE>"
		   "                      [<SPLITPART.ID>]]]]"))

(DEFUN OS.E=OPTION_ENTER.MODULE ()
  #+:mkrp.deutsch`("AUfruf des OPTIONEN-Moduls. H[ELP] erklaert die zulaessigen Befehle. ")
  #-:mkrp.deutsch`("This is the OPTIONS-module. For assistance type H[ELP]."))

(DEFUN OS.E=OPTION_ILLEGAL.OPTION.VALUE (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " UNZULAESSIGER WERT FUER: ~A -- KOMMANDO IGNORIERT. " FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil "ILLEGAL VALUE FOR: ~A -- COMMAND IGNORED. " FIRST.ARG)))

(DEFUN OS.E=OPTION_GET.HELP (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil "  SCHALTER ~A EXISTIERT NICHT." FIRST.ARG)
		   " H[ELP] ERKLAERT DIE ERLAUBTEN BEFEHLE.")
  #-:mkrp.deutsch`(,(format nil " THERE IS NO SWITCH ~A." FIRST.ARG)
		   " FOR ASSISTANCE TYPE H[ELP]."))

(DEFUN OS.E=OPT.PRINT_GET.HELP (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil " BEREICH ~A EXISTIERT NICHT." FIRST.ARG)
		   " H[ELP] ERKLAERT DIE ERLAUBTEN BEFEHLE.")
  #-:mkrp.deutsch`(,(format nil " AREA ~A DOES NOT EXIST." FIRST.ARG)
		   " TYPE H[ELP] FOR ASSISTANCE."))

(DEFUN OS.E=OPT.READ_FILENAME? ()
  #+:mkrp.deutsch`("Von welcher Datei sollen die Optionen gelesen werden? ")
  #-:mkrp.deutsch`("Please enter the name of the options-file: "))

(DEFUN OS.E=OPT.READ_OPTIONS.SET ()
  #+:mkrp.deutsch`("Die neuen Optionen sind gesetzt. ")
  #-:mkrp.deutsch`("The new options are set. "))

(DEFUN OS.E=OPT.READ_INCONSISTENT.OPTIONS ()
  #+:mkrp.deutsch`("DIE NEUEN OPTIONEN SIND INKONSISTENT --- BITTE DIE DATEI UEBERPRUEFEN! ")
  #-:mkrp.deutsch`("The new options are inconsistent - Please check your file! "))

(DEFUN OS.E=OPT.WRITE_FILENAME? ()
  #+:mkrp.deutsch`("Auf welche Datei sollen die Optionen geschrieben werden? ")
  #-:mkrp.deutsch`("File name to print options?"))

(DEFUN OS.E=OPT.PRETTYPRINT_FILENAME? ()
  #+:mkrp.deutsch`("Auf welche Datei sollen die Options-Erklaerungen Geschrieben werden? ")
  #-:mkrp.deutsch`("File name to print options explanations?"))

(DEFUN OS.E=OPT.PRETTYPRINT_HEADLINE ()
  #+:mkrp.deutsch`("LISTE ALLER OPTIONS-BEREICHE MIT DEN ZUGEHOERIGEN OPTIONEN:"
		   "  FUER JEDE OPTION IST IHR STANDARDWERT SOWIE EINE ERKLAERUNG DER ZU-"
		   "  LAESSIGEN WERTE UND IHRER WIRKUNG BEI DER BEWEISSTEUEREUNG ANGEGEBEN.")
  #-:mkrp.deutsch`("List Of All Option-Areas And Their Options"
		   "  For each option the default value ,the allowed option-values and an"
		   "  explanation of their effects on the proof-search is printed below. "))

(DEFUN OS.E=OPT.PRETTYPRINT_DEFAULTVAL (FIRST.ARG)
  #+:mkrp.deutsch`(,(format nil "Standardwert: ~A"  FIRST.ARG))
  #-:mkrp.deutsch`(,(format nil "Default value: ~A"  FIRST.ARG)))

(DEFUN OS.E=OPT_ERROR=ILLEGAL.FILENAME (FIRST.ARG  second.arg)
  #+:mkrp.deutsch`(,(format nil "  ~A IST EIN UNZULAESSIGER DATEINAME." FIRST.ARG)
		   ,(format nil "  KOMMANDO ~A IGNORIERT." SECOND.ARG))
  #-:mkrp.deutsch`(,(format nil "  ~A IS AN ILLEGAL FILE NAME. " FIRST.ARG)
		   ,(format nil "  COMMAND ~A IGNORED." SECOND.ARG)))

(DEFUN OS.E=OPT_OUTPUT.END ()
  #+:mkrp.deutsch`("Ausgabe abgeschlossen.")
  #-:mkrp.deutsch`("Output ended. "))

(DEFUN OS.E=OPT.HELP_HELP ()
  #+:mkrp.deutsch`(" H[ELP] <KOM>   ERKLAERT DAS KOMMANDO <KOM>."
		   " H[ELP]         DRUCKT EINE LISTE ALLER VERFUEGBAREN OPTIONS-KOMMANDOS.")
  #-:mkrp.deutsch`(" H[ELP] <COM>   Explains the command <COM>."
		   " H[ELP]         Prints a list of all available options-commands. "))

(DEFUN OS.E=OPT.HELP_PRINT ()
  #+:mkrp.deutsch`(" P[RINT] <BER>  DRUCKT DIE OPTIONEN DES BEREICHS <BER> UND IHRE"
		   "                AKTUELLEN WERTE. ")
  #-:mkrp.deutsch`(" P[RINT] <AREA>  PRINTS THE OPTIONS OF <AREA> AND THEIR CURRENT VALUES."))

(DEFUN OS.E=OPT.HELP_PPRINT ()
  #+:mkrp.deutsch`(" PP[RINT] <DAT>  Druckt alle Bereiche, ihre Optionen und Standardwerte "
		   "                 sowie eine detailierte Erklaerung auf die Datei <DAT>.")
  #-:mkrp.deutsch`(" PP[RINT] <FILE>  Prints all areas, their options and default values "
		   "                  together with a detailed explanation on <FILE>."))

(DEFUN OS.E=OPT.HELP_READ ()
  #+:mkrp.deutsch`(" R[EAD] <DATEI>  LIEST DIE OPTIONSWERTE VON <DATEI>. DIE DATEI <DATEI> "
		   "                 MUSS MIT  W[RITE] <DATEI>  ERSTELLT WORDEN SEIN.")
  #-:mkrp.deutsch`(" R[EAD] <FILE>  READS THE OPTION-VALUES FROM <FILE>. THIS FILE HAS TO"
		   "                BE CREATED BY A  W[RITE] <FILE>  COMMAND."))

(DEFUN OS.E=OPT.HELP_WRITE ()
  #+:mkrp.deutsch`(" W[RITE] <DATEI>  SCHREIBT ALLE OPTIONEN UND IHRE AKTUELLEN WERTE AUF"
		   "                  DIE DATEI <DATEI>.")
  #-:mkrp.deutsch`(" W[RITE] <FILE>  Writes all options and their current values on <FILE>."))

(DEFUN OS.E=OPT.HELP_LISP ()
  #+:mkrp.deutsch`(" L[ISP]         RUFT INTERLISP AUF. RUECKKEHR MIT 'OK'.")
  #-:mkrp.deutsch`(" L[ISP]         CALLS INTERLISP. RETURN WITH 'OK'. "))

(DEFUN OS.E=OPT.HELP_OK ()
  #+:mkrp.deutsch`(" OK           verlaesst den Optionen-Modul. Alle Optionen sind gesetzt.")
  #-:mkrp.deutsch`(" OK           leaves the options-module. All options are set."))

(DEFUN OS.E=OPT.HELP_V ()
  #+:mkrp.deutsch`(" V T/J/JA/OK     schaltet die manuelle Bildschirmsteuerung ein. "
		   " V NIL/N/NEIN    schaltet sie wieder aus.")
  #-:mkrp.deutsch`(" V T/Y/YES/OK    turns the manual terminalcontrol on."
		   " V NIL/N/NO      turns it off again."))

(DEFUN OS.E=OPT.HELP_AREA.ABBREVIATION ()
  #+:mkrp.deutsch`("DIE BEREICHSABKUERZUNG <BER> WIRKT ALS BEFEHL WIE  P[RINT] <BER>  .")
  #-:mkrp.deutsch`("The area abbreviation <AREA> as command is the same as  P[RINT] <AREA>."))

(DEFUN OS.E=OPT.HELP_ALL.COMMANDS ()
  #+:mkrp.deutsch`("FOLGENDE BEFEHLE STEHEN ZUR VERFUEGUNG:"
		   " H[ELP]   P[RINT]   PP[RINT]   R[EAD]   W[RITE]   L[ISP] "
		   " OK       V"
		   " H[ELP] <KOM>  ERKLAERT DAS KOMMANDO <KOM>."
		   " <BER>         DRUCKT DIE OPTIONEN DES BEREICHS <BER>."
		   " <OPT> <WERT>  WEIST DER OPTION <OPT> DEN NEUEN WERT <WERT> ZU. "
		   " "
		   "FOLGENDE OPTIONS-BEREICHE STEHEN ZUR VERFUEGUNG: ")
  #-:mkrp.deutsch`("The following commands are available: "
		   " H[ELP]   P[RINT]   PP[RINT]   R[EAD]   W[RITE]   L[ISP] "
		   " OK       V"
		   " H[ELP] <COM>  explains the command <COM>. "
		   " <AREA>        prints all options of the area <AREA>. "
		   " <OPT>  <VAL>  sets the option <OPT> to the new value <VAL>. "
		   " "
		   "The following option-areas are available:"))

(DEFUN OS.E=OTHERWISE (INDICATOR)
  (ERROR "UNKNOWN TEXTINDICATOR IN OS.E-GET.EXPLANATION: ~A" INDICATOR))
