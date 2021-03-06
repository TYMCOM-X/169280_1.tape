0001?	(DEFPROP %DEFIN
  0002?		 (LAMBDA (X V F P)
   0003?			 (PROG (R)
0004?			       (SETQ R (COND ((GETL X
 0005?						    (QUOTE (EXPR FEXPR
0006?								 SUBR
0007?								 FSUBR
    0008?								 LSUBR
    0009?								 MACRO)))
 0010?					      (LIST X (QUOTE REDEFINED)))
0011?					     (T X)))
 0012?			       (PUTPROP X (LIST (QUOTE LAMBDA) V F) P)
    0013?			       (RETURN R)))
 0014?		 EXPR)
0015?	
  0016?	(DEFPROP DE
 0017?	 (LAMBDA (L) (%DEFIN (CAR L) (CADR L) (CADDR L) (QUOTE EXPR)))
0018?	 FEXPR)
0019?	
  0020?	(DEFPROP DF
                                                                            0021?	 (LAMBDA (L) (%DEFIN (CAR L) (CADR L) (CADDR L) (QUOTE FEXPR)))
    0022?	 FEXPR)
0023?	
  0024?	(DEFPROP DM
 0025?	 (LAMBDA (L) (%DEFIN (CAR L) (CADR L) (CADDR L) (QUOTE MACRO)))
    0026?	 FEXPR)
0027?	
  0028?	(DEFPROP PLUS (LAMBDA (L) (*EXPAND L (QUOTE *PLUS))) MACRO)
   0029?	
  0030?	(DEFPROP DIFFERENCE (LAMBDA (L) (*EXPAND L (QUOTE *DIF))) MACRO)
   0031?	
  0032?	(DEFPROP TIMES (LAMBDA (L) (*EXPAND L (QUOTE *TIMES))) MACRO)
 0033?	
  0034?	(DEFPROP QUOTIENT (LAMBDA (L) (*EXPAND L (QUOTE *QUO))) MACRO)
0035?	
  0036?	(DEFPROP LESSP
   0037?	 (LAMBDA (L)
0038?	  (LIST	(QUOTE *LESS)
                     0039?		(*EXPAND1 (CDR (REVERSE (CDR L)))
   0040?			  (QUOTE (LAMBDA (X Y)
   0041?					 (COND ((AND X (*LESS X Y)) Y)))))
    0042?		(CAR (LAST L))))
0043?	 MACRO)
0044?	
      ?  0001?	(DEFPROP GREATERP
0002?	 (LAMBDA (L)
0003?	  (LIST	(QUOTE *GREAT)
0004?		(*EXPAND1 (CDR (REVERSE (CDR L)))
   0005?			  (QUOTE (LAMBDA (X Y)
   0006?					 (COND ((AND X (*GREAT X Y)) Y)))))
   0007?		(CAR (LAST L))))
0008?	 MACRO)
0009?	
  0010?	(DEFPROP %DEVP
   0011?		 (LAMBDA (X)
    0012?			 (OR (EQ (CAR (LAST (EXPLODE X))) (QUOTE :))
 0013?			     (AND (NOT (ATOM X)) (NOT (ATOM (CDR X))))))
  0014?		 EXPR)
0015?	
       0016?	(DE %READCHAN (%CHAN %TALK)
0017?		      (PROG (%OLDCHAN %SEXPR)
  0018?			    (SETQ %OLDCHAN (INC %CHAN NIL))
0019?		       LOOP (SETQ %SEXPR (ERRSET (READ)))
0020?			    (COND ((ATOM %SEXPR) (GO END)))
0021?			    (SETQ %SEXPR (EVAL (CAR %SEXPR)))
   0022?			    (COND (%TALK (PRINT %SEXPR)))
  0023?			    (GO LOOP)
  0024?		       END  (INC %OLDCHAN T)
   0025?			    (RETURN NIL)))
  0026?	
  0027?	(DE %READAFILE (%DEV %FNAM %TALK)
    0028?	 (%READCHAN (EVAL (LIST (QUOTE INPUT) (GENSYM) %DEV %FNAM)) %TALK))
0029?	
  0030?	(DE READIN (%DEV %FLIST %TALK)
  0031?	    (PROG NIL
                                  0032?	     LOOP (COND	((NULL %FLIST) (RETURN (QUOTE FINISHED-LOADING)))
  0033?			((%DEVP (CAR %FLIST)) (SETQ %DEV (CAR %FLIST))
    0034?					      (SETQ %FLIST (CDR %FLIST))
 0035?					      (GO LOOP)))
 0036?		  (%READAFILE %DEV (CAR %FLIST) %TALK)
   0037?		  (SETQ %FLIST (CDR %FLIST))
   0038?		  (GO LOOP)))
   0039?	
  0040?	(DF DSKIN (%L) (READIN (QUOTE DSK:) %L T))
0041?	
  0042?	(DF SYSIN (%L) (READIN (QUOTE SYS:) %L NIL))
   0043?	
  0044?	(DEFPROP PUTSYM
  0045?	 (LAMBDA (L)
0046?	  (MAPCAR (FUNCTION (LAMBDA (X)
 0047?			     (COND ((ATOM X) (*PUTSYM X X))
                                                  0048?				   (T (*PUTSYM (CAR X) (EVAL (CADR X)))))))
 0049?		  L))
 0050?	 FEXPR)
0051?	
      ?  0010?	(DEFPROP GETSYM
  0020?	 (LAMBDA (L)
0030?	  (MAPCAR
   0040?	   (FUNCTION (LAMBDA (X)
   0050?		      (PROG (V)
 0060?			    (SETQ V (*GETSYM X))
 0070?			    (COND (V (PUTPROP X (NUMVAL V) (CAR L)))
 0080?				  (T (PRINT (CONS X
0090?						  (QUOTE (NOT IN
 0100?							      SYMBOL
    0110?							      TABLE))))))
    0120?			    (RETURN V))))
   0130?	   (CDR L)))
0140?	 FEXPR)
0150?	
  0160?	(DF BREAK (%LL%)
 0170?		  (PROG (%EX% %ICH% %OCH%)
0180?			(SETQ %ICH% (INC NIL NIL))
                        0190?			(SETQ %OCH% (OUTC NIL NIL))
   0200?			(PRINT (CONS (QUOTE *BREAK*) (CAR %LL%)))
    0210?		   LOOP	(TERPRI)
0220?			(SETQ %EX% (ERRSET (READ)))
   0230?			(COND ((ATOM %EX%) (GO LOOP)))
0240?			(COND ((EQ (CAR %EX%) *BPROCEED*) (GO END)))
 0250?			(ERRSET (PRIN1 (EVAL (CAR %EX%))))
 0260?			(GO LOOP)
 0270?		   END	(INC %ICH% NIL)
    0280?			(OUTC %OCH% NIL)
    0290?			(RETURN (EVAL (CADR %LL%)))))
 0300?	
  0310?	(SETQ *BPROCEED* (QUOTE P))
0320?	
  0330?	(PROG (EX)
  0340?	      (SETQ EX (QUOTE (LAMBDA (L)
    0350?			       (PROG2 (SYSIN LAP)
0360?				      (LIST (QUOTE QUOTE) (EVAL L))))))
          0370?	      (MAPC (FUNCTION (LAMBDA (X) (PUTPROP X EX (QUOTE MACRO))))
   0380?		    (QUOTE (DEFSYM LAP OPS))))
 0390?	
  0400?	(PROG (EX)
  0410?	      (SETQ EX (QUOTE (LAMBDA (L)
    0420?			       (PROG2 (SYSIN LEDLNK)
  0430?				      (LIST (QUOTE QUOTE) (EVAL L))))))
0440?	      (MAPC (FUNCTION (LAMBDA (X) (PUTPROP X EX (QUOTE MACRO))))
   0450?		    (QUOTE (FILEIN EDFUN))))
   0460?	
  0470?	(PROG (EX)
  0480?	      (SETQ EX (QUOTE (LAMBDA (L)
    0490?			       (PROG2 (SYSIN GRIN)
    0500?				      (LIST (QUOTE QUOTE) (EVAL L))))))
0510?	      (MAPC (FUNCTION (LAMBDA (X) (PUTPROP X EX (QUOTE MACRO))))
        0520?		    (QUOTE (GRINDEF))))
       ?  0001?	(PROG (EX)
  0002?	      (SETQ EX (QUOTE (LAMBDA (L)
    0003?			       (PROG2 (SYSIN TRACE)
   0004?				      (LIST (QUOTE QUOTE) (EVAL L))))))
0005?	      (MAPC (FUNCTION (LAMBDA (X) (PUTPROP X EX (QUOTE MACRO))))
   0006?		    (QUOTE (TRACE UNTRACE
 0007?				  TRACET
 0008?				  UNTRACET
    0009?				  SLST
   0010?				  UNSLST
 0011?				  RESET))))
   0012?	
  0013?	(DF COMMENT (L) NIL)
  0014?	
  0015?	(DF DECLARE (L) NIL)
  0016?	
  0017?	(SETQ EIGHT (ADD1 7))
 0018?	
  0019?	(SETQ TEN (PLUS 2 EIGHT))
  0020?	
                                               0021?	(DE OCTAL NIL (SETQ BASE (SETQ IBASE EIGHT)))
  0022?	
  0023?	(DE DECIMAL NIL (SETQ BASE (SETQ IBASE TEN)))
  0024?	
  0025?	(COND ((NULL (ERRSET (INPUT INITCHAN DSK: (LISP . INI)) NIL)))
0026?	      (T (%READCHAN (QUOTE INITCHAN) NIL)))
    0027?	
  0028?	(PROG NIL (INC NIL T) (OUTC NIL T) (EXCISE) (CSYM G0000) (ERR))
    