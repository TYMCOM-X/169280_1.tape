        SUBROUTINE GDIS(V1,V2,V3,V4,V5,V6,TNUM,BAS)
C
C       GDIS ROUTINE
C
        INTEGER BAS,TNUM,V1,V2,V3,V4,V5,V6,NUM,I,B,C
	DOUBLE PRECISION BASE
	COMMON /BASE/BASE(20)
C
40      I=1
50      TYPE 55
55      FORMAT(1X,'TICKET NUMBER : ',T20,$)
        CALL DBOPEN(BASE(BAS))
58      ACCEPT 60,TNUM
60      FORMAT(I8)
65      IF (TNUM.EQ.0) RETURN
        CALL DBFIND(1,1,TNUM)
        CALL DBNREC(NUM)
        IF (NUM.GT.0) GOTO 70
        IF (I.EQ.3) GOTO 200
        TYPE 68,TNUM
68      FORMAT(/3X,'TICKET NUMBER ',I8,' CAN NOT BE FOUND.',/)
        I=I+1
        TYPE 55
        GOTO 58
70      CALL DBGREC($200,1)
        CALL DBVAL(34,V5,35,V6,36,V1,37,V2,38,V3,39,V4)
        CALL DBCLOS
        RETURN
200     TYPE 205
205     FORMAT(/3X,'USE THE "O" COMMAND IF YOU NEED HELP.',/)
        TNUM=0
        CALL DBCLOS
        RETURN
        END
        SUBROUTINE NPHONE(REPN,REPF,UNAM,JJ1)
C
C       SUBROUTINE TO DETERMINE PERIOD OF DAY FOR NETCON
C
        INTEGER REPN(4),REPF(4),UNAM(3),JJ1

        JJ1=1
160     IF (UNAM(1).EQ.'NETCO') GOTO 200
        IF (UNAM(1).EQ.'CSS80') GOTO 220
        IF (UNAM(1).EQ.'NSSC') GOTO 230
        IF (UNAM(1).EQ.'FLMTE') GOTO 240
        IF (UNAM(1).EQ.'ERNMC') GOTO 260
        IF (UNAM(1).EQ.'MURTA') GOTO 250
        RETURN

200     ENCODE(20,205,REPN)
205     FORMAT('NETWORK CONTROL     ')
155     FORMAT('408/922-7301        ')
        ENCODE(20,155,REPF)
        RETURN

220     ENCODE(20,225,REPN)
        ENCODE(20,227,REPF)
225     FORMAT('CS&S 800 SRVC CENTER')
227     FORMAT('610/666-1770        ')
        RETURN

230     ENCODE(20,235,REPN)
        ENCODE(20,237,REPF)
235     FORMAT('NETWORK SYSTEMS SUP.')
237     FORMAT('610/666-3401        ')
        RETURN

240     ENCODE(20,245,REPN)
        ENCODE(20,247,REPF)
245     FORMAT('1ST LINE MAINTENANCE')
247     FORMAT('610/666-3401        ')
        RETURN

250     ENCODE(20,252,REPN)
        ENCODE(20,254,REPF)
252     FORMAT('DON MURTAUGH       ')
254     FORMAT('408/922-7098       ')
        RETURN

260     ENCODE(20,262,REPN)
        ENCODE(20,264,REPF)
262     FORMAT('EURO NTWK MGMT CENTR')
264     FORMAT('331.4789.0817       ')
        RETURN

        END
        SUBROUTINE WWHD(TNUM)
C
C       SUB-ROUTINE TO PRINT THE HEADER OF A TICKET
C
	INTEGER AM,YN,NETOP
	COMMON /AM/AM(12)
	COMMON /YN/YN(4)
        COMMON /NETOP/NETOP(35)

        INTEGER TNUM,PRO,T40,F2,SDAT,TIM1,RNAM(4),RFON(4)
        INTEGER NET(2),INT,RS,SITC(4),SITP(4),PROD,STAT,DISC(169)
        INTEGER CCOD,HQCC,ONUM(2),SRGN,HRGN,LOC(4),KHST,COV,COMMNT(4)
        INTEGER CDAT,TCLSD,STH,EHR,PORT,CNAM(3),TECH(3),VER,V5,V6
        INTEGER V1,V2,V3,V4,NODE,SLOT,HOST,CKT(4),TRB(4),BILL,ESC
        INTEGER F1,NT,DUP,TUP,MAINT,SVER,HH,COC(2),CBY,PROBL(2)
        INTEGER JD,JM,JY,CBAK(4),SM,SD,SY,RECORD,NUMBER
        INTEGER IM,ID,IY,NBR,CHEX,ORDER,RECD
        REAL ELM,HOUR
C
        DIMENSION JUNK(1)
        DATA JUNK/'   '/

C
        CALL DBVAL(1,TNUM,2,PRO,3,T40,4,F2,5,SDAT,6,TIM1,7,RNAM,8,RFON,
     1  9,NET,10,INT,11,RS,12,SITC,13,SITP,14,PROD,15,STAT,16,DISC,17,
     2  CCOD,18,HQCC,19,ONUM,20,SRGN,21,HRGN,22,
     3  LOC,23,KHST,24,COV,25,COMMNT,26,CDAT,27,TCLSD,28,STH,29,EHR,
     4  30,PORT,31,CNAM,32,TECH,33,VER,34,V5,35,V6,36,V1,37,V2,38,V3,
     5  39,V4,40,NODE,41,SLOT,42,HOST,43,CKT,44,TRB,45,BILL,46,ESC,47,
     6  F1,48,NT,49,DUP,50,TUP,51,MAINT,52,SVER,53,HH,54,ELM,55,COC,
     7  56,CBY)
        IF(TNUM.GT.329999) CALL DBVAL(57,PROBL)
678     CALL DBNDAT(SDAT,IM,ID,IY)
        IF (DUP.GT.0) GOTO 686
        JY=0
        JD=0
        SM=0
        SD=0
        SY=0
        JM=0
        GOTO 687
686     IF (CDAT.EQ.0)GOTO 685
        CALL DBNDAT(CDAT,SM,SD,SY)
        SY=SY-1900
685     CALL DBNDAT(DUP,JM,JD,JY)
        JY=JY-1900
687     IY=IY-1900
        CALL MTTR(SDAT,DUP,TIM1,TUP,ELM,HOUR)
        IF (NT.EQ.2) GOTO 655
        IF (NT.EQ.3) GOTO 674
654     WRITE(8,680)NET,SVER,CNAM,TNUM,STAT,SITC,NODE,NETOP(HH),
     1  SITP,KHST,MAINT,LOC,PROD,V1,V2,V3,RNAM,VER,YN(ESC),RFON,
     2  SRGN,HQCC,COV,HRGN,V4,V6,STH,EHR,YN(PRO),ID,
     3  AM(IM),IY,TIM1,TECH,INT,JD,AM(JM),JY,TUP,CKT,F1,SD,AM(SM),
     4  SY,TCLSD,TRB
680     FORMAT(/1X,'NETWORK',T11,2H: ,2A5,T24,'SEVERITY',T37,2H: ,
     1  I1,T50,'CUSTOMER: ',3A5,/1X,'NODE TKT#: ',I8,T24,'STATE',
     2  T37,2H: ,A5,T50,'CONTACT : ',4A5,/1X,'NODE #',T11,2H: ,
     3  I5,T24,'CONTROL GROUP: ',A5,T51,'CPHONE : ',4A5,/1X,
     4  'KERNAL',T11,2H: ,I5,T24,'REFERRED TO',T37,2H: ,A5,T50,
     5  'LOCALE',T58,2H: ,4A5,/1X,'PRODUCT',T11,2H: ,A5,T24,
     6  'MGMT ESCAL',T37,3H: H,I1,2H S,I1,2H T,I1,T50,'RPTD ',
     7  'BY : ',4A5,/1X,'S/W VERS : ',A5,T24,'TECH ESCAL',T37,
     8  2H: ,A3,T51,'RPHONE : ',4A5,/1X,'S/W DIST : ',A5,T24,
     9  'DE-ESCAL CODE: ',I3,T50,'PLAN #',T58,2H: ,A2,/1X,
     A  'H/W DIST : ',A5,T24,'DEFER :  ',I3,1H/,I3,T50,'SITE HR ',
     B  2H: ,A5,' TO ',A5,/1X,'PRIMARY  : ',A3,T24,'OPEN  : ',
     C  I2,1H-,A3,1H-,I2,1X,A5,1HZ,2X,'COM SPEC: ',3A5,/1X,
     D  'OPEN BY',T11,2H: ,A3,T24,'RESLV : ',I2,1H-,A3,1H-,I2,
     E  1X,A5,1HZ,T50,'CKT #',T58,2H: ,4A5,/1X,'ZONE',T11,2H: ,I1,T24,
     F  'CLOSE : ',I2,1H-,A3,1H-,I2,1X,A5,1HZ,T50,'TELCO # : ',4A5)
        WRITE(8,688)HOUR,ONUM,COC,YN(BILL),YN(T40),PROBL
688     FORMAT(T24,'ELAPS :  ',F8.1,4H HR.,T50,'REF.TKT#: ',2A5,
     1  /,T24,'CLSCD : '2A5,T50,'BILL/T40: 'A3,1H/,A3,/,
     2  /1X,'REPORTED PROBLEM: ',2A5,/)
        GOTO 684
655     WRITE(8,689)NET,SVER,CNAM,TNUM,STAT,SITC,NODE,NETOP(HH),
     1  SITP,KHST,MAINT,LOC,HOST,V1,V2,V3,RNAM,SLOT,YN(ESC),RFON,
     2  PORT,HQCC,COV,PROD,V4,V6,STH,EHR,VER,ID,AM(IM),
     3  IY,TIM1,TECH,SRGN,JD,AM(JM),JY,TUP,CKT,HRGN,
     4  SD,AM(SM),SY,TCLSD,TRB
689     FORMAT(/1X,'NETWORK',T11,2H: ,2A5,T24,'SEVERITY',T37,2H: ,
     1  I1,T50,'CUSTOMER: ',3A5,/1X,'HOST TKT#: ',I8,T24,'STATE',
     2  T37,2H: ,A5,T50,'CONTACT : ',4A5,/1X,'NODE #',T11,2H: ,
     3  I5,T24,'CONTROL GROUP: ',A5,T51,'CPHONE : ',4A5,/1X,
     4  'KERNAL',T11,2H: ,I5,T24,'REFERRED TO',T37,2H: ,A5,T50,
     5  'LOCALE',T58,2H: ,4A5,/1X,'HOST #',T11,2H: ,I6,T24,
     6  'MGMT ESCAL',T37,3H: H,I1,2H S,I1,2H T,I1,T50,'RPTD ',
     7  'BY : ',4A5,/1X,'SLOT     : ',A2,T24,'TECH ESCAL',T37,
     8  2H: ,A3,T51,'RPHONE : ',4A5,/1X,'# PORTS  : ',I3,T24,
     9  'DE-ESCAL CODE: ',I3,T50,'PLAN #',T58,2H: ,A2,/1X,
     A  'PRODUCT  : ',A5,T24,'DEFER :  ',I3,1H/,I3,T50,'SITE HR ',
     B  2H: ,A5,' TO ',A5,/1X,'S/W VERS : ',A5,T24,'OPEN  : ',
     C  I2,1H-,A3,1H-,I2,1X,A5,1HZ,2X,'COM SPEC: ',3A5,/1X,
     D  'S/W DIST',T11,2H: ,A5,T24,'RESLV : ',I2,1H-,A3,1H-,I2,
     E  1X,A5,1HZ,T50,'CKT #',T58,2H: ,4A5,/1X,'H/W DIST : ',A5,T24,
     F  'CLOSE : ',I2,1H-,A3,1H-,I2,1X,A5,1HZ,T50,'TELCO # : ',4A5)
        WRITE(8,668)YN(PRO),HOUR,ONUM,INT,COC,YN(BILL),YN(T40),F1,PROBL
668     FORMAT(1X,'PRIMARY  : ',A3,T24,'ELAPS : ',F8.1,4H HR.,T50,
     1  'REF.TKT#: ',2A5,/1X,'OPEN BY  : ',A3,T24,'CLSCD : '2A5,T50,
     2  'BILL/T40: ',A3,1H/,A3,/1X,'ZONE',T11,2H: ,I1,/,
     3  /1X,'REPORTED PROBLEM: ',2A5,/)
        GOTO 684
C
C       CIRCUIT TICKET DISPLAY
C
674     WRITE(8,669)NET,SVER,CNAM,TNUM,STAT,SITC,NODE,NETOP(HH),
     1  SITP,KHST,MAINT,LOC,HOST,V1,V2,V3,RNAM,VER,YN(ESC),RFON,
     2  PROD,HQCC,COV,HRGN,V4,V6,STH,EHR,YN(PRO),
     3  ID,AM(IM),IY,TIM1,CKT,INT,JD,AM(JM),JY,TUP,TRB,F1,SD,AM(SM),
     4  SY,TCLSD,ONUM
669     FORMAT(/1X,'NETWORK',T11,2H: ,2A5,T24,'SEVERITY',T37,2H: ,
     1  I1,T50,'CUSTOMER: ',3A5,/1X,'LINE TKT#: ',I8,T24,'STATE',
     2  T37,2H: ,A5,T50,'CONTACT : ',4A5,/1X,'LONODE',T11,2H: ,
     3  I5,T24,'CONTROL GROUP: ',A5,T51,'CPHONE : ',4A5,/1X,
     4  'LO KHOST',T11,2H: ,I5,T24,'REFERRED TO',T37,2H: ,A5,T50,
     5  'LOCALE',T58,2H: ,4A5,/1X,'HI NODE',T11,2H: ,I5,T24,
     6  'MGMT ESCAL',T37,3H: H,I1,2H S,I1,2H T,I1,T50,'RPTD ',
     7  'BY : ',4A5,/1X,'HI KHOST : ',A5,T24,'TECH ESCAL',T37,
     8  2H: ,A3,T51,'RPHONE : ',4A5,/1X,'SPEED    : ',A5,T24,
     9  'DE-ESCAL CODE: ',I3,T50,'PLAN #',T58,2H: ,A2,/1X,
     A  'H/W DIST : ',A5,T24,'DEFER :  ',I3,1H/,I3,T50,'SITE HR ',
     B  2H: ,A5,' TO ',A5,/1X,'BACKBONE : ',A3,T24,'OPEN  : ',
     C  I2,1H-,A3,1H-,I2,1X,A5,1HZ,2X,'CIRCUIT : ',4A5,/1X,
     D  'OPEN BY',T11,2H: ,A3,T24,'RESLV : ',I2,1H-,A3,1H-,I2,
     E  1X,A5,1HZ,T50,'TELCO #',T58,2H: ,4A5,/1X,'ZONE',T11,2H: ,I1,T24,
     F  'CLOSE : ',I2,1H-,A3,1H-,I2,1X,A5,1HZ,T50,'REF TKT#: ',3A5)
        WRITE(8,667)HOUR,TECH,COC,YN(BILL),YN(T40),PROBL
667     FORMAT(T24,'ELAPS : ',F8.1,4H HR.,T50,'ANALYSIS: ',3A5,
     1  /,T24,'CLSCD : '2A5,T50,'BILL/T40: 'A3,1H/,A3,/,
     2  /1X,'REPORTED PROBLEM: ',2A5,/)
684	WRITE(8,691)
691	FORMAT(1X,'PROBLEM DESCRIPTION : ')
	CALL WRTINF(DISC,10)
690     CONTINUE
        CALL DBSET(3)
        CALL DBFIND('RS',1,1,'OR','RS',1,9,1,1,1,TNUM)
        CALL DBNREC(NBR)
        IF (NBR.EQ.0) GOTO 713
        WRITE(8,709)
709     FORMAT(/1X,'***ESCALATIONS PERFORMED ...***',/3X,'PERSON',T27,
     1  'DATE/TIME',/)
        DO 708 I=1,NBR
        CALL DBGREC($713,I)
        CALL DBVAL(2,SDAT,3,TIM1,4,RNAM)
        CALL DBNDAT(SDAT,IM,ID,IY)
        IY=IY-1900
        WRITE(8,707)RNAM,AM(IM),ID,IY,TIM1
707     FORMAT(3X,4A5,T27,A3,1H-,I2,1H-,I2,2X,A5)
708     CONTINUE
713     CALL DBFIND('RS',1,0,'OR','RS',1,8,1,1,1,TNUM)
        CALL DBNREC(RECD)
        IF (RECD.EQ.0) RETURN
        WRITE(8,714)
714     FORMAT(/1X,'CALL BACK LIST...',/3X,'PERSON',T25,'COMPANY',
     1  T46,'PHONE NUMBER',4X,'TALKED TO ?',/)
        DO 718 J=1,RECD
        CALL DBGREC($200,J)
        CALL DBVAL(4,RNAM,7,SITC,8,SITP,13,CHEX,12,cbak)
        WRITE(8,716)CHEX,SITC,RNAM,SITP(1),SITP(2),SITP(3),CBAK(1),
     1  CBAK(2),CBAK(3)
716     FORMAT(1X,A1,1X,4A5,T25,4A5,1X,A5,A5,A5,2X,A5,A5,A5)
718     CONTINUE
200     RETURN
        END
        SUBROUTINE CGDAT(DAT)
C
C       SUBROUTINE TO TAKE NORMAL DATE AND CHANGE TO 1022
C
        INTEGER DAT,SM,SD,SY
C
        TYPE 100
100     FORMAT(1X,'ENTER DATE MM/DD/YY : ',$)
        ACCEPT 105,SM,SD,SY
105     FORMAT(I2,1X,I2,1X,I4)
        IF (SM.EQ.0) GOTO 110
        IF (SY.GT.1800) SY=SY-1900
        IF (SY.LT.100) SY=SY+1900
        CALL DBDATN(DAT,SM,SD,SY)
        RETURN

110     DAT=0
        RETURN
        END
        SUBROUTINE CHSDT1(BASX)
C
C       CHANGE THE SDAT NUMBER
C
        INTEGER BAS,BASX,SDAT,NSDAT,TNUM,NUMBER,Z,IERT,IERC,DAT,ELM
        INTEGER MIDV(4),NT
	DOUBLE PRECISION BASE
	COMMON /BASE/BASE(20)
C
	BAS=BASX
90      Z=1
        ELM=0
100     TYPE 105
105     FORMAT(1X,'TICKET NUMBER : ',$)
        ACCEPT 110,TNUM
110     FORMAT(I8)
        IF (TNUM.EQ.0) RETURN
114     CALL DBOPEN(BASE(BAS))
        CALL DBFIND(1,1,TNUM)
        CALL DBNREC(NUMBER)
        IF (NUMBER.EQ.0) GOTO 200
        CALL DBGREC($300,NUMBER)
        CALL DBVAL('SDAT',SDAT)
        CALL DBNDAT(SDAT,SM,SD,SY)
        TYPE 115,TNUM,SM,SD,SY
115     FORMAT(/1X,'THE CURRENT DATE DOWN FOR TICKET NUMBER ',I8,
     1  ' IS... ',/,/5X,I2,1H-,I2,1H-,I4,/,/1X,'ENTER THE NEW VALUE ',
     2  'OR A C.R. FOR NO CHANGE : ')
        CALL CGDAT(DAT)
120     FORMAT(I6)
        IF (DAT.EQ.0) GOTO 300
        CALL CHA1I(BAS,5,DAT,TNUM)
        CALL CHA1I(BAS,54,ELM,TNUM)
        RETURN

200     IF (Z.EQ.2) GOTO 215
        Z=2
	BAS=4
        GOTO 114

215     TYPE 220,TNUM
220     FORMAT(1X,'TICKET NUMBER ',I8,' CAN NOT BE FOUND. PLEASE TRY',
     1  ' AGAIN.')
	BAS=1
        GOTO 90

295     TYPE 296,IERT,IERC
296     FORMAT(/3X,'ERROR TYPE = ',I5,4X,'ERROR CODE = ',I5,
     1  /,/2X,'CHANGE IS NOT COMPLETE.')
        RETURN

300     TYPE 305
305     FORMAT(1X,'NO CHANGE WAS MADE.')
        CALL DBCLOS
        RETURN

        END
        SUBROUTINE HLPREP

        TYPE 10
10      FORMAT(/1X'THE "REPORTED PROBLEM" CODES ARE...',/2X,
     1  '1 = OPER INTERCEPT',T22,
     1  '11 = MODEM',T42,'21 = HW/MS LINK',/2X,'2 = RNA',T22,
     2  '12 = INTL ACCESS',T42,'22 = DBU FAILED',/2X,
     3  '3 = BUSY',T22,'13 = PAP PROBLEM',T42,'23 = PERIPHERAL',
     4  /2X,'4 = GARBAGE',T22,'14 = NODE DOWN',T42,
     5  '24 = CHRONIC',/2X,'5 = NO CARRIER',T22,'15 = NODE RESTART',T42,
     6  '25 = HOST SHUT',/2X,'6 = TONE, NO RESP.',T22,'16 = HOST DOWN',
     7  T42,'26 = NONE ',/2X,'7 = NR TO TID',T22,'17 = HOST MISSING',
     8  T42,'27 = OTHER',/2X,'8 = SLOW RSPNSE',T22,'18 = SLOT RESTART',
     9  T42,'28 = DIALCOM',/2X,'9 = USER DRPD',T22,'19 = UTILITY CODE',
     A  T42,'29 = NET BILLING',/1X,'10 = OUTDIAL',T22,'20 = ',
     B  'LEASED LINE',T42,'30 = EDI BILLING',//1X,'31 = ETS BILLING',
     C  T22,'32 = DCOM BILLING',T42,'33 = ONTYM BILLING',/1X,
     D  '34 = ROUTER DOWN ',T22,'35 = CUST INQUIRY',T42,
     E  '36 = SEC. VIOLATION')
        RETURN

        END
        SUBROUTINE DEFCB(ME)
C
C       SUBROUTINE TO DEFER '60' COMMAND CALLBACKS
C
        INTEGER TNUM,IA1,IA2,IA3,IA4,IA5,IA6,SDAT,IERT,IERC
        INTEGER NUMBER,I,SEE,AHR,TOT1,TOT,TIM1,CNAM(3),IBF
        INTEGER SITP(4),CG,SITC(4),ME,ETIM,EDAT,EA4,EA5
	DOUBLE PRECISION BASE
	COMMON /BASE/BASE(20)
	COMMON /IBF/IBF
C
26      TYPE 10
10      FORMAT(/1X,'THIS WILL ALLOW YOU TO DEFER UPDATE CALLBACKS ',
     1  'FOR MORE THAN 2 HOURS',/1X,'ENTER THE TICKET NUMBER:  ',$)
        ACCEPT 15,TNUM
15      FORMAT(I8)
        IF (TNUM.EQ.0) GOTO 300
        CALL FGMT(IA1,IA2,IA3,IA4,IA5,IA6)
        IA3=IA3+1900
        CALL DBDATN(SDAT,IA1,IA2,IA3)
        CALL DBERR($20,IERT,IERC,IBF)
        GOTO 25
20      IF (IERT.NE.7) GOTO 200
        TYPE 22
22      FORMAT(/1X,'DATABASE BUSY....WAITING 10 SECONDS')
        CALL WAIT(8.0)
25      CALL DBOPEN(BASE(16))
        CALL DBFIND(2,1,TNUM,1,12,1,0)
        CALL DBNREC(NUMBER)
        IF (NUMBER.EQ.0) GOTO 190
29      TYPE 30
30      FORMAT(/1X,'********************************************',
     1  /,/2X,'SELECTION ',2X,'CUSTOMER',9X,'PERSON',16X,'NUMBER')
        DO 40 I=1,NUMBER
        CALL DBGREC($200,I)
        CALL DBVAL(1,CNAM,15,SITC,3,SITP)
        TYPE 41,I,CNAM,SITC,SITP
41      FORMAT(5X,I2,5X,3A5,2X,4A5,2X,4A5)
40      CONTINUE
        TYPE 42
42      FORMAT(/2X,'ENTER SELECTION # TO UPDATE, OR -1 FOR ALL :',$)
        ACCEPT 43,SEE
43      FORMAT(I2)
        IF (SEE.EQ.-1) GOTO 48
        IF (SEE.EQ.0) GOTO 300
        IF ((SEE.LT.-1).OR.(SEE.GT.NUMBER)) GOTO 29
48      TYPE 46
46      FORMAT(/1X,'HOW MANY HOURS FROM NOW SHOULD THE NEXT ',
     1  'CALLBACK BE MADE? ',$)
        ACCEPT 47,AHR
47      FORMAT(I3)
        IF (AHR.LT.0) GOTO 54
        IF (IA4+AHR.GT.23) GOTO 50
        IA4=IA4+AHR
        GOTO 49
50      TOT=IA4+AHR
        TOT1=TOT/24
        SDAT=SDAT+TOT1
        IA4=TOT-(TOT1*24)
49      ENCODE(5,51,TIM1)IA4,IA5
        IF (IA5.LE.9) ENCODE(5,52,TIM1)IA4,IA5
51      FORMAT(I2,':',I2)
52      FORMAT(I2,':0',I1)
54      CALL DBERR($44,IERT,IERC,IBF)
        GOTO 45
44      IF (IERT.NE.7) GOTO 200
        TYPE 22
        CALL WAIT(8.0)
45      CALL DBUPD(-1)
        CALL DBFIND(2,1,TNUM,1,12,1,0)
        CALL DBNREC(NUMBER)
        IF (SEE.EQ.-1) GOTO 80
        IF (AHR.LT.0) GOTO 120
        CALL DBGREC($200,SEE)
        CALL DBVAL(1,CNAM,3,SITP,7,CG,15,SITC,4,ETIM,5,EDAT)
        IF (EDAT.GT.SDAT) GOTO 53
        IF ((EDAT.EQ.SDAT).AND.(ETIM.GE.TIM1)) GOTO 53
        CALL DBCHNG(12,2,11,ME,8,'DEFERRED')
        CALL DBADD(1,CNAM,2,TNUM,3,SITP,4,TIM1,5,SDAT,6,0,7,CG,
     1  11,ME,12,0,15,SITC)
53      CALL DBCLOS
        GOTO 26
80      IF (AHR.LT.0) TYPE 81
81      FORMAT(/1X,'NEGATIVE DEFERRALS MUST BE DONE 1 AT A TIME.',
     1  /1X,'NO CHANGES MADE.')
        IF (AHR.LT.0) GOTO 26
        DO 90 I=1,NUMBER
        CALL DBFIND(2,1,TNUM,1,12,1,0)
        CALL DBGREC($200,1)
        CALL DBVAL(1,CNAM,3,SITP,7,CG,15,SITC,4,ETIM,5,EDAT)
        IF (EDAT.GT.SDAT) GOTO 90
        IF ((EDAT.EQ.SDAT).AND.(ETIM.GE.TIM1)) GOTO 90
        CALL DBCHNG(12,2,11,ME,8,'DEFERRED')
        CALL DBADD(1,CNAM,2,TNUM,3,SITP,4,TIM1,5,SDAT,6,0,7,CG,
     1  11,ME,12,0,15,SITC)
90      CONTINUE
        CALL DBCLOS
        GOTO 300
120     CALL DBGREC($200,SEE)
        CALL DBVAL(4,ETIM,5,EDAT)
        DECODE(5,121,ETIM)EA4,EA5
121     FORMAT(I2,1X,I2)
        IF (EA4+AHR.LT.0) GOTO 150
        EA4=EA4+AHR
        GOTO 149
150      TOT=EA4+AHR
        TOT1=(TOT/24)-1
        SDAT=EDAT+TOT1
        EA4=TOT-(TOT1*24)
149      ENCODE(5,151,TIM1)EA4,EA5
        IF (EA5.LE.9) ENCODE(5,152,TIM1)EA4,EA5
151      FORMAT(I2,':',I2)
152      FORMAT(I2,':0',I1)
        CALL DBCHNG(4,TIM1,5,SDAT)
        CALL DBCLOS
        GOTO 26

190     TYPE 191,TNUM
191     FORMAT(/1X,'NO RECORDS FOUND FOR TICKET # ',I8)
        CALL DBCLOS
        RETURN

200     TYPE 205,IERT,IERC
205     FORMAT(/3X,'ERROR TYPE = ',I5,4X,'ERROR CODE = ',I5,
     1  /,/2X,'DEFERRAL IS NOT COMPLETE.')
        CALL DBCLOS
300     RETURN

        END
        SUBROUTINE STBKCL(TNUM,SITC,REPF,ME,TIM1,SDAT)

        INTEGER TNUM,SITC(4),REPF(4),ME,TIM1,SDAT,IERT,IERC,NUMBER,IBF
	DOUBLE PRECISION BASE
	COMMON /BASE/BASE(20)
	COMMON /IBF/IBF

22      FORMAT(/1X,'DATABASE BUSY....WAITING 10 SECONDS')
        CALL DBERR($44,IERT,IERC,IBF)
        GOTO 45
44      IF (IERT.NE.7) GOTO 200
        TYPE 22
        CALL WAIT(8.0)
45      CALL DBOPEN(BASE(16))
        CALL DBUPD(-1)
        CALL DBFIND(2,1,TNUM,1,12,1,0)
        CALL DBNREC(NUMBER)
        IF (NUMBER.EQ.0) GOTO 199
        CALL DBSRCH(15,1,SITC)
        CALL DBNREC(NUMBER)
        IF (NUMBER.EQ.0) GOTO 199
        CALL DBCHNG(9,TIM1,10,SDAT,11,ME,8,REPF,12,2)
199     CALL DBCLOS
        RETURN

200     TYPE 205,IERT,IERC
205     FORMAT(/3X,'ERROR TYPE = ',I5,4X,'ERROR CODE = ',I5,/)
300     RETURN

        END
        SUBROUTINE IPXCHG(TNUM,ME)
C
C       ROUTINE TO CHANGE IPX NAME IN FRAMERELAY NET TICKETS
C
        INTEGER TNUM,NUMBER,NDAT,NTIM,STAT,MAINT,IBF
        INTEGER IPXNAM,SVER,F1,ME,NEWNAM,IERT,IERC,TDAT,TTIM
	DOUBLE PRECISION BASE,NET
	COMMON /BASE/BASE(20)
	COMMON /IBF/IBF

        CALL DBERR($10,IERT,IERC,IBF)
        GOTO 20
10      IF (IE.7) GOTO 500
        TYPE 490
        CALL WAIT(8.0)
20      CALL DBOPEN(BASE(1))
        CALL DBFIND(1,1,TNUM)
        CALL DBNREC(NUMBER)
        IF(NUMBER.EQ.0) RETURN
        CALL DBGREC($500,NUMBER)
        CALL DBVAL(5,NDAT,6,NTIM,51,MAINT,52,SVER,15,STAT,9,NET,
     1  14,IPXNAM,47,F1)
        TYPE 30,IPXNAM
30      FORMAT(/1X,'THE CURRENT VALUE FOR IPX NAME IS : ',A5,
     1  /1X,'ENTER THE NEW NAME OR A (CR.) FOR NO CHANGE:  ',$)
        ACCEPT 40,NEWNAM
40      FORMAT(A5)
        IF(NEWNAM.EQ.'     ') RETURN
        CALL UPCASE(NEWNAM)
        CALL DBERR($50,IERT,IERC,IBF)
        GOTO 60
50      IF (IERT.NE.7) GOTO 500
        TYPE 490
        CALL WAIT(8.0)
60      CALL DBUPD(-1)
        CALL DBCHNG(14,NEWNAM)
        CALL DBCLOS

        CALL DBERR($70,IERT,IERC,IBF)
        GOTO 80
70      IF (IERT.NE.7) GOTO 500
        TYPE 490
        CALL WAIT(8.0)
80      CALL DBOPEN(BASE(6))
        CALL DBUPD(-1)
        CALL DBFIND(1,1,TNUM)
        CALL DBNREC(NUMBER)
        IF (NUMBER.GT.0) GOTO 90
        CALL DBADD(1,TNUM,2,NDAT,3,NTIM,7,STAT,8,MAINT,10,NET,
     1  11,SVER,12,NEWNAM,17,F1,14,ME)
        CALL DBCLOS
        RETURN

90      CALL DBGREC($500,NUMBER)
        CALL DBCHNG(12,NEWNAM)
        CALL DBCLOS
        RETURN

490     FORMAT(1X,'DATA BASE BUSY... WAITING 10 SECS.')

500     TYPE 501,IERT,IERC
501     FORMAT(/3X,'ERROR TYPE = ',I5,4X,'ERROR CODE = ',I5,
     1  /,/2X,'PLEASE REPORT THIS')

        RETURN

        END
    m"6u