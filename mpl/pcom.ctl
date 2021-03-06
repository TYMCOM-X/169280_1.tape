:LOGFILE PCOM.LOG
:$Substitution=1
;Begin PCOM Command file [PCOM.CTL] 
;      at: \$Daytime\ on \$Weekday\, \$Month\ \$Day\, \$Year\
;
;   These files are needed to build PCOM
;
;	PCOM.CTL	This command file
;	PCOM.CMD	Assembler load file
;	PCOM.FIL	List of files for CKSUM
;	PCOM.SAI	Source file
;	PCOINT.SAI	Source file
;	PCOINT.DCL	Source file
;	PCODUL.SAI	Source file
;	PCODUL.DCL	Source file
;	PCOM.RND	Source Document
;	PCOM.RNH	Source Document
;	UUOSYM.DEF	Source file
;	SWDEF.DEF	Source file
;
;   These files are needed to build PEECOM
;
;	PEECOM.CMD	Assembler load file
;	PEECOM.SAI	Source file
;
;   These are to be archived with PCOM
;
;	PCOM.LOG	Output from this build
;
;   Use newest versions of MACRO, SAIL and LINK
;
;	SAIL		from (FTSYS)SAIL
;	MACRO %53B	from (FTSYS)MACRO.SHR
;	LINK  %4A	from (SYS)LINK via (SPL)LINKER
;	DOCGEN		from (SPL)DOCGEN.SAV
;	DIRIT		from (SYS)DIRIT.SHR
;	CKSUM		from (SYS)CKSUM.SAV
;
CTE SETPROC MACRO=(FTSYS)MACRO,SAIL=(FTSYS)SAIL,LOADER=(SPL)LINKER
;
;
;	Build PCOM
;
LOAD/COMPILE @PCOM.CMD
:ESCAPE
;
;	Build PEECOM
;
LOAD/COMPILE @PEECOM.CMD
:ESCAPE

;
;	Delete REL files no longer needed
;
DELETE PCOM.REL,PCOINT.REL,PCODUL.REL
;
;	Create DOCument file
;
GO (SPL)DOCGEN
PCOM.HLP=PCOM.RNH
PCOM.DOC=PCOM.RND
:ESCAPE
;	Obtain CHECKSUMS
;
R CKSUM
@PCOM.FIL

;	Give files appropriate protections
;
DECLARE ALL RUN RUN PCOM.SHR,PCOM.LOW,PEECOM.SAV
DECLARE ALL RD  RD  PCOM.DOC,PCOM.HLP
;
:REM	Don't forget to set license on PCOM.SHR,PCOM.LOW
;
R SETLIC
PCOM.SHR,WC RC OP SY GD TD ST HF JL AC XC RA WA
R SETLIC
PCOM.LOW,WC RC OP SY GD TD ST HF JL AC XC RA WA
;
;	Check protections and license
;
DIR PCOM.SHR,PCOM.LOW,PEECOM.SAV,PCOM.DOC,PCOM.HLP/PROT/LIC
;
;    at: \$Daytime\ on \$Weekday\, \$Month\ \$Day\, \$Year\
;End PCOM Command file. [PCOM.CTL]
    