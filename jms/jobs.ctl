;   HOSTS.CTL was last updated 1-Dec-82 to remove X14 and X17
;  Modified 16-Sep-93 to remove all but 26, 32, 34, 38
;Copy this file to your directory then "PCOM JOBS.CTL"
;The file HOSTS.CTL must also be in your directory.
;Both will get copied to directory PJ.
COPY HOSTS.CTL,(PJ)HOSTS.CTL
:IF (ERROR) ;Ignore error
COPY JOBS.CTL,(PJ)JOBS.CTL
:IF (ERROR) ;Ignore error
;
;First update JOBS.DAT, leaving words 0-14 alone.
;
COPY (PJ)JOBS.DAT,JOBS.DAT
RUN (MPL)FILDDT
JOBS.DAT/D/P
:COM (PJ)HOSTS.CTL

;System 1 must come last, since MHX stops at first negative number
DECLAR ALL RD RD JOBS.DAT
DELETE (PJ)JOBS.BAK
RENAME (PJ)JOBS.BAK=(PJ)JOBS.DAT
RENAME (PJ)JOBS.DAT=JOBS.DAT
;
;Now update HOSTS.SYS in the same manner.
;
COPY (SYS)HOSTS.SYS,HOSTS.SYS
RUN (MPL)FILDDT
HOSTS.SYS/D/P
:COM (PJ)HOSTS.CTL

DECLAR ALL RD RD HOSTS.SYS
DELETE (SYS)HOSTS.BAK
RENAME (SYS)HOSTS.BAK=(SYS)HOSTS.SYS
RENAME (SYS)HOSTS.SYS=HOSTS.SYS
;
SET TTY WIDTH 80 CRLF
PJOB
DIRECT (PJ)JOBS.DAT,JOBS.BAK/EVERYTHING
DIRECT (SYS)HOSTS.SYS,HOSTS.BAK/EVERYTHING
;
;End of JOBS.CTL
 