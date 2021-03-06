:LOGFILE MAILQ.OUT
GFD MAIL
; The following steps still must be done manually.
; RENAME 1.XXX=*.FEM
; RENAME 2.XXX=*.FEM
; RENAME 3.XXX=*.FEM
;  etc etc.
;  Use "SYSTAT (MAIL)" to see which FEM files are in use.
; COPY 1.XXX+2.XXX+3.XXX,ALL.XX
; DELETE ?.XXX
MUNG SPLIT.TEC,ALL.XX
; SPLIT.TEC creates QQ1###.XXX files, one message per file.
FINDIT IN.DAT=(MAIL)*.XXX
R SORT
IN.DAT=IN.DAT/R80/K54.6

EXECUTE MAILQ.FOR
; Get rid of exact duplicates; what's left are unique.
DELETE @DUP.DAT
DELETE DUP.DAT,IN.DAT,MAILQ.REL
; Warning: the last line of OUT.DAT may have an unwanted comma.
RUN (SPL)PIP
OUT.DAT@

DIRECT *.QQ/ALPHA
; DELETE QQ####.XX
; SUBMIT TYMIX.CTL to run every 5 minutes until the last .QQ is gone.
  