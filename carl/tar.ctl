:LOGFILE TAR.LOG
:PARAMETERS TAPE=FTA0:
:TIME 0

; This file is used to copy all the TYM and CMD files from a PDP-10
; to a UNIX system.  It is used for converting Private Nets to CMS
; and Subnets to GNS.      -Joe Smith and Carl Baltrunas.

set tty lc
run (xexec)minit
tar cvfxxxxx \TAPE\ *.BND *.NI# *.N0# *.N1# *.TMP *.*
    