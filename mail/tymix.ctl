:logfile tymix.out
run mailer
	; Clear out old TYMIX.Q if any
rename *.qq,alpha.q
	; Process one of the QQ files
run mailer
	; This will startup SMTP.SAV as TYMIX
dir *.q
	; TYMIX.Q will probably still be here
run mailer
	; This causes a delay of several seconds
dir *.q
	; TYMIX.Q should have been renamed to xxxx.FEM
rename *.qq,beta.q
	; Process a second one of the QQ files
dir *.q,*.fem
	; This TYMIX.Q will be ready to go when the FEM is done
    