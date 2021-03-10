  "/ IME.CMD - types out relevent locations for IME or AR.ARX crashes. /
1
PICON:%UPS<%UPS:
  "/ SAVAPR should be 7760,,1 and PARFLG zero if no parity error /
SAVAPR[ 
SAVERA[;.CPERA[;
SAVSB0+1[;
PARFLG[PARPC[;
%UPS=Q'2000=Q+EPT[
STOPPC/q-2/.CPCPC/
  "/ UPT+500 and .CPUP1 = page fail code (in bits 0-5) and virtual address /
  "/ 36=AR, 76=user AR, bad data stored in .CP7+0, follow virt addr to data /
  "/ 37=ARX, 77=user ARX, bad instr stored in .CP7+1, follow PC to instr /
%UPT+500[;/=
.CPUP1[;
  "/ UPT+501 and .CPUP1+1 = PC; 10000 in LH means user mode /
%UPT+501[;/=
.CPUP1+1[;
  "/ .CP7 has contents of AC block 7 accumulators 0 and 1 /
.CP7+0[;
.CP7+1[;
  "/ P points to new stack, first item on it is old contents of P (AC 1) /
P//=
  "/ J is user AC 2 if failure in user mode; don't trust it /
JOB[UPT+UPTJOB[q+JBTPRG6t/
    