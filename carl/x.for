	integer i,j,k
	integer m(4),mm(80)
	DATA (M(I),I=1,4)/'this ','is a ','test ','msg.'/
	integer dt(2),TM,TM1,TM2,H1,H2
	INTEGER HOUR,MINUTE,SECOND,MONTH,DAY,YEAR
	double precision foo(4)
	data (foo(i),i=1,4)/
     1  'abcd fghi klmn pqrs uvwx zabc efgh jklm '/
C     3  'opqr tuvw yzab defg ',
C     4  'ijkl nopq stuv xyz  '/

	I=IFOO

	type 1,foo(2),foo(4)
1	format(1x,'foo: ',a5)

10	FORMAT(80A1)
	DECODE(20,10,M)MM
	TYPE 20,MM
20	FORMAT(1X,80(A1,1X))
	do 30,i=1,4
30	call hiset(M(I))
	DECODE(20,10,M)MM
	TYPE 20,MM

	call date(dt)
	call time(tm)
	type 40,dt
	type 40,tm
40	FORMAT(1x,2a5,1x,i,2x,i,2x,o,2x,o)

	CALL TIME(TM1)
	CALL DATE(DT)
	CALL TIME(TM2)

50	FORMAT(I2,1X,I2)
60	FORMAT(I2,1X,A3,1X,I2)
	DECODE(2,50,TM1)H1
	DECODE(2,50,TM2)H2
	IF ( H2 .LT. H1 ) CALL DATE(DT)

	DECODE(5,50,TM2)HOUR,MINUTE
	TYPE 70,HOUR,MINUTE
70	FORMAT(1X,I2,'#',I2)

	i=1
	j=2
	k=3
	type 80,i,j,k
80	format(1x,3i)
	call y(i,j,k)
	type 80,i,j,k
	stop
	end
	subroutine y(l,m,n)

	integer l,m,n
	l=5
	m=20
	n=100
	return
	end
	FUNCTION IFOO
	INTEGER I,J,K,L
	I=2
	J=5
	IFOO=I/J
	RETURN
	END
	FUNCTION IBAR(I,J,K,L)
	INTEGER I,J,K,L
	I=2
	K=4
	J=L+I+K
	IBAR=J+1
	RETURN
	END
        SUBROUTINE HISET(IN)
C
C       ROUTINE TO FORCE 5 CHARACTERS TO UPPER CASE
C
        INTEGER IN,I,K,W(5)

10      FORMAT(5A1)
        DECODE(5,10,IN)W
100     DO 150 I=1,5
        IF ((W(I).ge.'a') .AND. (W(I).le.'z')) W(I)=W(I)-('a'-'A')
150     CONTINUE
        ENCODE(5,10,IN)W
        RETURN
	END
