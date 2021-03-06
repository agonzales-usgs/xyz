      SUBROUTINE FFTL(X,N,NDIR,IERR)
C  IF IABS(NDIR)=1 FFTL FOURIER TRANSFORMS THE N POINT REAL TIME SERIES IN ARRAY 
C  X. THE RESULT OVERWRITES X STORED AS (N+2)/2 COMPLEX NUMBERS (NON-NEGATIVE 
C  FREQUENCIES ONLY). 
C  IF IABS(NDIR)=2 FFTL FOURIER TRANSFORMS THE (N+2)/2 COMPLEX FOURIER COEFFICIENTS 
C  (NON-NEGATIVE FREQUENCIES ONLY) IN ARRAY X (ASSUMING THE SERIES IS HERMITIAN). 
C  THE RESULTING N POINT REAL TIME SERIES OVERWRITES X.  
C   NDIR>0 THE FORWARD TRANSFORM USES THE SIGN CONVENTION EXP(I*W*T).  
C   NDIR<0 THE FORWARD TRANSFORM USES THE SIGN CONVENTION EXP(-I*W*T).  
C  THE FORWARD TRANSFORM IS NORMALIZED SUCH THAT A SINE WAVE OF UNIT AMPLITUDE 
C  IS TRANSFORMED INTO DELTA FUNCTIONS OF UNIT AMPLITUDE.  THE BACKWARD TRANSFORM
C  IS NORMALIZED SUCH THAT TRANSFORMING FORWARD AND THEN BACK RECOVERS THE 
C  ORIGINAL SERIES.  IERR IS NORMALLY ZERO.  IF IERR.EQ.1 THEN FFT HAS NOT BEEN 
C  ABLE TO FACTOR THE SERIES.  HOWEVER, X HAS BEEN SCRAMBLED BY REALTR.  NOTE 
C  THAT IF N IS ODD THE LAST POINT WILL NOT  BE USED IN THE TRANSFORM.
C                                                     -RPB
      DIMENSION X(1)
      N2=N/2
      IDIR=IABS(NDIR)
      GO TO (1,2),IDIR
C   DO FORWARD TRANSFORM (IE. TIME TO FREQUENCY).
 1    CALL FFT(X,X(2),N2,N2,N2,2,IERR)
      CALL REALTR(X,X(2),N2,2)
      N1=2*N2+2
      SCALE=1./N
      IF(NDIR.GT.0) GO TO 3
      DO 5 I=4,N,2
 5    X(I)=-X(I)
      GO TO 3
C   DO BACKWARD TRANSFORM (IE. FREQUENCY TO TIME).
 2    IF(NDIR.GT.0) GO TO 6
      DO 7 I=4,N,2
 7    X(I)=-X(I)
 6    X(2)=0.
      X(2*N2+2)=0.
      CALL REALTR(X,X(2),N2,-2)
      CALL FFT(X,X(2),N2,N2,N2,-2,IERR)
      N1=2*N2
      SCALE=.5
C   NORMALIZE THE TRANSFORM.
 3    DO 4 I=1,N1
 4    X(I)=SCALE*X(I)
      RETURN
      END
