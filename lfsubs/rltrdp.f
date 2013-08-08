      SUBROUTINE RLTRDP(A,B,N,ISN)
C   RLTRDP IS A DOUBLE PRECISION VERSION OF REALTR. SEE REALTR FOR NOTES
C   ON USAGE.
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION A(1),B(1)
      IF(N .LE. 1) RETURN
      INC=ISN
      IF(INC.LT.0) INC=-INC
      NK = N * INC + 2
      NH = NK / 2
      SD = 3.1415926535898D0/(2.D0*N)
      CD = 2.D0 * DSIN(SD)**2
      SD = DSIN(SD+SD)
      SN = 0.D0
      IF(ISN .GT. 0) GO TO 10
      CN = -1.D0
      SD = -SD
      GO TO 20
 10   CN = 1.D0
      A(NK-1) = A(1)
      B(NK-1) = B(1)
 20   DO 30 J=1,NH,INC
      K = NK - J
      AA = A(J) + A(K)
      AB = A(J) - A(K)
      BA = B(J) + B(K)
      BB = B(J) - B(K)
      XX = CN * BA + SN * AB
      YY = SN * BA - CN * AB
      B(K) = YY - BB
      B(J) = YY + BB
      A(K) = AA - XX
      A(J) = AA + XX
      AA = CN - (CD * CN + SD * SN)
      SN = (SD * CN - CD * SN) + SN
 30   CN = AA
      RETURN
      END
