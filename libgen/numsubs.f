      SUBROUTINE FIT(X,Y,NDATA,SIG,MWT,A,B,SIGA,SIGB,CHI2,Q)
c From Numerical Recipes, pg. 508. Calls GAMMQ.
c Fits line to X and Y data of the form Y = BX + A
c input:
c   x,y = real arrays of length NDATA
c   sig = real array of standard deviations of X and Y
c   mwt = if 0, then std deviations are assumed to be unavailable
c output:
c   a,b       = slope and y-intercept of line
c   siga,sigb = probable uncertainties of a and b
c   chi2      = chi-square
c   q         = goodness-of-fit probablity (that fit would have chi2 
c               this large or larger). If mwt=0, then q=1.0 and 
c               normalization of chi2 is to unit std. dev. on all points.
c   
      DIMENSION X(NDATA),Y(NDATA),SIG(NDATA)
      SX=0.
      SY=0.
      ST2=0.
      B=0.
      IF(MWT.NE.0) THEN
        SS=0.
        DO 11 I=1,NDATA
          WT=1./(SIG(I)**2)
          SS=SS+WT
          SX=SX+X(I)*WT
          SY=SY+Y(I)*WT
11      CONTINUE
      ELSE
        DO 12 I=1,NDATA
          SX=SX+X(I)
          SY=SY+Y(I)
12      CONTINUE
        SS=FLOAT(NDATA)
      ENDIF
      SXOSS=SX/SS
      IF(MWT.NE.0) THEN
        DO 13 I=1,NDATA
          T=(X(I)-SXOSS)/SIG(I)
          ST2=ST2+T*T
          B=B+T*Y(I)/SIG(I)
13      CONTINUE
      ELSE
        DO 14 I=1,NDATA
          T=X(I)-SXOSS
          ST2=ST2+T*T
          B=B+T*Y(I)
14      CONTINUE
      ENDIF
      B=B/ST2
      A=(SY-SX*B)/SS
      SIGA=SQRT((1.+SX*SX/(SS*ST2))/SS)
      SIGB=SQRT(1./ST2)
      CHI2=0.
      IF(MWT.EQ.0) THEN
        DO 15 I=1,NDATA
          CHI2=CHI2+(Y(I)-A-B*X(I))**2
15      CONTINUE
        Q=1.
        SIGDAT=SQRT(CHI2/(NDATA-2))
        SIGA=SIGA*SIGDAT
        SIGB=SIGB*SIGDAT
      ELSE
        DO 16 I=1,NDATA
          CHI2=CHI2+((Y(I)-A-B*X(I))/SIG(I))**2
16      CONTINUE
        Q=GAMMQ(0.5*(NDATA-2),0.5*CHI2)
      ENDIF
      RETURN
      END
c
c-----------------------------------------------------------------
c
      FUNCTION GAMMQ(A,X)
      IF(X.LT.0..OR.A.LE.0.)PAUSE
      IF(X.LT.A+1.)THEN
        CALL GSER(GAMSER,A,X,GLN)
        GAMMQ=1.-GAMSER
      ELSE
        CALL GCF(GAMMQ,A,X,GLN)
      ENDIF
      RETURN
      END
c
c------------------------------------------------------------------
c
      SUBROUTINE GSER(GAMSER,A,X,GLN)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      IF(X.LE.0.)THEN
        IF(X.LT.0.)PAUSE
        GAMSER=0.
        RETURN
      ENDIF
      AP=A
      SUM=1./A
      DEL=SUM
      DO 11 N=1,ITMAX
        AP=AP+1.
        DEL=DEL*X/AP
        SUM=SUM+DEL
        IF(ABS(DEL).LT.ABS(SUM)*EPS)GO TO 1
11    CONTINUE
      PAUSE 'A too large, ITMAX too small'
1     GAMSER=SUM*EXP(-X+A*LOG(X)-GLN)
      RETURN
      END
c
c-----------------------------------------------------------------
c
      FUNCTION GAMMLN(XX)
      REAL*8 COF(6),STP,HALF,ONE,FPF,X,TMP,SER
      DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,
     *    -1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
      DATA HALF,ONE,FPF/0.5D0,1.0D0,5.5D0/
      X=XX-ONE
      TMP=X+FPF
      TMP=(X+HALF)*LOG(TMP)-TMP
      SER=ONE
      DO 11 J=1,6
        X=X+ONE
        SER=SER+COF(J)/X
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER)
      RETURN
      END
c
c----------------------------------------------------------------
c
      SUBROUTINE GCF(GAMMCF,A,X,GLN)
      PARAMETER (ITMAX=100,EPS=3.E-7)
      GLN=GAMMLN(A)
      GOLD=0.
      A0=1.
      A1=X
      B0=0.
      B1=1.
      FAC=1.
      DO 11 N=1,ITMAX
        AN=FLOAT(N)
        ANA=AN-A
        A0=(A1+A0*ANA)*FAC
        B0=(B1+B0*ANA)*FAC
        ANF=AN*FAC
        A1=X*A0+ANF*A1
        B1=X*B0+ANF*B1
        IF(A1.NE.0.)THEN
          FAC=1./A1
          G=B1*FAC
          IF(ABS((G-GOLD)/G).LT.EPS)GO TO 1
          GOLD=G
        ENDIF
11    CONTINUE
      PAUSE 'A too large, ITMAX too small'
1     GAMMCF=EXP(-X+A*ALOG(X)-GLN)*G
      RETURN
      END
c
c------------------------------------------------------------------
c
      SUBROUTINE MEDFIT(X,Y,NDATA,A,B,ABDEV)
      PARAMETER (NMAX=1000)
      EXTERNAL ROFUNC
      COMMON /ARRAYS/ NDATAT,XT(NMAX),YT(NMAX),ARR(NMAX),AA,ABDEVT
      DIMENSION X(NDATA),Y(NDATA)
      SX=0.
      SY=0.
      SXY=0.
      SXX=0.
      DO 11 J=1,NDATA
        XT(J)=X(J)
        YT(J)=Y(J)
        SX=SX+X(J)
        SY=SY+Y(J)
        SXY=SXY+X(J)*Y(J)
        SXX=SXX+X(J)**2
11    CONTINUE
      NDATAT=NDATA
      DEL=NDATA*SXX-SX**2
      AA=(SXX*SY-SX*SXY)/DEL
      BB=(NDATA*SXY-SX*SY)/DEL
      CHISQ=0.
      DO 12 J=1,NDATA
        CHISQ=CHISQ+(Y(J)-(AA+BB*X(J)))**2
12    CONTINUE
      SIGB=SQRT(CHISQ/DEL)
      B1=BB
      F1=ROFUNC(B1)
      B2=BB+SIGN(3.*SIGB,F1)
      F2=ROFUNC(B2)
1     IF(F1*F2.GT.0.)THEN
        BB=2.*B2-B1
        B1=B2
        F1=F2
        B2=BB
        F2=ROFUNC(B2)
        GOTO 1
      ENDIF
      SIGB=0.01*SIGB
2     IF(ABS(B2-B1).GT.SIGB)THEN
        BB=0.5*(B1+B2)
        IF(BB.EQ.B1.OR.BB.EQ.B2)GOTO 3
        F=ROFUNC(BB)
        IF(F*F1.GE.0.)THEN
          F1=F
          B1=BB
        ELSE
          F2=F
          B2=BB
        ENDIF
        GOTO 2
      ENDIF
3     A=AA
      B=BB
      ABDEV=ABDEVT/NDATA
      RETURN
      END
c
c------------------------------------------------------------------
c
      FUNCTION ROFUNC(B)
      PARAMETER (NMAX=1000)
      COMMON /ARRAYS/ NDATA,X(NMAX),Y(NMAX),ARR(NMAX),AA,ABDEV
      N1=NDATA+1
      NML=N1/2
      NMH=N1-NML
      DO 11 J=1,NDATA
        ARR(J)=Y(J)-B*X(J)
11    CONTINUE
      CALL SORT(NDATA,ARR)
      AA=0.5*(ARR(NML)+ARR(NMH))
      SUM=0.
      ABDEV=0.
      DO 12 J=1,NDATA
        D=Y(J)-(B*X(J)+AA)
        ABDEV=ABDEV+ABS(D)
        SUM=SUM+X(J)*SIGN(1.0,D)
12    CONTINUE
      ROFUNC=SUM
      RETURN
      END
c
c---------------------------------------------------------------
c
      SUBROUTINE SORT(N,RA)
      DIMENSION RA(N)
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          RRA=RA(L)
        ELSE
          RRA=RA(IR)
          RA(IR)=RA(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            RA(1)=RRA
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(RA(J).LT.RA(J+1))J=J+1
          ENDIF
          IF(RRA.LT.RA(J))THEN
            RA(I)=RA(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        RA(I)=RRA
      GO TO 10
      END
c
c--------------------------------------------------------
c
      SUBROUTINE KSTWO(DATA1,N1,DATA2,N2,D,PROB)
c Performs the Kolmogorov-Smirnov test to determine whether
c two data sets are drawn from the same distribution.
c Numerical Recipes, pg 472.
c 
c input:
c  data1, data2 = arrays of data values (Note: these arrays are modified)
c  n1, n2       = length of arrays data1 and data2
c output:
c  D = K-S statistic
c  Prob = significance level for the null hypothesis that the data
c         sets are drawn from the same distribution. Small values of
c         Prob show that the cumulative distribution function of Data1
c         is significantly different from that of Data2.
c
      DIMENSION DATA1(N1),DATA2(N2)
      CALL SORT(N1,DATA1)
      CALL SORT(N2,DATA2)
      EN1=N1
      EN2=N2
      J1=1
      J2=1
      FO1=0.
      FO2=0.
      D=0.
1     IF(J1.LE.N1.AND.J2.LE.N2)THEN
        IF(DATA1(J1).LT.DATA2(J2))THEN
          FN1=J1/EN1
          DT=AMAX1(ABS(FN1-FO2),ABS(FO1-FO2))
          IF(DT.GT.D)D=DT
          FO1=FN1
          J1=J1+1
        ELSE
          FN2=J2/EN2
          DT=AMAX1(ABS(FN2-FO1),ABS(FO2-FO1))
          IF(DT.GT.D)D=DT
          FO2=FN2
          J2=J2+1
        ENDIF
      GO TO 1
      ENDIF
      PROB=PROBKS(SQRT(EN1*EN2/(EN1+EN2))*D)
      RETURN
      END
c
c---------------------------------------------------------
c
      FUNCTION PROBKS(ALAM)
      A2=-2.*ALAM**2
      FAC=2.
      PROBKS=0.
      TERMBF=0.
      DO 11 J=1,100
        TERM=FAC*EXP(A2*J**2)
        PROBKS=PROBKS+TERM
        IF(ABS(TERM).LT.0.001*TERMBF)RETURN
        FAC=-FAC
        TERMBF=ABS(TERM)
11    CONTINUE
      PROBKS=1.
      RETURN
      END
