      SUBROUTINE FIXPNS(N,Y,IFLAG,ARCRE,ARCAE,ANSRE,ANSAE,TRACE,A,
     $   NFE,ARCLEN,YP,YOLD,YPOLD,QR,LENQR,PIVOT,WORK,SSPAR,
     $   PAR,IPAR)
C
C SUBROUTINE  FIXPNS  FINDS A FIXED POINT OR ZERO OF THE
C N-DIMENSIONAL VECTOR FUNCTION F(X), OR TRACKS A ZERO CURVE
C OF A GENERAL HOMOTOPY MAP RHO(A,X,LAMBDA).  FOR THE FIXED
C POINT PROBLEM F(X) IS ASSUMED TO BE A C2 MAP OF SOME BALL
C INTO ITSELF.  THE EQUATION  X = F(X)  IS SOLVED BY
C FOLLOWING THE ZERO CURVE OF THE HOMOTOPY MAP
C
C  LAMBDA*(X - F(X)) + (1 - LAMBDA)*(X - A)  ,
C
C STARTING FROM LAMBDA = 0, X = A.  THE CURVE IS PARAMETERIZED
C BY ARC LENGTH S, AND IS FOLLOWED BY SOLVING THE ORDINARY
C DIFFERENTIAL EQUATION  D(HOMOTOPY MAP)/DS = 0  FOR
C Y(S) = (X(S), LAMBDA(S)) USING A HERMITE CUBIC PREDICTOR AND A
C CORRECTOR WHICH RETURNS TO THE ZERO CURVE ALONG THE FLOW NORMAL
C TO THE DAVIDENKO FLOW (WHICH CONSISTS OF THE INTEGRAL CURVES OF
C D(HOMOTOPY MAP)/DS ).
C
C FOR THE ZERO FINDING PROBLEM F(X) IS ASSUMED TO BE A C2 MAP
C SUCH THAT FOR SOME R > 0,  X*F(X) >= 0  WHENEVER NORM(X) = R.
C THE EQUATION  F(X) = 0  IS SOLVED BY FOLLOWING THE ZERO CURVE
C OF THE HOMOTOPY MAP
C
C   LAMBDA*F(X) + (1 - LAMBDA)*(X - A)
C
C EMANATING FROM LAMBDA = 0, X = A.
C
C  A  MUST BE AN INTERIOR POINT OF THE ABOVE MENTIONED BALLS.
C
C FOR THE CURVE TRACKING PROBLEM RHO(A,X,LAMBDA) IS ASSUMED TO
C BE A C2 MAP FROM E**M X E**N X [0,1) INTO E**N, WHICH FOR
C ALMOST ALL PARAMETER VECTORS A IN SOME NONEMPTY OPEN SUBSET
C OF E**M SATISFIES
C
C  RANK [D RHO(A,X,LAMBDA)/DX , D RHO(A,X,LAMBDA)/D LAMBDA] = N
C
C FOR ALL POINTS (X,LAMBDA) SUCH THAT RHO(A,X,LAMBDA)=0.  IT IS
C FURTHER ASSUMED THAT
C
C           RANK [ D RHO(A,X0,0)/DX ] = N  .
C
C WITH A FIXED, THE ZERO CURVE OF RHO(A,X,LAMBDA) EMANATING
C FROM  LAMBDA = 0, X = X0  IS TRACKED UNTIL  LAMBDA = 1  BY
C SOLVING THE ORDINARY DIFFERENTIAL EQUATION
C D RHO(A,X(S),LAMBDA(S))/DS = 0  FOR  Y(S) = (X(S), LAMBDA(S)),
C WHERE S IS ARC LENGTH ALONG THE ZERO CURVE.  ALSO THE HOMOTOPY
C MAP RHO(A,X,LAMBDA) IS ASSUMED TO BE CONSTRUCTED SUCH THAT
C
C              D LAMBDA(0)/DS > 0  .
C
C
C FOR THE FIXED POINT AND ZERO FINDING PROBLEMS, THE USER
C MUST SUPPLY A SUBROUTINE  F(X,V)  WHICH EVALUATES F(X) AT X
C AND RETURNS THE VECTOR F(X) IN V, AND A SUBROUTINE
C  FJACS(X,QR,LENQR,PIVOT)  WHICH EVALUATES THE (SYMMETRIC)
C JACOBIAN MATRIX OF F(X) AT X, AND RETURNS THE SYMMETRIC
C JACOBIAN MATRIX IN PACKED SKYLINE STORAGE FORMAT IN QR.  LENQR
C AND PIVOT DESCRIBE THE DATA STRUCTURE IN QR.  FOR THE CURVE
C TRACKING PROBLEM, THE USER MUST SUPPLY A SUBROUTINE
C  RHO(A,LAMBDA,X,V,PAR,IPAR)  WHICH EVALUATES THE HOMOTOPY MAP RHO
C AT (A,X,LAMBDA) AND RETURNS THE VECTOR RHO(A,X,LAMBDA) IN V, AND
C A SUBROUTINE  RHOJS(A,LAMBDA,X,QR,LENQR,PIVOT,PP,PAR,IPAR)  WHICH
C RETURNS IN QR THE SYMMETRIC N X N JACOBIAN MATRIX [D RHO/DX]
C EVALUATED AT (A,X,LAMBDA) AND STORED IN PACKED SKYLINE FORMAT, AND
C RETURNS IN PP THE VECTOR -(D RHO/D LAMBDA) EVALUATED AT
C (A,X,LAMBDA).  LENQR AND PIVOT DESCRIBE THE DATA STRUCTURE
C IN QR.
C *** NOTE THE MINUS SIGN IN THE DEFINITION OF PP. ***
C
C
C FUNCTIONS AND SUBROUTINES DIRECTLY OR INDIRECTLY CALLED BY FIXPDS:
C  D1MACH , F (OR  RHO ), FJACS (OR  RHOJS ), GMFADS , MFACDS ,
C  MULTDS , PCGDS , PCGNS , QIMUDS , ROOT , ROOTNS , SOLVDS ,
C  STEPNS , TANGNS , AND THE BLAS FUNCTIONS  DAXPY , DCOPY , DDOT ,
C  DNRM2 , DSCAL , IDAMAX .  ONLY  D1MACH  CONTAINS MACHINE DEPENDENT
C CONSTANTS.  NO OTHER MODIFICATIONS BY THE USER ARE REQUIRED.
C
C
C ON INPUT:
C
C N  IS THE DIMENSION OF X, F(X), AND RHO(A,X,LAMBDA).
C
C Y  IS AN ARRRAY OF LENGTH  N + 1.  (Y(1),...,Y(N)) = A  IS THE
C    STARTING POINT FOR THE ZERO CURVE FOR THE FIXED POINT AND
C    ZERO FINDING PROBLEMS.  (Y(1),...,Y(N)) = X0  FOR THE CURVE
C    TRACKING PROBLEM.
C
C IFLAG  CAN BE -2, -1, 0, 2, OR 3.  IFLAG  SHOULD BE 0 ON THE
C    FIRST CALL TO  FIXPNS  FOR THE PROBLEM  X=F(X), -1 FOR THE
C    PROBLEM  F(X)=0, AND -2 FOR THE PROBLEM  RHO(A,X,LAMBDA)=0.
C    IN CERTAIN SITUATIONS  IFLAG  IS SET TO 2 OR 3 BY  FIXPNS,
C    AND  FIXPNS  CAN BE CALLED AGAIN WITHOUT CHANGING  IFLAG.
C
C ARCRE , ARCAE  ARE THE RELATIVE AND ABSOLUTE ERRORS, RESPECTIVELY,
C    ALLOWED THE NORMAL FLOW ITERATION ALONG THE ZERO CURVE.  IF
C    ARC?E .LE. 0.0  ON INPUT IT IS RESET TO  .5*SQRT(ANS?E) .
C    NORMALLY  ARC?E SHOULD BE CONSIDERABLY LARGER THAN  ANS?E .
C
C ANSRE , ANSAE  ARE THE RELATIVE AND ABSOLUTE ERROR VALUES USED FOR
C    THE ANSWER AT LAMBDA = 1.  THE ACCEPTED ANSWER  Y = (X, LAMBDA)
C    SATISFIES
C
C       |Y(NP1) - 1|  .LE.  ANSRE + ANSAE           .AND.
C
C       ||Z||  .LE.  ANSRE*||X|| + ANSAE          WHERE
C
C    (Z,.) IS THE NEWTON STEP TO Y.
C
C TRACE  IS AN INTEGER SPECIFYING THE LOGICAL I/O UNIT FOR
C    INTERMEDIATE OUTPUT.  IF  TRACE .GT. 0  THE POINTS COMPUTED ON
C    THE ZERO CURVE ARE WRITTEN TO I/O UNIT  TRACE .
C
C A(1:*)  CONTAINS THE PARAMETER VECTOR  A .  FOR THE FIXED POINT
C    AND ZERO FINDING PROBLEMS, A  NEED NOT BE INITIALIZED BY THE
C    USER, AND IS ASSUMED TO HAVE LENGTH  N.  FOR THE CURVE
C    TRACKING PROBLEM, A  MUST BE INITIALIZED BY THE USER.
C
C YP(1:N+1)  IS A WORK ARRAY CONTAINING THE TANGENT VECTOR TO
C    THE ZERO CURVE AT THE CURRENT POINT  Y .
C
C YOLD(1:N+1)  IS A WORK ARRAY CONTAINING THE PREVIOUS POINT FOUND
C    ON THE ZERO CURVE.
C
C YPOLD(1:N+1)  IS A WORK ARRAY CONTAINING THE TANGENT VECTOR TO
C    THE ZERO CURVE AT  YOLD .
C
C QR(1:LENQR)  IS A WORK ARRAY CONTAINING THE N X N SYMMETRIC
C    JACOBIAN MATRIX WITH RESPECT TO X STORED IN PACKED SKYLINE
C    STORAGE FORMAT.  LENQR  AND  PIVOT  DESCRIBE THE DATA
C    STRUCTURE IN  QR .
C
C LENQR  IS THE LENGTH OF THE ONE-DIMENSIONAL ARRAY  QR  .
C
C PIVOT(1:N+2)  IS A WORK ARRAY CONTAINING THE INDICES OF THE
C    DIAGONAL ELEMENTS OF THE N X N SYMMETRIC JACOBIAN MATRIX
C    (WITH RESPECT TO X) WITHIN  QR .
C
C WORK(1:13*(N+1)+2*N+LENQR)  IS A WORK ARRAY SPLIT UP AND USED
C    FOR THE CALCULATION OF THE JACOBIAN MATRIX KERNEL, THE
C    NEWTON STEP, INTERPOLATION, AND THE ESTIMATION OF THE OPTIMAL
C    STEP SIZE  H .
C
C SSPAR(1:8) = (LIDEAL, RIDEAL, DIDEAL, HMIN, HMAX, BMIN, BMAX, P)  IS
C    A VECTOR OF PARAMETERS USED FOR THE OPTIMAL STEP SIZE ESTIMATION.
C    IF  SSPAR(J) .LE. 0.0  ON INPUT, IT IS RESET TO A DEFAULT VALUE
C    BY  FIXPNS .  OTHERWISE THE INPUT VALUE OF  SSPAR(J)  IS USED.
C    SEE THE COMMENTS BELOW AND IN  STEPNS  FOR MORE INFORMATION ABOUT
C    THESE CONSTANTS.
C
C PAR(1:*) AND IPAR(1:*) ARE ARRAYS FOR (OPTIONAL) USER PARAMETERS,
C    WHICH ARE SIMPLY PASSED THROUGH TO THE USER WRITTEN SUBROUTINES
C    RHO, RHOJS.
C
C
C ON OUTPUT:
C
C N , TRACE , A  ARE UNCHANGED.
C
C (Y(1),...,Y(N)) = X, Y(NP1) = LAMBDA, AND Y IS AN APPROXIMATE
C    ZERO OF THE HOMOTOPY MAP.  NORMALLY LAMBDA = 1 AND X IS A
C    FIXED POINT(ZERO) OF F(X).  IN ABNORMAL SITUATIONS LAMBDA
C    MAY ONLY BE NEAR 1 AND X IS NEAR A FIXED POINT(ZERO).
C
C IFLAG =
C  -2   CAUSES  FIXPNS  TO INITIALIZE EVERYTHING FOR THE PROBLEM
C       RHO(A,X,LAMBDA) = 0 (USE ON FIRST CALL).
C
C  -1   CAUSES  FIXPNS  TO INITIALIZE EVERYTHING FOR THE PROBLEM
C       F(X) = 0 (USE ON FIRST CALL).
C
C   0   CAUSES  FIXPNS  TO INITIALIZE EVERYTHING FOR THE PROBLEM
C       X = F(X) (USE ON FIRST CALL).
C
C   1   NORMAL RETURN.
C
C   2   SPECIFIED ERROR TOLERANCE CANNOT BE MET.  SOME OR ALL OF
C       ARCRE , ARCAE , ANSRE , ANSAE  HAVE BEEN INCREASED TO
C       SUITABLE VALUES.  TO CONTINUE, JUST CALL  FIXPNS  AGAIN
C       WITHOUT CHANGING ANY PARAMETERS.
C
C   3   STEPNS  HAS BEEN CALLED 1000 TIMES.  TO CONTINUE, CALL
C       FIXPNS  AGAIN WITHOUT CHANGING ANY PARAMETERS.
C
C   4   THE PRECONDITIONED CONJUGATE GRADIENT ITERATION FAILED TO
C       CONVERGE (PROBABLY BECAUSE THE JACOBIAN MATRIX DID NOT HAVE
C       FULL RANK).  THE ALGORITHM HAS FAILED (THE ZERO CURVE OF
C       THE HOMOTOPY MAP CANNOT BE FOLLOWED ANY FURTHER).
C
C   5   THE TRACKING ALGORITHM HAS LOST THE ZERO CURVE OF THE
C       HOMOTOPY MAP AND IS NOT MAKING PROGRESS.  THE ERROR TOLERANCES
C       ARC?E  AND  ANS?E  WERE TOO LENIENT.  THE PROBLEM SHOULD BE
C       RESTARTED BY CALLING  FIXPNS  WITH SMALLER ERROR TOLERANCES
C       AND  IFLAG = 0 (-1, -2).
C
C   6   THE NORMAL FLOW NEWTON ITERATION IN  STEPNS  OR  ROOTNS
C       FAILED TO CONVERGE.  THE ERROR TOLERANCES  ANS?E  MAY BE TOO
C       STRINGENT.
C
C   7   ILLEGAL INPUT PARAMETERS, A FATAL ERROR.
C
C ARCRE , ARCAE , ANSRE , ANSAE  ARE UNCHANGED AFTER A NORMAL RETURN
C    (IFLAG = 1).  THEY ARE INCREASED TO APPROPRIATE VALUES ON THE
C    RETURN  IFLAG = 2 .
C
C NFE  IS THE NUMBER OF FUNCTION EVALUATIONS (= NUMBER OF
C    JACOBIAN EVALUATIONS).
C
C ARCLEN  IS THE LENGTH OF THE PATH FOLLOWED.
C
C
C
      DOUBLE PRECISION ABSERR,ANSAE,ANSRE,ARCAE,ARCLEN,ARCRE,
     1   CURSW,CURTOL,D1MACH,DNRM2,H,HOLD,RELERR,S
      INTEGER IFLAG,IFLAGC,IPP,IRHO,ITANGW,ITER,ITZ,IW,IWP,
     1   IZ0,IZ1,JW,LENQR,LIMIT,LIMITD,N,NC,NFE,NFEC,NP1,TRACE
      LOGICAL START,CRASH
C
C ***** ARRAY DECLARATIONS. *****
C
      DOUBLE PRECISION Y(N+1),YP(N+1),YOLD(N+1),YPOLD(N+1),A(N),
     $   QR(LENQR),WORK(13*(N+1)+2*N+LENQR),SSPAR(8),PAR(1)
      INTEGER PIVOT(N+2),IPAR(1)
C
C ***** END OF DIMENSIONAL INFORMATION. *****
C
      SAVE
C
C LIMITD  IS AN UPPER BOUND ON THE NUMBER OF STEPS.  IT MAY BE
C CHANGED BY CHANGING THE FOLLOWING PARAMETER STATEMENT:
      PARAMETER (LIMITD=1000)
C
C SWITCH FROM THE TOLERANCE  ARC?E  TO THE (FINER) TOLERANCE  ANS?E  IF
C THE CURVATURE OF ANY COMPONENT OF  Y  EXCEEDS  CURSW.
      PARAMETER (CURSW=10.0)
C
C
C
C :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :  :
      IF (N .LE. 0  .OR.  ANSRE .LE. 0.0  .OR.  ANSAE .LT. 0.0)
     $                                                     IFLAG=7
      IF (IFLAG .GE. -2  .AND.  IFLAG .LE. 0) GO TO 20
      IF (IFLAG .EQ. 2) GO TO 120
      IF (IFLAG .EQ. 3) GO TO 90
C ONLY VALID INPUT FOR  IFLAG  IS -2, -1, 0, 2, 3.
      IFLAG=7
      RETURN
C
C *****  INITIALIZATION BLOCK.  *****
C
20    ARCLEN=0.0
      IF (ARCRE .LE. 0.0) ARCRE=.5*SQRT(ANSRE)
      IF (ARCAE .LE. 0.0) ARCAE=.5*SQRT(ANSAE)
      NC=N
      NFEC=0
      IFLAGC=IFLAG
      NP1=N+1
C SET INDICES FOR SPLITTING UP WORK ARRAY.
      IPP=1
      IRHO=N+1
      IW=IRHO+N
      IWP=IW+NP1
      ITZ=IWP+NP1
      IZ0=ITZ+NP1
      IZ1=IZ0+NP1
      ITANGW=IZ1+NP1
C SET INITIAL CONDITIONS FOR FIRST CALL TO  STEPNS .
      START=.TRUE.
      CRASH=.FALSE.
      HOLD=1.0
      H=.1
      S=0.0
      YPOLD(NP1)=1.0
      YP(NP1)=1.0
      Y(NP1)=0.0
      DO 40 JW=1,N
        YPOLD(JW)=0.0
        YP(JW)=0.0
40    CONTINUE
      DO 50 JW=ITANGW,ITANGW+NP1+N
        WORK(JW)=0.0
50    CONTINUE
C SET OPTIMAL STEP SIZE ESTIMATION PARAMETERS.
C LET Z[K] DENOTE THE NEWTON ITERATES ALONG THE FLOW NORMAL TO THE
C DAVIDENKO FLOW AND Y THEIR LIMIT.
C IDEAL CONTRACTION FACTOR:  ||Z[2] - Z[1]|| / ||Z[1] - Z[0]||
      IF (SSPAR(1) .LE. 0.0) SSPAR(1)= .5
C IDEAL RESIDUAL FACTOR:  ||RHO(A, Z[1])|| / ||RHO(A, Z[0])||
      IF (SSPAR(2) .LE. 0.0) SSPAR(2)= .01
C IDEAL DISTANCE FACTOR:  ||Z[1] - Y|| / ||Z[0] - Y||
      IF (SSPAR(3) .LE. 0.0) SSPAR(3)= .5
C MINIMUM STEP SIZE  HMIN .
      IF (SSPAR(4) .LE. 0.0) SSPAR(4)= (SQRT(N+1.0)+4.0)*D1MACH(4)
C MAXIMUM STEP SIZE  HMAX .
      IF (SSPAR(5) .LE. 0.0) SSPAR(5)= 1.0
C MINIMUM STEP SIZE REDUCTION FACTOR  BMIN .
      IF (SSPAR(6) .LE. 0.0) SSPAR(6)= .1
C MAXIMUM STEP SIZE EXPANSION FACTOR  BMAX .
      IF (SSPAR(7) .LE. 0.0) SSPAR(7)= 3.0
C ASSUMED OPERATING ORDER  P .
      IF (SSPAR(8) .LE. 0.0) SSPAR(8)= 2.0
C
C LOAD  A  FOR THE FIXED POINT AND ZERO FINDING PROBLEMS.
      IF (IFLAGC .GE. -1) THEN
        CALL DCOPY(N,Y,1,A,1)
      ENDIF
90    LIMIT=LIMITD
C
C *****  END OF INITIALIZATION BLOCK.  *****
C
C
C *****  MAIN LOOP.  *****
C
120   DO 400 ITER=1,LIMIT
      IF (Y(NP1) .LT. 0.0) THEN
        ARCLEN=S
        IFLAG=5
        RETURN
      ENDIF
C
C SET DIFFERENT ERROR TOLERANCE IF THE TRAJECTORY Y(S) HAS ANY HIGH
C CURVATURE COMPONENTS.
140   CURTOL=CURSW*HOLD
      RELERR=ARCRE
      ABSERR=ARCAE
      DO 160 JW=1,NP1
        IF (ABS(YP(JW)-YPOLD(JW)) .GT. CURTOL) THEN
          RELERR=ANSRE
          ABSERR=ANSAE
          GO TO 200
        ENDIF
160   CONTINUE
C
C TAKE A STEP ALONG THE CURVE.
200   CALL STEPNS(NC,NFEC,IFLAGC,START,CRASH,HOLD,H,RELERR,ABSERR,
     +     S,Y,YP,YOLD,YPOLD,A,QR,LENQR,PIVOT,WORK,SSPAR,PAR,IPAR)
C PRINT LATEST POINT ON CURVE IF REQUESTED.
      IF (TRACE .GT. 0) THEN
        WRITE (TRACE,217) ITER,NFEC,S,Y(NP1),(Y(JW),JW=1,NC)
217     FORMAT(/' STEP',I5,3X,'NFE =',I5,3X,'ARC LENGTH =',F9.4,3X,
     $  'LAMBDA =',F7.4,5X,'X vector:'/1P,(1X,6E12.4))
      ENDIF
      NFE=NFEC
C CHECK IF THE STEP WAS SUCCESSFUL.
      IF (IFLAGC .GT. 0) THEN
        ARCLEN=S
        IFLAG=IFLAGC
        RETURN
      ENDIF
      IF (CRASH) THEN
C RETURN CODE FOR ERROR TOLERANCE TOO SMALL.
        IFLAG=2
C CHANGE ERROR TOLERANCES.
        IF (ARCRE .LT. RELERR) ARCRE=RELERR
        IF (ANSRE .LT. RELERR) ANSRE=RELERR
        IF (ARCAE .LT. ABSERR) ARCAE=ABSERR
        IF (ANSAE .LT. ABSERR) ANSAE=ABSERR
C CHANGE LIMIT ON NUMBER OF ITERATIONS.
        LIMIT=LIMIT-ITER
        RETURN
      ENDIF
C
      IF (Y(NP1) .GE. 1.0) THEN
C
C USE HERMITE CUBIC INTERPOLATION AND NEWTON ITERATION TO GET THE
C ANSWER AT LAMBDA = 1.0 .
C
C SAVE  YOLD  FOR ARC LENGTH CALCULATION LATER.
        CALL DCOPY(NP1,YOLD,1,WORK(IZ0),1)
C
        CALL ROOTNS(NC,NFEC,IFLAGC,ANSRE,ANSAE,Y,YP,YOLD,YPOLD,
     $              A,QR,LENQR,PIVOT,WORK,PAR,IPAR)
C
        NFE=NFEC
        IFLAG=1
C SET ERROR FLAG IF  ROOTNS  COULD NOT GET THE POINT ON THE ZERO
C CURVE AT  LAMBDA = 1.0  .
        IF (IFLAGC .GT. 0) IFLAG=IFLAGC
C CALCULATE FINAL ARC LENGTH.
        DO 290 JW=1,NP1
          WORK(JW)=Y(JW) - WORK(IZ0+JW-1)
290     CONTINUE
        ARCLEN=S - HOLD + DNRM2(NP1,WORK,1)
        RETURN
      ENDIF
C
400   CONTINUE
C
C *****  END OF MAIN LOOP.  *****
C
C LAMBDA HAS NOT REACHED 1 IN 1000 STEPS.
      IFLAG=3
      ARCLEN=S
      RETURN
C
C
      END
