      SUBROUTINE POSNAM(KULNAM,CDNAML)
!-------------------------------------

!--- position in namelist file. 
!    author:  Mats Hamrud, ECMWF

      INTEGER, INTENT(IN)       :: KULNAM
      CHARACTER*(*), INTENT(IN) :: CDNAML
      CHARACTER*120 CLINE
      CHARACTER*1 CLTEST
      REWIND(KULNAM)
      ILEN=LEN(CDNAML)
 102  CONTINUE
      CLINE=' '
      READ(KULNAM,'(A)') CLINE
      IND1=INDEX(CLINE,'&'//CDNAML)
      IF(IND1.EQ.0) GO TO 102
      CLTEST=CLINE(IND1+ILEN+1:IND1+ILEN+1)
      IF((LGE(CLTEST,'0').AND.LLE(CLTEST,'9')).OR.
     &   (LGE(CLTEST,'A').AND.LLE(CLTEST,'Z'))) GO TO 102
      BACKSPACE(KULNAM)

      RETURN
      END SUBROUTINE POSNAM

