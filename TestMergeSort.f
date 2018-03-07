
      PROGRAM TMS                              ! main program for test of merge sort

      PARAMETER (M=99999999)

      REAL, ALLOCATABLE :: A(:)
      INTEGER, ALLOCATABLE :: IA(:), IB(:)

      ALLOCATE(A(M))
      ALLOCATE(IA(M))
      ALLOCATE(IB(M))

      DO I=1,M                                 ! initialize array to be sorted
       IF (MOD(I,2).EQ.0) THEN
        A(I)=(M-I)/3.0
       ELSE
        A(I)=(2*I-M)/7.0
       ENDIF
      ENDDO

      CALL CPU_Time(RTIME0)

      CALL MSORT(M,A,IA,IB)                    ! sort indexes of IA array
                                               ! so that A(IA(I)) is sorted
      CALL CPU_Time(RTIME1)
      WRITE(*,710) RTIME1-RTIME0
  710 FORMAT('Time elapsed:',F7.2,'  [sec]')

      write(*,*) A(IA(1)), A(IA(M))

      DEALLOCATE(IB)
      DEALLOCATE(IA)
      DEALLOCATE(A)

      END

!*****************************************************

      SUBROUTINE MSORT(N,A,IB,IC)        !   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15  ...
      REAL A(*)                          ! |--|--|--|--|--|--|--|--|--|--|--|--|--|--|--| ...
      INTEGER IB(*), IC(*)               !  LL          LR          LE                    ... example for K=4
                                         !        SA1         SA2
      DO 1 I=1,N
    1 IB(I)=I                            ! set initial indexes

      K=1                                ! K = subarray (SA) size
    2 IF (K.GT.N) RETURN                 ! outer cycle for increasing SA size
      K2=K+K

      LL=1                               ! LL = left index of the first SA
    3 LR=LL+K; IF (LR.GT.N)   GOTO 4     ! LR = right index of SA; inner cycle
      LE=LR+K; IF (LE.GT.N+1) LE=N+1     ! LE = end of second SA
      M=LL; I=LL; J=LR                   ! indexes for merging SA

      DO WHILE (I.LT.LR.AND.J.LT.LE)     ! merge the first and the second SA into IC
       IF (A(IB(I)).LE.A(IB(J))) THEN
        IC(M)=IB(I); I=I+1               ! move from the first SA
       ELSE
        IC(M)=IB(J); J=J+1               ! move from the second SA
       ENDIF
       M=M+1
      ENDDO

      DO WHILE (I.LT.LR)
       IC(M)=IB(I); I=I+1; M=M+1         ! move the rest of the first SA
      ENDDO

      DO WHILE (J.LT.LE)
       IC(M)=IB(J); J=J+1; M=M+1         ! move the rest of the second SA
      ENDDO

      DO M=LL,LE-1
       IB(M)=IC(M)                       ! move merged part into IB, merging finished
      ENDDO

      LL=LL+K2                           ! set left index of new SA
      GOTO 3                             ! end of inner cycle, go to the next two SA

    4 K=K2                               ! increase SA size two times
      GOTO 2                             ! end of outer cycle

      END



