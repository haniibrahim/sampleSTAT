module ReadData

! ================== Prologue ==========================================
!
! Purpose:
!    Read 1D-numerical-data from stdin (pipe/redirection) and store it in a
!    1D pointer array.
!
!
! History:
!    Version   Programmer         Date       Description
!    -------   ----------         ---------- -----------
!    1.0       Ibrahim, Hani      2020-11-07 Linked list for reading data
!
! Processing:
!   Read 1-dimensional numerical data from stdin (pipe/redirection) in an
!   1D-pointer-array via linked list method.
!
!
! Special requirements:
!
!
! ------------------ Use Module / Include files ------------------------
    USE SysConst
! ------------------ Implicit ------------------------------------------
    IMPLICIT NONE
! ------------------ Local declarations --------------------------------
    PUBLIC ReadInData
! ------------------ Constant declarations -----------------------------

    contains

    subroutine ReadInData (dat_ptr, nvals, error)
        REAL(DP), DIMENSION(:), POINTER                 :: dat_ptr  ! OUT: Data values (pointer array)
        INTEGER, INTENT(OUT)                            :: nvals    ! OUT: Numbers of values
        INTEGER, INTENT(OUT)                            :: error    ! OUT: Error code:
                                                                    !      0 -> no error
                                                                    !      1 -> NOT USED ANYMORE
                                                                    !      2 -> NOT USED ANYMORE
                                                                    !      3 -> alloc error

        type :: dat_values
            Real(DP)                                    :: dat
            type(dat_values), pointer                   :: next
        end type
        type(dat_values), pointer                       :: head    => null()
        type(dat_values), pointer                       :: current => null()
        type(dat_values), pointer                       :: tail    => null()
        integer                                         :: istat
        integer                                         :: i = 1
        Real(DP)                                        :: temp

        dat_ptr => null()    !
        nvals = 0           ! Set counter to 0
        error = 0           ! Set error flag to 0 (no error)



        input: do
            READ(*,*,iostat=istat) temp
            if (istat == -1) EXIT ! exit on end of file
            IF (istat > 0) THEN
                cycle input ! skip error lines
            ELSE
               nvals = nvals + 1
            END IF
            err_chk: IF (error == 0) THEN
            linked_list: if (.not. associated(head)) then ! first value
                allocate(head, stat=istat)
                tail => head
                nullify(tail%next)
                tail%dat = temp
            else linked_list
                allocate(tail%next, stat=istat)
                tail => tail%next
                nullify(tail%next)
                tail%dat = temp
            end if linked_list
            END IF err_chk
        end do input

        ! Allocate output array
        allocate(dat_ptr(nvals), stat=istat)
        if (istat /= 0) error = 3 ! stop 'Allocation error: output array'

        ! pointer array take over linked list
        current => head
        output: do while (associated(current))
            dat_ptr(i) = current%dat
            current => current%next ! get next values
            i = i + 1
        end do output

        ! dealloc linked list memory
        current => head
        delete_list: do while(associated(current))
            head => current
            current => current%next
            deallocate(head)
        end do delete_list

    end subroutine ReadInData

end module ReadData

