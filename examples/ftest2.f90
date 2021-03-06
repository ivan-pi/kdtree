program main

    use iso_c_binding
    use fkdtree

    implicit none

    integer(c_int) :: i, num_pts = 10

    type(c_ptr) :: c_ptree
    type(kdtree), pointer :: ptree => null()


    character(len=1), allocatable, target :: data(:)
    type(c_ptr) :: c_pch
    character(len=1), pointer :: pch => null()

    type(c_ptr) :: c_presults
    type(kdres), pointer :: presults => null()

    real(c_double) :: pos(3), dist
    real(c_double) :: pt(3) = [0, 0, 1]
    real(c_double) :: radius = 10

    character(len=32) :: cmd
    integer(c_int) :: ierr

    if (command_argument_count() > 0) then
        call get_command_argument(1,cmd)
        if (is_digit(cmd)) read(cmd,*) num_pts
    end if

    allocate(data(num_pts),stat=ierr)
    if (ierr /= 0) then
        write(*,*) "allocate failed"
        error stop 1
    end if

    ! seed random number generator

    ! create a k-d tree for 3-dimensional points
    c_ptree = kd_create(3)
    call c_f_pointer(c_ptree,ptree)

    ! add some random nodes to the tree (assert nodes are succesfully inserted)
    do i = 1, num_pts
        data(i) = achar(iachar('a') + i)
        ierr = kd_insert3(ptree,rd(),rd(),rd(),c_loc(data(i)))
        if (ierr /= 0) then
            write(*,*) "kd_insert3 failed with error: ", ierr
            error stop 1
        end if
    end do

    ! find points closest to the origin and within distance radius
    c_presults = kd_nearest_range(ptree,pt,radius)
    call c_f_pointer(c_presults,presults)

    ! print out all the points found in results
    write(*,'(a,i0,a)') "found ", kd_res_size(presults), " results:"

    do while (kd_res_end(presults) == 0)
        ! get the data and position of the current result item
        c_pch = kd_res_item(presults,pos)
        call c_f_pointer(c_pch,pch)

        ! compute the distance of the current result from the pt
        dist = sqrt(dist_sq(pt,pos,3))

        ! print out the retrieved data
        write(*,'("node at (",f0.3,",",f0.3,",",f0.3,") is ",f0.3," away and has data=",a)') pos(1), pos(2), pos(3), dist, pch

        ! go to the next entry
        ierr = kd_res_next(presults)
    end do

    ! free our tree, results set, and other allocated memory
    call kd_res_free(presults)
    nullify(presults)
    write(*,*) c_associated(c_presults),c_associated(c_presults,c_loc(presults)), associated(presults)

    call c_f_pointer(c_presults,presults)
    write(*,*) kd_res_size(presults)

    call kd_free(ptree)
    nullify(ptree)
    write(*,*) c_associated(c_presults,c_loc(presults)), associated(ptree)

contains

    real(c_double) function dist_sq(a1,a2,dims)
        integer(c_int), intent(in) :: dims
        real(c_double), intent(in) :: a1(dims), a2(dims)
        real(c_double) :: diff

        dist_sq = 0.0_c_double
        do i = dims, 1, -1
            diff = (a1(i) - a2(i))
            dist_sq = dist_sq + diff*diff
        end do

    end function

    real(c_double) function rd()
        call random_number(rd)
        rd = rd*20.0_c_double - 10.0_c_double
    end function

    logical function is_digit(string)
        character(len=*), intent(in) :: string
        integer(c_int) :: d, e
        read(string,*,iostat=e) d
        is_digit = e == 0
    end function

end program