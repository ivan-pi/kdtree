program main

    use iso_c_binding, only: c_int, c_double, c_ptr, c_null_ptr, c_f_pointer, c_loc
    use fkdtree
    implicit none

    integer :: i, ierr, vcount = 10
    type(c_ptr) :: kd_p = c_null_ptr
    type(c_ptr) :: set_p = c_null_ptr

    type(kdtree), pointer :: kd
    type(kdres), pointer :: set

    real(c_double) :: xyz(3), start, end
    character(len=32) :: cmd

    if (command_argument_count() > 0) then
        call get_command_argument(1,cmd)
        if (is_digit(cmd)) read(cmd,*) vcount
    end if

    write(*,"(a,i0,a)") "inserting ", vcount, " random vectors"

    kd_p = kd_create(3)
    call c_f_pointer(kd_p,kd)

    call cpu_time(start)
    do i = 1, vcount
        call random_number(xyz)
        xyz = xyz*200._c_double - 100._c_double
        ! ierr = kd_insert3(kd,xyz(1),xyz(2),xyz(3),c_null_ptr)
        ierr = kd_insert(kd,xyz,c_null_ptr)
        if (ierr /= 0) stop "[kd_insert3] error"
    end do
    call cpu_time(end)
    write(*,"(f0.3,a)") end-start, " sec"

    call cpu_time(start)
    set_p = kd_nearest_range3(kd,0._c_double,0._c_double,0._c_double,40._c_double)
    call c_f_pointer(set_p,set)
    call cpu_time(end)

    write(*,"(a,i0,a,f0.5,a)") "range query returned ", kd_res_size(set), " items in ", end - start, " sec"

    call kd_res_free(set)
    call kd_free(kd)

contains

    logical function is_digit(string)
        character(len=*), intent(in) :: string
        integer(c_int) :: d, e
        read(string,*,iostat=e) d
        is_digit = e == 0
    end function

end program