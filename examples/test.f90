program main

    use iso_c_binding, only: c_int, c_double, c_ptr
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

        ierr = kd_insert3(kd,xyz(1),xyz(2),xyz(3),c_null_ptr)
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

    ! int i, vcount = 10;
    ! void *kd, *set;
    ! unsigned int msec, start;

    ! if(argc > 1 && isdigit(argv[1][0])) {
    !     vcount = atoi(argv[1]);
    ! }
    ! printf("inserting %d random vectors... ", vcount);
    ! fflush(stdout);

    ! kd = kd_create(3);

    ! start = get_msec();
    ! for(i=0; i<vcount; i++) {
    !     float x, y, z;
    !     x = ((float)rand() / RAND_MAX) * 200.0 - 100.0;
    !     y = ((float)rand() / RAND_MAX) * 200.0 - 100.0;
    !     z = ((float)rand() / RAND_MAX) * 200.0 - 100.0;

    !     assert(kd_insert3(kd, x, y, z, 0) == 0);
    ! }
    ! msec = get_msec() - start;
    ! printf("%.3f sec\n", (float)msec / 1000.0);

    ! start = get_msec();
    ! set = kd_nearest_range3(kd, 0, 0, 0, 40);
    ! msec = get_msec() - start;
    ! printf("range query returned %d items in %.5f sec\n", kd_res_size(set), (float)msec / 1000.0);
    ! kd_res_free(set);

    ! kd_free(kd);
    ! return 0;