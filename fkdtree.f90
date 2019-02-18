module fkdtree

    use iso_c_binding
    implicit none
    public

    ! struct kdtree {
    !     int dim;
    !     struct kdnode *root;
    !     struct kdhyperrect *rect;
    !     void (*destr)(void*);
    ! };
    type, bind(c) :: kdtree
        integer(c_int) :: dim
        type(c_ptr) :: root
        type(c_ptr) :: rect
        type(c_funptr) :: destr
    end type

    ! struct kdres {
    !     struct kdtree *tree;
    !     struct res_node *rlist, *riter;
    !     int size;
    ! };
    type, bind(c) :: kdres
        type(c_ptr) :: tree
        type(c_ptr) :: rlist, riter
        integer(c_int) :: size
    end type

    interface

        ! create a kd-tree for "k"-dimensional data
        ! struct kdtree *kd_create(int k);
        type(c_ptr) function kd_create(k) bind(c,name="kd_create")
            import c_int, c_ptr
            integer(c_int), value :: k
        end function
        
        ! free the struct kdtree
        ! void kd_free(struct kdtree *tree);
        subroutine kd_free(tree) bind(c,name="kd_free")
            import kdtree
            type(kdtree) :: tree
        end subroutine

        ! remove all the elements from the tree
        ! void kd_clear(struct kdtree *tree);
        subroutine kd_clear(tree) bind(c,name="kd_clear")
            import kdtree
            type(kdtree) :: tree
        end subroutine

        ! if called with non-null 2nd argument, the function provided
        ! will be called on data pointers (see kd_insert) when nodes
        ! are to be removed from the tree.
        ! 
        ! void kd_data_destructor(struct kdtree *tree, void (*destr)(void*));
        subroutine kd_data_destructor(tree,destr) bind(c,name="kd_data_destructor")
            import kdtree, c_funptr
            type(kdtree) :: tree
            type(c_funptr), value :: destr
        end subroutine

        ! insert a node, specifying its position, and optional data
        ! int kd_insert(struct kdtree *tree, const double *pos, void *data);
        ! int kd_insertf(struct kdtree *tree, const float *pos, void *data);
        ! int kd_insert3(struct kdtree *tree, double x, double y, double z, void *data);
        ! int kd_insert3f(struct kdtree *tree, float x, float y, float z, void *data);

        integer(c_int) function kd_insert(tree,pos,data) bind(c,name="kd_insert")
            import c_int, c_double, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_double), intent(in) :: pos
            type(c_ptr), value :: data
        end function
        integer(c_int) function kd_insertf(tree,pos,data) bind(c,name="kd_insertf")
            import c_int, c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), intent(in) :: pos
            type(c_ptr), value :: data
        end function
        integer(c_int) function kd_insert3(tree,x,y,z,data) bind(c,name="kd_insert3")
            import c_int, c_double, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_double), value :: x, y, z
            type(c_ptr), value :: data
        end function
        integer(c_int) function kd_insert3f(tree,x,y,z,data) bind(c,name="kd_insert3f")
            import c_int, c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), value :: x, y, z
            type(c_ptr), value :: data
        end function

        ! Find the nearest node from a given point.
        ! 
        ! This function returns a pointer to a result set with at most one element.
        ! 
        ! struct kdres *kd_nearest(struct kdtree *tree, const double *pos);
        ! struct kdres *kd_nearestf(struct kdtree *tree, const float *pos);
        ! struct kdres *kd_nearest3(struct kdtree *tree, double x, double y, double z);
        ! struct kdres *kd_nearest3f(struct kdtree *tree, float x, float y, float z);
        type(c_ptr) function kd_nearest(tree,pos) bind(c,name="kd_nearest")
            import c_double, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_double), intent(in) :: pos
        end function
        type(c_ptr) function kd_nearestf(tree,pos) bind(c,name="kd_nearestf")
            import c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), intent(in) :: pos
        end function
        type(c_ptr) function kd_nearest3(tree,x,y,z) bind(c,name="kd_nearest3")
            import c_double, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_double), value :: x, y, z
        end function
        type(c_ptr) function kd_nearest3f(tree,x,y,z) bind(c,name="kd_nearest3f")
            import c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), value :: x, y, z
        end function


        ! Find any nearest nodes from a given point within a range.
        ! 
        ! This function returns a pointer to a result set, which can be manipulated
        ! by the kd_res_* functions.
        ! The returned pointer can be null as an indication of an error. Otherwise
        ! a valid result set is always returned which may contain 0 or more elements.
        ! The result set must be deallocated with kd_res_free after use.
        ! 
        ! struct kdres *kd_nearest_range(struct kdtree *tree, const double *pos, double range);
        ! struct kdres *kd_nearest_rangef(struct kdtree *tree, const float *pos, float range);
        ! struct kdres *kd_nearest_range3(struct kdtree *tree, double x, double y, double z, double range);
        ! struct kdres *kd_nearest_range3f(struct kdtree *tree, float x, float y, float z, float range);
        type(c_ptr) function kd_nearest_range(tree,pos,range) bind(c,name="kd_nearest_range")
            import c_double, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_double), intent(in) :: pos
            real(c_double), value :: range
        end function
        type(c_ptr) function kd_nearest_rangef(tree,pos,range) bind(c,name="kd_nearest_rangef")
            import c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), intent(in) :: pos
            real(c_float), value :: range
        end function
        type(c_ptr) function kd_nearest_range3(tree,x,y,z,range) bind(c,name="kd_nearest_range3")
            import c_double, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_double), value :: x, y, z
            real(c_double), value :: range
        end function
        type(c_ptr) function kd_nearest_range3f(tree,x,y,z,range) bind(c,name="kd_nearest_range3f")
            import c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), value :: x, y, z
            real(c_float), value :: range
        end function


        ! frees a result set returned by kd_nearest_range()
        ! void kd_res_free(struct kdres *set);
        subroutine kd_res_free(set) bind(c,name="kd_res_free")
            import kdres
            type(kdres) :: set
        end subroutine

        ! returns the size of the result set (in elements)
        ! int kd_res_size(struct kdres *set);
        integer(c_int) function kd_res_size(set) bind(c,name="kd_res_size")
            import c_int, kdres
            type(kdres) :: set
        end function

        ! rewinds the result set iterator
        ! void kd_res_rewind(struct kdres *set);
        subroutine kd_res_rewind(set) bind(c,name="kd_res_rewind")
            import kdres
            type(kdres) :: set
        end subroutine

        ! returns non-zero if the set iterator reached the end after the last element
        ! int kd_res_end(struct kdres *set);
        integer(c_int) function kd_res_end(set) bind(c,name="kd_res_end")
            import c_int, kdres
            type(kdres) :: set
        end function

        ! advances the result set iterator, returns non-zero on success, zero if
        ! there are no more elements in the result set.
        !
        ! int kd_res_next(struct kdres *set);
        integer(c_int) function kd_res_next(set) bind(c,name="kd_res_next")
            import c_int, kdres
            type(kdres) :: set
        end function

        ! returns the data pointer (can be null) of the current result set item
        ! and optionally sets its position to the pointers(s) if not null.
        ! 
        ! void *kd_res_item(struct kdres *set, double *pos);
        ! void *kd_res_itemf(struct kdres *set, float *pos);
        ! void *kd_res_item3(struct kdres *set, double *x, double *y, double *z);
        ! void *kd_res_item3f(struct kdres *set, float *x, float *y, float *z);
        type(c_ptr) function kd_res_item(set,pos) bind(c,name="kd_res_item")
            import c_double,c_ptr,kdres
            type(kdres) :: set
            real(c_double) :: pos
        end function
        type(c_ptr) function kd_res_itemf(set,pos) bind(c,name="kd_res_itemf")
            import c_float,c_ptr,kdres
            type(kdres) :: set
            real(c_float) :: pos
        end function
        type(c_ptr) function kd_res_item3(set,x,y,z) bind(c,name="kd_res_item3")
            import c_double,c_ptr,kdres
            type(kdres) :: set
            real(c_double) :: x,y,z
        end function
        type(c_ptr) function kd_res_item3f(set,x,y,z) bind(c,name="kd_res_item3f")
            import c_float,c_ptr,kdres
            type(kdres) :: set
            real(c_float) :: x,y,z
        end function

        ! equivalent to kd_res_item(set, 0) */
        ! void *kd_res_item_data(struct kdres *set);
        type(c_ptr) function kd_res_item_data(set) bind(c,name="kd_res_item_data")
            import c_ptr, kdres
            type(kdres) :: set
        end function

    end interface

end module

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