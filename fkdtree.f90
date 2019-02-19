module fkdtree

    use iso_c_binding, only: c_int, c_float, c_double, c_ptr, c_funptr
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
        type(c_funptr) :: destr ! subroutine with type(c_ptr) argument
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
            real(c_double), intent(in) :: pos(*)
            type(c_ptr), value :: data
        end function
        integer(c_int) function kd_insertf(tree,pos,data) bind(c,name="kd_insertf")
            import c_int, c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), intent(in) :: pos(*)
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
            real(c_double), intent(in) :: pos(*)
        end function
        type(c_ptr) function kd_nearestf(tree,pos) bind(c,name="kd_nearestf")
            import c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), intent(in) :: pos(*)
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
            real(c_double), intent(in) :: pos(*)
            real(c_double), value :: range
        end function
        type(c_ptr) function kd_nearest_rangef(tree,pos,range) bind(c,name="kd_nearest_rangef")
            import c_float, c_ptr, kdtree
            type(kdtree) :: tree
            real(c_float), intent(in) :: pos(*)
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
        ! subroutine kd_res_free(set) bind(c,name="kd_res_free")
        !     import c_ptr
        !     type(c_ptr), value :: set
        ! end subroutine

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
            real(c_double) :: pos(*)
        end function
        type(c_ptr) function kd_res_itemf(set,pos) bind(c,name="kd_res_itemf")
            import c_float,c_ptr,kdres
            type(kdres) :: set
            real(c_float) :: pos(*)
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

module kdtree_oo

    use iso_c_binding
    use fkdtree
    implicit none

    type, public :: kdtree_type
        private
        integer(c_int) :: dim = 0
        type(c_ptr) :: kd_c = c_null_ptr
        type(kdtree), pointer :: kd => null()
    contains
        procedure :: clear
        procedure :: insert
        procedure :: set_data_destructor
        final :: free_kdtree_type
    end type

    abstract interface
        subroutine destr_interface(data)
            import c_ptr
            type(c_ptr), value :: data
        end subroutine
    end interface

    interface kdtree_type
        module procedure new_kdtree
    end interface

contains

    type(kdtree_type) function new_kdtree(dim) result(self)
        integer(c_int), intent(in) :: dim

        self%dim = dim
        self%kd_c = kd_create(self%dim)
        call c_f_pointer(self%kd_c,self%kd)
    end function

    subroutine free_kdtree_type(self)
        type(kdtree_type) :: self
        print *, "finalizing"
        self%dim = 0
        call kd_free(self%kd)
        self%kd => null()
        self%kd_c = c_null_ptr
    end subroutine

    subroutine clear(self)
        class(kdtree_type), intent(inout) :: self
        call kd_clear(self%kd)
    end subroutine

    subroutine set_data_destructor(self,destr)
        class(kdtree_type) :: self
        procedure(destr_interface) :: destr

        type(c_funptr) :: cdestr

        cdestr = c_funloc(destr)
        call kd_data_destructor(self%kd,cdestr)
    end subroutine

    subroutine insert(self,pos,data,ierr)
        class(kdtree_type), intent(inout) :: self
        real(c_double), intent(in) :: pos(self%dim)
        type(c_ptr), value, optional :: data
        integer(c_int), intent(out), optional :: ierr
        integer(c_int) :: ret

        ret =  kd_insert(self%kd,pos,data)
        if (present(ierr)) ierr = ret

    end subroutine
end module

! program main

!     use iso_c_binding, only: c_double
!     use kdtree_oo

!     type(kdtree_type) :: kd
!     integer(c_int) :: ierr

!     kd = kdtree_type(3)

!     call kd%insert(pos=[1.0_c_double,2.0_c_double,3.0_c_double],ierr=ierr)
!     print *, ierr

!     call kd%clear()

! contains


! end program