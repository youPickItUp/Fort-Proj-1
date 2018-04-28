program main

use gau_jor_elim, only: solve_equation
use coefficients_gen, only: generate_coef
use measure_error, only: measure_err

implicit none

!        integer:: args_count,first_argument,lenght,status
!        character(10):: value
!        args_count = command_argument_count()
!        call get_command_argument(1,value,length,status)
!        read(value,*) first_argument

        !coefficients matrixes
        real (kind=4), dimension(:,:), allocatable :: coefficients_k4
        real (kind=8), dimension(:,:), allocatable :: coefficients_k8
        real (kind=16), dimension(:,:), allocatable :: coefficients_k16

        real (kind=4), dimension(:), allocatable :: results_k4
        real (kind=8), dimension(:), allocatable :: results_k8
        real (kind=16), dimension(:), allocatable :: results_k16
        
        !finnal average error
        real (kind=16) :: average_error

        !to mark error of allocation
        integer (kind=4) :: allocate_status
        
        !indicates success or error in solving equation subroutine
        logical :: success

        !needed for main loop in program
        integer (kind=4) :: i
        
        !program parameters or needed to get them 
        integer (kind=4) :: division_n, step_size, args_count, length, status
        character(10) :: value
        
        !getting parameters "in place" or throwing error
        args_count = command_argument_count()
        if(args_count .NE. 2) stop "Wrong args num. Should be max_div & step."
        
        call get_command_argument(1, value, length, status)
        read(value, *) division_n
        
        call get_command_argument(2, value, length, status)
        read(value, *) step_size
        
        !main loop
        do i=step_size, division_n, step_size
            
            write(*,*) "Division number: ", i
            write(*,*)
        
            !#####################################################################################
            ! CALCULATIONS FOR KIND 4
            !#####################################################################################

            
            
            !allocation4
            allocate(coefficients_k4(i - 1, i - 1), stat = allocate_status)
            if(allocate_status .NE. 0) stop "Allocation error11"

            allocate(results_k4(i - 1), stat = allocate_status)
            if(allocate_status .NE. 0) then
                    deallocate(coefficients_k4)
                    stop "Allocation error12"
            end if

            !initializing variables
            call generate_coef(coefficients_k4, i)

            results_k4 = 0
            if(i - 1 .GT. 0) then
                    results_k4(i - 1) = -i ** 2
            end if

            !solving
            call solve_equation(coefficients_k4, results_k4, success)

            !if managed to solve write
            if(success) then
                call measure_err(results_k4, average_error)
                write(*,*) "Kind 4:  ", average_error
            else
                write(*,*) "Failure"
            end if

            !clearing4
            deallocate(coefficients_k4)
            deallocate(results_k4)
            
            

            !#####################################################################################
            ! CALCULATIONS FOR KIND 8
            !#####################################################################################



            !allocation8
            allocate(coefficients_k8(i - 1, i - 1), stat = allocate_status)
            if(allocate_status .NE. 0) then
                stop "Allocation error21"
            end if

            allocate(results_k8(i - 1), stat = allocate_status)
            if(allocate_status .NE. 0) then
                deallocate(coefficients_k8)
                stop "Allocation error22"
            end if

            !initializing variables
            call generate_coef(coefficients_k8, i)

            results_k8 = 0
            if(i - 1 .GT. 0) then
                    results_k8(i - 1) = -i ** 2
            end if

            !solving
            call solve_equation(coefficients_k8, results_k8, success)

            !if managed to solve write
            if(success) then
                call measure_err(results_k8, average_error)
                write(*,*) "Kind 8:  ", average_error
            else
                write(*,*) "Failure"
            end if

            !clearing8
            deallocate(coefficients_k8)
            deallocate(results_k8)

            

            !#####################################################################################
            ! CALCULATIONS FOR KIND 16
            !#####################################################################################



            !allocation16
            allocate(coefficients_k16(i - 1, i - 1), stat = allocate_status)
            if(allocate_status .NE. 0) then
                stop "Allocation error31"
            end if

            allocate(results_k16(i - 1), stat = allocate_status)
            if(allocate_status .NE. 0) then
                deallocate(coefficients_k16)
                stop "Allocation error32"
            end if
            
            !initializing variables
            call generate_coef(coefficients_k16, i)

            results_k16 = 0
            if(i - 1 .GT. 0) then
                    results_k16(i - 1) = -i ** 2
            end if

            !solving
            call solve_equation(coefficients_k16, results_k16, success)

            !if managed to solve write
            if(success) then
                call measure_err(results_k16, average_error)
                write(*,*) "Kind 16: ", average_error
                else
                write(*,*) "Failure"
            end if
            
            !clearing16
            deallocate(coefficients_k16)
            deallocate(results_k16)
            
            write(*,*)
            write(*,*)
            
            
        end do


 
end program 
 
 

