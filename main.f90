program main

use gau_jor_elim, only: solve_equation
use coefficients_gen, only: generate_coef
use measure_error, only: measure_err

implicit none

        real (kind=4), dimension(:,:), allocatable :: coefficients_k4
        real (kind=8), dimension(:,:), allocatable :: coefficients_k8
        real (kind=16), dimension(:,:), allocatable :: coefficients_k16

        real (kind=4), dimension(:), allocatable :: results_k4
        real (kind=8), dimension(:), allocatable :: results_k8
        real (kind=16), dimension(:), allocatable :: results_k16
        
        real (kind=16) :: average_error

        integer (kind=4) :: division_n, kind_p, allocate_status

        logical :: success

        integer (kind=4) :: i, j

        read(*,*) division_n
        
        !#####################################################################################
        ! CALCULATIONS FOR KIND 4
        !#####################################################################################

        allocate(coefficients_k4(division_n - 1, division_n - 1), stat = allocate_status)
        if(allocate_status .NE. 0) stop "Allocation error1"

        allocate(results_k4(division_n - 1), stat = allocate_status)
        if(allocate_status .NE. 0) then
                deallocate(coefficients_k4)
                stop "Allocation error2"
        end if

        call generate_coef(coefficients_k4, division_n)

        results_k4 = 0
        if(division_n - 1 .GT. 0) then
                results_k4(division_n - 1) = -division_n ** 2
        end if

        call solve_equation(coefficients_k4, results_k4, success)

        if(success) then
            call measure_err(results_k4, average_error)
            write(*,*) "Kind 4: ", average_error
        else
            write(*,*) "Failure"
        end if

        deallocate(coefficients_k4)
        deallocate(results_k4)
        
        !#####################################################################################
        ! CALCULATIONS FOR KIND 8
        !#####################################################################################
        
        allocate(coefficients_k8(division_n - 1, division_n - 1), stat = allocate_status)
        if(allocate_status .NE. 0) stop "Allocation error1"

        allocate(results_k8(division_n - 1), stat = allocate_status)
        if(allocate_status .NE. 0) then
                deallocate(coefficients_k8)
                stop "Allocation error2"
        end if

        call generate_coef(coefficients_k8, division_n)

        results_k8 = 0
        if(division_n - 1 .GT. 0) then
                results_k8(division_n - 1) = -division_n ** 2
        end if

        call solve_equation(coefficients_k8, results_k8, success)

        if(success) then
            call measure_err(results_k8, average_error)
            write(*,*) "Kind 8: ", average_error
        else
            write(*,*) "Failure"
        end if

        deallocate(coefficients_k8)
        deallocate(results_k8)

        !#####################################################################################
        ! CALCULATIONS FOR KIND 16
        !#####################################################################################
        
        allocate(coefficients_k16(division_n - 1, division_n - 1), stat = allocate_status)
        if(allocate_status .NE. 0) stop "Allocation error1"

        allocate(results_k16(division_n - 1), stat = allocate_status)
        if(allocate_status .NE. 0) then
                deallocate(coefficients_k16)
                stop "Allocation error2"
        end if

        call generate_coef(coefficients_k16, division_n)

        results_k16 = 0
        if(division_n - 1 .GT. 0) then
                results_k16(division_n - 1) = -division_n ** 2
        end if

        call solve_equation(coefficients_k16, results_k16, success)

        if(success) then
            call measure_err(results_k16, average_error)
            write(*,*) "Kind 16: ", average_error
        else
            write(*,*) "Failure"
        end if

        deallocate(coefficients_k16)
        deallocate(results_k16)

 
end program 
 
 

