module coefficients_gen
implicit none

public  generate_coef
private

        interface generate_coef

                module procedure generate_coef4
                module procedure generate_coef8
                module procedure generate_coef16

        end interface generate_coef

contains

        subroutine generate_coef4(coefficients, division_n)
                real (kind=4), dimension (:,:), intent(inout) :: coefficients

                integer (kind=4), intent(in) :: division_n
                integer (kind=4) :: i, j

                do i=1, division_n - 1
                        do j=1, division_n - 1
                                if(i - 1 .EQ. j) then
                                        coefficients(j, i) = division_n ** 2
                                else if(i .EQ. j) then
                                        coefficients(j, i) = -2 * division_n ** 2
                                else if(i + 1 .EQ. j) then
                                        coefficients(j, i) = division_n ** 2
                                else
                                        coefficients(j, i) = 0
                                end if
                        end do
                end do

                
        end subroutine generate_coef4

        subroutine generate_coef8(coefficients, division_n)
                real (kind=8), dimension (:,:), intent(inout) :: coefficients

                integer (kind=4), intent(in) :: division_n
                integer (kind=4) :: i, j
                
                do i=1, division_n - 1
                        do j=1, division_n - 1
                                if(i - 1 .EQ. j) then
                                        coefficients(j, i) = division_n ** 2
                                else if(i .EQ. j) then
                                        coefficients(j, i) = -2 * division_n ** 2
                                else if(i + 1 .EQ. j) then
                                        coefficients(j, i) = division_n ** 2
                                else
                                        coefficients(j, i) = 0
                                end if
                        end do
                end do
                

        end subroutine generate_coef8

        subroutine generate_coef16(coefficients, division_n)
                real (kind=16), dimension (:,:), intent(inout) :: coefficients

                integer (kind=4), intent(in) :: division_n
                integer (kind=4) :: i, j
                
                do i=1, division_n - 1
                        do j=1, division_n - 1
                                if(i - 1 .EQ. j) then
                                        coefficients(j, i) = division_n ** 2
                                else if(i .EQ. j) then
                                        coefficients(j, i) = -2 * division_n ** 2
                                else if(i + 1 .EQ. j) then
                                        coefficients(j, i) = division_n ** 2
                                else
                                        coefficients(j, i) = 0
                                end if
                        end do
                end do

        end subroutine generate_coef16



end module coefficients_gen
