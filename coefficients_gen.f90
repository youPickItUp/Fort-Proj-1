module coefficients_gen
implicit none

        interface generate_coef

                module procedure generate_coef4
                module procedure generate_coef8
                module procedure generate_coef16

        end interface generate_coef

contains

        subroutine generate_coef4(coefficients, division_n)
                real (kind=4), dimension (:,:), intent(inout) :: coefficients

                integer (kind=4), intent(in) :: division_n
                integer (kind=4) :: i
                
                coefficients(1, 1) = -2 * division_n ** 2
                coefficients(1, 2) = division_n ** 2
                do i=2, division_n - 2
                        coefficients(i - 1, i) = division_n ** 2
                        coefficients(i, i) = -2 * division_n ** 2
                        coefficients(i + 1, i) = division_n ** 2
                end do
                coefficients(division_n - 1, division_n - 2) = division_n ** 2
                coefficients(division_n - 1, division_n - 1) = -2 * division_n ** 2
        end subroutine generate_coef4

        subroutine generate_coef8(coefficients, division_n)
                real (kind=8), dimension (:,:), intent(inout) :: coefficients

                integer (kind=4), intent(in) :: division_n
                integer (kind=4) :: i
                
                coefficients(1, 1) = -2 * division_n ** 2
                coefficients(1, 2) = division_n ** 2
                do i=2, division_n - 2
                        coefficients(i - 1, i) = division_n ** 2
                        coefficients(i, i) = -2 * division_n ** 2
                        coefficients(i + 1, i) = division_n ** 2
                end do
                coefficients(division_n - 1, division_n - 2) = division_n ** 2
                coefficients(division_n - 1, division_n - 1) = -2 * division_n ** 2
        end subroutine generate_coef8

        subroutine generate_coef16(coefficients, division_n)
                real (kind=16), dimension (:,:), intent(inout) :: coefficients

                integer (kind=4), intent(in) :: division_n
                integer (kind=4) :: i
                
                coefficients(1, 1) = -2 * division_n ** 2
                coefficients(1, 2) = division_n ** 2
                do i=2, division_n - 2
                        coefficients(i - 1, i) = division_n ** 2
                        coefficients(i, i) = -2 * division_n ** 2
                        coefficients(i + 1, i) = division_n ** 2
                end do
                coefficients(division_n - 1, division_n - 2) = division_n ** 2
                coefficients(division_n - 1, division_n - 1) = -2 * division_n ** 2
        end subroutine generate_coef16



end module coefficients_gen
