module gau_jor_elim
implicit none
        private swap_rows, swap
        
        interface swap

                module procedure swap4
                module procedure swap8
                module procedure swap16

        end interface swap
        
        
        interface swap_rows

                module procedure swap_rows4
                module procedure swap_rows8
                module procedure swap_rows16

        end interface swap_rows
        
        
        interface solve_equation

                module procedure solve_equation_kind4
                module procedure solve_equation_kind8
                module procedure solve_equation_kind16	

        end interface solve_equation
		
contains


        subroutine swap4(val1, val2)
            real (kind=4), intent(inout) :: val1, val2
            real (kind=4) :: tmp
            
            tmp = val1
            val1 = val2
            val2 = tmp
        end subroutine
        
        subroutine swap8(val1, val2)
            real (kind=8), intent(inout) :: val1, val2
            real (kind=8) :: tmp
            
            tmp = val1
            val1 = val2
            val2 = tmp
        end subroutine
        
        subroutine swap16(val1, val2)
            real (kind=16), intent(inout) :: val1, val2
            real (kind=16) :: tmp
            
            tmp = val1
            val1 = val2
            val2 = tmp
        end subroutine
        
        
        
        subroutine swap_rows4(coefficients, row1, row2)
                real (kind=4), dimension (:,:), intent(inout) :: coefficients
                integer (kind=4), intent(in) :: row1, row2
                
                integer (kind=4) :: i
                
                do i=1, size(coefficients(:, 1))
                    call swap(coefficients(i, row1), coefficients(i, row2))
                end do
        end subroutine swap_rows4
        
        subroutine swap_rows8(coefficients, row1, row2)
                real (kind=8), dimension (:,:), intent(inout) :: coefficients
                integer (kind=4), intent(in) :: row1, row2
                
                integer (kind=4) :: i
                
                do i=1, size(coefficients(:, 1))
                    call swap(coefficients(i, row1), coefficients(i, row2))
                end do
        end subroutine swap_rows8
        
        subroutine swap_rows16(coefficients, row1, row2)
                real (kind=16), dimension (:,:), intent(inout) :: coefficients
                integer (kind=4), intent(in) :: row1, row2
                
                integer (kind=4) :: i
                
                do i=1, size(coefficients(:, 1))
                    call swap(coefficients(i, row1), coefficients(i, row2))
                end do
        end subroutine swap_rows16
        
        
        
        
        subroutine solve_equation_kind4(coefficients, results, success)
                real (kind=4), dimension(:,:), intent(inout) :: coefficients
                real (kind=4), dimension(:), intent(inout) :: results
                
                logical, intent(out) :: success
                !TRUE if solved, FALSE if there is no unique solution

                real (kind=4) :: quotient

                integer (kind=4) :: i, j, row_n, a_size

                a_size = size(results)
                success = .TRUE.

                outter: do j=1, a_size !loop over diagonal

                            !finds row with nonzero coefficient in column j
                            !and swaps rows j and one found if necessary
                            row_n = j + 1
                            if(coefficients(j, j) .EQ. 0) then
                                
                                do while(row_n .NE. a_size + 1)
                                        if(coefficients(j, row_n) .NE. 0) then
                                            
                                                call swap_rows(coefficients, row_n, j)
                                                call swap(results(row_n), results(j))
                                                exit
                                                
                                        end if
                                        row_n = row_n + 1
                                end do
                                
                            end if

                            if(coefficients(j, j) .EQ. 0) then
                                    success = .FALSE.
                                    exit outter
                                    !this equation doesn't have unique solution, so subroutine returns FALSE as error and terminates
                            end if

                            !starts real elimination
                            !loop over rows - creates diagonal matrix
                            do i=1, j-1
                                    quotient = coefficients(j, i) / coefficients(j, j)
                                    coefficients(j:, i) = coefficients(j:, i) - coefficients(j:, j) * quotient
                                    results(i) = results(i) - results(j) * quotient
                            end do

                            do i=j+1, a_size
                                    quotient = coefficients(j, i) / coefficients(j, j)
                                    coefficients(j:, i) = coefficients(j:, i) - coefficients(j:, j) * quotient
                                    results(i) = results(i) - results(j) * quotient
                            end do

                end do outter

                !calculate final results - 1s on diagonal
                if(success) then
                            do i=1, a_size
                                    results(i) = results(i) / coefficients(i, i)
                                    coefficients(i, i) = 1
                            end do
                end if

        end subroutine solve_equation_kind4




        subroutine solve_equation_kind8(coefficients, results, success)
                real (kind=8), dimension(:,:), intent(inout) :: coefficients
                real (kind=8), dimension(:), intent(inout) :: results
                logical, intent(out) :: success
                !TRUE if solved, FALSE if there is no unique solution

                real (kind=8) :: quotient

                integer (kind=4) :: i, j, row_n, a_size

                a_size = size(results)
                success = .TRUE.

                outter: do j=1, a_size !loop over diagonal

                            !finds row with nonzero coefficient in column j
                            !and swaps rows j and found if necessary
                            row_n = j + 1
                            if(coefficients(j, j) .EQ. 0) then
                                
                                do while(row_n .NE. a_size + 1)
                                        if(coefficients(j, row_n) .NE. 0) then
                                            
                                                call swap_rows(coefficients, row_n, j)
                                                call swap(results(row_n), results(j))
                                                exit
                                                
                                        end if
                                        row_n = row_n + 1
                                end do
                                
                            end if

                            if(coefficients(j, j) .EQ. 0) then
                                    success = .FALSE.
                                    exit outter
                                    !this equation doesn't have unique solution, so subroutine returns FALSE as error and terminates
                            end if

                            !starts real elimination
                            !loop over rows - creates diagonal matrix
                            do i=1, j-1
                                    quotient = coefficients(j, i) / coefficients(j, j)
                                    coefficients(j:, i) = coefficients(j:, i) - coefficients(j:, j) * quotient
                                    results(i) = results(i) - results(j) * quotient
                            end do

                            do i=j+1, a_size
                                    quotient = coefficients(j, i) / coefficients(j, j)
                                    coefficients(j:, i) = coefficients(j:, i) - coefficients(j:, j) * quotient
                                    results(i) = results(i) - results(j) * quotient
                            end do
                            
                end do outter

                !calculate final results - 1s on diagonal
                if(success) then
                            do i=1, a_size
                                    results(i) = results(i) / coefficients(i, i)
                                    coefficients(i, i) = 1
                            end do
                end if

        end subroutine solve_equation_kind8


        subroutine solve_equation_kind16(coefficients, results, success)
                real (kind=16), dimension(:,:), intent(inout) :: coefficients
                real (kind=16), dimension(:), intent(inout) :: results
                logical, intent(out) :: success
                !TRUE if solved, FALSE if there is no unique solution

                real (kind=16) :: quotient

                integer (kind=4) :: i, j, row_n, a_size

                a_size = size(results)
                success = .TRUE.

                outter: do j=1, a_size !loop over diagonal

                            !finds row with nonzero coefficient in column j
                            !and swaps rows j and found if necessary
                            row_n = j + 1
                            if(coefficients(j, j) .EQ. 0) then
                                
                                do while(row_n .NE. a_size + 1)
                                        if(coefficients(j, row_n) .NE. 0) then
                                            
                                                call swap_rows(coefficients, row_n, j)
                                                call swap(results(row_n), results(j))
                                                exit
                                                
                                        end if
                                        row_n = row_n + 1
                                end do
                                
                            end if

                            if(coefficients(j, j) .EQ. 0) then
                                    success = .FALSE.
                                    exit outter
                                    !this equation doesn't have unique solution, so subroutine returns FALSE as error and terminates
                            end if

                            !starts real elimination
                            !loop over rows - creates diagonal matrix
                            do i=1, j-1
                                    quotient = coefficients(j, i) / coefficients(j, j)
                                    coefficients(j:, i) = coefficients(j:, i) - coefficients(j:, j) * quotient
                                    results(i) = results(i) - results(j) * quotient
                            end do

                            do i=j+1, a_size
                                    quotient = coefficients(j, i) / coefficients(j, j)
                                    coefficients(j:, i) = coefficients(j:, i) - coefficients(j:, j) * quotient
                                    results(i) = results(i) - results(j) * quotient
                            end do

                end do outter

                !calculate final results - ones on diagonal
                if(success) then
                            do i=1, a_size
                                    results(i) = results(i) / coefficients(i, i)
                                    coefficients(i, i) = 1
                            end do
                end if

        end subroutine solve_equation_kind16

end module gau_jor_elim
