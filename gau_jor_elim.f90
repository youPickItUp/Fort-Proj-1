module gau_jor_elim
implicit none
        private swap_rows
        private row_tmp

        real (kind=4), dimension(:), allocatable :: row_tmp
        
		interface solve_equation

			module procedure solve_equation_kind4
			module procedure solve_equation_kind8
			module procedure solve_equation_kind16	
		    
		end interface solve_equation
		
		interface swap_rows
			
			module procedure swap_rows4
			module procedure swap_rows8
			module procedure swap_rows16
			
		end interface swap_rows
		
contains
        
        !happens only ones for each row, so I agree on worse performens
        !with (sth, :) because I need (:, sth) later in nested loop
        subroutine swap_rows4(coefficients, row1, row2)
                real (kind=4), dimension (:,:) :: coefficients
                integer (kind=4) :: row1, row2

                row_tmp = coefficients(row1, :)
                coefficients(row1, :) = coefficients(:, row2)
                coefficients(row2, :) = row_tmp
        end subroutine swap_rows4
        
        subroutine swap_rows8(coefficients, row1, row2)
                real (kind=8), dimension (:,:) :: coefficients
                integer (kind=8) :: row1, row2

                row_tmp = coefficients(row1, :)
                coefficients(row1, :) = coefficients(:, row2)
                coefficients(row2, :) = row_tmp
        end subroutine swap_rows8
        
        subroutine swap_rows16(coefficients, row1, row2)
                real (kind=16), dimension (:,:) :: coefficients
                integer (kind=16) :: row1, row2

                row_tmp = coefficients(row1, :)
                coefficients(row1, :) = coefficients(:, row2)
                coefficients(row2, :) = row_tmp
        end subroutine swap_rows16
        
        
        
        
		subroutine solve_equation_kind4(coefficients, results, success)
			real (kind=4), dimension(:,:), intent(inout) :: coefficients
			real (kind=4), dimension(:), intent(inout) :: results
			logical, intent(out) :: success!TRUE if solved, FALSE if there is no unique solution

			real (kind=4) :: quotient

			integer (kind=4) :: i, j, row_n, a_size

			a_size = size(results)
			success = .TRUE.
		   
			allocate(row_tmp(a_size)) !in case I need to use swap_rows

			outter: do j=1, a_size !loop over columns
				    
				    !finds row with nonzero coefficient in column j
				    !and swaps rows j and found if necessary
				    row_n = 1
				    do while(coefficients(j, j) .EQ. 0  .AND.  row_n .NE. a_size)
				            if(coefficients(j, row_n) .NE. 0) then
				                    call swap_rows(coefficients, row_n, j)
				                    exit
				            end if
				            row_n = row_n + 1
				    end do

				    if(coefficients(j, j) .EQ. 0) then
				            success = .FALSE.
				            exit outter
				            !this equation doesn't have unique solution, so subroutine returns 1 as error and terminates
				    end if

				    !starts real elimination
				    
				    !do i=1, a_size
				    !        write(*,*) coefficients(:, i)
				    !end do


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
				    !write(*,*) "quotient"
				    !write(*,*) quotient

			end do outter

			!calculate final results - ones on diagonal
			if(success) then
				    !write(*,*) results
				    do i=1, a_size
				            results(i) = results(i) / coefficients(i, i)
				            coefficients(i, i) = 1
				    end do
				    !write(*,*) results
			end if

			deallocate(row_tmp)

		end subroutine solve_equation_kind4




		subroutine solve_equation_kind8(coefficients, results, success)
			real (kind=8), dimension(:,:), intent(inout) :: coefficients
			real (kind=8), dimension(:), intent(inout) :: results
			logical, intent(out) :: success!TRUE if solved, FALSE if there is no unique solution

			real (kind=8) :: quotient

			integer (kind=8) :: i, j, row_n, a_size

			a_size = size(results)
			success = .TRUE.
		   
			allocate(row_tmp(a_size)) !in case I need to use swap_rows

			outter: do j=1, a_size !loop over columns
				    
				    !finds row with nonzero coefficient in column j
				    !and swaps rows j and found if necessary
				    row_n = 1
				    do while(coefficients(j, j) .EQ. 0  .AND.  row_n .NE. a_size)
				            if(coefficients(j, row_n) .NE. 0) then
				                    call swap_rows(coefficients, row_n, j)
				                    exit
				            end if
				            row_n = row_n + 1
				    end do

				    if(coefficients(j, j) .EQ. 0) then
				            success = .FALSE.
				            exit outter
				            !this equation doesn't have unique solution, so subroutine returns 1 as error and terminates
				    end if

				    !starts real elimination
				    
				    !do i=1, a_size
				    !        write(*,*) coefficients(:, i)
				    !end do


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
				    !write(*,*) "quotient"
				    !write(*,*) quotient

			end do outter

			!calculate final results - ones on diagonal
			if(success) then
				    !write(*,*) results
				    do i=1, a_size
				            results(i) = results(i) / coefficients(i, i)
				            coefficients(i, i) = 1
				    end do
				    !write(*,*) results
			end if

			deallocate(row_tmp)

		end subroutine solve_equation_kind8


		subroutine solve_equation_kind16(coefficients, results, success)
			real (kind=16), dimension(:,:), intent(inout) :: coefficients
			real (kind=16), dimension(:), intent(inout) :: results
			logical, intent(out) :: success!TRUE if solved, FALSE if there is no unique solution

			real (kind=16) :: quotient

			integer (kind=16) :: i, j, row_n, a_size

			a_size = size(results)
			success = .TRUE.
		   
			allocate(row_tmp(a_size)) !in case I need to use swap_rows

			outter: do j=1, a_size !loop over columns
				    
				    !finds row with nonzero coefficient in column j
				    !and swaps rows j and found if necessary
				    row_n = 1
				    do while(coefficients(j, j) .EQ. 0  .AND.  row_n .NE. a_size)
				            if(coefficients(j, row_n) .NE. 0) then
				                    call swap_rows(coefficients, row_n, j)
				                    exit
				            end if
				            row_n = row_n + 1
				    end do

				    if(coefficients(j, j) .EQ. 0) then
				            success = .FALSE.
				            exit outter
				            !this equation doesn't have unique solution, so subroutine returns 1 as error and terminates
				    end if

				    !starts real elimination
				    
				    !do i=1, a_size
				    !        write(*,*) coefficients(:, i)
				    !end do


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
				    !write(*,*) "quotient"
				    !write(*,*) quotient

			end do outter

			!calculate final results - ones on diagonal
			if(success) then
				    !write(*,*) results
				    do i=1, a_size
				            results(i) = results(i) / coefficients(i, i)
				            coefficients(i, i) = 1
				    end do
				    !write(*,*) results
			end if

			deallocate(row_tmp)

		end subroutine solve_equation_kind16

end module gau_jor_elim
