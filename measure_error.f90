module measure_error
implicit none

        interface measure_err

                module procedure measure_err_k4
                module procedure measure_err_k8
                module procedure measure_err_k16

        end interface measure_err

contains

        subroutine measure_err_k4(results, average_err)
                real (kind=4), dimension (:), intent(in) :: results
                real (kind=16), intent(out) :: average_err

                integer (kind=4) :: i
                average_err = 0.0d0
                
                do i=1, size(results)
                    average_err = average_err + abs(results(i) - real(i, 16) / real((size(results) + 1), 16))
                end do

                average_err = average_err / (size(results) + 1)
                
        end subroutine measure_err_k4

        subroutine measure_err_k8(results, average_err)
                real (kind=8), dimension (:), intent(in) :: results
                real (kind=16), intent(out) :: average_err

                integer (kind=4) :: i
                average_err = 0.0d0

                do i=1, size(results)
                    average_err = average_err + abs(results(i) - real(i, 16) / real((size(results) + 1), 16))
                end do

                average_err = average_err / (size(results) + 1)
                
        end subroutine measure_err_k8

        subroutine measure_err_k16(results, average_err)
                real (kind=16), dimension (:), intent(in) :: results
                real (kind=16), intent(out) :: average_err

                integer (kind=4) :: i
                average_err = 0.0d0

                do i=1, size(results)
                    average_err = average_err + abs(results(i) - real(i, 16) / real((size(results) + 1), 16))
                end do

                average_err = average_err / (size(results) + 1)
                
        end subroutine measure_err_k16



end module measure_error
      
