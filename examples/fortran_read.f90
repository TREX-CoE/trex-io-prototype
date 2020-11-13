program write_ezfio_example
  use trexio
  implicit none
  integer                        :: nucl_num         ! Number of nuclei
  character*(256)                :: title            ! Title of the file
  character*(64), allocatable    :: nucl_label(:)    ! Atom labels
  real*8, allocatable            :: nucl_charge(:)   ! Nuclear charges
  real*8, allocatable            :: nucl_coord(:,:)  ! Nuclear coordinates
  integer                        :: alpha_num        ! Number of alpha electrons
  integer                        :: beta_num         ! Number of beta  electrons

  integer*8                      :: trex_file        ! Handle for the TREX file
  integer                        :: i
  integer                        :: info
  character*(*), parameter       :: trex_filename = 'trex_file'
  double precision, parameter    :: a0 = 0.52917721067d0

  ! Read the data from the TREX file
  ! ================================

  ! Open the file
  ! -------------

  info = 0
  info = trexio_open(trex_filename,'r', trex_file)
  call check_success(info, 'Unable to open file')


  ! Read the metadata
  ! ------------------

  info = trexio_get_metadata_description(trex_file,title)
  call check_success(info, 'Unable to get description')
  print *, 'Description: ', trim(title)

  ! Read data for the electrons
  ! ----------------------------

  info = trexio_get_electron_up_num(trex_file,alpha_num)
  call check_success(info, 'Unable to get up electrons')

  info = trexio_get_electron_dn_num(trex_file,beta_num)
  call check_success(info, 'Unable to get dn electrons')
  print *, 'Electrons: ', alpha_num, ' up, ', beta_num, ' down'


  ! Read the data for the nuclei
  ! -----------------------------

  info = trexio_get_nucleus_num(trex_file,nucl_num)
  call check_success(info, 'Unable to get number of nuclei')

  allocate( nucl_coord(3,nucl_num), nucl_charge(nucl_num), &
       nucl_label(nucl_num) )

  ! Convert into atomic units
  nucl_coord = nucl_coord / a0
  info = trexio_get_nucleus_coord(trex_file,nucl_coord)
  call check_success(info, 'Unable to get nuclear coordinates')

  info = trexio_get_nucleus_charge(trex_file,nucl_charge)
  call check_success(info, 'Unable to get nuclear charges')

  info = trexio_get_nucleus_label(trex_file,nucl_label)
  call check_success(info, 'Unable to get nuclear labels')

  do i=1,nucl_num
     print '(A4, 2X, F4.1,3(3X,F12.8))', nucl_label(i), nucl_charge(i), nucl_coord(1:3,i)
  end do


  ! Close the file
  ! --------------

  info = trexio_close(trex_file)
  call check_success(info, 'Unable to close file')

end

subroutine check_success(info,message)
  use trexio
  implicit none
  integer, intent(in) :: info
  character*(*)       :: message

  if (info /= TREXIO_SUCCESS) then
     print *, message
     stop info
  end if
end subroutine check_success
