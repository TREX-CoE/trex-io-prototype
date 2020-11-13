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

  character*(128)                :: xyz_filename     ! Name of the xyz file
  integer*8                      :: trex_file        ! Handle for the TREX file
  character*(64)                 :: username
  integer                        :: i
  integer                        :: info
  character*(*), parameter       :: trex_filename = 'trex_file'
  double precision, parameter    :: a0 = 0.52917721067d0

  ! Get the xyz file name from the command line and user name
  ! =========================================================

  i = command_argument_count()
  if (i /= 1) then
     print *, 'Expected xyz file as 1st argument:'
     print *, './fortran_write cyanoformaldehyde.xyz'
     stop -1
  end if

  call get_command_argument(1,xyz_filename)

  call getenv('USER',username)


  ! Read the xyz file
  ! =================

  open(unit=10,file=xyz_filename)

  read(10,*) nucl_num

  allocate( nucl_label(nucl_num)  &
       ,nucl_charge(nucl_num)     &
       ,nucl_coord(3,nucl_num)    &
       )

  read(10,'(A)') title

  do i=1,nucl_num
     read(10,*) nucl_label(i), nucl_coord(1:3,i)

     if (trim(nucl_label(i)) == 'C') then
        nucl_charge(i) = 6.d0
     else if (trim(nucl_label(i)) == 'H') then
        nucl_charge(i) = 1.d0
     else if (trim(nucl_label(i)) == 'O') then
        nucl_charge(i) = 8.d0
     else if (trim(nucl_label(i)) == 'N') then
        nucl_charge(i) = 7.d0
     end if
  end do

  close(10)


  ! Store the data in the TREX file
  ! ===============================

  ! Open the file
  ! -------------

  info = 0
  info = trexio_open(trex_filename,'w', trex_file)
  call check_success(info, 'Unable to open file')


  ! Write the metadata
  ! ------------------

  info = trexio_set_metadata_description(trex_file,title)
  call check_success(info, 'Unable to set description')

! info = trexio_append_unique_metadata_code(trex_file,'Fortran Example')
! call check_success(info, 'Unable to set code')

! info = trexio_append_unique_metadata_author(trex_file,username)
! call check_success(info, 'Unable to set code')


  ! Write the data for the nuclei
  ! -----------------------------

  info = trexio_set_nucleus_num(trex_file,nucl_num)
  call check_success(info, 'Unable to set number of nuclei')

  ! Convert into atomic units
  nucl_coord = nucl_coord / a0
  info = trexio_set_nucleus_coord(trex_file,nucl_coord)
  call check_success(info, 'Unable to set nuclear coordinates')

  info = trexio_set_nucleus_charge(trex_file,nucl_charge)
  call check_success(info, 'Unable to set nuclear charges')

  info = trexio_set_nucleus_label(trex_file,nucl_label)
  call check_success(info, 'Unable to set nuclear labels')


  ! Write data for the electrons
  ! ----------------------------

  beta_num  = int(sum(nucl_charge(:)))/2
  alpha_num  = int(sum(nucl_charge(:))) - beta_num

  info = trexio_set_electron_up_num(trex_file,alpha_num)
  call check_success(info, 'Unable to set up electrons')

  info = trexio_set_electron_dn_num(trex_file,beta_num)
  call check_success(info, 'Unable to set dn electrons')


  ! Close the file
  ! --------------

  info = trexio_close(trex_file)
  call check_success(info, 'Unable to close file')

  print *, 'Wrote file '//trim(trex_filename)

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
