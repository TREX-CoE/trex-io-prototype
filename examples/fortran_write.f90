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

!--------------------------------------------------------------------------------

subroutine read_xyz(trex_file, xyz_filename)
  use trexio
  implicit none
  integer*8, intent(in)          :: trex_file
  character*(128), intent(in)    :: xyz_filename

  integer                        :: nucl_num         ! Number of nuclei
  character*(256)                :: title            ! Title of the file
  character*(64), allocatable    :: nucl_label(:)    ! Atom labels
  real*8, allocatable            :: nucl_charge(:)   ! Nuclear charges
  real*8, allocatable            :: nucl_coord(:,:)  ! Nuclear coordinates
  integer                        :: alpha_num        ! Number of alpha electrons
  integer                        :: beta_num         ! Number of beta  electrons
  integer                        :: i,j
  integer                        :: info
  double precision, parameter    :: a0 = 0.52917721067d0

  open(unit=10,file=xyz_filename)

  read(10,*) nucl_num

  allocate( nucl_label(nucl_num)  &
       ,nucl_charge(nucl_num)     &
       ,nucl_coord(3,nucl_num)    &
       )

  read(10,'(A)') title

  do i=1,nucl_num
     read(10,*) nucl_label(i), nucl_coord(1:3,i)

     info = trexio_element_number_of_symbol(trim(nucl_label(i)), j)
     call check_success(info, 'Unable to convert symbol to number')

     nucl_charge(i) = dble(j)
  end do

  close(10)

  ! Convert into atomic units
  nucl_coord = nucl_coord / a0

  info = trexio_set_metadata_description(trex_file,title)
  call check_success(info, 'Unable to set description')

  info = trexio_set_nucleus_num(trex_file,nucl_num)
  call check_success(info, 'Unable to set number of nuclei')

  info = trexio_set_nucleus_coord(trex_file,nucl_coord)
  call check_success(info, 'Unable to set nuclear coordinates')

  info = trexio_set_nucleus_charge(trex_file,nucl_charge)
  call check_success(info, 'Unable to set nuclear charges')

  info = trexio_set_nucleus_label(trex_file,nucl_label)
  call check_success(info, 'Unable to set nuclear labels')

  beta_num  = int(sum(nucl_charge(:)))/2
  alpha_num = int(sum(nucl_charge(:))) - beta_num

  info = trexio_set_electron_up_num(trex_file,alpha_num)
  call check_success(info, 'Unable to set up electrons')

  info = trexio_set_electron_dn_num(trex_file,beta_num)
  call check_success(info, 'Unable to set dn electrons')

end subroutine read_xyz

!--------------------------------------------------------------------------------

subroutine read_basis(trex_file, basis_filename)
  use trexio
  implicit none
  integer*8, intent(in)          :: trex_file
  character*(128), intent(in)    :: basis_filename

  integer                        :: nucl_num         ! Number of nuclei
  character*(64), allocatable    :: nucl_label(:)    ! Atom labels

  integer                        :: shell_num, prim_num
  integer, allocatable           :: shell_center(:)
  character, allocatable         :: shell_ang_mom(:)
  integer, allocatable           :: shell_prim_num(:)
  integer, allocatable           :: prim_index(:)
  double precision, allocatable  :: shell_factor(:)
  double precision, allocatable  :: exponent(:)
  double precision, allocatable  :: coefficient(:)
  character*(64)                 :: label
  character*(80)                 :: buffer
  integer                        :: i,j,k,n_shell,n_prim
  integer                        :: info

  open(unit=10, file=basis_filename)

  info = trexio_get_nucleus_num(trex_file, nucl_num)
  call check_success(info, 'Unable to read number of nuclei')

  allocate(nucl_label(nucl_num))
  info = trexio_get_nucleus_label(trex_file, nucl_label)
  call check_success(info, 'Unable to read nuclear label')

  shell_num = 0
  prim_num  = 0
  ! Find dimensioning variables
  do i=1,nucl_num
    info = trexio_element_name_of_symbol(nucl_label(i), label)
    call check_success(info, 'Unable to get name of label')

    ! Find element
    rewind(10)
    do
       read(10,*, iostat=j) buffer
       if (j < 0) exit
       if (trim(buffer) == trim(label)) then
          j=0
          exit
       end if
    end do
    if (j < 0) exit

    ! Read shell
    do
       read(10,*,iostat=k) buffer, j
       if (k /= 0) exit
       shell_num = shell_num + 1
       prim_num  = prim_num  + j
       do k=1,j
          read(10,*)
       end do
    end do

  end do

  buffer = 'Gaussian'
  info = trexio_set_basis_type(trex_file, buffer(1:32))
  call check_success(info, 'Unable to set basis type')

  info = trexio_set_basis_shell_num(trex_file, shell_num)
  call check_success(info, 'Unable to set basis shell_num')

  info = trexio_set_basis_prim_num(trex_file, prim_num)
  call check_success(info, 'Unable to set basis prim_num')

  allocate(shell_center(shell_num), shell_ang_mom(shell_num), shell_prim_num(shell_num), &
       prim_index(shell_num), shell_factor(shell_num))
  allocate(exponent(prim_num), coefficient(prim_num))

  shell_num = 1
  prim_num  = 1
  ! Read data
  do i=1,nucl_num
    info = trexio_element_name_of_symbol(nucl_label(i), label)
    call check_success(info, 'Unable to get name of label')

    ! Find element
    rewind(10)
    do
       read(10,*, iostat=j) buffer
       if (j < 0) exit
       if (trim(buffer) == trim(label)) then
          j=0
          exit
       end if
    end do

    ! Read shell
    do
       read(10,*,iostat=j) label, k
       if (j /= 0) exit
       shell_ang_mom(shell_num) = label(1:1)
       shell_prim_num(shell_num) = k
       shell_center(shell_num) = i
       prim_index(shell_num) = prim_num
       do j=1,shell_prim_num(shell_num)
          read(10,*) buffer, exponent(prim_num), coefficient(prim_num)
          prim_num = prim_num + 1
       end do
       shell_num = shell_num + 1
    end do

  end do

  close(10)

  info = trexio_set_basis_shell_center(trex_file, shell_center)
  call check_success(info, 'Unable to set basis shell_center')

  info = trexio_set_basis_shell_ang_mom(trex_file, shell_ang_mom)
  call check_success(info, 'Unable to set basis shell_ang_mom')

  info = trexio_set_basis_shell_prim_num(trex_file, shell_prim_num)
  call check_success(info, 'Unable to set basis shell_prim_num')

  info = trexio_set_basis_prim_index(trex_file, prim_index)
  call check_success(info, 'Unable to set basis prim_index')

  info = trexio_set_basis_exponent(trex_file, exponent)
  call check_success(info, 'Unable to set basis exponent')

  info = trexio_set_basis_coefficient(trex_file, coefficient)
  call check_success(info, 'Unable to set basis coefficient')

  return

  10 continue
  stop 'Unable to find element in basis set file'
end subroutine read_basis

!--------------------------------------------------------------------------------

program write_example
  use trexio
  implicit none

  character*(128)                :: xyz_filename     ! Name of the xyz file
  character*(128)                :: basis_filename   ! Name of the basis file
  integer*8                      :: trex_file        ! Handle for the TREX file
  character*(64)                 :: username
  integer                        :: i
  integer                        :: info
  character*(*), parameter       :: trex_filename = 'trex_file'

  ! Get the xyz file name from the command line and user name
  ! =========================================================

  i = command_argument_count()
  if (i /= 2) then
     print *, 'Expected:'
     print *, ' - xyz file as 1st argument'
     print *, ' - basis file as 2nd argument'
     print *, './fortran_write cyanoformaldehyde.xyz cc-pvtz'
     stop -1
  end if

  call get_command_argument(1,xyz_filename)
  call get_command_argument(2,basis_filename)

  call getenv('USER',username)


  ! Open the TREX file
  ! ------------------

  info = 0
  info = trexio_open(trex_filename,'w', trex_file)
  call check_success(info, 'Unable to open file')


  ! Write the metadata
  ! ------------------

! info = trexio_append_unique_metadata_code(trex_file,'Fortran Example')
! call check_success(info, 'Unable to set code')

! info = trexio_append_unique_metadata_author(trex_file,username)
! call check_success(info, 'Unable to set code')


  ! Write the data
  ! --------------

  call read_xyz(trex_file, xyz_filename)
  call read_basis(trex_file, basis_filename)

  ! Close the file
  ! --------------

  info = trexio_close(trex_file)
  call check_success(info, 'Unable to close file')

  print *, 'Wrote file '//trim(trex_filename)

end program write_example
