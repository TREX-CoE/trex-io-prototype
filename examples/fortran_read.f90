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

subroutine read_metadata(trex_file, title)
  use trexio
  implicit none
  integer*8, intent(in)          :: trex_file
  character*(256), intent(out)   :: title

  integer :: info

  info = trexio_get_metadata_description(trex_file,title)
  call check_success(info, 'Unable to get description')
end subroutine read_metadata

!--------------------------------------------------------------------------------

subroutine read_electrons(trex_file, alpha_num, beta_num)
  use trexio
  implicit none
  integer*8, intent(in)          :: trex_file
  integer*8, intent(out)         :: alpha_num, beta_num
  integer :: info

  info = trexio_get_electron_up_num(trex_file,alpha_num)
  call check_success(info, 'Unable to get up electrons')

  info = trexio_get_electron_dn_num(trex_file,beta_num)
  call check_success(info, 'Unable to get dn electrons')
end subroutine read_electrons

!--------------------------------------------------------------------------------

subroutine read_nuclei(trex_file, nucl_num, nucl_coord, nucl_charge, nucl_label)
  use trexio
  implicit none
  integer*8, intent(in)          :: trex_file
  integer*8, intent(in)          :: nucl_num
  double precision, intent(out)  :: nucl_coord(3,nucl_num)
  double precision, intent(out)  :: nucl_charge(nucl_num)
  character*(64), intent(out)    :: nucl_label(nucl_num)

  integer :: info

  info = trexio_get_nucleus_coord(trex_file,nucl_coord)
  call check_success(info, 'Unable to get nuclear coordinates')

  info = trexio_get_nucleus_charge(trex_file,nucl_charge)
  call check_success(info, 'Unable to get nuclear charges')

  info = trexio_get_nucleus_label(trex_file,nucl_label)
  call check_success(info, 'Unable to get nuclear labels')

end subroutine read_nuclei

!--------------------------------------------------------------------------------

subroutine read_basis(trex_file, shell_num, prim_num, center, ang_mom, &
     shell_prim_num, prim_index, expo, coef)
  use trexio
  implicit none
  integer*8, intent(in)                :: trex_file
  integer*8, intent(in)                :: shell_num, prim_num
  integer*8, intent(out)               :: center(shell_num), shell_prim_num(shell_num)
  integer  , intent(out)               :: ang_mom(shell_num)
  integer*8, intent(out)               :: prim_index(shell_num)
  double precision, intent(out)        :: expo(prim_num)
  double precision, intent(out)        :: coef(prim_num)

  integer                        :: info

  info = trexio_get_basis_shell_center(trex_file, center)
  call check_success(info, 'Unable to get basis shell_center')

  info = trexio_get_basis_shell_ang_mom(trex_file, ang_mom)
  call check_success(info, 'Unable to get basis shell_ang_mom')

  info = trexio_get_basis_shell_prim_num(trex_file, shell_prim_num)
  call check_success(info, 'Unable to get basis shell_prim_num')

  info = trexio_get_basis_prim_index(trex_file, prim_index)
  call check_success(info, 'Unable to get basis prim_index')

  info = trexio_get_basis_exponent(trex_file, expo)
  call check_success(info, 'Unable to get basis exponent')

  info = trexio_get_basis_coefficient(trex_file, coef)
  call check_success(info, 'Unable to get basis coefficient')

end subroutine read_basis

!--------------------------------------------------------------------------------

program read_example
  use trexio
  implicit none
  integer*8                      :: nucl_num         ! Number of nuclei
  character*(256)                :: title            ! Title of the file
  character*(64), allocatable    :: nucl_label(:)    ! Atom labels
  real*8, allocatable            :: nucl_charge(:)   ! Nuclear charges
  real*8, allocatable            :: nucl_coord(:,:)  ! Nuclear coordinates
  integer*8                      :: alpha_num        ! Number of alpha electrons
  integer*8                      :: beta_num         ! Number of beta  electrons

  integer*8                      :: trex_file        ! Handle for the TREX file
  integer                        :: i,j,k
  integer                        :: info
  character*(*), parameter       :: trex_filename = 'trex_file'
  double precision, parameter    :: a0 = 0.52917721067d0

  integer*8                      :: shell_num, prim_num
  integer*8, allocatable         :: shell_center(:)
  integer  , allocatable         :: shell_ang_mom(:)
  integer*8, allocatable         :: shell_prim_num(:)
  integer*8, allocatable         :: prim_index(:)
  double precision, allocatable  :: shell_factor(:)
  double precision, allocatable  :: exponent(:)
  double precision, allocatable  :: coefficient(:)
  character*(64)                 :: bastype
  character*(64)                 :: label
  character, parameter           :: ang_mom(0:6) = (/ 'S', 'P', 'D', 'F', 'G', 'H', 'I' /)

  ! Read the data from the TREX file
  ! ================================

  ! Open the file
  ! -------------

  info = 0
  info = trexio_open(trex_filename,'r', trex_file)
  call check_success(info, 'Unable to open file')

  ! Read the data
  ! -------------

  call read_metadata(trex_file, title)
  print *, 'Description: ', trim(title)

  call read_electrons(trex_file,alpha_num,beta_num)
  print *, 'Electrons: ', alpha_num, ' up, ', beta_num, ' down'


  info = trexio_get_nucleus_num(trex_file,nucl_num)
  call check_success(info, 'Unable to get number of nuclei')

  allocate( nucl_coord(3,nucl_num), nucl_charge(nucl_num), &
       nucl_label(nucl_num) )

  call read_nuclei(trex_file, nucl_num, nucl_coord, nucl_charge, nucl_label)

  do i=1,nucl_num
     print '(A4, 2X, F4.1,3(3X,F12.8))', nucl_label(i), nucl_charge(i), nucl_coord(1:3,i)
  end do


  bastype=''
  print *, bastype
  info = trexio_get_basis_type(trex_file, bastype)
  print *, bastype
  call check_success(info, 'Unable to get basis type')
  print *, 'Basis type: ', trim(bastype)

  info = trexio_get_basis_shell_num(trex_file, shell_num)
  call check_success(info, 'Unable to get basis shell_num')

  info = trexio_get_basis_prim_num(trex_file, prim_num)
  call check_success(info, 'Unable to get basis prim_num')

  allocate(shell_center(shell_num), shell_ang_mom(shell_num), shell_prim_num(shell_num), &
       prim_index(shell_num), shell_factor(shell_num))
  allocate(exponent(prim_num), coefficient(prim_num))

  call read_basis(trex_file, shell_num, prim_num, shell_center, shell_ang_mom, &
     shell_prim_num, prim_index, exponent, coefficient)

  k=0
  do i=1,shell_num
     if (shell_center(i) /= k) then
        k = shell_center(i)
        info = trexio_element_name_of_symbol(trim(nucl_label(k)),label)
        call check_success(info, 'Unable to get name of element :')
        print *, ''
        print *, trim(label)
     end if
     print *, ang_mom(shell_ang_mom(i)), shell_prim_num(i)
     do j=1,shell_prim_num(i)
        print '(I3,X,E16.10,3X,E16.10)', j, &
             exponent(prim_index(i)+j-1) , coefficient(prim_index(i)+j-1)
     end do
  end do


  ! Close the file
  ! --------------

  info = trexio_close(trex_file)
  call check_success(info, 'Unable to close file')

end program read_example
