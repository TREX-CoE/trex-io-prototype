       program  testconvert
       implicit none
       character typepol
       integer l,m,i
       real*8, dimension(:), allocatable:: coeff,coefftrex
       write(6,*) ' Input l, typepol '
       read(5,*) l,typepol
       if(typepol.eq.'h'.or.typepol.eq.'H') then 
       m=2*l+1
       else
       m=((1+l)*(2+l))/2
       endif
       allocate(coeff(m),coefftrex(m))
       coefftrex=0 
       do i=1,m
       coeff(i)=i
       enddo
       call convertTurboRVBfromtoTREX('t',typepol,l,coeff,coefftrex)
       write(6,*) ' After conversion '
       do i=1,m
       write(6,*) i,coefftrex(i)
       enddo
       coeff=0
       call convertTurboRVBfromtoTREX('f',typepol,l,coeff,coefftrex)
       write(6,*) ' Back to original '
       do i=1,m
       write(6,*)  i,coeff(i) 
       if(i.ne.coeff(i)) write(6,*) ' ERROR  f after  t should  be the identity '
       enddo 
       stop
       end

       subroutine convertTurboRVBfromtoTREX(scope,typepol,l,coeff,coefftrex)
       implicit none
       character, intent(in) ::  scope,typepol
       real*8, intent(inout) :: coeff(*),coefftrex(*) 
       integer, intent(in) :: l
       integer   i,m
!  scope    character*1  input
!  scope='t' or 'T'  translate TurboRVB to TREX convention

!  typepol  character*1  input
!  Convention polynomial 'h' or 'H'  --> Spherical Harmonics
!                        'c' or 'C'  --> Cartesian conventions

!  l integer, input 
!  order of the  homogeneous polynomial in x,y,z  or principal quantum number for the 'h' case   

!    coeff real*8     input (output)  if scope='t'('f')
!    coefftrex real*8 input (output)  if scope='f'('t')


!  The  coefficients are ordered in a fortran fashion, i.e. the lowest integer definition
!  (e.g. m=-l for the spherical harmonic case) corresponds to  coeff(1) or coefftrex(1) 
       if(typepol.eq.'h'.or.typepol.eq.'H') then
         if(scope.eq.'t'.or.scope.eq.'T') then
         select case(l)
           case(0) 
           coefftrex(1)=coeff(1)
           case(1)
           coefftrex(1)=coeff(2)
           coefftrex(2)=coeff(3)
           coefftrex(3)=coeff(1)
           case(2)
           coefftrex(1)=coeff(3)
           coefftrex(2)=coeff(4)
           coefftrex(3)=coeff(1)
           coefftrex(4)=coeff(5)
           coefftrex(5)=coeff(2)
           case default
           do i=1,l+1
           coefftrex(i)=coeff(2*l+3-2*i)
           enddo
           do i=1,l
           coefftrex(i+l+1)=coeff(2*i)
           enddo 
!          coefftrex(1)=coeff(7)
!          coefftrex(2)=coeff(5)
!          coefftrex(3)=coeff(3)
!          coefftrex(4)=coeff(1)
!          coefftrex(5)=coeff(2)
!          coefftrex(6)=coeff(4)
!          coefftrex(7)=coeff(6)
         end select 
         elseif(scope.eq.'f'.or.scope.eq.'F') then
         select case(l)
           case(0)
           coeff(1)=coefftrex(1)
           case(1)
           coeff(2)=coefftrex(1)
           coeff(3)=coefftrex(2)
           coeff(1)=coefftrex(3)
           case(2)
           coeff(3)=coefftrex(1)
           coeff(4)=coefftrex(2)
           coeff(1)=coefftrex(3)
           coeff(5)=coefftrex(4)
           coeff(2)=coefftrex(5)

           case default
           do i=1,l+1
           coeff(2*l+3-2*i)=coefftrex(i)
           enddo
           do i=1,l
           coeff(2*i)=coefftrex(i+l+1)
           enddo 
         end select 
         else
         write(6,*) ' ERROR case scope not found!!! '
         endif
       elseif(typepol.eq.'c'.or.typepol.eq.'C') then
!        assuming the  identity
         m=((l+1)*(l+2))/2
         if(scope.eq.'t'.or.scope.eq.'T') then
         do i=1,m
         coefftrex(i)=coeff(i)
         enddo
         elseif(scope.eq.'f'.or.scope.eq.'F') then
         do i=1,m
         coeff(i)=coefftrex(i)
         enddo
         else
         write(6,*) ' ERROR case scope not found!!! '
         endif
       else
         write(6,*) ' ERROR case typepol not found!!! '
       endif
       return 
       end 
