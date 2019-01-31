!> The program solves classical Newton's Eq. for H2O \\
!> Non-adiabatic transitions take place according to Landau-Zener Surface Hopping algorithm (see J. Chem. Phys. 142, 104307 (2015)) \\
!> The PES must be given as follows rOH1, rOH2, angle_H1OH2, energies
program dynamic_h2o
implicit none

integer, parameter :: npart = 3
!> xyz contains the Cartesian coord of all atoms e.g. xyz(1,:) are X coordinates for all atoms, same for velocities in vxyz 
double precision, dimension(3,npart) :: xyz, vxyz
double precision, dimension(npart) :: mass

double precision :: r1, r2, theta

double precision :: ti, tf
double precision :: dt, dr

!> allows to rescale the velocities after hopping (0: no rescale, 1: rescale)
integer :: rescal

!> fsta defines the current state 
integer :: fsta

!> fpot is the file providing the PES, fxyz provides masses, initial pos. and vel.
character(64) :: fpot, fxyz, arg

integer :: i, j, l

!> all inputs read from namelist input
namelist /input/ dt, dr, fpot, fxyz, tf, fsta, rescal

if(iargc()==0) then
   write(*,*) "Please provide the input file in command line"
   stop
else
 do i = 1, iargc()
   call getarg(i, arg)
!   write(*,*) arg
 enddo
endif

 write(*,'(A)')"Here is the input"
 open(unit=10,file=arg)
   read(10,nml=input)
   write(*,nml=input)
 close(10)
 write(*,*)
 tf = tf * 41d0

! reads the initial conditions
write(*,'(A)')"Reads the initial conditions"
write(*,'(A)')"!!!  Enter Oxygen first !!!"
 open(unit=10,file=fxyz)
 do i = 1, npart
   read(10,*)mass(i),xyz(1,i),xyz(2,i),xyz(3,i),vxyz(1,i),vxyz(2,i),vxyz(3,i)
   write(100,'(6(f20.15,1X))')xyz(1,i),xyz(2,i),xyz(3,i),vxyz(1,i),vxyz(2,i),vxyz(3,i)
   mass(i)=mass(i)*1836.15d0
 enddo
 close(10)
 write(100,*)
 write(*,*)

!  if(mass(1)/=16.0d0*1836.15d0) then
!    write(*,'(A)')"I said !!!  Enter Oxygen first !!!"
!    stop
!  endif

 call compute_dist(npart,xyz,r1,r2,theta)
 write(10,*)r1,r2,theta

! starts with the dynamics

 ti = 0d0
 call dyn(npart,mass,xyz,vxyz,ti,tf,fpot,fsta,dt,dr,rescal)

 call compute_dist(npart,xyz,r1,r2,theta)
 write(*,*)r1,r2,theta
 write(*,*)
 do i = 1, npart
   write(100,'(6(f20.10,1X))')xyz(:,i),vxyz(:,i)
 enddo
  write(100,*)

end program dynamic_h2o

