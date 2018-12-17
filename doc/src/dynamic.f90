!> The program solves classical Newton's Eq. for H2O \\
!> Non-adiabatic transitions take place according to Landau-Zener Surface Hopping algorithm (see J. Chem. Phys. 142, 104307 (2015)) \\
!> The PES must be given as follows rOH1, rOH2, angle_H1OH2, energies
program dynamic_h2o
implicit none

integer, parameter :: npart = 3
double precision, dimension(3,npart) :: xyz, vxyz
double precision, dimension(npart) :: mass

double precision :: r1, r2, theta

double precision :: ti, tf
double precision :: dt, dr

integer :: fsta

character(64) :: fpot

integer :: i, j, l

! reads the initial conditions
  write(*,*)"!!!  Enter Oxygen first !!!"

  read(*,*)dt, dr
  read(*,*)fpot, tf, fsta
  tf = tf * 41d0
do i = 1, npart
  read(*,*)mass(i),xyz(1,i),xyz(2,i),xyz(3,i),vxyz(1,i),vxyz(2,i),vxyz(3,i)
  write(100,'(6(f20.15,1X))')xyz(1,i),xyz(2,i),xyz(3,i),vxyz(1,i),vxyz(2,i),vxyz(3,i)
  mass(i)=mass(i)*1836.15d0
enddo
  write(100,*)

  if(mass(1)/=16.0d0*1836.15d0) then
    write(*,*)"I said !!!  Enter Oxygen first !!!"
    stop
  endif

 call compute_dist(npart,xyz,r1,r2,theta)
 write(10,*)r1,r2,theta

! starts with the dynamics

 ti = 0d0
 call dyn(npart,mass,xyz,vxyz,ti,tf,fpot,fsta,dt,dr)

 call compute_dist(npart,xyz,r1,r2,theta)
 write(*,*)r1,r2,theta
 write(*,*)
 do i = 1, npart
   write(100,'(6(f20.10,1X))')xyz(:,i),vxyz(:,i)
 enddo
  write(100,*)

end program dynamic_h2o

