subroutine dyn(npart,mass,xyz,vxyz,ti,tf,fpot,fsta,dt,dr)
use bspline_module
use interpolation
use RDistributions
implicit none

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Nico 24.05.2016 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! performs the dynamics until tf                        !!
! uses Verlet algorithom                                !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! for the dynamics
double precision, intent(in) :: dt, dr ! in atomic units
double precision, intent(inout) :: ti, tf
double precision, dimension(3,npart) :: xyzm, xyzt, xyznew, xyzdr
double precision :: time, grade
double precision, dimension(:), allocatable :: valdrp, valdrm, val

! for the system
integer, intent(in) :: npart
double precision, dimension(npart), intent(in) :: mass
double precision, dimension(3,npart), intent(inout) :: xyz, vxyz

!for the electronic state
character(64) :: fpot

!for Landau-Zener 
double precision, dimension(:), allocatable :: epair, epair_t1, d_epair, d_epair_t1, d2_epair
double precision :: plz

!for the interpolation
integer :: nsta
integer :: nr1, nr2, nthe
double precision, dimension(:), allocatable :: r1, r2, the
double precision, dimension(:,:,:,:), allocatable :: energy

integer, intent(inout) :: fsta
double precision, dimension(:,:), allocatable :: tr1, tr2, tthe

integer,parameter :: kr1 = 4     !! order in r1
integer,parameter :: kr2 = 4     !! order in r2
integer,parameter :: kt = 2     !! order in theta
integer,parameter :: iknot = 0  !! automatically select the knots

double precision :: tol
logical :: fail, file_e

integer :: inbvx,inbvy,inbvz
integer :: iloy,iloz,iflag
integer :: idr1, idr2, idthe

double precision :: newr1, newr2, newthe

integer :: i, j, k, ista

! Reads the data

inquire( file=trim(fpot), exist=file_e )
if ( file_e .eqv. .false. ) then
 write(*,*) trim(fpot), " does not exist"
 stop
endif

write(*,'(A,A)')'Read PESs in ',trim(fpot)
open(unit=10,file=trim(fpot))
 read(10,*)nr1, nr2, nthe, nsta
 write(*,'(A,I4,A,I4,A,I4,A)')'There are',nr1,' * ',nr2,' * ',nthe,' points'
 write(*,'(A,I4,A)')'There are', nsta, 'states'

allocate(r1(nr1),r2(nr2),the(nthe))
allocate(energy(nsta,nr2,nr1,nthe))
!
do i = 1, nr2
  do j = 1, nr1
    do k = 1, nthe
      read(10,*)r2(i),r1(j),the(k),(energy(ista,i,j,k),ista=1,nsta)
!      write(*,'(5(f20.10,1X))')r2(i),r1(j),the(k),(energy(ista,i,j,k),ista=1,nsta)
    enddo
  enddo
enddo 
close(10)
write(*,'(A)')'Reading PESs done '
!!energy(:,:,:,:)=0d0

allocate(epair(nsta),epair_t1(nsta),d_epair(nsta),d_epair_t1(nsta),d2_epair(nsta))
d_epair_t1(:) = 0d0
epair_t1(:) = 0d0

!
! Set-up the interpolation
inbvx = 1
inbvy = 1
inbvz = 1
iloy  = 1
iloz  = 1

idr1 = 0
idr2 = 0
idthe = 0

fail = .false.
tol = 1.0e-14

allocate(tr1(nsta,nr1+kr1),tr2(nsta,nr2+kr2),tthe(nsta,nthe+kt))
allocate(val(nsta),valdrp(nsta),valdrm(nsta))

do ista = 1, nsta
 iflag=0
 call db3ink(r2,nr2,r1,nr1,the,nthe,energy(ista,:,:,:),kr1,kr2,kt,iknot,tr2(ista,:),tr1(ista,:),tthe(ista,:),energy(ista,:,:,:),iflag)
 if(iflag/=0) then
  write(*,*)"error in db3ink", iflag
  return
 endif
enddo

! landau-zenner surf. hopp. stuff
 call compute_dist(npart,xyz,newr1,newr2,newthe)
do ista=1,nsta
  call db3val(newr2,newr1,newthe,idr1,idr2,idthe,tr2(ista,:),tr1(ista,:),tthe(ista,:),nr2,nr1,nthe,kr1,kr2,kt,energy(ista,:,:,:),val(ista),iflag,&
 inbvx,inbvy,inbvz,iloy,iloz)
 if(iflag/=0) then
  write(*,*)"error in db3val at time",time
  stop
 endif
enddo

do ista=1,nsta
  epair(ista) = abs(val(ista)-val(fsta))
enddo

! first step of the Verlet algorithm
do i = 1, npart
  do j = 1, 3
    xyzm(j,i) = xyz(j,i)
    xyzt(j,i) = xyz(j,i) + vxyz(j,i)*dt
  enddo
enddo

time=ti
do while(time<tf)

! landau-zenner surf. hopp. stuff
 epair_t1(:) = epair(:)
 d_epair_t1(:) = d_epair(:)
 epair(:)=0d0
 
! computes the energy at position at time t
 call compute_dist(npart,xyz,newr1,newr2,newthe)
 do ista=1,nsta
   call db3val(newr2,newr1,newthe,idr1,idr2,idthe,tr2(ista,:),tr1(ista,:),tthe(ista,:),nr2,nr1,nthe,kr1,kr2,kt,energy(ista,:,:,:),val(ista),iflag,& 
 inbvx,inbvy,inbvz,iloy,iloz)
 write(200,'(5(f20.10,1X),i3)')time,newr1,newr2,newthe,val(fsta),fsta
 if(iflag/=0) then
  write(*,*)"error in db3val at time",time
  stop
 endif
 enddo

! apply LZ surface hopping here
do ista=1,nsta
  epair(ista) = abs(val(ista)-val(fsta))
enddo

 d_epair(:) = (epair(:)-epair_t1(:))/dt
 d2_epair(:) = (d_epair(:)-d_epair_t1(:))/dt

 do ista=1,nsta 
  if(d_epair(ista)*d_epair_t1(ista) < 0d0 .and. d2_epair(ista)>0d0) then
      plz = exp(-0.5d0*pi*sqrt(epair(ista)**3/d2_epair(ista)))
      if(plz>rand_uniform(0d0,1d0)) then
        write(*,*)"HOP"
        fsta=ista
        epair_t1(:) = 0d0
        epair(:) = 0d0
        d_epair(:) = 0d0
        d_epair_t1(:) = 0d0
       exit ! only one hop allowed
      endif
  endif
 enddo

!! end surface hopping

! computes the new position
 do i = 1, npart
   do j = 1, 3
  
    xyzdr(:,:) =  xyzt(:,:) 
    xyzdr(j,i) = xyzt(j,i) + dr
    call compute_dist(npart,xyz,newr1,newr2,newthe)
 do ista=1,nsta
    call db3val(newr2,newr1,newthe,idr1,idr2,idthe,tr2(ista,:),tr1(ista,:),tthe(ista,:),nr2,nr1,nthe,kr1,kr2,kt,energy(ista,:,:,:),valdrp(ista),iflag,& 
    inbvx,inbvy,inbvz,iloy,iloz)
 enddo
!    write(*,'(i4,i4,5(f15.5))')j,i,newr1,newr2,newthe,valdrp(fsta)

    xyzdr(:,:) =  xyzt(:,:) 
    xyzdr(j,i) = xyzt(j,i) - dr
    call compute_dist(npart,xyz,newr1,newr2,newthe)
 do ista=1,nsta
    call db3val(newr2,newr1,newthe,idr1,idr2,idthe,tr2(ista,:),tr1(ista,:),tthe(ista,:),nr2,nr1,nthe,kr1,kr2,kt,energy(ista,:,:,:),valdrp(ista),iflag,& 
    inbvx,inbvy,inbvz,iloy,iloz)
 enddo
!    write(*,'(i4,i4,5(f15.5))')j,i,newr1,newr2,newthe,valdrm(fsta)

    grade = 0.5*( (valdrp(fsta)-val(fsta))/dr - (valdrm(fsta)-val(fsta))/dr )
!    write(*,'(i4,i4,5(f15.5))')j,i,grade,newr,newrh2,newthe
!    write(*,*)

    xyznew(j,i) = 2*xyzt(j,i) - xyzm(j,i) - grade*dt**2/mass(i) 

   enddo
 enddo

 xyzm(:,:) = xyzt(:,:)
 xyzt(:,:) = xyznew(:,:)
 time = time + dt

enddo
!stop

xyz(:,:) = xyznew(:,:)
vxyz(:,:) = (xyzt(:,:)-xyzm(:,:))/dt

deallocate(r1,r2,the,energy)
deallocate(tr1,tr2,tthe)
deallocate(val,valdrp,valdrm)
deallocate(epair,epair_t1,d_epair,d2_epair)

end subroutine dyn


subroutine compute_dist(npart,xyz,r1,r2,theta)
implicit none

integer, intent(in) :: npart
double precision, dimension(3,npart), intent(in) :: xyz

double precision :: r1, r2, costhe, theta

! atom 1 must be oxygen
! atoms 2 and 3 are then hydrogen

r1 = sqrt( (xyz(1,1)-xyz(1,2))**2 + (xyz(2,1)-xyz(2,2))**2 + (xyz(3,1)-xyz(3,2))**2 )
r2 = sqrt( (xyz(1,1)-xyz(1,3))**2 + (xyz(2,1)-xyz(2,3))**2 + (xyz(3,1)-xyz(3,3))**2 )

costhe = ( (xyz(1,2)-xyz(1,1))*(xyz(1,3)-xyz(1,1)) + (xyz(2,2)-xyz(2,1))*(xyz(2,3)-xyz(2,1)) + (xyz(3,2)-xyz(3,1))*(xyz(3,3)-xyz(3,1)) ) / (r1*r2)
theta = dacos(costhe)*180d0/dacos(-1d0)

end subroutine compute_dist

