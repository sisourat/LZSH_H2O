module interpolation
implicit none

   contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine interp(xa,ya,n,x,y)
implicit none

integer, intent(in) :: n
double precision, dimension(n), intent(in) :: xa, ya
double precision :: x, y

double precision :: a
integer :: i, j

!! Linear interpolation

! searching for two closer points of x in xa
! xa must be ascending sorted

 if(x .lt. xa(1) .or. x .gt. xa(n)) then
  write(*,*)'x out of range in interp, I stop', x
  stop
 endif

! call hunt(xa,n,x,i)
 call locate(xa,n,x,i)

 a = (ya(i+1) - ya(i))/(xa(i+1) - xa(i))
 y = ya(i) + a*(x-xa(i))

end subroutine interp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE hunt(xx,n,x,jlo)
      INTEGER jlo,n
      REAL*8 x,xx(n)
      INTEGER inc,jhi,jm
      LOGICAL ascnd
      ascnd=xx(n).gt.xx(1)
      if(jlo.le.0.or.jlo.gt.n)then
        jlo=0
        jhi=n+1
        goto 3
      endif
      inc=1
      if(x.ge.xx(jlo).eqv.ascnd)then
1       jhi=jlo+inc
        if(jhi.gt.n)then
          jhi=n+1
        else if(x.ge.xx(jhi).eqv.ascnd)then
          jlo=jhi
          inc=inc+inc
          goto 1
        endif
      else
        jhi=jlo
2       jlo=jhi-inc
        if(jlo.lt.1)then
          jlo=0
        else if(x.lt.xx(jlo).eqv.ascnd)then
          jhi=jlo
          inc=inc+inc
          goto 2
        endif
      endif
3     if(jhi-jlo.eq.1)return
      jm=(jhi+jlo)/2
      if(x.gt.xx(jm).eqv.ascnd)then
        jlo=jm
      else
        jhi=jm
      endif
      goto 3
      END SUBROUTINE hunt
!  (C) Copr. 1986-92 Numerical Recipes Software #>.)@1.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SUBROUTINE locate(xx,n,x,j)
      INTEGER j,n
      REAL*8 x,xx(n)
      INTEGER jl,jm,ju
      jl=0
      ju=n+1
10    if(ju-jl.gt.1)then
        jm=(ju+jl)/2
        if((xx(n).gt.xx(1)).eqv.(x.gt.xx(jm)))then
          jl=jm
        else
          ju=jm
        endif
      goto 10
      endif
      j=jl
      return
      END SUBROUTINE locate
!  (C) Copr. 1986-92 Numerical Recipes Software #>.)@1.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module interpolation
