!
!-----------------------------------------------------------------------
!                                                                     72
!
      subroutine init_sound(t,qv,u1z,u2z,u3z,zg,n)

      integer n
      real    t(n),qv(n),u1z(n),u2z(n),u3z(n),zg(n)

      pi     = 4.*atan(1.)
      nz1    = n
      ZTR    = 12000.
      ALPHA  = 4.
!c      vs     = 12.0
      vs     = 30.0
!c      vs     = 0.0
      angle  = 0.
!      angle  = 45.
      THETAR = 343.
      TTR    = 213.
      QV0M   = 0.014
!      QV0M   = 0.013
      THETAS = 300.5
!c      ZTS    = 2500.
      ZTS    = 5000.
      CC1 = 1.
      CC2 = 0.
      umax  = vs*cos(angle/180.*pi)
      vmax  = vs*sin(angle/180.*pi)
      u1max = .5*sqrt(3.)*umax-.5*vmax
      u3max = .5*sqrt(3.)*umax+.5*vmax
      u2max = vmax

      do k=1,nz1
         z = zg(k)
         ztp = cc1*z + cc2*z**2
         if(ztp .gt. ztr) then
            t(k) = thetar*exp(9.8*(ztp-ztr)/(1003.*ttr))
            qv(k) = 0.25
         else
            t(k) = 300.+43.*(ztp/ztr)**1.25
            qv(k) = (1.-0.75*(ztp/ztr)**1.25)
            if(t(k).lt.thetas) t(k)=thetas
         end if

!cc         t(k)=300.
!cc         qv(k) = 0.

         u1z(k) = u1max*min(ztp/zts,1.)
         u2z(k) = u2max*min(ztp/zts,1.)
         u3z(k) = u3max*min(ztp/zts,1.)
      end do

      write(6,*) ' initial conditions '
!
      do k=1,nz1
!         write  (6,10) k,zg(k),t(k),u1z(k),u2z(k),u3z(k),qv(k)
   10    format (1x,i3,f8.1,f7.2,3f6.2,f8.5)
      end do
      return
      end subroutine init_sound

! =============================================================
      subroutine initialize(nx,ny,nz,nx1,ny1,nz1)
      
      use rk3_grid,  only : xl,yl,zl, &
                            xpll,xplr,ypll,yplr, zplb, zplt, wmplt,dxp,dyp,dzp


      use rk3_param, only : pi,angle,um,vm,u1m,u3m,u2m,ur,side, d, &
                            g, f, t0, r, cp, p0, rcv, cti, c2, cb, &
                            delt
      
      implicit none
      
      integer :: nx,ny,nz,nx1,ny1,nz1
      integer :: i,j,k

! local vars
      real :: xn2,xn2l,xn2m,xn
      real :: zinv
      real :: hm ,ampl,xa,ya
      real :: rad, radx, rady, radz, rd, zd, zt


     ur   = -15.

!     angle= 0.
!    angle= 45.
     um  = ur*cos(angle/180.*pi)
     vm  = ur*sin(angle/180.*pi)
     u1m = .5*sqrt(3.)*um-.5*vm
     u3m = .5*sqrt(3.)*um+.5*vm
     u2m = vm

     write(6,*) 'U1M = ',u1m,'  U3M = ',u3m,' U2M = ',u2m

!     xl = 84000.

! orientation of hexes are flat on N/S, vertex on E/W 
!    __    __    __
! ^ /  \__/  \__/  \__
! | \__/  \__/  \__/  \
! y    \__/  \__/  \__/
!
! x--> 

     side  = 2.*xl/(3.*float(nx1))
     d     = sqrt(3.)*side
     yl    = d*float(ny1)

! initialization for 2-d y-z simulation

!    yl    = 50000.
!    d     = yl/float(ny1)
!    side  = d/sqrt(3.)
!    xl    = 1.5*side*float(nx1)

     write(6,*) 'XL = ',xl,'   YL = ',yl,'   D = ',d

     xn2   = 0.0001
     xn2m  = 0.0001
     xn2l  = 0.0001
     zinv  = 10000.
     xn    = sqrt(xn2)

     hm    = 0. ! mountain height
     ampl  = 1.
     xa    = 10000. ! mountain x rad?
     ya    = 10000. ! mountain y rad?

     c2    = cp*rcv*t0
!     cb    = 25.
     delt = delt/t0

     zt    = zl ! 20000.
     zd    = Min( 10000., zt)

    xpll   = 0.
    xplr   = xl
!    xpll   = 0.25*xl
!    xplr   = 0.75*xl
!     xpll   =    xl/6.
!     xplr   = 5.*xl/6.
    ypll   = 0.
    yplr   = yl
!    ypll   = 0.25*yl
!    yplr   = 0.75*yl
!     ypll   =    yl/6.
!     yplr   = 5.*yl/6.
     zplb   =  0.
!    zplt   =  zd
     zplt   =  zt
     wmplt  = 50.

      end subroutine initialize
!
