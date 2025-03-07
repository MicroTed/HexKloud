c********** nonlinear nonhydrostatic time-split model on hexagonal grid
c********** mass conserving, flux-form variables
c********** open or periodic boundaries.
c********** transformed terrain-following vertical coordinate.
c********** includes atmospheric mean state. constant stability
c********** reference code for terrain-folling height coordinate model
c
c  this code incorporates the wicker and skamarock RK2 timesplitting
c  algorithm
c                                                                     72
c  this code also includes kessler microphysics
c
      program hexgrid
c      parameter (nz= 41, nx= 61, ny= 53, nz1=nz-1, nx1=nx-1, ny1=ny-1)
c      parameter (nz= 41, nx=181, ny=157, nz1=nz-1, nx1=nx-1, ny1=ny-1)
      parameter (nz= 41, nx= 91, ny= 79, nz1=nz-1, nx1=nx-1, ny1=ny-1)
c      parameter (nz= 41, nx= 47, ny= 40, nz1=nz-1, nx1=nx-1, ny1=ny-1)
c      parameter (nz= 41, nx= 121, ny=105, nz1=nz-1, nx1=nx-1, ny1=ny-1)
c      parameter (nz= 41, nx= 181, ny= 53, nz1=nz-1, nx1=nx-1, ny1=ny-1)
c      parameter (nz= 41, nx= 101, ny= 5, nz1=nz-1, nx1=nx-1, ny1=ny-1)
c      parameter (nz= 41, nx= 5, ny=101, nz1=nz-1, nx1=nx-1, ny1=ny-1)

      real u1  (nz1,0:nx,ny), u11 (nz1,0:nx,ny), ru1 (nz1,0:nx,ny)
     &    ,ru11(nz1,0:nx,ny), ru1t(nz1,0:nx,ny), fu1 (nz1,0:nx,ny)
     &    ,ru1i(nz1,0:nx,ny), gu1 (nz1,0:nx,ny), du1 (nz1,0:nx,ny)
     &    ,u3  (nz1,0:nx,ny), u31 (nz1,0:nx,ny), ru3 (nz1,0:nx,ny)
     &    ,ru31(nz1,0:nx,ny), ru3t(nz1,0:nx,ny), fu3 (nz1,0:nx,ny)
     &    ,ru3i(nz1,0:nx,ny), gu3 (nz1,0:nx,ny), du3 (nz1,0:nx,ny)
     &    ,u2  (nz1,nx,0:ny), u21 (nz1,nx,0:ny), ru2 (nz1,nx,0:ny)
     &    ,ru21(nz1,nx,0:ny), ru2t(nz1,nx,0:ny), fu2 (nz1,nx,0:ny)
     &    ,ru2i(nz1,nx,0:ny), gu2 (nz1,nx,0:ny), du2 (nz1,nx,0:ny)
     &    ,w   (nz ,nx,  ny), w1  (nz ,nx  ,ny), rw  (nz ,nx  ,ny)
     &    ,rw1 (nz ,nx  ,ny), rwt (nz1,nx  ,ny), fw  (nz1,nx  ,ny)
     &    ,t   (nz1,nx  ,ny), t1  (nz1,nx  ,ny), rt  (nz1,nx  ,ny)
     &    ,rt1 (nz1,nx  ,ny), rtt (nz1,nx  ,ny), ft  (nz1,nx  ,ny)
     &    ,ti  (nz1,nx  ,ny), rti (nz1,nx  ,ny), rtb (nz1,nx  ,ny)
     &    ,rr  (nz1,nx  ,ny), rr1 (nz1,nx  ,ny), rrt (nz1,nx  ,ny)
     &    ,rri (nz1,nx  ,ny), rb  (nz1,nx  ,ny), fr  (nz1,nx  ,ny)
     &    ,p   (nz1,nx  ,ny), pb  (nz1,nx  ,ny), pii (nz1,nx  ,ny)
     &    ,ww  (nz ,nx  ,ny), rho (nz1,nx  ,ny), tb  (nz1,nx  ,ny)
     &    ,rs  (nz1,nx  ,ny), ts  (nz1,nx  ,ny), div (nz1,nx  ,ny)
     &    ,a   (nz1,nx  ,ny), b   (nz1,nx  ,ny), c   (nz1,nx  ,ny)
     &    ,cofwz (nz1,nx,ny), coftz (nz ,nx,ny), cofwt (nz1,nx,ny)
     &    ,alpha (nz1,nx,ny), gamma (nz1,nx,ny), rhom  (nz1,nx,ny)
     &    ,flux1(nz1,0:nx,ny),flux2(nz1,nx,0:ny),flux3(nz1,0:nx,ny)
     &    ,cofwrr(nz1,nx,ny), cofwr     (nx,ny), dhh1      (nx,ny)
     &    ,dhh2      (nx,ny), dhh3      (nx,ny), hh        (nx,ny)
     &    ,hs        (nx,ny), wdtz(nz), zu(nz1), zw(nz),   ds(nz1)
     &    , u1z(nz1),u2z(nz1), u3z(nz1), tz(nz1), fluxz(0:nz1,nx,ny)
     &    ,ax(nz), tzv (nz1), rqvb(nz1),  rel_hum (nz1), qvzv(nz1)

      real ru1_save (nz1,0:nx,ny), ru3_save (nz1,0:nx,ny)
     &    ,ru2_save (nz1,nx,0:ny), rw_save  (nz ,  nx,ny)
     &    ,rt_save  (nz1,nx  ,ny), rr_save  (nz1,  nx,ny)
     &    ,t_d_tend (nz1,nx  ,ny)

      real rqv (nz1,nx,ny), rqc(nz1,nx,ny) , rqr (nz1,nx,ny)
     &    ,rqv1(nz1,nx,ny), rqc1(nz1,nx,ny), rqr1(nz1,nx,ny)
     &    ,qv1 (nz1,nx,ny), qc1 (nz1,nx,ny), qr1 (nz1,nx,ny)
     &    ,qv  (nz1,nx,ny), qc  (nz1,nx,ny), qr  (nz1,nx,ny)
     &    ,fqv (nz1,nx,ny), fqc (nz1,nx,ny), fqr (nz1,nx,ny)

      real*4 plt (nx,ny), pltx(nx,nz), plty(ny,nz), hxpl  (nx)
     &      ,xh  (nx,ny), xu1 (nx,ny), xu2 (nx,ny), xu3(nx,ny)
     &      ,yh  (nx,ny), yu1 (nx,ny), yu2 (nx,ny), yu3(nx,ny)
     &      ,x(nx),y(ny),time,pxl,pxr,pyl,pyr,pzl,zptop,xpll,xplr
     &      ,ypll,yplr,zplb,zplt,dxp,dyp,dzp,wmax(2401),waxis(2401)
     &      ,wmplt

      real*4 Azero(1)

c      common /grid/ xh(nx,ny), xu1(nx,ny), xu2(nx,ny), xu3(nx,ny),
c     &              yh(nx,ny), yu1(nx,ny), yu2(nx,ny), yu3(nx,ny)

      common /grid/ xh, xu1, xu2, xu3, yh, yu1, yu2, yu3

      integer imass, rk_step, ns_rk, total_steps
	  character*3 slice(2)
	  character*6 plane
	  equivalence (plane,slice)
      logical second
	  parameter(second = .true.)
c	  parameter(second = .false.)

      integer, PARAMETER :: IERF=6,LUNI=2,IWTY=20,IWID=1  !  PostScript
c--------------
c
      include "initialize.inc"
c
c--------------

      Azero(1) = 0.0

c***********************************************************************
c     timestep loop
c***********************************************************************
c
c*****Large time step calculations
c
      kkk = ip
      do nit = 0, total_steps

      if(nit .ne. 0) then

      kkk   = kkk+1
      npr   = npr+1
      time  = nit*dt
      tinit =.05*xa
      if(npr.eq.1)  then
         write(6,*) time, wmax(nit)
         write(0,*) time, wmax(nit)
         npr=0
      end if

      do j=1,ny
         do i=0,nx
            do k=1,nz1
               ru1_save(k,i,j) = ru1(k,i,j)
               ru3_save(k,i,j) = ru3(k,i,j)
            end do
         end do
      end do
      do j=0,ny
         do i=1,nx
            do k=1,nz1
               ru2_save(k,i,j) = ru2(k,i,j)
            end do
         end do
      end do
      do j=1,ny
         do i=1,nx
            do k=1,nz
               rw_save(k,i,j) = rw(k,i,j)
            end do
            do k=1,nz1
               rt_save(k,i,j) = rt(k,i,j)
               rr_save(k,i,j) = rr(k,i,j)
            end do
         end do
      end do
c
c*****Beginning of Runge Kutta time steps
c
      do rk_step = 1,3
c**********
c      do rk_step = 3,3

      if(rk_step .eq. 1) then
c         ns_rk = ns0/3
         ns_rk = 1
         dts = dt/float(ns_rk)/3.
      else if(rk_step .eq. 2) then
         ns_rk = ns0/2
         dts = dt/float(ns_rk)/2.
      else if(rk_step .eq. 3) then
         ns_rk = ns0
         dts = dt/float(ns_rk)
      end if
c**********
c      ns_rk = 1
c	  dts = dt
c**********
      dtseps = .25*dts*(1.+epssm)
      cofrz = 2.*dtseps*rdz
      dtsa   = dts*side/area
      dtsd   = dts/d
      dtsf   = dts*sqrt(3.)/6.*f
      dtsg   = dts*.5/d*g
	  xnus   = dts*xnus0
	  xnusz  = dts*xnusz0
      do j=1,ny
         do i=1,nx
            cofwr(i,j) = dtseps*g*hh(i,j)
         end do
      end do
c
c        calculation of omega, ww = gu1*ru1 + gu2*ru2 + gu3*ru3 + hh* rw
c
         do j=1,ny
            jm1 = j-1
            if(jper*j.eq.1) jm1=ny1
            do i=1,nx
               if(mod(i,2).eq.0)  then
                  jpj=j
                  jpm=jm1
               else
                  jpj=jp1
                  jpm=j
               end if
               im1 = i-1
               if(iper*i.eq.1) im1=nx1
               do k=2,nz1
                  ww(k,i,j) = .25*(gu1(k  ,i  ,j  )*ru1(k  ,i  ,j  )
     &                            +gu1(k  ,im1,jpj)*ru1(k  ,im1,jpj)
     &                            +gu1(k-1,i  ,j  )*ru1(k-1,i  ,j  )
     &                            +gu1(k-1,im1,jpj)*ru1(k-1,im1,jpj))
     &                       +.25*(gu3(k  ,i  ,j  )*ru3(k  ,i  ,j  )
     &                            +gu3(k  ,im1,jpm)*ru3(k  ,im1,jpm)
     &                            +gu3(k-1,i  ,j  )*ru3(k-1,i  ,j  )
     &                            +gu3(k-1,im1,jpm)*ru3(k-1,im1,jpm))
     &                       +.25*(gu2(k  ,i  ,j  )*ru2(k  ,i  ,j  )
     &                            +gu2(k  ,i  ,jm1)*ru2(k  ,i  ,jm1)
     &                            +gu2(k-1,i  ,j  )*ru2(k-1,i  ,j  )
     &                            +gu2(k-1,i  ,jm1)*ru2(k-1,i  ,jm1))
     &                        +hh(i,j)*rw(k,i,j)
               end do
            end do
         end do

         call rhs_u1(u1,u11,ru1,fu1,ww,rho,ru2,ru3,u1z,u2z,u3z,u1m,u2m,
     &             u3m,ds,dtsa,dtsd,dtsf,dts,c1f,c2f,rdz,xnus,xnusz,
     &             nz1,nx,ny,iper,jper,flux1,flux2,flux3,fluxz,'fifth ')

         call rhs_u3(u3,u31,ru3,fu3,ww,rho,ru1,ru2,u1z,u2z,u3z,u1m,u2m,
     &             u3m,ds,dtsa,dtsd,dtsf,dts,c1f,c2f,rdz,xnus,xnusz,
     &             nz1,nx,ny,iper,jper,flux1,flux2,flux3,fluxz,'fifth ')

         call rhs_u2(u2,u21,ru2,fu2,ww,rho,ru1,ru3,u1z,u2z,u3z,u1m,u2m,
     &             u3m,ds,dtsa,dtsd,dtsf,dts,c1f,c2f,rdz,xnus,xnusz,
     &             nz1,nx,ny,iper,jper,flux1,flux2,flux3,fluxz,'fifth ')

         call rhs_w( w,w1,fw,ww,p,pb,rt,rtb,rho,ru1,ru2,ru3,rcv,rb,rqv,
     &               rqc,rqr,rqvb,dtsa,g,ds,dts,rdz,f,xnus,xnusz,nz1,
     &               nx,ny,iper,jper,flux1,flux2,flux3,fluxz,'fifth ')

         call rhs_s( t ,t1 ,ft ,ww,ru1,ru2,ru3,rho,ds,dts,dtsa,rdz,
     $               xnus,xnusz,nz1,nx,ny,iper,jper,
     &               ti,nz1,nx,ny,flux1,flux2,flux3,fluxz,'fifth ')

         call rhs_s( qv,qv1,fqv,ww,ru1,ru2,ru3,rho,ds,dts,dtsa,rdz,
     $               xnus,xnusz,nz1,nx,ny,iper,jper,
     &               qvzv,nz1,1,1,flux1,flux2,flux3,fluxz,'fifth ')

         call rhs_s( qc,qc1,fqc,ww,ru1,ru2,ru3,rho,ds,dts,dtsa,rdz,
     $               xnus,xnusz,nz1,nx,ny,iper,jper,
     &               Azero, 1  ,1,1,flux1,flux2,flux3,fluxz,'fifth ')

         call rhs_s( qr,qr1,fqr,ww,ru1,ru2,ru3,rho,ds,dts,dtsa,rdz,
     $               xnus,xnusz,nz1,nx,ny,iper,jper,
     &               Azero, 1  ,1,1,flux1,flux2,flux3,fluxz,'fifth ')


         call rhs_rho( fr,ru1,ru2,ru3,ww,dts,dtsa,rdz,
     &                 nz1,nx,ny,iper,jper      )

c
c--------------

      include "boundaries.inc"

c--------------
c
c        advance moisture variables over interval
c
         do j=1,ny
            do i=1,nx
               do k=1,nz1
                  rqv(k,i,j) = amax1(rqv1(k,i,j) + ns_rk*fqv(k,i,j),0.0)
                  rqc(k,i,j) = amax1(rqc1(k,i,j) + ns_rk*fqc(k,i,j),0.0)
                  rqr(k,i,j) = amax1(rqr1(k,i,j) + ns_rk*fqr(k,i,j),0.0)
               end do
            end do
         end do
c
c        add in the diabatic theta_v tendency from the last timestep
c
         do j=1,ny
            do i=1,nx
               do k=1,nz1
                  ft(k,i,j) = ft(k,i,j) + dts*rho(k,i,j)*t_d_tend(k,i,j)
               end do
            end do
         end do
c
c********small time step calculations
c
c        coefficients for tri-diagonal matrix and do small steps
c
         call calc_scoef( dtseps, c2, hh, rdz, t, p, tb, 
     *                       rho, rb, rqv, rqc, rqr, rqvb,
     *                       g, rcv, cofwz, coftz, cofwt,
     *                       cofwr, cofwrr, cofrz,
     *                       a, b, c, alpha, gamma, nx,ny,nz1    )
c
         call smlstep( ru1,ru11,fu1,ru2,ru21,fu2,ru3,ru31,fu3, 
     &                    rw,rw1,fw,t,ts,rt1,ft,rs,rr1,fr,
     *                    p,ww,div,du1,du2,du3,hh,gu1,gu2,gu3,
     *                    a,alpha,gamma,dhh1,dhh2,dhh3,
     *                    cofrz,coftz,cofwz,cofwr,cofwrr,cofwt,
     *                    rdz,dts,dtsa,dtsd,c2,smdivx,smdivz,resm,
     *                    nx,ny,nz1,ns_rk,iper,jper  )

         if(rk_step .le. 2) then
c
c********set levels for full step
c
            do j=1,ny
               do i=0,nx
                  do k=1,nz1
                     ru1 (k,i,j) = ru11    (k,i,j)
                     ru11(k,i,j) = ru1_save(k,i,j)
                     ru3 (k,i,j) = ru31    (k,i,j)
                     ru31(k,i,j) = ru3_save(k,i,j)
                  end do
               end do
            end do
            do j=0,ny
               do i=1,nx
                  do k=1,nz1
                     ru2 (k,i,j) = ru21    (k,i,j)
                     ru21(k,i,j) = ru2_save(k,i,j)
                  end do
               end do
            end do
            do j=1,ny
               do i=1,nx
                  do k=1,nz
                     rw (k,i,j) = rw1    (k,i,j)
                     rw1(k,i,j) = rw_save(k,i,j)
                  end do
                  do k=1,nz1
                     rt (k,i,j) = rt1    (k,i,j)
                     rt1(k,i,j) = rt_save(k,i,j)
                     rr (k,i,j) = rr1    (k,i,j)
                     rr1(k,i,j) = rr_save(k,i,j)
                     rho(k,i,j) = rb(k,i,j) + rr(k,i,j)
                  end do
               end do
            end do

            do j=1,ny
               jp1 = min(j+1,ny)
               if(jper*j.eq.ny)  jp1 = 2
               jv1 = j-1
               if(jper*j.eq.1 )  jv1 = ny1
               jm1 = max(j-1,1)
               if(jper*j.eq.1 )  jm1 = ny1
               do i=1,nx
                  if(mod(i,2).eq.0)  then
                     jpj = j
                     jpm = jm1
                  else
                     jpj = jp1
                     jpm = j
                  end if
                  ip1 = min(i+1,nx)
                  if(iper*i.eq.nx)  ip1 = 2
                  im1 = i-1
                  if(iper*i.eq.1 )  im1 = nx1
                  do k=1,nz1
                     u1(k,i,j) = ru1(k,i,j)
     &                              /(.5*(rho(k,i,j)+rho(k,ip1,jpm)))
                     u3(k,i,j) = ru3(k,i,j)
     &                              /(.5*(rho(k,i,j)+rho(k,ip1,jpj)))
                     u2(k,i,j) = ru2(k,i,j)
     &                              /(.5*(rho(k,i,j)+rho(k,i  ,jp1)))
                  end do

                  do k=1,nz1
                     t (k,i,j) = (rtb(k,i,j)+rt(k,i,j))/rho(k,i,j)
                     p (k,i,j) = (hh(i,j)*(rtb(k,i,j)+rt(k,i,j))
     &                                   /(100000./287./300.) )**rcv
                     qv(k,i,j) = rqv(k,i,j)/rho(k,i,j)
                     qc(k,i,j) = rqc(k,i,j)/rho(k,i,j)
                     qr(k,i,j) = rqr(k,i,j)/rho(k,i,j)

                  end do

                  k=1
                     rw(k,i,j) = - .25/hh(i,j)*
     &                         (3.*(gu1(k  ,i  ,j  )*ru1(k  ,i  ,j  )
     &                             +gu1(k  ,im1,jpj)*ru1(k  ,im1,jpj))
     &                            -(gu1(k+1,i  ,j  )*ru1(k+1,i  ,j  )
     &                             +gu1(k+1,im1,jpj)*ru1(k+1,im1,jpj))
     &                         +3.*(gu3(k  ,i  ,j  )*ru3(k  ,i  ,j  )
     &                             +gu3(k  ,im1,jpm)*ru3(k  ,im1,jpm))
     &                            -(gu3(k+1,i  ,j  )*ru3(k+1,i  ,j  )
     &                             +gu3(k+1,im1,jpm)*ru3(k+1,im1,jpm))
     &                         +3.*(gu2(k  ,i  ,j  )*ru2(k  ,i  ,j  )
     &                             +gu2(k  ,i  ,jv1)*ru2(k  ,i  ,jv1))
     &                            -(gu2(k+1,i  ,j  )*ru2(k+1,i  ,j)
     &                             +gu2(k+1,i  ,jv1)*ru2(k+1,i  ,jv1)))
                     w (k,i,j)=2.*rw(k,i,j)/(3.*rho(k,i,j)-rho(k+1,i,j))
                  do k=2,nz1
                     w (k,i,j) =rw(k,i,j)/(.5*(rho(k,i,j)+rho(k-1,i,j)))
                  end do
               end do
            end do
            if(iper.eq.0)  then
               do j=1,ny
                  do k=1,nz1
                     u1  (k,0,j) = ru1(k,0,j)/rho(k,1,j)
                     u3  (k,0,j) = ru3(k,0,j)/rho(k,1,j)
                  end do
               end do
            end if
            if(jper.eq.0)  then
               do i=1,nx
                  do k=1,nz1
                     u2  (k,i,0) = ru2(k,i,0)/rho(k,i,1)
                  end do
               end do
            end if
         else if ( rk_step .eq. 3) then
c
c********reset levels for full step
c
c        subtract out diabatic theta_v tendency from the last timestep
c
            do j=1,ny
               do i=1,nx
                  do k=1,nz1
                     rt1(k,i,j) = rt1(k,i,j) 
     &                           - ns_rk*dts*rho(k,i,j)*t_d_tend(k,i,j)
                  end do
               end do
            end do

            do j=1,ny
               do i=1,nx
                  do k=1,nz1
                     ru1(k,i,j) = ru11(k,i,j)
                     ru3(k,i,j) = ru31(k,i,j)
                     ru2(k,i,j) = ru21(k,i,j)
                     rw (k,i,j) = rw1 (k,i,j)
                     rt (k,i,j) = rt1 (k,i,j)
                     rr (k,i,j) = rr1 (k,i,j)    
                     rho(k,i,j) = rb  (k,i,j)+rr(k,i,j)
                  end do
               end do
            end do
            do j=1,ny
               jp1 = min(j+1,ny)
               if(jper*j.eq.ny) jp1 = 2
               jv1 = j-1
               if(jper*j.eq.1)  jv1 = ny1
               jm1 = max(j-1,1)
               if(jper*j.eq.1)  jm1 = ny1
               do i=1,nx
                  if(mod(i,2).eq.0)  then
                     jpj = j
                     jpm = jm1
                  else
                     jpj = jp1
                     jpm = j
                  end if
                  ip1 = min(i+1,nx)
                  if(iper*i.eq.nx)  ip1 = 2
                  im1 = i-1
                  if(iper*i.eq.1 )  im1 = nx1
                  do k=1,nz1
                     u1(k,i,j)  = ru1(k,i,j)
     &                              /(.5*(rho(k,i,j)+rho(k,ip1,jpm)))
                     u3(k,i,j)  = ru3(k,i,j)
     &                              /(.5*(rho(k,i,j)+rho(k,ip1,jpj)))
                     u2(k,i,j)  = ru2(k,i,j)
     &                              /(.5*(rho(k,i,j)+rho(k,i  ,jp1)))
                     u11(k,i,j) = u1 (k,i,j)
                     u31(k,i,j) = u3 (k,i,j)
                     u21(k,i,j) = u2 (k,i,j)
                  end do
                  do k=1,nz1
                     t  (k,i,j) = (rtb(k,i,j)+rt(k,i,j))/rho(k,i,j)
                     t1 (k,i,j) = t(k,i,j)
                     p  (k,i,j) = (hh(i,j)*(rtb(k,i,j)+rt(k,i,j))
     &                                   /(100000./287./300.) )**rcv
                     qv (k,i,j) = rqv(k,i,j)/rho(k,i,j)
                     qc (k,i,j) = rqc(k,i,j)/rho(k,i,j)
                     qr (k,i,j) = rqr(k,i,j)/rho(k,i,j)

                     qv1(k,i,j) = qv(k,i,j)
c                    qc1(k,i,j) = qc(k,i,j)
c                    qr1(k,i,j) = qr(k,i,j)

                     rqv1(k,i,j) = rqv(k,i,j)
                     rqc1(k,i,j) = rqc(k,i,j)
                     rqr1(k,i,j) = rqr(k,i,j)
               
                  end do

                  k=1
                     rw(k,i,j) = - .25/hh(i,j)*
     &                         (3.*(gu1(k  ,i  ,j  )*ru1(k  ,i  ,j  )
     &                             +gu1(k  ,im1,jpj)*ru1(k  ,im1,jpj))
     &                            -(gu1(k+1,i  ,j  )*ru1(k+1,i  ,j  )
     &                             +gu1(k+1,im1,jpj)*ru1(k+1,im1,jpj))
     &                         +3.*(gu3(k  ,i  ,j  )*ru3(k  ,i  ,j  )
     &                             +gu3(k  ,im1,jpm)*ru3(k  ,im1,jpm))
     &                            -(gu3(k+1,i  ,j  )*ru3(k+1,i  ,j  )
     &                             +gu3(k+1,im1,jpm)*ru3(k+1,im1,jpm))
     &                         +3.*(gu2(k  ,i  ,j  )*ru2(k  ,i  ,j  )
     &                             +gu2(k  ,i  ,jv1)*ru2(k  ,i  ,jv1))
     &                            -(gu2(k+1,i  ,j  )*ru2(k+1,i  ,j)
     &                             +gu2(k+1,i  ,jv1)*ru2(k+1,i  ,jv1)))
                     w (k,i,j)=2.*rw(k,i,j)/(3.*rho(k,i,j)-rho(k+1,i,j))
                     w1(k,i,j)=   w (k,i,j)
                  do k=2,nz1
                     w (k,i,j) =rw(k,i,j)/(.5*(rho(k,i,j)+rho(k-1,i,j)))
                     w1(k,i,j) =w (k,i,j)
                  end do
               end do
            end do
            if(iper.eq.0)  then
               do j=1,ny
                  do k=1,nz1
                     ru1(k,0,j) = ru11(k,0,j)
                     u1 (k,0,j) = ru1 (k,0,j)/rho(k,1,j)
                     u11(k,0,j) = u1  (k,0,j)
                     ru3(k,0,j) = ru31(k,0,j)
                     u3 (k,0,j) = ru3 (k,0,j)/rho(k,1,j)
                     u31(k,0,j) = u3  (k,0,j)
                  end do
               end do
            end if
            if(jper.eq.0)  then
               do i=1,nx
                  do k=1,nz1
                     ru2(k,i,0) = ru21(k,i,0)
                     u2 (k,i,0) = ru2 (k,i,0)/rho(k,i,1)
                     u21(k,i,0) = u2  (k,i,0)
                  end do
               end do
            end if
         end if ! if rk_step = 2

      end do  ! rk step loop

c-----------------------------------------------------------------------
c
c  here is the microphysics (warm rain)
c
      do j=1,ny
         do i=1,nx
            do k=1,nz1
               t_d_tend(k,i,j) = t(k,i,j)
               t       (k,i,j) = t0*t(k,i,j)/(1.+1.61*qv(k,i,j))
            end do
         end do
      end do

c      call kessler( t, qv, qc, qc1, qr, qr1, rho, p,
c     *              dt, dz, nz1, nx, ny                  )
c      call kessler( t, qv, qc, qc1, qr, qr1, rb, pb,
c     *              dt, dz, nz1, nx, ny                  )
cc    call kessler_joe( t, qv, qc, qc1, qr, qr1, rho, pb,
      call kessler_joe( t, qv, qc, qc, qr, qr, rho, pb,
     *              dt, dz, nz1, nx, ny                  )

      do j=1,ny
         do i=1,nx
            do k=1,nz1

               t   (k,i,j) = t(k,i,j)*(1.+1.61*qv(k,i,j))/t0 
               t_d_tend(k,i,j) = (t(k,i,j) - t_d_tend(k,i,j))/(ns*dts)
cc             t_d_tend(k,i,j) = 0.
               t1  (k,i,j) = t(k,i,j)
               rt  (k,i,j) = t(k,i,j)*rho(k,i,j) - rtb(k,i,j)
               rt1 (k,i,j) = rt(k,i,j)
        
               rqv (k,i,j) = qv(k,i,j)*rho(k,i,j)
               rqv1(k,i,j) = rqv(k,i,j)
               qv1 (k,i,j) = qv(k,i,j)

               rqc (k,i,j) = qc(k,i,j)*rho(k,i,j)
               rqc1(k,i,j) = rqc(k,i,j)
               qc1 (k,i,j) = qc(k,i,j)

               rqr (k,i,j) = qr(k,i,j)*rho(k,i,j)
               rqr1(k,i,j) = rqr(k,i,j)
               qr1 (k,i,j) = qr(k,i,j)

               p   (k,i,j) = (hh(i,j)*(rtb(k,i,j)+rt(k,i,j))
     &                               /(100000./287./300.))**rcv
            end do
         end do
      end do

c      wmax = 0.
c      do j=1,ny
c         do i=1,nx
c            do k=2,nz1
c               wmax = amax1(wmax,w(k,i,j))
c            end do
c         end do
c      end do

c      vdiffm = 0.
c      do j=1,ny
c         do i=1,nx
c            do k=1,nz1
c			   if(mod(i,2).eq.0.)  then
c			      nyj=ny+1-j
cc			      nyj=nyc+1-j
c				  if(nyj.lt.1)  nyj=nyj+ny1
c                  vdiff=abs(u2(k,i,j)+u2(k,i,nyj))
c			   else
c			      nyj=ny-j
c			      nyj=nyc-j
c				  if(nyj.lt.1)  nyj=nyj+ny1
c                  vdiff=abs(u2(k,i,j)+u2(k,i,nyj))
c			   end if
c			   if(vdiff.gt.vdiffm)  then
c                  vdiffm = vdiff
c				  ivm=i
c				  jvm=j
c				  kvm=k
c			   end if
c            end do
c         end do
cc         write(6,*) j,yu2(91,j),u2(1,91,j),yu2(92,j),u2(1,92,j)
c      end do
c	  write(6,*) vdiffm,u2(kvm,ivm,jvm),ivm,jvm,kvm

      end if !  take step only after plotting first
c
c
c**** processing for plotting
c
c--------------
c
      include "plotting.inc"
c
c--------------
c
      end do  ! for timestep loop

      call clsgks()

      stop
      end
	 
