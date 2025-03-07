c
         if(iper.eq.0) then
c
c           boundary conditions at i=1
c
            i=1
            do j=1,ny
               jm1 = j-1
               if(jper*j.eq.1)  jm1 = ny1
               do k=1,nz1
                  ub        = amin1(ru1(k,0,j)-cb*rho(k,1,j) , 0.)
                  fu1(k,0,j) = - dts*rdx*ub*(u11(k,i,j)-u11(k,i-1,j))
c     &                        + dts*  f*.5*(ru2(k,i,j)+ru2(k,i,jm1))
     &                        - ds(k)*rho(k,i,j)*(u11(k,0,j)-u1z(k)-u1m)
                  ub      = amin1(.5*(ru1(k,i,j)+ru1(k,i-1,j)), 0.)
                  ft (k,i,j) = ft (k,i,j)
     &                          - dts*rdx*(ub*(t1 (k,i+1,j)-t1 (k,i,j))
     &                             +t (k,i,j)*(ru1(k,i,j)-ru1(k,i-1,j)))
                  fqv(k,i,j) = fqv(k,i,j) 
     &                          - dts*rdx*(ub*(qv1(k,i+1,j)-qv1(k,i,j))
     &                             +qv(k,i,j)*(ru1(k,i,j)-ru1(k,i-1,j)))
                  fqc(k,i,j) = fqc(k,i,j) 
     &                          - dts*rdx*(ub*(qc1(k,i+1,j)-qc1(k,i,j))
     &                             +qc(k,i,j)*(ru1(k,i,j)-ru1(k,i-1,j)))
                  fqr(k,i,j) = fqr(k,i,j)
     &                          - dts*rdx*(ub*(qr1(k,i+1,j)-qr1(k,i,j))
     &                             +qr(k,i,j)*(ru1(k,i,j)-ru1(k,i-1,j)))
               end do
            end do
            do j=1,ny1+jper
               jm1 = j-1
               if(jper*j.eq.1)  jm1 = ny1
               jp1 = j+1
               if(j.eq.ny)  jp1 = 2
               do k=1,nz1
                  ub        = amin1(ru1(k,0,j)-cb*rho(k,1,j) , 0.)
                  fu2 (k,i,j) = fu2 (k,i,j)
     &                    - dts*rdx*(ub*(u21(k,i+1,j)-u21(k,i  ,j  ))
     &                    +.5*u2 (k,i,j)*(ru1(k,i,jp1)-ru1(k,i-1,jp1)
     &                                  +ru1(k,i,j  )-ru1(k,i-1,j  )))
               end do
            end do
            do k=2,nz1
               ub      = amin1(.25*(ru1(k,i  ,j)+ru1(k-1,i  ,j)
     &                             +ru1(k,i-1,j)+ru1(k-1,i-1,j)),0.)
               fw(k,i,j) = fw(k,i,j) 
     &                      - dts*rdx*(ub*(w1(k,i+1,j)-w1(k  ,i  ,j))
     &                      + w(k,i,j)*.5*(ru1(k  ,i,j)+ru1(k-1,i  ,j)
     &                                    -ru1(k,i-1,j)-ru1(k-1,i-1,j))) 
            end do
            do j=1,ny
               do k=2,nz2
                  fu1(k,0,j)=fu1(k,0,j)+xnus*rho(k,i,j)
     &                       *(u11(k+1,0,j)-2.*u11(k,0,j)+u11(k-1,0,j))
               end do
            end do
c
c           boundary conditions at i=nx
c
            i=nx
            do j=1,ny
               jm1 = j-1
               if(jper*j.eq.1)  jm1 = ny1
               do k=1,nz1
                  ub        = amax1(ru1(k,i,j)+cb*rho(k,i,j) , 0.)
                  fu1(k,i,j) = - dts*rdx*ub*(u11(k,i,j)-u11(k,i-1,j))
c     &                        + dts*  f*.5*(ru2(k,i,j)+ru2(k,i,jm1))
     &                        - ds(k)*rho(k,i,j)*(u11(k,i,j)-u1z(k)-u1m)
                  ub      = amax1(.5*(ru1(k,i,j)+ru1(k,i-1,j)), 0.)
                  ft (k,i,j) = ft (k,i,j)
     &                          - dts*rdx*(ub*(t1 (k,i,j)-t1 (k,i-1,j))
     &                             +t (k,i,j)*(ru1(k,i,j)-ru1(k,i-1,j)))
                  fqv(k,i,j) = fqv(k,i,j) 
     &                          - dts*rdx*(ub*(qv1(k,i,j)-qv1(k,i-1,j))
     &                             +qv(k,i,j)*(ru1(k,i,j)-ru1(k,i-1,j)))
                  fqc(k,i,j) = fqc(k,i,j) 
     &                          - dts*rdx*(ub*(qc1(k,i,j)-qc1(k,i-1,j))
     &                             +qc(k,i,j)*(ru1(k,i,j)-ru1(k,i-1,j)))
                  fqr(k,i,j) = fqr(k,i,j)
     &                          - dts*rdx*(ub*(qr1(k,i,j)-qr1(k,i-1,j))
     &                             +qr(k,i,j)*(ru1(k,i,j)-ru1(k,i-1,j)))
               end do
            end do
            do j=1,ny1+jper
               jm1 = j-1
               if(jper*j.eq.1)  jm1 = ny1
               jp1 = j+1
               if(j.eq.ny)  jp1 = 2
               do k=1,nz1
                  ub        = amax1(ru1(k,i,j)-cb*rho(k,i,j) , 0.)
                  fu2 (k,i,j) = fu2 (k,i,j)
     &                    - dts*rdx*(ub*(u21 (k,i,j  )-u21(k,i-1,j  ))
     &                    +.5*u2(k,i,j)*(ru1(k,i,jp1)-ru1(k,i-1,jp1)
     &                                  +ru1(k,i,j  )-ru1(k,i-1,j  )))
               end do
            end do
            do k=2,nz1
               ub      = amax1(.25*(ru1(k,i  ,j)+ru1(k-1,i  ,j)
     &                             +ru1(k,i-1,j)+ru1(k-1,i-1,j)),0.)
               fw(k,i,j) = fw(k,i,j) 
     &                      - dts*rdx*(ub*(w1(k,i  ,j)-w1(k  ,i-1,j))
     &                      + w(k,i,j)*.5*(ru1(k,i  ,j)+ru1(k-1,i  ,j)
     &                                    -ru1(k,i-1,j)-ru1(k-1,i-1,j))) 
            end do
            do j=1,ny
               do k=2,nz2
                  fu1(k,i,j)=fu1(k,i,j)+xnus*rho(k,i,j)
     &                       *(u11(k+1,i,j)-2.*u11(k,i,j)+u11(k-1,i,j))
               end do
            end do
c
c           mass conservation at open x boundaries
c
            if(imass .eq. 1) then
               sum=0.
               do j=1,ny
                  do k=1,nz1
                     sum = sum + fu1(k,0,j)
                  end do
               end do
               sum = sum/float(nz1*ny)
               fura = 0.
               rula = 0.
               do j=1,ny
                  do k=1,nz1
                     fu1(k,0,j) = fu1(k,0,j) - sum
                  end do
               end do

               sum=0.
               do j=1,ny
                  do k=1,nz1
                     sum = sum + fu1(k,nx,j)
                  end do
               end do
               sum = sum/float(nz1*ny)
               do j=1,ny
                  do k=1,nz1
                     fu1(k,nx,j) = fu1(k,nx,j) - sum
                  end do
               end do

            end if

         else

            do j=1,ny
               do k=1,nz1
                  fu1(k,nx,j) = fu1(k,1  ,j)
                  fu1(k,0 ,j) = fu1(k,nx1,j)
               end do
            end do

         end if
c
         if(jper.eq.0) then
c
c           boundary conditions at j=1
c
            j=1
            do i=1,nx
               im1 = i-1
               if(iper*i.eq.1)  im1 = nx1
               do k=1,nz1
                  ub        = amin1(ru2(k,i,0)-cb*rho(k,i,1) , 0.)
                  fu2(k,i,0) = - dts*rdy*ub*(u21(k,i,j)-u21(k,i,j-1))
c     &                        + dts*  f*.5*(ru1(k,i,j)+ru1(k,im1,j))
     &                        - ds(k)*rho(k,i,j)*(u21(k,i,0)-u2z(k)-u2m)
                  ub      = amin1(.5*(ru2(k,i,j)+ru2(k,i,j-1)), 0.)
                  ft (k,i,j) = ft (k,i,j)
     &                          - dts*rdy*(ub*(t1 (k,i,j+1)-t1 (k,i,j))
     &                             +t (k,i,j)*(ru2(k,i,j)-ru2(k,i,j-1)))
                  fqv(k,i,j) = fqv(k,i,j) 
     &                          - dts*rdy*(ub*(qv1(k,i,j+1)-qv1(k,i,j))
     &                             +qv(k,i,j)*(ru2(k,i,j)-ru2(k,i,j-1)))
                  fqc(k,i,j) = fqc(k,i,j) 
     &                          - dts*rdy*(ub*(qc1(k,i,j+1)-qc1(k,i,j))
     &                             +qc(k,i,j)*(ru2(k,i,j)-ru2(k,i,j-1)))
                  fqr(k,i,j) = fqr(k,i,j)
     &                          - dts*rdy*(ub*(qr1(k,i,j+1)-qr1(k,i,j))
     &                             +qr(k,i,j)*(ru2(k,i,j)-ru2(k,i,j-1)))
               end do
            end do
            do i=1,nx1+iper
               im1 = i-1
               if(iper*i.eq.1)  im1 = nx1
               ip1 = i+1
               if(i.eq.nx)  ip1 = 2
               do k=1,nz1
                  ub        = amin1(ru2(k,i,0)-cb*rho(k,i,1) , 0.)
                  fu1 (k,i,j) = fu1 (k,i,j)
     &                    - dts*rdy*(ub*(u11 (k,i,j+1)-u11(k,i  ,j  ))
     &                    +.5*u1 (k,i,j)*(ru2(k,ip1,j)-ru2(k,ip1,j-1)
     &                                  +ru2(k,i,j  )-ru2(k,i  ,j-1)))
               end do
            end do
            do k=2,nz1
               ub      = amin1(.25*(ru2(k,i,j  )+ru2(k-1,i,j  )
     &                             +ru2(k,i,j-1)+ru2(k-1,i,j-1)),0.)
               fw(k,i,j) = fw(k,i,j) 
     &                      - dts*rdy*(ub*(w1(k,i,j+1)-w1(k  ,i,j  ))
     &                      + w(k,i,j)*.5*(ru2(k  ,i,j)+ru2(k-1,i,j  )
     &                                    -ru2(k,i,j-1)-ru2(k-1,i,j-1))) 
            end do
            do i=1,nx
               do k=2,nz2
                  fu2(k,i,0)=fu2(k,i,0)+xnus*rho(k,i,j)
     &                       *(u21(k+1,i,0)-2.*u21(k,i,0)+u21(k-1,i,0))
               end do
            end do
c
c           boundary conditions at j=ny
c
            j = ny
            do i=1,nx
               im1 = i-1
               if(iper*i.eq.1)  im1 = nx1
               do k=1,nz1
                  ub        = amax1(ru2(k,i,j)+cb*rho(k,i,j) , 0.)
                  fu2(k,i,j) = - dts*rdy*ub*(u21(k,i,j)-u21(k,i,j-1))
c     &                        + dts*  f*.5*(ru1(k,i,j)+ru1(k,im1,j))
     &                        - ds(k)*rho(k,i,j)*(u21(k,i,j)-u2z(k)-u2m)
                  ub      = amax1(.5*(ru2(k,i,j)+ru2(k,i,j-1)), 0.)
                  ft (k,i,j) = ft (k,i,j)
     &                          - dts*rdy*(ub*(t1 (k,i,j)-t1 (k,i,j-1))
     &                             +t (k,i,j)*(ru2(k,i,j)-ru2(k,i,j-1)))
                  fqv(k,i,j) = fqv(k,i,j) 
     &                          - dts*rdy*(ub*(qv1(k,i,j)-qv1(k,i,j-1))
     &                             +qv(k,i,j)*(ru2(k,i,j)-ru2(k,i,j-1)))
                  fqc(k,i,j) = fqc(k,i,j) 
     &                          - dts*rdy*(ub*(qc1(k,i,j)-qc1(k,i,j-1))
     &                             +qc(k,i,j)*(ru2(k,i,j)-ru2(k,i,j-1)))
                  fqr(k,i,j) = fqr(k,i,j)
     &                          - dts*rdy*(ub*(qr1(k,i,j)-qr1(k,i,j-1))
     &                             +qr(k,i,j)*(ru2(k,i,j)-ru2(k,i,j-1)))
               end do
            end do
            do i=1,nx1+iper
               im1 = i-1
               if(iper*i.eq.1)  im1 = nx1
               ip1 = i+1
               if(i.eq.nx)  ip1 = 2
               do k=1,nz1
                  ub        = amin1(ru2(k,i,j)-cb*rho(k,i,j) , 0.)
                  fu1 (k,i,j) = fu1 (k,i,j)
     &                    - dts*rdy*(ub*(u1 (k,i,j  )-u1(k,i  ,j-1))
     &                    +.5*u1 (k,i,j)*(ru2(k,ip1,j)-ru2(k,ip1,j-1)
     &                                  +ru2(k,i,j  )-ru2(k,i  ,j-1)))
               end do
            end do
            do k=2,nz1
               ub      = amax1(.25*(ru2(k,i,j  )+ru2(k-1,i,j  )
     &                             +ru2(k,i,j-1)+ru2(k-1,i,j-1)),0.)
               fw(k,i,j) = fw(k,i,j) 
     &                      - dts*rdy*(ub*(w1(k,i,j  )-w1(k  ,i,j-1))
     &                      + w(k,i,j)*.5*(ru2(k,i,j  )+ru2(k-1,i,j  )
     &                                    -ru2(k,i,j-1)-ru2(k-1,i,j-1))) 
            end do
            do i=1,nx
               do k=2,nz2
                  fu2(k,i,j)=fu2(k,i,j)+xnus*rho(k,i,j)
     &                       *(u21(k+1,i,j)-2.*u21(k,i,j)+u21(k-1,i,j))
               end do
            end do
c
c  mass conservation at open y boundaries
c
            if(imass .eq. 1) then
               sum=0.
               do i=1,nx
                  do k=1,nz1
                     sum = sum+fu2(k,i,0)
                  end do
               end do
               sum=sum/float(nz1*nx)
               fura = 0.
               rula = 0.
               do i=1,nx
                  do k=1,nz1
                     fu2(k,i,0)=fu2(k,i,0)-sum
                  end do
               end do

               sum=0.
               do i=1,nx
                  do k=1,nz1
                     sum = sum+fu2(k,i,ny)
                  end do
               end do
               sum = sum/float(nz1*nx)
               do i=1,nx
                  do k=1,nz1
                     fu2(k,i,ny) = fu2(k,i,ny)-sum
                  end do
               end do

            end if

         else

            do i=1,nx
               do k=1,nz1
                  fu2(k,i,ny) = fu2(k,i,1  )
                  fu2(k,i,0 ) = fu2(k,i,ny1)
               end do
            end do

         end if
c
