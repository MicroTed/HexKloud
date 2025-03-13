     MODULE RK3_GRID
     
     implicit none
     
      real, allocatable, dimension(:,:) :: xh,xu1,xu2,xu3, yh,yu1,yu2,yu3
      integer ::  nx= 91, ny= 79, nz= 41
      real    :: xl = 84000., zl = 20000., yl
      real    :: dx,dy,dz

! plotting related vars
      real    :: xpll,xplr,ypll,yplr, zplb, zplt, wmplt, dxp,dyp,dzp
     
      END MODULE RK3_GRID

! =============================================================

     MODULE RK3_PARAM
     
       implicit none
       
       integer :: iper = 1, jper = 1
       integer :: imass = 0
       
       real    :: pi = 3.141592653589793
       real    :: angle = 0.0
       real    :: um,vm,u1m,u3m,u2m 
       real    :: ur = -15.0 ! grid motion
       real    :: side, d
       real    :: g = 9.81, f = 0.0
       real    :: t0 = 300.
       real, parameter :: r = 287., cp = 1003., p0 = 100000.
       real    :: rcv, cti, c2
       real    :: cb = 25., delt = 3.0


     END MODULE RK3_PARAM

