c     TO COMPILE: gfortran -o elections elections.for subrotelections.for subrotspg.for random.for -fno-align-commons

c     MAIN PROGRAM
      implicit none
      integer m, n, maxit, maxav, i, j, k, p
      double precision x(400), xn(400), g(400), gn(400), l(400), u(400)
      double precision alfa, eps, z, seed, y(600), yp(600)
      double precision prev, gama, sigma, beta, teta
      common /pesq/ p,m,y,gama,sigma,beta,teta

      write(*,*) 'Number of candidates:'
      read(*,*) m
      write(*,*) 'Number of researches:'
      read(*,*) p

      n = m**2
      maxit = 100
      maxav = 2000
      seed = 1717.d0
      alfa = 1.d-4
      eps = 1.d-4
      gama = 5.d2
c      sigma = 0.d0
      sigma = 4.d2
      beta = 101.d-2
      teta = 1.d2

c     votes from each candidate, for each research, are filled
      do i = 1,p
            write(*,*) 'Votes in research number', i
            read(*,*) (y(k), k=(i-1)*m+1,i*m)
      end do

c      write(*,*) 'Prediction vector:'
c      read(*,*) (yp(k), k=1,m)

c      calculates an initial solution guess
c      do i = 1,m
c            do j = 1,m
c                  if(i.eq.j) then
c                        x((i-1)*m + j) = 1.d0
c                  else
c                        x((i-1)*m + j) = 0.d0
c                  end if
c            end do
c      end do
      do i = 1,n
            call random(seed,z)
            x(i) = z
      end do
c     settle the constraint "box" (the solutions upper and lower bounds)
      do i=1,n
            l(i) = 0.d0
            u(i) = 1.d0
      end do

      call spg(n,x,l,u,alfa,g,xn,gn,eps,maxit,maxav)

      write(*,*) 'Previsão:'
c     Apply the solution matrix into the last data, printing the votes's prediction
      do i = 1,m
            prev = 0.d0
            do j = 1,m
c                  prev = prev + x((i-1)*m + j)*yp(j)
                  prev = prev + x((i-1)*m + j)*y((p-1)*m + j)
            end do
            write(*,'(f0.1)',advance='no') prev
            write(*,*)
      end do
c     Print the transference of votes's array solution (in percentage)
      do i = 1,m
            do j = 1,m
                  write(*,'(f0.0)',advance='no') 1.d2*x((i-1)*m + j)
                  write(*,'(a)',advance='no') '   '
            end do
            write(*,*)
      end do

      do j = 1,m
            prev = 0.d0
            do i = 1,m
                  prev = prev + x((i-1)*m + j)
            end do
            write(*,*) 'column sum', j, prev
      end do



      stop
      end
