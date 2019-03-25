c     subroutine for the objective function
c     X(ij) = x((i-1)*m + j)
c     Yk(t) = y((t-1)*m + k)
      
      subroutine fun(n,x,f)
      implicit none
      integer n, m, i, j, k, p
      double precision x(n), y(600), f, z, d, gama, sigma, beta, teta
      common /pesq/ p,m,y,gama,sigma,beta,teta
      f = 0.d0
      z = 0.d0
c     this part computes, for both data bases, ||Y(k+1) - X*Y(k)||
      do k = 1,p-1
            do i = 1,m
                  d = y(k*m + i)
                  do j = 1,m
                        d = d - x((i-1)*m + j)*y((k-1)*m + j)
                  end do
                  z = z + d**2
            end do
      end do
      f = f + z

c     this part computes, for each column, ||(sum of X rows) - 1||
      do j = 1,m
            z = 0.d0
            do i = 1,m
                  z = z + x((i-1)*m + j)
            end do
            f = f + gama*(z-1)**2
      end do

c     this part punishes the votes's transfer to undecided category
      do j = 1,m
            f = f + sigma*x((m-1)*m + j)
      end do

      do i = 1,m
            do j = 1,m
                  if(i.ne.j) then
                        f = f + teta*dabs(x((i-1)*m + j))**beta
                  end if
            end do
      end do

      return
      end

c     subroutine for the objective function's gradient
c     X(ij) = x((i-1)*m + j)
c     Yk(t) = y((t-1)*m + k)
      subroutine grad(n,x,g)
      implicit none
      integer n, i, j, k, m, l, t, p
      double precision x(n), d, z, y(600), g(n), gama, beta, sigma,teta
      common /pesq/ p,m,y,gama,sigma,beta,teta
      d = 0.d0
      z = 0.d0
c     computes the gradient regarding to each Xij
      do i = 1,m
            do j = 1,m
                  l = (i-1)*m + j
                  g(l) = 0

c                 for each two researches, calculates the gradient of ||Y(k+1) - X*Y(k)||
                  do k = 1,p-1
                        d = y(k*m + i)
                        do t = 1,m
                              d = d - x((i-1)*m + t)*y((k-1)*m + t)
                        end do
                        z = z - 2.d0*d*y((k-1)*m + j)
                        g(l) = g(l) + z
                  end do

                  z = 0.d0
c                 computes the gradient of ||(sum of X rows) - 1||
                  do t = 1,m
                        z = z + x((t-1)*m + j)
                  end do
                  g(l) = g(l) + 2.d0*gama*(z-1)

c                 computes the gradient of the votes penalty
                  if(i.eq.m) then
                        g(l) = g(l) + sigma
                  end if

                  if((i.ne.j).AND.(x(l).ne.0)) then
                        d = x(l)*(dabs(x(l))**(beta-2.d0))
                        g(l) = g(l) + teta*beta*d
                  end if
            end do
      end do
      return
      end
