c     subroutine of Spectral Projected Gradient method

      subroutine spg(n,x,l,u,alfa,g,xn,gn,eps,maxit,maxav)
      implicit none
      integer n, m, it, av, maxit, maxav, i
      double precision x(n), xn(n), g(n), gn(n), l(n), u(n), f
      double precision fn, num, den, eps, t, p, alfa, gnor
      it = 0
      av = 1
      t = 1.d0
      
      call fun(n, x, f)
      call grad(n, x, g)
      
c     checks if the function is already enough minimized
10    if(dabs(f).le.2.d0) then
        write(*,*) 'reached the function accuracy limit'
        return
      end if
      
c    	checks the number of iterations
      if(it.ge.maxit) then
        write(*,*) 'reached the iterations limit'
        return
      end if
      
c     computes the projected gradient modulus
      do i=1,n
        xn(i) = x(i) - g(i)
        xn(i) = dmax1(l(i), dmin1(xn(i),u(i)))
      end do
      
      gnor = 0.0d0
      
      do i=1,n
        gnor = dmax1(gnor, dabs(xn(i)-x(i)))
      end do
      
c     verifies if the gradient is small enough
      if(gnor.le.eps) then
        write(*,*) 'reached the gradient accuracy limit'
        return
      end if
   
c     computes the projection of the point inside the "box"
20    do i = 1,n
        xn(i) = x(i) - t*g(i)
        xn(i) = dmax1(l(i), dmin1(xn(i), u(i)))
      end do
      
      
c    	determines p: intern product between gradient and d
c    	where d = distance between the current and the projected point
      p = 0.d0
      do i = 1,n
        p = p + g(i)*(xn(i) - x(i))
      end do

c    	computes the obj fun value at the new point (projected one)
      call fun(n, xn, fn)
      av = av + 1

c     verifies the number of function evaluations
      if(av.ge.maxav) then
        write(*,*) 'reached the evaluations limit'
        return
      end if

c    	checks if the function value at the new point (projected) is better than the value at the current one
      if(fn.le.(f+alfa*t*p)) then
        call grad(n,xn,gn)
        f = fn

c     	computes the new step size t (secant step)
        num = 0.d0
        den = 0.d0
        
        do i = 1, n
          num = num + (xn(i) - x(i))**2
          den = den + (xn(i) - x(i))*(gn(i) - g(i))
        end do
        
        if(den.eq.0.d0) then
          t = 1.d0
        else
          t = num/den
        end if

c    		verifies if the computed value is too larg or negative
        if(t.le.0.d0) t = 1.d0
        if(t.gt.1.d20) t = 1.d0

c    		uptade the current point
        do i = 1,n
          x(i) = xn(i)
        end do
        
        do i = 1,n
          g(i) = gn(i)
        end do

        it = it + 1
        go to 10
      end if

c     if it does not satisfy the down condition, another step size will be tested
      t = t/2.d0
      go to 20
      end
