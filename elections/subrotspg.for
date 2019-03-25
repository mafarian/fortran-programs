    c Subrotina do Método do Gradiente Espectral Projetado
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

    C	Verifica se a função já está satisfatoriamente minimizada
    10	if(dabs(f).le.2.d0) then
        write(*,*) 'atingiu limite de precisão da função'
        return
      end if
    C	Verifica a quantidade de iterações rodadas
      if(it.ge.maxit) then
        write(*,*) 'atingiu limite de iterações'
        write(*,*) it
        return
      end if
    C	Calcula a norma do gradiente projetado
      do i=1,n
        xn(i) = x(i) - g(i)
        xn(i) = dmax1(l(i), dmin1(xn(i),u(i)))
      end do
      gnor = 0.0d0
      do i=1,n
        gnor = dmax1(gnor, dabs(xn(i)-x(i)))
      end do
    C 	Verifica se o gradiente está suficientemente pequeno
      if(gnor.le.eps) then
        write(*,*) 'atingiu limite de precisão do gradiente'
        write(*,*) it
        return
      end if
    C	Acha a projeção do ponto a ser calculado dentro da caixa
    20	do i = 1,n
        xn(i) = x(i) - t*g(i)
        xn(i) = dmax1(l(i), dmin1(xn(i), u(i)))
      end do
    C	Calcula p: produto interno entre o gradiente e d
    C	onde d = distância entre o ponto atual e o projetado
      p = 0.d0
      do i = 1,n
        p = p + g(i)*(xn(i) - x(i))
      end do

    C	Calcula o valor da função no novo ponto (projetado)
      call fun(n, xn, fn)
      av = av + 1
    C	Verifica a quantidade de avaliações feitas na função
      if(av.ge.maxav) then
        write(*,*) 'atingiu limite de avaliações'
        write(*,*) it
        return
      end if

    C	Verifica se a função no novo ponto (projetado) é melhor que no ponto atual
      if(fn.le.(f+alfa*t*p)) then
        call grad(n,xn,gn)
        f = fn

    C 		Calcula o novo tamanho de passo t (passo secante)
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
c    		Verifica se o valor calculado é muito grande ou negativo
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
c    	if it does not satisfy the down condition, another step size will be tested
      t = t/2.d0
      go to 20
      end
