C - gfortran -o boredom boredom.for -fdec-structure
C - programa para Boredom Problem
	implicit none   
      external caminhos
	integer i, j, n, auxlucro, auxv, maxi, lucropasso3, m, mini
	parameter(m=1000)
	integer lucromax, percurso(m)
	structure /seq/
	  	integer frequencia(m)
	  	integer lucro(m)
		integer sucessor1(m)
		integer sucessor2(m)
	end structure 
	record /seq/ sequencia
	
	common /cam/ sequencia, percurso

	write (*,*)	'Tamanho da sequencia:'
      read (*,*) n
	
	
	do i = 1, n
		sequencia.frequencia(i)=0 
		sequencia.lucro(i)=0 
		sequencia.sucessor1(i)=0
		sequencia.sucessor2(i)=0 	
	end do	

	write(*,*) 'Digite a sequencia:'

	maxi=0
	mini=100000
	 
	do i = 1, n
		read(*,*) auxv
			sequencia.frequencia(auxv) = sequencia.frequencia(auxv)+1
			if (auxv.gt.maxi)	maxi=auxv
			if (auxv.lt.mini) mini=auxv
	end do	
	
	do i = mini, maxi
		sequencia.lucro(i) = i*sequencia.frequencia(i)
	end do
	
	do i = maxi, mini, -1
		if(i+2.le.maxi) then
			auxlucro = sequencia.lucro(i)
			sequencia.lucro(i) = sequencia.lucro(i) + sequencia.lucro(i+2)
			sequencia.sucessor1(i) = i + 2
			sequencia.sucessor2(i) = i + 2
		end if
		if(i+3.le.maxi) then
			lucropasso3 = auxlucro + sequencia.lucro(i+3)
			if(lucropasso3.gt.sequencia.lucro(i))  then
				sequencia.lucro(i) = lucropasso3
				sequencia.sucessor1(i) = i + 3
				sequencia.sucessor2(i) = i + 3
			else if(lucropasso3.eq.sequencia.lucro(i))  then
				sequencia.sucessor2(i) = i + 3
			end if
		end if
	end do
	
	write(*,*)
	write(*,*) 'soluções ótimas por estágio:'
	call estagios(sequencia,maxi,mini)
	
c	write(*,*) 'SOLUÇÃO DESSA COISA:'	

	if(sequencia.lucro(mini).eq.sequencia.lucro(mini+1)) then
		lucromax = sequencia.lucro(mini)
		call caminhos(mini,1,caminhos)
		write(*,*)
		call caminhos(mini+1,1,caminhos)
	else if(sequencia.lucro(mini).gt.sequencia.lucro(mini+1))  then
		lucromax = sequencia.lucro(mini)
		call caminhos(mini,1,caminhos)
	else
		lucromax = sequencia.lucro(mini+1)
		call caminhos(mini+1,1,caminhos)
	end if

	write(*,*)
	write (*,'(a,i0)',advance='no') 'pontuação máxima: ', lucromax
	write(*,*)

	do i = mini, maxi
		write (*,*)	i, sequencia.frequencia(i), sequencia.lucro(i),
     *sequencia.sucessor1(i), sequencia.sucessor2(i)
	end do
		
	stop
	end
	
C	subrotina pra mostrar as soluções ótimas em CADA estágio      
      subroutine estagios(sequencia,maxi,mini)
     	
     	implicit none
      integer percurso(1000), j, maxi, mini
      external caminhos
      structure /seq/
	  	integer frequencia(1000)
	  	integer lucro(1000)
		integer sucessor1(1000)
		integer sucessor2(1000)
	end structure 
	record /seq/ sequencia
	
c	write(*,'(a,i0)',advance='no') 'A partir de ', maxi
	write(*,*)
	write(*,'(i0)',advance='no') maxi
	write(*,*)
	
	if(sequencia.frequencia(maxi-1).ne.0) then
c		write(*,'(a,i0)',advance='no') 'A partir de ', maxi-1
c		write(*,*)
		write(*,'(i0)',advance='no') maxi-1
		write(*,*)
	end if
	
	do j = maxi-2,mini,-1
		if(sequencia.frequencia(j).ne.0) then
c			write(*,'(a,i0)',advance='no') 'A partir de ', j
c			write(*,*)	
			call caminhos(j,1,caminhos)
		end if
	end do
	
	return
      end
      
C	subrotina que mostra o caminho de escolhas ótimo
	subroutine caminhos(pos,k,recursao)
     	
     	implicit none
      integer pos, percurso(1000), k, i
      external recursao
      structure /seq/
	  	integer frequencia(1000)
	  	integer lucro(1000)
		integer sucessor1(1000)
		integer sucessor2(1000)
	end structure 
	record /seq/ sequencia
	common /cam/ sequencia, percurso
	
	if(pos.eq.0) then
		do i = 1,k-1
			if(percurso(i).ne.0) then
				write(*,'(i0,a)',advance='no') percurso(i), ' '
			end if
		end do
		write(*,*)
		return
	end if
	
	if(sequencia.frequencia(pos).ne.0) then
		percurso(k) = pos
	else
		percurso(k) = 0
	end if
	
	call recursao(sequencia.sucessor1(pos),k+1,recursao)	

	if(sequencia.sucessor2(pos).ne.sequencia.sucessor1(pos)) then
		call recursao(sequencia.sucessor2(pos),k+1,recursao)
	end if
	
	return
      end
