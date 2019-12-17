      program dummy
        !===================================
        ! Driver Hello World for MPICH2
        !
        ! How to compile in gfortran
        ! mpif90 mpi_mod.f90 driver.f90 -o t
        !
        ! How to run 
        ! mpirun -np 4 ./t
        !
        !===================================
        use mpi_mod
        implicit none
        integer(4)::cont,i,j,iz,ix,iz0,itime,izz
        real::tic,tac,r,ini,fin
        integer::nz=3
        integer::nx=2
        !mpi
        integer::tag,datalength,dest,iproc,istatus(MPI_STATUS_SIZE),mpierr,origin,nprocsbyz
        logical,allocatable::lrecv(:)
        logical,allocatable::lsend(:)
        logical,allocatable::lproc(:)

        ! Initialize the MPI  (Modulo mpi_mod.f90)
        call mpi_on(ierr) 
       
        ! Allocate the send and receiver variables 
        allocate(lsend(nz),lrecv(nz*nprocs),lproc(nprocs))

        ! Take cara about the amount of process 
        if(nprocs > nx)then
           nprocsbyz = nprocs/nx
        else
           nprocsbyz = nprocs
        end if

        itime = 1  ! numero de vez que o processo fez aquela profundidade
        lsend(:) = .FALSE.  ! Inicialmente todos não fizeram nenhuma profundidade

        loopj: do j=myrank+1,nz*nx,nprocs ! Vetorização da matriz e serparação por processos independentemente


          ! Indexes da matrix. Serão entradas para a subroutina do
          ! marchenko
          ix = 1+MOD(j-1,nx) ! internal loop
          iz = 1+(j-1)/nx    ! external loop

          !/////////////////////////////////////////
          ! CALCULE MARCHENKO HERE (iz,ix) !
          !/////////////////////////////////////////

          ! se o processo n ainda não terminou aquela profundidade
          ! mande continuar para outro ponto em x
          if (ix+nprocs <= nx) cycle

          ! Como meu processo terminou essa profundidade flag como
          ! verdadeiro para essa profundidade
          lsend(iz)=.TRUE.
          ! Faça o meu processo descobrir se os outros terminaram
          CALL mpi_allgather(lsend,nz,MPI_LOGICAL,&
                             lrecv,nz,MPI_LOGICAL,&
                             MPI_COMM_WORLD,ierr)

          ! Lista os falsos e verdadeiros dessa profundidade em cada
          ! processo
          do iproc=1,nprocsbyz
             lproc(iproc) = lrecv( iz + (iproc-1)*nz )
          end do

          
          !if(myrank==0) print*, myrank,lrecv
                             
          !Catch a process to do the image
          ! Se todos os lprocs naquela profundidade é verdadeiro
          ! e se essa profundidade obdece ess regra com myrank
          ! diga a esse myrank para fazer a imagem
          if(ALL(lproc(1:nprocsbyz)) .AND. &
             myrank+1 + (itime-1)*nprocs == iz)then
                            
             ! DO THE IMAGE BY DEPTH HERE!!!
              !!Suboroutina da image(iz)

             !print*, myrank,iz
                            
             ! Meu processo fez a itime profunidade 
             itime = itime   + 1
          end if             
                             
        end do loopj         
                            
        ! Close the mpi 
        call mpi_off(ierr)   
                             
      end program dummy      
                             
                             
                             
                             
                             
                             
                             
                             
                             
