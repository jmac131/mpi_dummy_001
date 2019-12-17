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

     
        call mpi_on(ierr) 
        
        allocate(lsend(nz),lrecv(nz*nprocs),lproc(nprocs))

        
        if(nprocs > nx)then
           nprocsbyz = nprocs/nx
        else
           nprocsbyz = nprocs
        end if

        itime = 1
        lsend(:) = .FALSE.
        loopj: do j=myrank+1,nz*nx,nprocs

          ix = 1+MOD(j-1,nx) ! internal loop
          iz = 1+(j-1)/nx    ! external loop

          !/////////////////////////////////////////
          ! CALCULE MARCHENKO HERE (iz,ix) !
          !/////////////////////////////////////////

          if (ix+nprocs <= nx) cycle

          ! Aqui todos os processos precisam da informação dos outros
          ! se eles terminaram a profundidade
          lsend(iz)=.TRUE.
          CALL mpi_allgather(lsend,nz,MPI_LOGICAL,&
                             lrecv,nz,MPI_LOGICAL,&
                             MPI_COMM_WORLD,ierr)

          do iproc=1,nprocsbyz
             lproc(iproc) = lrecv( iz + (iproc-1)*nz )
          end do

         ! if(myrank==0)then 
         !   do izz=1,nz
         !      print*, izz,lrecv(izz+myrank*nz)
         !   end do
         ! end if
          
          if(myrank==0) print*, myrank,lrecv
                             
          !Catch a process to do the image
          if(ALL(lproc(1:nprocsbyz)) .AND. &
             myrank+1 + (itime-1)*nprocs == iz)then
                            
             !Myrank is complete certain that at depth 
             ! all other process had finished
             ! DO THE IMAGE BY DEPTH HERE!i

             !print*, myrank,iz
                             
             itime = itime   + 1
          end if             
                             
        end do loopj         
                             
        call mpi_off(ierr)   
                             
      end program dummy      
                             
                             
                             
                             
                             
                             
                             
                             
                             
