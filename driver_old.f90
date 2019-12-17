      program driver
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
        integer(4)::cont,i,j,iz,ix,ierror,iz0,itime
        real::tic,tac,r,ini,fin
        integer::nz=10
        integer::nx=10
        !mpi
        integer::tag,datalength,dest,iproc,istatus(MPI_STATUS_SIZE),mpierr,origin,nprocsbyz
        logical,allocatable::lrecv(:)
        logical,allocatable::lsend(:)
        logical,allocatable::lproc(:)
     
        call mpi_on(ierr) 
        

       !   ! Test run
       !   call cpu_time(tic)
       !   cont  = 0
       !   do j=myrank+1,nx,2
       !      do i=myrank+1,nz,2
       !         cont = cont + 1
       !      end do
       !   end do
       !   call cpu_time(tac)
       !   !print*, "T01 myrank",myrank," tempo: ", tac-tic,"s"
 
          allocate(lsend(nz),lrecv(nz*nprocs),lproc(nprocs))

          !If nprocs > nx make nprocs = nx
          If(nprocs > nx)then
             nprocsbyz = nprocs/nx
          else
             nprocsbyz = nprocs
          end if

          !rbuf, sbuf

        !  call cpu_time(tic)
        !  cont  = 0
        !  lsend(:)=.FALSE.
        !  loopj: do j=myrank+1,nz*nx,nprocs
        !        ix = 1+MOD(j-1,nx) ! external loop
        !        iz = 1+(j-1)/nx    ! internal loop

        !        !print*, iz,ix
        !       ! if(myrank==1)then
        !       !   do i=1,1000000000
        !       !     cont = cont + 1
        !       !   end do
        !       !   do i=1,1000000000
        !       !     cont = cont + 1
        !       !   end do
        !       ! end if
        !        if (ix+nprocs <= nx) cycle
        !        lsend(iz)=.TRUE.
        !        print*, myrank
        !        !print*, myrank,iz,lsend(iz)
        !        call mpi_allgather(lsend,nz,MPI_LOGICAL,&
        !                           lrecv,nz,MPI_LOGICAL,&
        !                           MPI_COMM_WORLD,ierror)
        !        do iproc=1,nprocsbyz
        !           lproc(iproc) = lrecv( 1 + (iproc-1)*nz )
        !        end do
        !        !if(myrank==0) print*,iz,lproc(1:nprocsbyz)
        !        !if(myrank==0) print*,iz,"---",lsend,"---",lproc
        !  end do loopj
        !  call cpu_time(tac)
          !print*, "T02 myrank",myrank," tempo: ", tac-tic,"s"



          !//////// second version ////////


          itime = 0
          loopj: do j=myrank+1,nz*nx,nprocs

            ix = 1+MOD(j-1,nx) ! external loop
            iz = 1+(j-1)/nx    ! internal loop

            ! CALCULE MARCHENKO HERE (iz,ix) !

            if (ix+nprocs <= nx) cycle
            ! I have finished my depth. 
            ! Inform the other that I finished. 

            CALL MPI_IScatter(lsend,nz,MPI_LOGICAL, &
                             lrecv,nz,MPI_LOGICAL, &
                             myrank, MPI_COMM_WORLD)

            !Catch a process to do the image
            if(myrank+1 + (itime-1)*nprocs == iz)then

               !Myrank is complete certain that at depth 
               ! all other process had finished

               ! DO THE IMAGE BY DEPTH HERE!

               itime = itime + 1
            end if

          end do loopj



          CALL MPI_BARRIER(MPI_COMM_WORLD,ierr)


      !    if ( myrank == admrank ) then
      !       tag=7;
      !       datalength=1
      !       do origin=1,nprocs-1
      !          call MPI_Recv(mpierr,datalength,MPI_INTEGER, &
      !               origin,tag,MPI_COMM_WORLD,istatus,ierr)
      !          print*, origin,mpierr
      !       end do
      !    else
      !       dest = admrank
      !       tag  = 7
      !       datalength=1
      !       call RANDOM_NUMBER(r)
      !       mpierr = NINT(r*100)
      !       call MPI_Send(mpierr,datalength,MPI_INTEGER, &
      !            dest,tag,MPI_COMM_WORLD,ierr)
      !    end if

        call mpi_off(ierr) 
      
      end program driver
      

