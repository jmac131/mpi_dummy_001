      MODULE mpi_mod
       USE mpi
       IMPLICIT NONE
       INTEGER(4),PARAMETER :: admrank = 0
       INTEGER(4)           :: myrank
       INTEGER(4)           :: nprocs
       INTEGER(4)           :: ierr

       CONTAINS


        SUBROUTINE mpi_on(ierr)
         IMPLICIT NONE
         INTEGER(4),INTENT(OUT)::ierr
    
         CALL mpi_init(ierr)
         IF(ierr/=mpi_success) PRINT*, 'ERROR: mpi_init'
    
         CALL mpi_comm_rank(mpi_comm_world,myrank,ierr)
         IF(ierr/=mpi_success) PRINT*, 'ERROR: mpi_comm_rank'
    
         CALL mpi_comm_size(mpi_comm_world,nprocs,ierr)
         IF(ierr/=mpi_success) PRINT*, 'ERROR: mpi_comm_size'
    
        END SUBROUTINE mpi_on
    



        
        SUBROUTINE mpi_off(ierr)
          IMPLICIT NONE
          INTEGER(4),INTENT(OUT)::ierr
    
          CALL mpi_barrier(mpi_comm_world,ierr)
          if(ierr/=mpi_success) PRINT*, 'ERROR: mpi_barrier'
    
          CALL mpi_finalize(ierr)
          IF(ierr/=mpi_success) PRINT*, 'ERROR: mpi_finalize'      
    
        END SUBROUTINE mpi_off



      END MODULE mpi_mod













