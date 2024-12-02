MODULE tensor_interfaces

   USE message_passing,                 ONLY: mp_cart_type, &
                                              mp_dims_create, &
                                              mp_comm_type
   
   USE cp_fm_types,                     ONLY: cp_fm_type 
   
   USE kinds,                           ONLY: int_8,&
                                              dp
   
   USE tensor_constants,                ONLY: case_dbt, &
                                              case_fmt
   
   USE dbt_api,                         ONLY: dbt_type, &
                                              dbt_clear, &
                                              dbt_create, &
                                              dbt_destroy, &
                                              dbt_copy, &
                                              dbt_contract, &
                                              dbt_copy_matrix_to_tensor, &
                                              dbt_copy_tensor_to_matrix, &
                                              dbt_filter, &
                                              dbt_iterator_next_block, &
                                              dbt_iterator_start, &
                                              dbt_iterator_type, &
                                              dbt_iterator_stop, &
                                              dbt_mp_environ_pgrid, &
                                              dbt_pgrid_create, &
                                              dbt_pgrid_destroy, &
                                              dbt_pgrid_type, &
                                              dbt_iterator_blocks_left, &
                                              dbt_iterator_num_blocks, &
                                              dbt_distribution_type, &
                                              dbt_ndims, &
                                              dbt_write_tensor_info
                                               
   
   USE cp_dbcsr_api,                    ONLY: dbcsr_type, &
                                              dbcsr_iterator_type 
                                               
   USE tensor_types,                    ONLY:  fmt_type,tensor_type, &
                                               tensor_iterator_type, &
                                               tensor_pgrid_type, &
                                               tensor_distribution_type, &
                                               fm_tensor_clear, &
                                               fm_tensor_create_template, &
                                               fm_tensor_create_matrix, &
                                               fm_tensor_copy, &
                                               fm_tensor_contract, &
                                               fm_tensor_destroy, &
                                               fm_tensor_copy_matrix_to_tensor, &
                                               fm_tensor_copy_tensor_to_matrix, &
                                               fm_tensor_filter 


#include "../base/base_uses.f90"

   IMPLICIT NONE
   PRIVATE
   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_interfaces'

   PUBLIC  :: tensor_type, tensor_clear, tensor_create, tensor_destroy, &
              tensor_copy, tensor_contract, tensor_copy_matrix_to_tensor, &
              tensor_copy_tensor_to_matrix, tensor_filter, tensor_iterator_next_block, &
              tensor_iterator_start, tensor_iterator_type, tensor_iterator_stop, &
              tensor_mp_environ_pgrid, tensor_pgrid_create, tensor_pgrid_destroy, &
              tensor_pgrid_type, tensor_iterator_blocks_left, tensor_distribution_type

   INTERFACE tensor_create
          MODULE PROCEDURE tensor_create_new
          MODULE PROCEDURE tensor_create_template
          MODULE PROCEDURE tensor_create_matrix
   END INTERFACE

CONTAINS

   SUBROUTINE tensor_clear(tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
          SELECT CASE(tensor%t_type)
          CASE (case_dbt)
               CALL dbt_clear(tensor%dbt)
          CASE (case_fmt)
          CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
          END SELECT
   END SUBROUTINE


   SUBROUTINE tensor_create_template(tensor_t,tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor_t,tensor
          SELECT CASE(tensor_t%t_type)
          CASE (case_dbt)
               CALL dbt_create(tensor_t%dbt,tensor%dbt)
          CASE (case_fmt)
          CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized") 
          END SELECT
          tensor%t_type=tensor_t%t_type
   END SUBROUTINE

   SUBROUTINE tensor_create_matrix(mat_local,tensor,mode)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
          TYPE(dbcsr_type), INTENT(INOUT)  :: mat_local
          INTEGER                          :: mode
          SELECT CASE(mode)
          CASE (case_dbt)
               CALL dbt_create(mat_local,tensor%dbt)
          CASE(case_fmt)
          CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
          END SELECT
          tensor%t_type=mode
   END SUBROUTINE

   SUBROUTINE tensor_create_new(tensor, name, dist, map1_2d, map2_2d,mode)
            TYPE(tensor_type), INTENT(OUT)                    :: tensor
            CHARACTER(len=*), INTENT(IN)                      :: name
            TYPE(tensor_distribution_type), INTENT(INOUT)     :: dist
            INTEGER, DIMENSION(:), INTENT(IN)                 :: map1_2d
            INTEGER, DIMENSION(:), INTENT(IN)                 :: map2_2d
            INTEGER                                           :: mode
            SELECT CASE(mode)
            CASE(case_dbt)
                CALL dbt_create(tensor%dbt, name, dist%dist, map1_2d, map2_2d)
            CASE(case_fmt)
            CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
            END SELECT
            tensor%t_type=mode
   END SUBROUTINE


   SUBROUTINE tensor_copy(tensor_in, tensor_out, order, summation, bounds, move_data, unit_nr)
          TYPE(tensor_type), INTENT(INOUT),target              :: tensor_in, tensor_out
          INTEGER, DIMENSION(dbt_ndims(tensor_in%dbt)), INTENT(IN), OPTIONAL   :: order
          LOGICAL, INTENT(IN), OPTIONAL                 :: summation, move_data
          INTEGER, DIMENSION(2,dbt_ndims(tensor_in%dbt)),INTENT(IN), OPTIONAL  :: bounds
          INTEGER, INTENT(IN), OPTIONAL                 :: unit_nr
          SELECT CASE(tensor_in%t_type)
          CASE (case_dbt)
               CALL dbt_copy(tensor_in%dbt, tensor_out%dbt, order, summation, bounds, move_data, unit_nr) 
          CASE (case_fmt)
          CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
          END SELECT
          tensor_out%t_type=tensor_in%t_type
   END SUBROUTINE

   SUBROUTINE tensor_destroy(tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
          SELECT CASE(tensor%t_type)
          CASE (case_dbt)
               CALL dbt_destroy(tensor%dbt)
          CASE (case_fmt)
          CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
          END SELECT
   END SUBROUTINE 
   
   SUBROUTINE tensor_contract(alpha, tensor_1, tensor_2, beta, tensor_3, &
                           contract_1, notcontract_1, &
                           contract_2, notcontract_2, &
                           map_1, map_2, &
                           bounds_1, bounds_2, bounds_3, &
                           optimize_dist, pgrid_opt_1, pgrid_opt_2, pgrid_opt_3, &
                           filter_eps, flop, move_data, retain_sparsity, unit_nr, log_verbose)
          REAL(dp), INTENT(IN)                           :: alpha
          TYPE(tensor_type), INTENT(INOUT), TARGET          :: tensor_1
          TYPE(tensor_type), INTENT(INOUT), TARGET          :: tensor_2
          REAL(dp), INTENT(IN)                           :: beta
          INTEGER, DIMENSION(:), INTENT(IN)              :: contract_1
          INTEGER, DIMENSION(:), INTENT(IN)              :: contract_2
          INTEGER, DIMENSION(:), INTENT(IN)              :: map_1
          INTEGER, DIMENSION(:), INTENT(IN)              :: map_2
          INTEGER, DIMENSION(:), INTENT(IN)              :: notcontract_1
          INTEGER, DIMENSION(:), INTENT(IN)              :: notcontract_2
          TYPE(tensor_type), INTENT(INOUT), TARGET          :: tensor_3
          INTEGER, DIMENSION(2, SIZE(contract_1)), &
           INTENT(IN), OPTIONAL                        :: bounds_1
          INTEGER, DIMENSION(2, SIZE(notcontract_1)), &
           INTENT(IN), OPTIONAL                        :: bounds_2
          INTEGER, DIMENSION(2, SIZE(notcontract_2)), &
           INTENT(IN), OPTIONAL                        :: bounds_3
          LOGICAL, INTENT(IN), OPTIONAL                  :: optimize_dist
          TYPE(dbt_pgrid_type), INTENT(OUT), &
           POINTER, OPTIONAL                           :: pgrid_opt_1
          TYPE(dbt_pgrid_type), INTENT(OUT), &
           POINTER, OPTIONAL                           :: pgrid_opt_2
          TYPE(dbt_pgrid_type), INTENT(OUT), &
           POINTER, OPTIONAL                           :: pgrid_opt_3
          REAL(KIND=dp), INTENT(IN), OPTIONAL            :: filter_eps
          INTEGER(KIND=int_8), INTENT(OUT), OPTIONAL     :: flop
          LOGICAL, INTENT(IN), OPTIONAL                  :: move_data
          LOGICAL, INTENT(IN), OPTIONAL                  :: retain_sparsity
          INTEGER, OPTIONAL, INTENT(IN)                  :: unit_nr
          LOGICAL, INTENT(IN), OPTIONAL                  :: log_verbose
          SELECT CASE(tensor_1%t_type)
          CASE (case_dbt)
               CALL  dbt_contract(alpha, tensor_1%dbt, tensor_2%dbt, beta, tensor_3%dbt, &
                           contract_1, notcontract_1, &
                           contract_2, notcontract_2, &
                           map_1, map_2, &
                           bounds_1, bounds_2, bounds_3, &
                           optimize_dist, pgrid_opt_1, pgrid_opt_2, pgrid_opt_3, &
                           filter_eps, flop, move_data, retain_sparsity, unit_nr, log_verbose)
          CASE (case_fmt)
          CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
          END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_copy_matrix_to_tensor(matrix_in, tensor_out, summation, mode)
      TYPE(dbcsr_type), TARGET, INTENT(IN)               :: matrix_in
      TYPE(tensor_type), INTENT(INOUT)                   :: tensor_out
      LOGICAL, INTENT(IN), OPTIONAL                      :: summation
      INTEGER                                            :: mode
      SELECT CASE (mode)
      CASE (case_dbt)
           CALL dbt_copy_matrix_to_tensor(matrix_in, tensor_out%dbt, summation)
      CASE (case_fmt)
      CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")    
      END SELECT
      tensor_out%t_type=mode 

   END SUBROUTINE

    SUBROUTINE tensor_copy_tensor_to_matrix(tensor_in, matrix_out, summation)
      TYPE(tensor_type), INTENT(INOUT)      :: tensor_in
      TYPE(dbcsr_type), INTENT(INOUT)             :: matrix_out
      LOGICAL, INTENT(IN), OPTIONAL          :: summation

      SELECT CASE (tensor_in%t_type)
      CASE (case_dbt)
           CALL dbt_copy_tensor_to_matrix(tensor_in%dbt, matrix_out, summation)
      CASE (case_fmt)
      CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
      END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_filter(tensor, eps)
      TYPE(tensor_type), INTENT(INOUT)    :: tensor
      REAL(dp), INTENT(IN)                :: eps
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CALL dbt_filter(tensor%dbt, eps)
      CASE (case_fmt)
      CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
      END SELECT
   END SUBROUTINE

 SUBROUTINE tensor_iterator_next_block(tensor, iterator, ind_nd, blk_size, blk_offset)
      TYPE(tensor_type), INTENT(IN)                 :: tensor  
      TYPE(tensor_iterator_type), INTENT(INOUT)     :: iterator
      INTEGER, DIMENSION(:), INTENT(OUT)            :: ind_nd
      INTEGER, DIMENSION(:), INTENT(OUT), OPTIONAL  :: blk_size, blk_offset
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CALL dbt_iterator_next_block(iterator%iter, ind_nd, blk_size, blk_offset)
      CASE (case_fmt)
      CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
      END SELECT
   END SUBROUTINE

   
   FUNCTION tensor_iterator_blocks_left(tensor,iterator)
      TYPE(tensor_type), INTENT(IN)           :: tensor
      TYPE(tensor_iterator_type)              :: iterator
      LOGICAL                                 :: tensor_iterator_blocks_left
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           tensor_iterator_blocks_left=dbt_iterator_blocks_left(iterator%iter)
      CASE (case_fmt)
      CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
      END SELECT
   END FUNCTION    

   FUNCTION tensor_iterator_num_blocks(tensor,iterator)
      TYPE(tensor_type), INTENT(IN)           :: tensor
      TYPE(tensor_iterator_type)                 :: iterator
      INTEGER                                    :: tensor_iterator_num_blocks
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           tensor_iterator_num_blocks=dbt_iterator_num_blocks(iterator%iter)
      CASE (case_fmt)
      CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
      END SELECT
   END FUNCTION    

   SUBROUTINE tensor_iterator_start(iterator, tensor)
      TYPE(tensor_iterator_type), INTENT(OUT)           :: iterator
      TYPE(tensor_type), INTENT(IN)                  :: tensor
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CALL dbt_iterator_start(iterator%iter,tensor%dbt)
      CASE (case_fmt)
      CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
      END SELECT 
   END SUBROUTINE

   SUBROUTINE tensor_iterator_stop(tensor, iterator)
      TYPE(tensor_iterator_type), INTENT(INOUT) :: iterator
      TYPE(tensor_type), INTENT(IN)                  :: tensor
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CALL dbt_iterator_stop(iterator%iter)
      CASE (case_fmt)
      CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
      END SELECT 
   END SUBROUTINE


   SUBROUTINE tensor_mp_environ_pgrid(tensor,pgrid, dims, task_coor)
            TYPE(tensor_type), INTENT(IN)       :: tensor
            TYPE(tensor_pgrid_type), INTENT(IN) :: pgrid
            INTEGER, DIMENSION(:), INTENT(OUT) :: dims
            INTEGER, DIMENSION(:), INTENT(OUT) :: task_coor
            SELECT CASE (tensor%t_type)
            CASE (case_dbt)
                 CALL dbt_mp_environ_pgrid(pgrid%grid, dims, task_coor)
            CASE (case_fmt)
            CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
            END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_pgrid_create(tensor, mp_comm, dims, pgrid, tensor_dims,mode)
            TYPE(tensor_type), INTENT(IN)   :: tensor
            CLASS(mp_comm_type), INTENT(IN) :: mp_comm
            INTEGER, DIMENSION(:), INTENT(INOUT) :: dims
            TYPE(tensor_pgrid_type), INTENT(OUT) :: pgrid
            INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL :: tensor_dims
            INTEGER                                     :: mode
            SELECT CASE (mode)
            CASE (case_dbt)
                 CALL dbt_pgrid_create(mp_comm, dims, pgrid%grid, tensor_dims)
            CASE (case_fmt)
            CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
            END SELECT

   END SUBROUTINE
 
   SUBROUTINE tensor_pgrid_destroy(tensor,pgrid, keep_comm)
            TYPE(tensor_type),INTENT(IN)            :: tensor 
            TYPE(tensor_pgrid_type), INTENT(INOUT)  :: pgrid
            LOGICAL, INTENT(IN), OPTIONAL           :: keep_comm
            SELECT CASE (tensor%t_type)
            CASE (case_dbt)
                 CALL dbt_pgrid_destroy(pgrid%grid,keep_comm)
            CASE (case_fmt)
            CASE DEFAULT
               CPABORT("ERROR: GW tensor type not recognized")
            END SELECT

   END SUBROUTINE


END MODULE tensor_interfaces
