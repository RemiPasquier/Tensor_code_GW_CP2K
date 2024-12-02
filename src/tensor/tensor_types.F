MODULE tensor_types

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
                                               
#include "../base/base_uses.f90"

   IMPLICIT NONE
   PRIVATE
   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_types'

   PUBLIC  :: fmt_type,tensor_type, tensor_iterator_type, tensor_pgrid_type, &
              tensor_distribution_type, fm_tensor_clear, fm_tensor_create_template, &
              fm_tensor_create_matrix, fm_tensor_copy, fm_tensor_contract, &
              fm_tensor_destroy, fm_tensor_copy_matrix_to_tensor, &
              fm_tensor_copy_tensor_to_matrix, fm_tensor_filter

   TYPE fmt_type
      ! @Remi: check whether we need pointers or not
      TYPE(cp_fm_type)           :: fm
      INTEGER                    :: rank
      ! dimension of index_sizes is rank
      INTEGER(KIND=int_8), DIMENSION(:), ALLOCATABLE   :: index_sizes
      ! dimension of maps_to_row_or_col_index is rank; 
      ! 1: maps to row index of fm; 2: maps to col index of fm
      INTEGER, DIMENSION(:), ALLOCATABLE   ::maps_to_row_or_col_index
   END TYPE


   TYPE tensor_type
      ! @Remi: check whether we need pointers or not
      TYPE(dbt_type)                         :: dbt
      TYPE(fmt_type)                         :: fm_t
      INTEGER                                :: t_type
   END TYPE


   TYPE tensor_iterator_type
        TYPE(dbt_iterator_type)      :: iter
   END TYPE

   TYPE tensor_pgrid_type
        TYPE(dbt_pgrid_type)         :: grid
   END TYPE

   TYPE tensor_distribution_type
     TYPE(dbt_distribution_type)     :: dist
   END TYPE



CONTAINS


   SUBROUTINE fm_tensor_clear(fm_tensor)
          TYPE(fmt_type), INTENT(INOUT) :: fm_tensor
   END SUBROUTINE

   SUBROUTINE fm_tensor_create_template(fm_tensor_t,fm_tensor)
          TYPE(fmt_type), INTENT(INOUT) :: fm_tensor_t,fm_tensor
   END SUBROUTINE

   SUBROUTINE fm_tensor_create_matrix(mat_local,fm_tensor)
          TYPE(fmt_type), INTENT(INOUT)    :: fm_tensor
           TYPE(dbcsr_type), INTENT(INOUT) :: mat_local
   END SUBROUTINE

   SUBROUTINE fm_tensor_copy(fm_tensor_in, fm_tensor_out, order, summation, bounds, move_data, unit_nr)
          TYPE(fmt_type), INTENT(INOUT), TARGET         :: fm_tensor_in, fm_tensor_out
          INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL   :: order
          LOGICAL, INTENT(IN), OPTIONAL                 :: summation, move_data
          INTEGER, DIMENSION(:,:),INTENT(IN), OPTIONAL  :: bounds
          INTEGER, INTENT(IN), OPTIONAL                 :: unit_nr
   END SUBROUTINE

   SUBROUTINE fm_tensor_contract(alpha, fm_tensor_1, fm_tensor_2, beta, fm_tensor_3, &
                           contract_1, notcontract_1, &
                           contract_2, notcontract_2, &
                           map_1, map_2, &
                           bounds_1, bounds_2, bounds_3, &
                           optimize_dist, pgrid_opt_1, pgrid_opt_2, pgrid_opt_3, &
                           filter_eps, flop, move_data, retain_sparsity, unit_nr, log_verbose)
          REAL(dp), INTENT(IN)                           :: alpha
          TYPE(fmt_type), INTENT(INOUT), TARGET          :: fm_tensor_1
          TYPE(fmt_type), INTENT(INOUT), TARGET          :: fm_tensor_2
          REAL(dp), INTENT(IN)                           :: beta
          INTEGER, DIMENSION(:), INTENT(IN)              :: contract_1
          INTEGER, DIMENSION(:), INTENT(IN)              :: contract_2
          INTEGER, DIMENSION(:), INTENT(IN)              :: map_1
          INTEGER, DIMENSION(:), INTENT(IN)              :: map_2
          INTEGER, DIMENSION(:), INTENT(IN)              :: notcontract_1
          INTEGER, DIMENSION(:), INTENT(IN)              :: notcontract_2
          TYPE(fmt_type), INTENT(INOUT), TARGET          :: fm_tensor_3
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
   END SUBROUTINE

   SUBROUTINE fm_tensor_destroy(fm_tensor)
          TYPE(fmt_type), INTENT(INOUT) :: fm_tensor
   END SUBROUTINE

   SUBROUTINE fm_tensor_copy_matrix_to_tensor(matrix_in, tensor_out, summation)
     TYPE(dbcsr_type), TARGET, INTENT(IN)               :: matrix_in
     TYPE(fmt_type), INTENT(INOUT)                   :: tensor_out
     LOGICAL, INTENT(IN), OPTIONAL                      :: summation
   END SUBROUTINE  

   
   SUBROUTINE fm_tensor_copy_tensor_to_matrix(tensor_in, matrix_out, summation)
     TYPE(fmt_type), INTENT(INOUT)      :: tensor_in
     TYPE(dbcsr_type), INTENT(INOUT)             :: matrix_out
     LOGICAL, INTENT(IN), OPTIONAL          :: summation
   END SUBROUTINE

   SUBROUTINE fm_tensor_filter(tensor, eps)
      TYPE(fmt_type), INTENT(INOUT)    :: tensor
      REAL(dp), INTENT(IN)                :: eps
   END SUBROUTINE

    
END MODULE tensor_types
