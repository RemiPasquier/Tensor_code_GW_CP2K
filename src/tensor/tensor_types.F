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
                                              dbt_iterator_num_blocks
                                               
   
   USE cp_dbcsr_api,                    ONLY: dbcsr_type, &
                                              dbcsr_iterator_type 
                                               
   USE qs_environment_types,            ONLY: qs_environment_type
#include "../base/base_uses.f90"

   IMPLICIT NONE
   PRIVATE
   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_types'

   PUBLIC  :: tensor_type, tensor_clear, tensor_create, tensor_destroy, &
              tensor_copy, tensor_contract, tensor_copy_matrix_to_tensor, &
              tensor_copy_tensor_to_matrix, tensor_filter, tensor_iterator_next_block, &
              tensor_iterator_start, tensor_iterator_type, tensor_iterator_stop, &
              tensor_mp_environ_pgrid, tensor_pgrid_create, tensor_pgrid_destroy, &
              tensor_pgrid_type, tensor_iterator_blocks_left


   TYPE tensor_type
      ! @Remi: check whether we need pointers or not
      TYPE(dbt_type), POINTER                :: dbt => NULL()
      TYPE(fmt_type), POINTER                :: fm_t => NULL()
      INTEGER                                :: t_type
   END TYPE

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

   TYPE tensor_iterator_type
        TYPE(dbt_iterator_type), POINTER :: iter
   END TYPE

   TYPE tensor_pgrid_type
        TYPE(dbt_pgrid_type), POINTER :: grid
   END TYPE


   INTERFACE tensor_create
          MODULE PROCEDURE tensor_create_template
          MODULE PROCEDURE tensor_create_matrix
   END INTERFACE

CONTAINS

   SUBROUTINE tensor_clear(tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
          SELECT CASE(tensor%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor%dbt))
               CALL dbt_clear(tensor%dbt)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor%fm_t))
               CALL tensor_clear_low(tensor%fm_t)
          END SELECT
   END SUBROUTINE


!@Remi : there may be a more clever way of doing this using interfaces, which would avoid uselessly
!        reduplicating the parameters between the dbt and scalapack implementations, although I can
!        see it causing issues in some cases as the procedures might have exactly the same parameters.
   SUBROUTINE tensor_create_template(tensor_t,tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor_t,tensor
          CPASSERT(tensor_t%t_type==tensor%t_type)
          SELECT CASE(tensor%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor_t%dbt))
               CPASSERT(ASSOCIATED(tensor%dbt))
               CALL dbt_create(tensor_t%dbt,tensor%dbt)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor_t%fm_t))
               CPASSERT(ASSOCIATED(tensor%fm_t))
               CALL tensor_create_low_template(tensor_t%fm_t,tensor%fm_t)
          END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_create_matrix(mat_local,tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
          TYPE(dbcsr_type), INTENT(INOUT)  :: mat_local
          SELECT CASE(tensor%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor%dbt))
               CALL dbt_create(mat_local,tensor%dbt)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor%fm_t))
               CALL tensor_create_low_matrix(mat_local,tensor%fm_t)
          END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_copy(tensor_in, tensor_out, order, summation, bounds, move_data, unit_nr)
          TYPE(tensor_type), INTENT(INOUT), TARGET      :: tensor_in, tensor_out
          INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL   :: order
          LOGICAL, INTENT(IN), OPTIONAL                 :: summation, move_data
          INTEGER, DIMENSION(:,:),INTENT(IN), OPTIONAL  :: bounds
          INTEGER, INTENT(IN), OPTIONAL                 :: unit_nr
          CPASSERT(tensor_in%t_type==tensor_out%t_type)
          SELECT CASE(tensor_in%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor_in%dbt))
               CPASSERT(ASSOCIATED(tensor_out%dbt))
               CALL dbt_copy(tensor_in%dbt, tensor_out%dbt, order, summation, bounds, move_data, unit_nr)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor_in%fm_t))
               CPASSERT(ASSOCIATED(tensor_out%fm_t))
               CALL tensor_copy_low(tensor_in%fm_t, tensor_out%fm_t, order)
          END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_destroy(tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
          SELECT CASE(tensor%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor%dbt))
               CALL dbt_destroy(tensor%dbt)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor%fm_t))
               CALL tensor_destroy_low(tensor%fm_t)
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
          CPASSERT(tensor_1%t_type==tensor_2%t_type)
          CPASSERT(tensor_2%t_type==tensor_3%t_type)
          SELECT CASE(tensor_1%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor_1%dbt))
               CPASSERT(ASSOCIATED(tensor_2%dbt))
               CPASSERT(ASSOCIATED(tensor_3%dbt))
               CALL  dbt_contract(alpha, tensor_1%dbt, tensor_2%dbt, beta, tensor_3%dbt, &
                           contract_1, notcontract_1, &
                           contract_2, notcontract_2, &
                           map_1, map_2, &
                           bounds_1, bounds_2, bounds_3, &
                           optimize_dist, pgrid_opt_1, pgrid_opt_2, pgrid_opt_3, &
                           filter_eps, flop, move_data, retain_sparsity, unit_nr, log_verbose)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor_1%fm_t))
               CPASSERT(ASSOCIATED(tensor_2%fm_t))
               CPASSERT(ASSOCIATED(tensor_3%fm_t))
               CALL  tensor_contract_low(alpha, tensor_1%fm_t, tensor_2%fm_t, beta, tensor_3%fm_t, &
                           contract_1, notcontract_1, &
                           contract_2, notcontract_2, &
                           map_1, map_2, &
                           bounds_1, bounds_2, bounds_3, &
                           optimize_dist, pgrid_opt_1, pgrid_opt_2, pgrid_opt_3, &
                           filter_eps, flop, move_data, retain_sparsity, unit_nr, log_verbose)
          END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_copy_matrix_to_tensor(matrix_in, tensor_out, summation)
      TYPE(dbcsr_type), TARGET, INTENT(IN)               :: matrix_in
      TYPE(tensor_type), INTENT(INOUT)                   :: tensor_out
      LOGICAL, INTENT(IN), OPTIONAL                      :: summation


      SELECT CASE (tensor_out%t_type)
      CASE (case_dbt)
           CPASSERT(ASSOCIATED(tensor_out%dbt))
           CALL dbt_copy_matrix_to_tensor(matrix_in, tensor_out%dbt, summation)
      CASE (case_fmt)
           CPASSERT(ASSOCIATED(tensor_out%fm_t))
           CALL tensor_copy_matrix_to_tensor_low(matrix_in, tensor_out%fm_t, summation)    
      END SELECT 

   END SUBROUTINE

    SUBROUTINE tensor_copy_tensor_to_matrix(tensor_in, matrix_out, summation)
      TYPE(tensor_type), INTENT(INOUT)      :: tensor_in
      TYPE(dbcsr_type), INTENT(INOUT)             :: matrix_out
      LOGICAL, INTENT(IN), OPTIONAL          :: summation

      SELECT CASE (tensor_in%t_type)
      CASE (case_dbt)
           CPASSERT(ASSOCIATED(tensor_in%dbt))
           CALL dbt_copy_tensor_to_matrix(tensor_in%dbt, matrix_out, summation)
      CASE (case_fmt)
           CPASSERT(ASSOCIATED(tensor_in%fm_t))
           CALL tensor_copy_tensor_to_matrix_low(tensor_in%fm_t, matrix_out, summation)
      END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_filter(tensor, eps)
      TYPE(tensor_type), INTENT(INOUT)    :: tensor
      REAL(dp), INTENT(IN)                :: eps
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CPASSERT(ASSOCIATED(tensor%dbt))
           CALL dbt_filter(tensor%dbt, eps)
      CASE (case_fmt)
           CPASSERT(ASSOCIATED(tensor%fm_t))
           CALL tensor_filter_low(tensor%fm_t, eps)
      END SELECT
   END SUBROUTINE

 SUBROUTINE tensor_iterator_next_block(tensor, iterator, ind_nd, blk_size, blk_offset)
      TYPE(tensor_type), INTENT(IN)                 :: tensor  
      TYPE(tensor_iterator_type), INTENT(INOUT)     :: iterator
      INTEGER, DIMENSION(:), INTENT(OUT)            :: ind_nd
      INTEGER, DIMENSION(:), INTENT(OUT), OPTIONAL  :: blk_size, blk_offset
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CPASSERT(ASSOCIATED(tensor%dbt))
           CALL dbt_iterator_next_block(iterator%iter, ind_nd, blk_size, blk_offset)
      CASE (case_fmt)
      END SELECT
   END SUBROUTINE

   
   FUNCTION tensor_iterator_blocks_left(tensor,iterator)
      TYPE(tensor_type), INTENT(IN)           :: tensor
      TYPE(tensor_iterator_type)              :: iterator
      LOGICAL                                 :: tensor_iterator_blocks_left
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CPASSERT(ASSOCIATED(tensor%dbt))
           tensor_iterator_blocks_left=dbt_iterator_blocks_left(iterator%iter)
      CASE (case_fmt)
      END SELECT
   END FUNCTION    

   FUNCTION tensor_iterator_num_blocks(tensor,iterator)
      TYPE(tensor_type), INTENT(IN)           :: tensor
      TYPE(tensor_iterator_type)                 :: iterator
      INTEGER                                    :: tensor_iterator_num_blocks
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CPASSERT(ASSOCIATED(tensor%dbt))
           tensor_iterator_num_blocks=dbt_iterator_num_blocks(iterator%iter)
      CASE (case_fmt)
      END SELECT
   END FUNCTION    

   SUBROUTINE tensor_iterator_start(iterator, tensor)
      TYPE(tensor_iterator_type), INTENT(OUT)           :: iterator
      TYPE(tensor_type), INTENT(IN)                  :: tensor
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CPASSERT(ASSOCIATED(tensor%dbt))
           CALL dbt_iterator_start(iterator%iter,tensor%dbt)
      CASE (case_fmt)
      END SELECT 
   END SUBROUTINE

   SUBROUTINE tensor_iterator_stop(tensor, iterator)
      TYPE(tensor_iterator_type), INTENT(INOUT) :: iterator
      TYPE(tensor_type), INTENT(IN)                  :: tensor
      SELECT CASE (tensor%t_type)
      CASE (case_dbt)
           CPASSERT(ASSOCIATED(tensor%dbt)) 
           CALL dbt_iterator_stop(iterator%iter)
      CASE (case_fmt)
      END SELECT 
   END SUBROUTINE


   SUBROUTINE tensor_mp_environ_pgrid(tensor,pgrid, dims, task_coor)
            TYPE(tensor_type), INTENT(IN)       :: tensor
            TYPE(tensor_pgrid_type), INTENT(IN) :: pgrid
            INTEGER, DIMENSION(:), INTENT(OUT) :: dims
            INTEGER, DIMENSION(:), INTENT(OUT) :: task_coor
            SELECT CASE (tensor%t_type)
            CASE (case_dbt)
                 CPASSERT(ASSOCIATED(tensor%dbt))
                 CALL dbt_mp_environ_pgrid(pgrid%grid, dims, task_coor)
            CASE (case_fmt)
            END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_pgrid_create(tensor, mp_comm, dims, pgrid, tensor_dims)
            TYPE(tensor_type), INTENT(IN)   :: tensor
            CLASS(mp_comm_type), INTENT(IN) :: mp_comm
            INTEGER, DIMENSION(:), INTENT(INOUT) :: dims
            TYPE(tensor_pgrid_type), INTENT(OUT) :: pgrid
            INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL :: tensor_dims
            SELECT CASE (tensor%t_type)
            CASE (case_dbt)
                 CPASSERT(ASSOCIATED(tensor%dbt))
                 CALL dbt_pgrid_create(mp_comm, dims, pgrid%grid, tensor_dims)
            CASE (case_fmt)
            END SELECT

   END SUBROUTINE
 
   SUBROUTINE tensor_pgrid_destroy(tensor,pgrid, keep_comm)
            TYPE(tensor_type),INTENT(IN)            :: tensor 
            TYPE(tensor_pgrid_type), INTENT(INOUT)  :: pgrid
            LOGICAL, INTENT(IN), OPTIONAL           :: keep_comm
            SELECT CASE (tensor%t_type)
            CASE (case_dbt)
                 CPASSERT(ASSOCIATED(tensor%dbt))
                 CALL dbt_pgrid_destroy(pgrid%grid,keep_comm)
            CASE (case_fmt)
            END SELECT

   END SUBROUTINE

    SUBROUTINE tensor_build_3c_integrals(t3c, filter_eps, qs_env, &
                                 nl_3c, basis_i, basis_j, basis_k, &
                                 potential_parameter, int_eps, &
                                 op_pos, do_kpoints, do_hfx_kpoints, desymmetrize, cell_sym, &
                                 bounds_i, bounds_j, bounds_k, &
                                 RI_range, img_to_RI_cell, cell_to_index_ext)

      TYPE(tensor_type), DIMENSION(:, :), INTENT(INOUT)  :: t3c
      REAL(KIND=dp), INTENT(IN)                          :: filter_eps
      TYPE(qs_environment_type), POINTER                 :: qs_env
      TYPE(neighbor_list_3c_type), INTENT(INOUT)         :: nl_3c
      TYPE(gto_basis_set_p_type), DIMENSION(:)           :: basis_i, basis_j, basis_k
      TYPE(libint_potential_type), INTENT(IN)            :: potential_parameter
      REAL(KIND=dp), INTENT(IN), OPTIONAL                :: int_eps
      INTEGER, INTENT(IN), OPTIONAL                      :: op_pos
      LOGICAL, INTENT(IN), OPTIONAL                      :: do_kpoints, do_hfx_kpoints, &
                                                            desymmetrize, cell_sym
      INTEGER, DIMENSION(2), INTENT(IN), OPTIONAL        :: bounds_i, bounds_j, bounds_k
      REAL(dp), INTENT(IN), OPTIONAL                     :: RI_range
      INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL        :: img_to_RI_cell
      INTEGER, DIMENSION(:, :, :), OPTIONAL, POINTER     :: cell_to_index_ext
   END SUBROUTINE


   SUBROUTINE tensor_clear_low(fm_tensor)
          TYPE(fmt_type), INTENT(INOUT) :: fm_tensor
   END SUBROUTINE

   SUBROUTINE tensor_create_low_template(fm_tensor_t,fm_tensor)
          TYPE(fmt_type), INTENT(INOUT) :: fm_tensor_t,fm_tensor
   END SUBROUTINE

   SUBROUTINE tensor_create_low_matrix(mat_local,fm_tensor)
          TYPE(fmt_type), INTENT(INOUT)    :: fm_tensor
           TYPE(dbcsr_type), INTENT(INOUT) :: mat_local
   END SUBROUTINE

   SUBROUTINE tensor_copy_low(fm_tensor_in, fm_tensor_out, order, summation, bounds, move_data, unit_nr)
          TYPE(fmt_type), INTENT(INOUT), TARGET         :: fm_tensor_in, fm_tensor_out
          INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL   :: order
          LOGICAL, INTENT(IN), OPTIONAL                 :: summation, move_data
          INTEGER, DIMENSION(:,:),INTENT(IN), OPTIONAL  :: bounds
          INTEGER, INTENT(IN), OPTIONAL                 :: unit_nr
   END SUBROUTINE

   SUBROUTINE tensor_contract_low(alpha, fm_tensor_1, fm_tensor_2, beta, fm_tensor_3, &
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

   SUBROUTINE tensor_destroy_low(fm_tensor)
          TYPE(fmt_type), INTENT(INOUT) :: fm_tensor
   END SUBROUTINE

   SUBROUTINE tensor_copy_matrix_to_tensor_low(matrix_in, tensor_out, summation)
     TYPE(dbcsr_type), TARGET, INTENT(IN)               :: matrix_in
     TYPE(fmt_type), INTENT(INOUT)                   :: tensor_out
     LOGICAL, INTENT(IN), OPTIONAL                      :: summation
   END SUBROUTINE  

   
   SUBROUTINE tensor_copy_tensor_to_matrix_low(tensor_in, matrix_out, summation)
     TYPE(fmt_type), INTENT(INOUT)      :: tensor_in
     TYPE(dbcsr_type), INTENT(INOUT)             :: matrix_out
     LOGICAL, INTENT(IN), OPTIONAL          :: summation
   END SUBROUTINE

   SUBROUTINE tensor_filter_low(tensor, eps)
      TYPE(fmt_type), INTENT(INOUT)    :: tensor
      REAL(dp), INTENT(IN)                :: eps
   END SUBROUTINE

    
END MODULE tensor_types
