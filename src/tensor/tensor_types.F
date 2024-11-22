MODULE tensor_types

   USE message_passing,                 ONLY: mp_cart_type, &
                                              mp_dims_create, &
                                              mp_comm_type
   USE cp_fm_types,                     ONLY: cp_fm_type 
   USE kinds,                           ONLY: int_8,&
                                              dp
   USE tensor_constants,                ONLY: case_dbt, &
                                              case_fmt
   
   USE dbt_api,                         ONLY: dbt_clear, &
                                              dbt_type, &
                                              dbt_create, &
                                              dbt_pgrid_type   
                                               

#include "../base/base_uses.f90"

   IMPLICIT NONE
   PRIVATE
   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_types'

   PUBLIC  :: tensor_type, tensor_clear, tensor_create, tensor_destroy, &
              tensor_copy, tensor_contract

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
   SUBROUTINE tensor_create(tensor_t,tensor)
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
               CALL tensor_create_low(tensor_t%fm_t,tensor%fm_t)
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

   SUBROUTINE tensor_clear_low(fm_tensor)
          TYPE(fmt_type), INTENT(INOUT) :: fm_tensor
   END SUBROUTINE

   SUBROUTINE tensor_create_low(fm_tensor_t,fm_tensor)
          TYPE(fmt_type), INTENT(INOUT) :: fm_tensor_t,fm_tensor
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

END MODULE tensor_types
