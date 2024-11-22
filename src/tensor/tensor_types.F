MODULE tensor_types

   USE message_passing,                 ONLY: mp_cart_type, &
                                              mp_dims_create, &
                                              mp_comm_type
   USE cp_fm_types,                     ONLY: cp_fm_type 
   USE kinds,                           ONLY: int_8,&
                                              dp
   USE tensor_constants                 ONLY: case_dbt, &
                                              case_fmt
   
   USE dbt_api,                         ONLY: dbt_clear, &
                                              dbt_type, &
                                              dbt_create
                                               

#include "../base/base_uses.f90"

   IMPLICIT NONE
   PRIVATE
   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_types'

   PUBLIC  :: tensor_type, tensor_clear, tensor_create

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

   INTERFACE tensor_copy
             MODULE PROCEDURE tensor_copy_low, dbt_copy_interface
   END INTERFACE

   INTERFACE tensor_destroy
             MODULE PROCEDURE tensor_destroy_low
             PROCEDURE        dbt_destroy
   END INTERFACE

CONTAINS

   SUBROUTINE tensor_clear(tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
          SELECT CASE(tensor%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor%dbt))
               CALL dbt_clear(tensor%dbt)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor%fm_t)
               CALL tensor_clear_low(tensor%fm_t)
          END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_create(tensor_t,tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
          SELECT CASE(tensor%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor_t%dbt))
               CPASSERT(ASSOCIATED(tensor%dbt))
               CALL dbt_create(tensor_t%dbt,tensor%dbt)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor_t%fm_t)
               CPASSERT(ASSOCIATED(tensor%fm_t)
               CALL tensor_create_low(tensor_t%fm_t,tensor%fm_t)
          END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_copy(tensor_in, tensor_out, order, summation, bounds, move_data, unit_nr)
          TYPE(tensor_type), INTENT(INOUT), TARGET      :: tensor_in, tensor_out
          INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL   :: order
          LOGICAL, INTENT(IN), OPTIONAL                 :: summation, move_data
          INTEGER, DIMENSION(2,:),INTENT(IN), OPTIONAL  :: bounds
          INTEGER, INTENT(IN), OPTIONAL                 :: unit_nr
          SELECT CASE(tensor_in%t_type)
          CASE (case_dbt)
               CPASSERT(ASSOCIATED(tensor_in%dbt))
               CPASSERT(ASSOCIATED(tensor_out%dbt))
               CALL dbt_copy(tensor_in, tensor_out, order, summation, bounds, move_data, unit_nr)
          CASE (case_fmt)
               CPASSERT(ASSOCIATED(tensor_t%fm_t)
               CPASSERT(ASSOCIATED(tensor%fm_t)
               CALL tensor_copy(tensor_in, tensor_out, order, summation, bounds, move_data, unit_nr)
          END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_clear_low(tensor)
          TYPE(tensor_type), INTENT(INOUT) :: tensor
   END SUBROUTINE

   SUBROUTINE tensor_create_low(tensor)
          TYPE(tensor_type), INTENT(INOUT)
   END SUBROUTINE

   SUBROUTINE tensor_copy_low(tensor)

END MODULE tensor_types
