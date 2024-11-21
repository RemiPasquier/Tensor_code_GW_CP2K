MODULE tensor_types

   USE message_passing,                 ONLY: mp_cart_type, &
                                              mp_dims_create, &
                                              mp_comm_type
   USE cp_fm_types,                     ONLY: cp_fm_type 
   USE kinds,                           ONLY: int_8
   !USE input_constants                  ONLY: case_dbt, &
   !                                           case_fmt
   
   USE dbt_api,                         ONLY: dbt_clear, &
                                              dbt_type
                                               

#include "../base/base_uses.f90"

   IMPLICIT NONE
   PRIVATE
   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_types'

   PUBLIC  :: tensor_type, tensor_clear

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
          CASE(1)
            CALL dbt_clear(tensor%dbt)
          CASE(2)

          END SELECT
   END SUBROUTINE

END MODULE tensor_types
