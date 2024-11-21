MODULE tensor_types

   USE message_passing, ONLY: &
      mp_cart_type, mp_dims_create, mp_comm_type

#include "../base/base_uses.f90"

   IMPLICIT NONE
   PRIVATE
   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'dbt_types'

   PUBLIC  :: tensor_type, tensor_create

END MODULE tensor_types
