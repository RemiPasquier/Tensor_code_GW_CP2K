MODULE tensor_api

   USE tensor_types,                       ONLY: tensor_type, tensor_clear, &
                                                 tensor_create, tensor_destroy, &
                                                 tensor_copy, tensor_contract

   IMPLICIT NONE

   PRIVATE

   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_api'

   PUBLIC  :: tensor_type, tensor_clear, tensor_create, tensor_destroy, &
              tensor_copy, tensor_contract


END MODULE tensor_api
