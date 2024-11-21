MODULE tensor_api

   USE tensor_types,                       ONLY: tensor_create, tensor_type

   IMPLICIT NONE

   PRIVATE

   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_api'

   PUBLIC :: tensor_create

END MODULE tensor_api
