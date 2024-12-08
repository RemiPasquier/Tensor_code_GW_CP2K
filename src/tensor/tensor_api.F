MODULE tensor_api

   USE tensor_interfaces,                       ONLY: tensor_type, tensor_clear, &
                                                 tensor_create, tensor_destroy, &
                                                 tensor_copy, tensor_contract, &
                                                 tensor_copy_matrix_to_tensor, &
                                                 tensor_copy_tensor_to_matrix, &
                                                 tensor_filter, &
                                                 tensor_iterator_blocks_left, &
                                                 tensor_iterator_next_block, &
                                                 tensor_iterator_start, &
                                                 tensor_iterator_stop, &
                                                 tensor_iterator_type, &
                                                 tensor_mp_environ_pgrid, &
                                                 tensor_pgrid_create, &
                                                 tensor_pgrid_destroy, &
                                                 tensor_pgrid_type, &
                                                 tensor_distribution_type
   USE tensor_constants,                   ONLY: case_dbt, case_fmt
   
   IMPLICIT NONE

   PRIVATE

   CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'tensor_api'

   PUBLIC  :: tensor_type, tensor_clear, tensor_create, tensor_destroy, &
              tensor_copy, tensor_contract,tensor_copy_matrix_to_tensor, &
              tensor_copy_tensor_to_matrix,tensor_filter,tensor_iterator_blocks_left, &
              tensor_iterator_next_block,tensor_iterator_start,tensor_iterator_stop, & 
              tensor_iterator_type,tensor_mp_environ_pgrid,tensor_pgrid_create, &
              tensor_pgrid_destroy, tensor_pgrid_type, case_dbt, case_fmt, &
              tensor_distribution_type
 



END MODULE tensor_api
