MODULE gw_qs_tensor

USE qs_environment_types,            ONLY: qs_environment_type
USE qs_tensors_types,                ONLY: neighbor_list_3c_type, create_3c_tensor, &
                                           create_2c_tensor
USE qs_tensors,                      ONLY: build_3c_integrals, get_tensor_occupancy
USE basis_set_types,                 ONLY: gto_basis_set_p_type
USE libint_2c_3c,                    ONLY: libint_potential_type
USE tensor_api,                      ONLY: tensor_type,case_dbt,case_fmt, &
                                           tensor_pgrid_type, &
                                           tensor_distribution_type
USE kinds,                           ONLY: dp, int_8
USE dbt_api,                         ONLY: dbt_distribution_type  

IMPLICIT NONE

PRIVATE

PUBLIC :: tensor_build_3c_integrals, tensor_create_3c_tensor, tensor_create_2c_tensor, &
          tensor_get_tensor_occupancy

CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'gw_qs_tensor'



CONTAINS

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
      SELECT CASE(t3c(1,1)%t_type)
      CASE (case_dbt)
            CALL build_3c_integrals(t3c%dbt, filter_eps, qs_env, &
                                 nl_3c, basis_i, basis_j, basis_k, &
                                 potential_parameter, int_eps, &
                                 op_pos, do_kpoints, do_hfx_kpoints, desymmetrize, cell_sym, &
                                 bounds_i, bounds_j, bounds_k, &
                                 RI_range, img_to_RI_cell, cell_to_index_ext)
      CASE (case_fmt)
      END SELECT
   END SUBROUTINE

   SUBROUTINE tensor_create_3c_tensor(t3c, dist_1, dist_2, dist_3, pgrid, sizes_1, sizes_2, sizes_3, map1, map2, name,mode)
      TYPE(tensor_type), INTENT(OUT)                     :: t3c
      INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT)    :: dist_1, dist_2, dist_3
      TYPE(tensor_pgrid_type), INTENT(IN)                :: pgrid
      INTEGER, DIMENSION(:), INTENT(IN)                  :: sizes_1, sizes_2, sizes_3, map1, map2
      CHARACTER(len=*), INTENT(IN)                       :: name
      INTEGER                                            :: mode

      SELECT CASE(t3c%t_type)
      CASE (case_dbt)
            CALL create_3c_tensor(t3c%dbt, dist_1, dist_2, dist_3, pgrid%grid, sizes_1, sizes_2, sizes_3, map1, map2, name)
      CASE (case_fmt)
      END SELECT
      t3c%t_type=mode
   END SUBROUTINE

   SUBROUTINE tensor_create_2c_tensor(t2c, dist_1, dist_2, pgrid, sizes_1, sizes_2, order, name, mode)
      TYPE(tensor_type), INTENT(OUT)                     :: t2c
      INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(OUT)    :: dist_1, dist_2
      TYPE(tensor_pgrid_type), INTENT(IN)                :: pgrid
      INTEGER, DIMENSION(:), INTENT(IN)                  :: sizes_1, sizes_2
      INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL        :: order
      CHARACTER(len=*), INTENT(IN)                       :: name
      INTEGER                                            :: mode

      SELECT CASE(t2c%t_type)
      CASE (case_dbt)
            CALL create_2c_tensor(t2c%dbt, dist_1, dist_2, pgrid%grid, sizes_1, sizes_2, order, name)
      CASE (case_fmt)
      END SELECT
      t2c%t_type=mode
   END SUBROUTINE

   SUBROUTINE tensor_get_tensor_occupancy(tensor, nze, occ)
      TYPE(tensor_type), INTENT(IN)                      :: tensor
      INTEGER(int_8), INTENT(OUT)                        :: nze
      REAL(dp), INTENT(OUT)                              :: occ

      SELECT CASE(tensor%t_type)
      CASE (case_dbt)
           CALL get_tensor_occupancy(tensor%dbt,nze,occ)
      CASE (case_fmt)
      END SELECT

   END SUBROUTINE



END MODULE gw_qs_tensor
