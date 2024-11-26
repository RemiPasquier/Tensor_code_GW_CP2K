!--------------------------------------------------------------------------------------------------!
!   CP2K: A general program to perform molecular dynamics simulations                              !
!   Copyright 2000-2024 CP2K developers group <https://cp2k.org>                                   !
!                                                                                                  !
!   SPDX-License-Identifier: GPL-2.0-or-later                                                      !
!--------------------------------------------------------------------------------------------------!

! **************************************************************************************************
!> \brief constants required for the tensor package, which cannot be put
!         in the main input_constants.F file because of circular
!         dependencies
!> \par History
!>      11.2024 created [pasquier]
! **************************************************************************************************
MODULE tensor_constants

   USE kinds,                           ONLY: dp

   IMPLICIT NONE

   PRIVATE

! tensor parameters (for switching between full and sparse tensors)
   INTEGER, PARAMETER, PUBLIC               :: case_dbt = 1, &
                                               case_fmt = 2
END MODULE tensor_constants
