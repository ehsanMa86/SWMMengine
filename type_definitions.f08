!*---------------------------------------------------------------------------*!
!    |
!    | SWMM Engine: Storm Water Management Model
!    | Website:  https://ehsanmadadi.com
!    | Copyright (C) 2018-2020 Ehsan Madadi-Kandjani
!-------------------------------------------------------------------------------
!License
!    This file is part of SWMM Engine.
!    SWMM Engine is free software: you can redistribute it and/or modify it
!    under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!    SWMM Engine is distributed in the hope that it will be useful, but WITHOUT
!    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!    for more details.
!    You should have received a copy of the GNU General Public License
!    along with SWMM Engine.  If not, see <http://www.gnu.org/licenses/>.
!*---------------------------------------------------------------------------*!
!
! module type defintions
!
! These are derived type definitions that are used in globals, setting, and
! elsewhere.
!
! Note that each of these is intended to be dependent only on derived
! types within this module.
!
! Types that are defined outside of here or setting_definition should
! be confined to the module in which they are defined.
!
!==========================================================================
 module type_definitions

    implicit none

    public

    type string
        character(len=:), allocatable :: str
    end type string


    !%  diagnostic%Volume
    type diagnosticVolumeType
        integer  :: Step
        real     :: Time
        real     :: Volume
        real     :: VolumeChange
        real     :: NetInflowVolume
        real     :: InflowRate
        real     :: OutflowRate
        real     :: ConservationThisStep ! + is artificial source, - is sink
        real     :: ConservationTotal
    end type diagnosticVolumeType
    
    type diagnosticType
        type(diagnosticVolumeType)  :: Volume
    end type diagnosticType

    !% boundary condition data
    type bcType
        integer :: Idx
        integer :: NodeID
        integer :: FaceID
        integer :: ElemGhostID
        integer :: ElemInsideID
        integer :: Updn      ! bc_updn_...  (0 = upstream,  1 = downstream)
        integer :: Category  ! bc_category_... (0 = elevation, 1 = inflowrate)
        real, dimension(:), allocatable :: TimeArray
        real, dimension(:), allocatable :: ValueArray
        real    :: ThisValue
        real    :: ThisTime
        real    :: ThisFlowrate
    end type bcType

    !% output file location
    type outputfileType
        integer        :: UnitNumber = 0
        character(256) :: FileName   = 'dummy.txt'
        character(256) :: FolderName = 'dummyFolder'
        character(256) :: FolderPath = './'
        character(32)  :: FileStatus = 'new'
        character(512) :: WriteName  = ''
        logical        :: IsOpen     = .false.
    end type outputfileType

    !% specific information for a debug file
    type debugfileType
        type (outputfileType) :: FileInfo
        character(32)         :: ArrayName
        integer               :: ColumnIndex
    end type debugfileType

    !% specific information for a threaded output file
    type threadedfileType
        type (outputfileType) :: FileInfo
        character(32)         :: DataName
    end type threadedfileType


!==========================================================================
! END OF MODULE type_definitions
!==========================================================================
 end module type_definitions
