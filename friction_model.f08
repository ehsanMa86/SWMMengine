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
!==========================================================================
!
 module friction_model
! 
! Compute the friction term used in the time advance
! Note that this is done separately so that it can be limited to prevent
! overestimation of explicit friction when flow is reversing.
!
    use array_index
    use data_keys
    use globals
    use setting_definition

    implicit none
    
    private
    
    public  :: friction_on_element

    integer :: debuglevel = 0
    
 contains
!
!========================================================================== 
!==========================================================================
!
 subroutine friction_on_element &
    (elemR, elemI, er_Friction, er_Velocity, er_Volume, &
     er_Roughness, er_HydRadius, ei_elem_type, ei_roughness_type,ThisElemType)
!
! friction term in momentum on one type of element
! The general form is grav * volume * FrictionSlope
! This model uses Manning's n form for FrictionSlope
! 
 character(64) :: subroutine_name = 'friction_on_element'
    
 real,      target, intent(in out)  ::  elemR(:,:)
 
 integer,           intent(in)      ::  elemI(:,:)
 
 integer,           intent(in)      ::  er_Friction, er_Velocity, er_Volume
 integer,           intent(in)      ::  er_Roughness, er_HydRadius 
 integer,           intent(in)      ::  ei_elem_type, ei_roughness_type 
 integer,           intent(in)      ::  ThisElemType
 
 real,  pointer :: friction(:), velocity(:), volume(:), manningsn(:), rh(:)
 
!-------------------------------------------------------------------------- 
 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** enter ',subroutine_name 
 
 friction =>  elemR(:,er_Friction)
 velocity =>  elemR(:,er_Velocity)
 volume   =>  elemR(:,er_Volume)
 manningsn=>  elemR(:,er_Roughness)
 rh       =>  elemR(:,er_HydRadius)
 
 where ( ( elemI(:,ei_elem_type) == ThisElemType ) .and. &
         ( elemI(:,ei_roughness_type) == eManningsN ) )
    friction = sign(grav * (manningsn**2) * (velocity**2) * volume / (rh**(4.0/3.0)), velocity)
 endwhere

 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** leave ',subroutine_name
 end subroutine friction_on_element
!
!========================================================================== 
! END OF MODULE friction_model
!==========================================================================
 end module friction_model
