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
! module initialization
!
! This creates the arrays index structures that are used for accessing
! data. Arguably, this could be done more simply but we want the fundamental
! column indexes in array_index to be parameters rather than variables. By
! using parameters we reduce the possibility of accidentally changing a
! column definition.
!
!==========================================================================
!
 module initialization
!
! general initialization of data structures (not including network)
!
    use array_index
    use data_keys
    use globals
    
    implicit none
    private
    
    public :: initialize_arrayindex ! handles indexes for multiple face per element
    public :: initialize_arrayindex_status ! status check at end of simulation 
    public :: initialize_array_zerovalues ! sets some elemMR values to zero
    public :: initialize_dummy_values
    
    integer, private :: debuglevel = 0
    
 contains
!
!==========================================================================
!==========================================================================
!
 subroutine initialize_arrayindex 
!
! Initializes the array indexes for the parameter values.
! Note that the use of these indexes as variable arrays in the code is
! necessary for simplicity in coding, but leaves a potential bug if the
! variables are ever changed during the code. As such, we have an
! additional subroutine (initialization_arrayindex_status) that checks to
! see of the indexes have retained their values at the end of a
! simulation.   
    
 character(64) :: subroutine_name =  'initialize_arrayindex'   
 integer :: ii, kk
 
!-------------------------------------------------------------------------- 
 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** enter ',subroutine_name
 
 ! initialize the indexes for temp arrays
 e2r_Temp(1) = e2r_temp1
 e2r_Temp(2) = e2r_temp2
 e2r_Temp(3) = e2r_temp3
 e2r_Temp(4) = e2r_temp4
 e2r_Temp(5) = e2r_temp5
 e2r_Temp(6) = e2r_temp6
 if (e2r_n_temp > 6) then
    print *, 'error need to update initialization of e2r_Temp in ',subroutine_name
    stop
 endif

 eMr_Temp(1) = eMr_temp1
 eMr_Temp(2) = eMr_temp2
 eMr_Temp(3) = eMr_temp3
 eMr_Temp(4) = eMr_temp4
 eMr_Temp(5) = eMr_temp5
 eMr_Temp(6) = eMr_temp6
 if (eMr_n_temp > 6) then
    print *, 'error need to update initialization of eMr_Temp in ',subroutine_name
    stop
 endif

 e2i_Temp(1) = e2i_temp1
 e2i_Temp(2) = e2i_temp2
 if (e2i_n_temp > 2) then
    print *, 'error need to update initialization of e2i_Temp in ',subroutine_name
    stop
 endif

 eMi_Temp(1) = eMi_temp1
 eMi_Temp(2) = eMi_temp2
 if (eMi_n_temp > 2) then
    print *, 'error need to update initialization of eMi_Temp in ',subroutine_name
    stop
 endif
 
 fi_Temp(1) = fi_temp1
 fi_Temp(2) = fi_temp2
 if (fi_n_temp > 2) then
    print *, 'error need to update initialization of fi_Temp in ',subroutine_name
    stop
 endif
    
 fr_Temp(1) = fr_temp1
 fr_Temp(2) = fr_temp2
 fr_Temp(3) = fr_temp3
 fr_Temp(4) = fr_temp4
 if (fr_n_temp > 4) then
    print *, 'error need to update initialization of fr_Temp in ',subroutine_name
    stop
 endif
    
 e2YN_Temp(1) = e2YN_temp1
 e2YN_Temp(2) = e2YN_temp2 
 e2YN_Temp(3) = e2YN_temp3
 e2YN_Temp(4) = e2YN_temp4
 if (e2YN_n_temp > 4) then
    print *, 'error need to update initialization of e2YN_Temp in ',subroutine_name
    stop
 endif
 
 eMYN_Temp(1) = eMYN_temp1
 eMYN_Temp(2) = eMYN_temp2 
 eMYN_Temp(3) = eMYN_temp3
 eMYN_Temp(4) = eMYN_temp4
 if (eMYN_n_temp > 4) then
    print *, 'error need to update initialization of eMYN_Temp in ',subroutine_name
    stop
 endif
 
 fYN_Temp(1) = fYN_temp1
 fYN_Temp(2) = fYN_temp2
 if (fYN_n_temp > 2) then
    print *, 'error need to update initialization of fYN_Temp in ',subroutine_name
    stop
 endif
    
! the following are required index values
 ii=1
 ni_MlinkUp(ii) = ni_Mlink_u1
 ni_MlinkDn(ii) = ni_Mlink_d1

 eMi_MfaceUp(ii) = eMi_Mface_u1
 eMi_MfaceDn(ii) = eMi_Mface_d1

 eMr_EtaUp(ii) = eMr_Eta_u1
 eMr_EtaDn(ii) = eMr_Eta_d1

 eMr_FlowrateUp(ii) = eMr_Flowrate_u1
 eMr_FlowrateDn(ii) = eMr_Flowrate_d1
 
 eMr_VelocityUp(ii) = eMr_Velocity_u1
 eMr_VelocityDn(ii) = eMr_Velocity_d1
 
 eMr_TimescaleUp(ii) = eMr_Timescale_u1
 eMr_TimescaleDn(ii) = eMr_Timescale_d1  
 
 eMr_AreaUp(ii) = eMr_Area_u1
 eMr_AreaDn(ii) = eMr_Area_d1

 eMr_TopwidthUp(ii) = eMr_Topwidth_u1
 eMr_TopwidthDn(ii) = eMr_Topwidth_d1
 
 eMr_HydDepthUp(ii) = eMr_HydDepth_u1
 eMr_HydDepthDn(ii) = eMr_HydDepth_d1
 
 eMr_LengthUp(ii) = eMr_Length_u1
 eMr_LengthDn(ii) = eMr_Length_d1
 
 eMr_ZbottomUp(ii) = eMr_Zbottom_u1
 eMr_ZbottomDn(ii) = eMr_Zbottom_d1 
 
 eMr_BreadthScaleUp(ii) = eMr_BreadthScale_u1
 eMr_BreadthScaleDn(ii) = eMr_BreadthScale_d1 
    
 nr_ElementLengthUp(ii) = nr_ElementLength_u1
 nr_ElementLengthDn(ii) = nr_ElementLength_d1

! the following depends on the number of upstream faces per element allowed
 ii=2
 if (upstream_face_per_elemM > ii-1) then
    ni_MlinkUp(ii)           = ni_Mlink_u2
    eMi_MfaceUp(ii)          = eMi_Mface_u2
    eMr_EtaUp(ii)            = eMr_Eta_u2
    eMr_FlowrateUp(ii)       = eMr_Flowrate_u2
    eMr_VelocityUp(ii)       = eMr_Velocity_u2    
    eMr_TimescaleUp(ii)      = eMr_Timescale_u2
    eMr_AreaUp(ii)           = eMr_Area_u2
    eMr_TopwidthUp(ii)       = eMr_Topwidth_u2
    eMr_HydDepthUp(ii)       = eMr_HydDepth_u2
    eMr_LengthUp(ii)         = eMr_Length_u2
    eMr_ZbottomUp(ii)        = eMr_Zbottom_u2   
    eMr_BreadthScaleUp(ii)   = eMr_BreadthScale_u2    
    nr_ElementLengthUp(ii)   = nr_ElementLength_u2
 endif
 ii=3
 if (upstream_face_per_elemM > ii-1) then
    ni_MlinkUp(ii)           = ni_Mlink_u3
    eMi_MfaceUp(ii)          = eMi_Mface_u3
    eMr_EtaUp(ii)            = eMr_Eta_u3
    eMr_FlowrateUp(ii)       = eMr_Flowrate_u3
    eMr_VelocityUp(ii)       = eMr_Velocity_u3    
    eMr_TimescaleUp(ii)      = eMr_Timescale_u3 
    eMr_AreaUp(ii)           = eMr_Area_u3
    eMr_TopwidthUp(ii)       = eMr_Topwidth_u3
    eMr_HydDepthUp(ii)       = eMr_HydDepth_u3
    eMr_LengthUp(ii)         = eMr_Length_u3
    eMr_ZbottomUp(ii)        = eMr_Zbottom_u3   
    eMr_BreadthScaleUp(ii)   = eMr_BreadthScale_u3   
    nr_ElementLengthUp(ii)   = nr_ElementLength_u3
 endif    
 if (upstream_face_per_elemM > 3) then
    print *, 'error: code does not support upstream_face_per_elemM > 3 in ',subroutine_name
 endif

! the following depends on the number of downstream faces per element allowed
 ii=2
 if (dnstream_face_per_elemM > ii-1) then
    ni_MlinkDn(ii)           = ni_Mlink_d2
    eMi_MfaceDn(ii)          = eMi_Mface_d2
    eMr_EtaDn(ii)            = eMr_Eta_d2
    eMr_FlowrateDn(ii)       = eMr_Flowrate_d2
    eMr_VelocityDn(ii)       = eMr_Velocity_d2    
    eMr_TimescaleDn(ii)      = eMr_Timescale_d2
    eMr_AreaDn(ii)           = eMr_Area_d2
    eMr_TopwidthDn(ii)       = eMr_Topwidth_d2
    eMr_HydDepthDn(ii)       = eMr_HydDepth_d2
    eMr_LengthDn(ii)         = eMr_Length_d2
    eMr_ZbottomDn(ii)        = eMr_Zbottom_d2   
    eMr_BreadthScaleDn(ii)    = eMr_BreadthScale_d2
    nr_ElementLengthDn(ii)   = nr_ElementLength_d2
 endif
 
 ii=3
 if (dnstream_face_per_elemM > ii-1) then
    ni_MlinkDn(ii)           = ni_Mlink_d3
    eMi_MfaceDn(ii)          = eMi_Mface_d3
    eMr_EtaDn(ii)            = eMr_Eta_d3
    eMr_FlowrateDn(ii)       = eMr_Flowrate_d3
    eMr_VelocityDn(ii)       = eMr_Velocity_d3   
    eMr_TimescaleDn(ii)      = eMr_Timescale_d3
    eMr_AreaDn(ii)           = eMr_Area_d3
    eMr_TopwidthDn(ii)       = eMr_Topwidth_d3
    eMr_HydDepthDn(ii)       = eMr_HydDepth_d3
    eMr_LengthDn(ii)         = eMr_Length_d3
    eMr_ZbottomDn(ii)        = eMr_Zbottom_d3   
    eMr_BreadthScaleDn(ii)   = eMr_BreadthScale_d3   
    nr_ElementLengthDn(ii)   = nr_ElementLength_d3
 endif    
 if (dnstream_face_per_elemM > 3) then
    print *, 'error: code does not support dnstream_face_per_elemM > 3 in ',subroutine_name
 endif
 
 do ii=1,upstream_face_per_elemM
    eMr_EtaAll(ii)          = eMr_EtaUp(ii)
    eMr_FlowrateAll(ii)     = eMr_FlowrateUp(ii)
    eMr_VelocityAll(ii)     = eMr_VelocityUp(ii)
    eMr_TimescaleAll(ii)    = eMr_TimescaleUp(ii)
    eMr_AreaAll(ii)         = eMr_AreaUp(ii) 
    eMr_TopwidthAll(ii)     = eMr_TopwidthUp(ii)
    eMr_HydDepthAll(ii)     = eMr_HydDepthUp(ii)
    eMr_LengthAll(ii)       = eMr_LengthUp(ii)
    eMr_ZbottomAll(ii)      = eMr_ZbottomUp(ii)
    eMr_BreadthScaleAll(ii) = eMr_BreadthScaleUp(ii)
    eMi_MfaceAll(ii)        = eMi_MfaceUp(ii)
 end do
 
 do ii=1,dnstream_face_per_elemM   
    kk = ii+upstream_face_per_elemM
    eMr_EtaAll(kk)          = eMr_EtaDn(ii)
    eMr_FlowrateAll(kk)     = eMr_FlowrateDn(ii)
    eMr_VelocityAll(kk)     = eMr_VelocityDn(ii)
    eMr_TimescaleAll(kk)    = eMr_TimescaleDn(ii)
    eMr_AreaAll(kk)         = eMr_AreaDn(ii) 
    eMr_TopwidthAll(kk)     = eMr_TopwidthDn(ii)
    eMr_HydDepthAll(kk)     = eMr_HydDepthDn(ii)
    eMr_LengthAll(kk)       = eMr_LengthDn(ii)
    eMr_ZbottomAll(kk)      = eMr_ZbottomDn(ii)
    eMr_BreadthScaleAll(kk) = eMr_BreadthScaleDn(ii)
    eMi_MfaceAll(kk)        = eMi_MfaceDn(ii)
end do 
 
 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** leave ',subroutine_name 
 end subroutine initialize_arrayindex
!
!==========================================================================
!========================================================================== 
!
 subroutine initialize_arrayindex_status
!
! Companion to initialize_faceindex that checks to see if the face index
! arrays were accidentally changed during code execution. Such a change
! would invalidate the indexing and create a subtle bug in the results.
!    
 character(64) :: subroutine_name =  'initialize_arrayindex_status'   
 integer :: ii
!-------------------------------------------------------------------------- 
 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** enter ',subroutine_name
 
 if (eMr_Temp(1) /= eMr_temp1) then
    print *, 'error: eMr_Temp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMr_Temp(2) /= eMr_temp2) then
    print *, 'error: eMr_Temp(2) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMr_Temp(3) /= eMr_temp3) then
    print *, 'error: eMr_Temp(3) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMr_Temp(4) /= eMr_temp4) then
    print *, 'error: eMr_Temp(4) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMr_Temp(5) /= eMr_temp5) then
    print *, 'error: eMr_Temp(5) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMr_Temp(6) /= eMr_temp6) then
    print *, 'error: eMr_Temp(6) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 
 if (e2r_Temp(1) /= e2r_temp1) then
    print *, 'error: e2r_Temp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (e2r_Temp(2) /= e2r_temp2) then
    print *, 'error: e2r_Temp(2) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (e2r_Temp(3) /= e2r_temp3) then
    print *, 'error: e2r_Temp(3) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (e2r_Temp(4) /= e2r_temp4) then
    print *, 'error: e2r_Temp(4) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (e2r_Temp(5) /= e2r_temp5) then
    print *, 'error: e2r_Temp(5) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (e2r_Temp(6) /= e2r_temp6) then
    print *, 'error: e2r_Temp(6) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 ii = 1
 if (ni_MlinkUp(ii)         /= ni_Mlink_u1) then
    print *, 'error: ni_MlinkUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (ni_MlinkDn(ii)         /= ni_Mlink_d1) then
    print *, 'error: ni_MlinkDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif

 if (eMi_MfaceUp(ii)         /= eMi_Mface_u1) then
    print *, 'error: eMi_MfaceUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMi_MfaceDn(ii)         /= eMi_Mface_d1) then
    print *, 'error: eMi_MfaceDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif

 if (eMr_EtaUp(ii)      /= eMr_Eta_u1) then
    print *, 'error: eMr_EtaUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (emr_EtaDn(ii)      /= eMr_Eta_d1) then
    print *, 'error: eMr_EtaDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif

 if (eMr_FlowrateUp(ii)      /= eMr_Flowrate_u1) then
    print *, 'error: eMr_FlowrateUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (emr_FlowrateDn(ii)      /= eMr_Flowrate_d1) then
    print *, 'error: eMr_FlowrateDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif


 if (eMr_VelocityUp(ii)      /= eMr_Velocity_u1) then
    print *, 'error: eMr_VelocityUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMr_VelocityDn(ii)      /= eMr_Velocity_d1) then
    print *, 'error: eMr_VelocityDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_TimescaleUp(ii)        /= eMr_Timescale_u1) then
    print *, 'error: eMr_TimescaleUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_TimescaleDn(ii)        /= eMr_Timescale_d1) then
    print *, 'error: eMr_TimescaleDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_AreaUp(ii)        /= eMr_Area_u1) then
    print *, 'error: eMr_AreaUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMr_AreaDn(ii)        /= eMr_Area_d1) then
    print *, 'error: eMr_AreaDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_TopwidthUp(ii)        /= eMr_Topwidth_u1) then
    print *, 'error: eMr_TopwidthUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_TopwidthDn(ii)        /= eMr_Topwidth_d1) then
    print *, 'error: eMr_TopwidthDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif 
 if (eMr_HydDepthUp(ii)        /= eMr_HydDepth_u1) then
    print *, 'error: eMr_HydDepthUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_HydDepthDn(ii)        /= eMr_HydDepth_d1) then
    print *, 'error: eMr_HydDepthDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif 
 
 
 if (eMr_LengthUp(ii)        /= eMr_Length_u1) then
    print *, 'error: eMr_LengthUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (eMr_LengthDn(ii)        /= eMr_Length_d1) then
    print *, 'error: eMr_LengthDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_ZbottomUp(ii)        /= eMr_Zbottom_u1) then
    print *, 'error: eMr_ZbottomUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_ZbottomDn(ii)        /= eMr_Zbottom_d1) then
    print *, 'error: eMr_ZbottomDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif

 if (eMr_BreadthScaleUp(ii)        /= eMr_BreadthScale_u1) then
    print *, 'error: eMr_BreadthScaleUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 
 if (eMr_BreadthScaleDn(ii)        /= eMr_BreadthScale_d1) then
    print *, 'error: eMr_BreadthScaleDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif

 if (nr_ElementLengthUp(ii) /= nr_ElementLength_u1) then
     print *, 'error: nr_ElementLengthUp(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif
 if (nr_ElementLengthDn(ii) /= nr_ElementLength_d1) then
    print *, 'error: ni_ElementLengthDn(1) unexpectedly changed in code in ',subroutine_name
    stop
 endif

! the following depends on the number of upstream faces per element allowed
 ii=2
 if (upstream_face_per_elemM > ii-1) then
    if (ni_MlinkUp(ii)          /= ni_Mlink_u2) then
        print *, 'ii = ',ii
        print *, 'error: ni_MlinkUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMi_MfaceUp(ii)          /= eMi_Mface_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMi_MfaceUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_EtaUp(ii)       /= eMr_Eta_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_EtaUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_FlowrateUp(ii)       /= eMr_Flowrate_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_FlowrateUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_VelocityUp(ii)       /= eMr_Velocity_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_VelocityUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_TimescaleUp(ii)         /= eMr_Timescale_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_TimescaleUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif      
    if (eMr_AreaUp(ii)         /= eMr_Area_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_AreaUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_TopwidthUp(ii)         /= eMr_Topwidth_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_TopwidthUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_HydDepthUp(ii)         /= eMr_HydDepth_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_HydDepthUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_LengthUp(ii)         /= eMr_Length_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_LengthUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif 
    if (eMr_ZbottomUp(ii)         /= eMr_Zbottom_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_ZbottomUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_BreadthScaleUp(ii)    /= eMr_BreadthScale_u2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_BreadthScaleUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (nr_ElementLengthUp(ii)  /= nr_ElementLength_u2) then
        print *, 'ii = ',ii
        print *, 'error: nr_ElementLengthUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
 endif
 ii=3
 if (upstream_face_per_elemM > ii-1) then
    if (ni_MlinkUp(ii)          /= ni_Mlink_u3) then
        print *, 'ii = ',ii
        print *, 'error: ni_MlinkUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMi_MfaceUp(ii)          /= eMi_Mface_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMi_MfaceUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_EtaUp(ii)       /= eMr_Eta_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_EtaUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_FlowrateUp(ii)       /= eMr_Flowrate_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_FlowrateUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_VelocityUp(ii)       /= eMr_Velocity_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_VelocityUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_TimescaleUp(ii)         /= eMr_Timescale_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_TimescaleUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_AreaUp(ii)         /= eMr_Area_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_AreaUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_TopwidthUp(ii)            /= eMr_Topwidth_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_TopwidthUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_HydDepthUp(ii)            /= eMr_HydDepth_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_HydDepthUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_LengthUp(ii)         /= eMr_Length_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_LengthUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_ZbottomUp(ii)         /= eMr_Zbottom_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_ZbottomUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (eMr_BreadthScaleUp(ii)    /= eMr_BreadthScale_u3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_BreadthScaleUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
    if (nr_ElementLengthUp(ii)  /= nr_ElementLength_u3) then
        print *, 'ii = ',ii
        print *, 'error: nr_ElementLengthUp(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif
 endif    
 if (upstream_face_per_elemM > 3) then
    print *, 'error: code does not support upstream_face_per_elemM > 3 in ',subroutine_name
 endif
 
! the following depends on the number of downstream faces per element allowed
 ii=2
 if (dnstream_face_per_elemM > ii-1) then
    if (ni_MlinkDn(ii)          /= ni_Mlink_d2) then
        print *, 'ii = ',ii
        print *, 'error: ni_MlinkDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMi_MfaceDn(ii)          /= eMi_Mface_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMi_MfaceDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_EtaDn(ii)       /= eMr_Eta_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_EtaDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_FlowrateDn(ii)       /= eMr_Flowrate_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_FlowrateDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_VelocityDn(ii)       /= eMr_Velocity_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_VelocityDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_TimescaleDn(ii)         /= eMr_Timescale_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_TimescaleDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif     
    if (eMr_AreaDn(ii)         /= eMr_Area_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_AreaDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif      
    if (eMr_TopwidthDn(ii)         /= eMr_Topwidth_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_TopwidthDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_HydDepthDn(ii)         /= eMr_HydDepth_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_HydDepthDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_LengthDn(ii)         /= eMr_Length_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_LengthDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif      
    if (eMr_ZbottomDn(ii)         /= eMr_Zbottom_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_ZbottomDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_BreadthScaleDn(ii)         /= eMr_BreadthScale_d2) then
        print *, 'ii = ',ii
        print *, 'error: eMr_BreadthScaleDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (nr_ElementLengthDn(ii)  /= nr_ElementLength_d2) then
        print *, 'ii = ',ii
        print *, 'error: nr_ElementLengthDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
 endif
 ii=3
 if (dnstream_face_per_elemM > ii-1) then
    if (ni_MlinkDn(ii)          /= ni_Mlink_d3) then
        print *, 'ii = ',ii
        print *, 'error: ni_MlinkDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMi_MfaceDn(ii)          /= eMi_Mface_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMi_MfaceDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_EtaDn(ii)       /= eMr_Eta_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_EtaDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_FlowrateDn(ii)       /= eMr_Flowrate_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_FlowrateDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_VelocityDn(ii)       /= eMr_Velocity_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_VelocityDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_TimescaleDn(ii)         /= eMr_Timescale_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_Timescale_Dn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_AreaDn(ii)         /= eMr_Area_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_AreaDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_TopwidthDn(ii)         /= eMr_Topwidth_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_TopwidthDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_HydDepthDn(ii)         /= eMr_HydDepth_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_HydDepthDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_LengthDn(ii)         /= eMr_Length_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_LengthDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_ZbottomDn(ii)         /= eMr_Zbottom_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_Zbottom_Dn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (eMr_BreadthScaleDn(ii)         /= eMr_BreadthScale_d3) then
        print *, 'ii = ',ii
        print *, 'error: eMr_BreadthScale_Dn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
    if (nr_ElementLengthDn(ii)  /= nr_ElementLength_d3) then
        print *, 'ii = ',ii
        print *, 'error: ElementLengthDn(ii) unexpectedly changed in code in ',subroutine_name
        stop
    endif    
 endif    
 if (dnstream_face_per_elemM > 3) then
    print *, 'error: code does not support dnstream_face_per_elemM > 3 in ',subroutine_name
 endif
 
 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** leave ',subroutine_name 
 end subroutine initialize_arrayindex_status
!
!========================================================================== 
!==========================================================================
!
 subroutine initialize_array_zerovalues &
    (elemMR)
! 
! array allocation stores a null value. For certain columns of 
! some arrays we want to initialize with zero. 
! 
 character(64) :: subroutine_name = 'initialize_array_zerovalues'

 real,  intent(in out)  :: elemMR(:,:) 
 
 integer    :: mm
  
!-------------------------------------------------------------------------- 
 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** enter ',subroutine_name 
 
 elemMR(:,eMr_Volume) = zeroR
 
 do mm=1,upstream_face_per_elemM
    !elemMR(:,eMr_EtaUp(mm)          = zeroR
    elemMR(:,eMr_LengthUp(mm))      = zeroR
    elemMR(:,eMr_AreaUp(mm))        = zeroR
    elemMR(:,eMr_TopwidthUp(mm))    = zeroR
    elemMR(:,eMr_HydDepthUp(mm))    = zeroR
 enddo

 do mm=1,dnstream_face_per_elemM
    !elemMR(:,eMr_EtaDn(mm))         = zeroR
    elemMR(:,eMr_LengthDn(mm))      = zeroR
    elemMR(:,eMr_AreaDn(mm))        = zeroR
    elemMR(:,eMr_TopwidthDn(mm))    = zeroR
    elemMR(:,eMr_HydDepthDn(mm))    = zeroR
 enddo

 
 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** leave ',subroutine_name
 end subroutine initialize_array_zerovalues 
!
!==========================================================================
!
 subroutine initialize_dummy_values &
    (elem2R, elem2I, elem2YN, &
     elemMR, elemMI, elemMYN, &
     faceR,  faceI,  faceYN)
 
 character(64) :: subroutine_name = 'initialize_dummy_values'
 
 integer,   intent(in out) :: elem2I(:,:),  elemMI(:,:),  faceI(:,:)
 real,      intent(in out) :: elem2R(:,:),  elemMR(:,:),  faceR(:,:)
 logical,   intent(in out) :: elem2YN(:,:), elemMYN(:,:), faceYN(:,:)
  
!-------------------------------------------------------------------------- 
 if ((debuglevel > 0) .or. (debuglevelall > 0)) print *, '*** enter ',subroutine_name 
 
 !% define the dummy location
 dummy_face_index  = N_face + 1
 dummy_elem2_index = N_elem2 + 1
 dummy_elemM_index = N_elemM + 1
 
 faceI(dummy_face_index,:)  = nullvalueI
 faceR(dummy_face_index,:)  = nullvalueR
 faceYN(dummy_face_index,:) = nullvalueL
 
 elem2I(dummy_elem2_index,:)  = nullvalueI
 elem2R(dummy_elem2_index,:)  = nullvalueR
 elem2YN(dummy_elem2_index,:) = nullvalueL 
 
 elemMI(dummy_elemM_index,:)  = nullvalueI
 elemMR(dummy_elemM_index,:)  = nullvalueR 
 elemMYN(dummy_elemM_index,:) = nullvalueL
 
 elemMI(:,eMi_MfaceUp(1:upstream_face_per_elemM)) = dummy_face_index
 elemMI(:,eMi_MfaceDn(1:dnstream_face_per_elemM)) = dummy_face_index
 
 faceI(:,fi_Melem_u) = min(dummy_elemM_index,dummy_elem2_index)
 faceI(:,fi_Melem_d) = min(dummy_elemM_index,dummy_elem2_index)
 
 elem2I(:,e2i_Mface_u) = dummy_face_index
 elem2I(:,e2i_Mface_d) = dummy_face_index
 
 if ((debuglevel > 0) .or. (debuglevelall > 0))  print *, '*** leave ',subroutine_name
 end subroutine initialize_dummy_values
!
!========================================================================== 
!==========================================================================
! END OF MODULE initialization
!========================================================================== 
 end module initialization
