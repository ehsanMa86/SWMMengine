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
! 2019-11-11 ==> contributed by Sazzad Sharior
!==========================================================================
program Plot

use postProcessing

    integer                                 :: iunit = 21, specific_linkElement
    integer                                 :: istat
    integer                                 :: n_cells
    integer                                 :: n_links
    integer                                 :: max_linkItems
    integer                                 :: n_timeSteps
    integer                                 :: specific_link
    integer, dimension(:), allocatable      :: time_steps, data_idx, n_linkItems
    integer, dimension(:), allocatable      :: length_idx 
    real, dimension(:,:), allocatable       :: link_data
    real, dimension(:,:), allocatable       :: link_lengths        
    
    real, dimension(:,:), allocatable       :: specific_linkData
    real, dimension(:)  , allocatable       :: xx,yy


open(newunit=iunit, file='/home/saz/git_repo/SWMMengine/OutputThreaded/out_depth__20191111_1407.txt', status='OLD')

specific_link = 3
call get_specific_link_data &
    (iunit, n_cells, n_links, n_linkItems, max_linkItems, n_timeSteps, &
    time_steps, data_idx, length_idx, link_lengths, link_data, &
    specific_link, specific_linkData)

specific_linkElement = 10
xx = time_steps
yy = specific_linkData(:,specific_linkElement)

open (unit = 7, action = 'write', file = 'data.txt', status = 'replace')
do i = 1,n_timeSteps
    write(7,*)xx(i), yy(i)
end do

call system('gnuplot -p plot.plt')
close(7, status = 'delete')

end program Plot
