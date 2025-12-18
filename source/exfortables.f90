program exfortables
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : Translate EXFOR database into x-y tables
!
! Author    : Arjan Koning
! 2025-12-18: Current revision
!-----------------------------------------------------------------------------------------------------------------------------------
!
!   |-------------------------------------------------------|
!   |                 EXFORTABLES-2.2                       |
!   |                 Arjan Koning                          |
!   |                                                       |
!   | Email: A.Koning@@iaea.org                             |
!   |-------------------------------------------------------|
!
! MIT License
!
! Copyright (c) 2025 Arjan Koning
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! use A1_exfortables_mod, only: & ! All global variables
!              flagtables, & ! flag for new database
!              flagstat      ! flag for statistics
!
! ********** Input, initialization, creation and test of database ******
!
! alloc         : subroutine to allocate arrays
! machine       : subroutine for machine dependent statements
! constants     : subroutine for constants
! tablesinput   : subroutine for input
! tablesinitial : subroutine for initialization
! processxc5    : subroutine to process XC5 data into tables
! statistics    : subroutine for statistics of EXFOR analysis and TALYS+library comparison
! mainout       : subroutine for main output
! dealloc       : subroutine to de-allocate arrays
! timer         : subroutine for output of execution time
!
  call alloc
  call machine
  call constants
  call tablesinput
  call tablesinitial
  if (flagtables) call processxc5
  if (flagstat) call statistics
  call mainout
  call dealloc
  call timer
end program exfortables
! Copyright A.J. Koning 2025
