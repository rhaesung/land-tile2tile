program vector2tile_converter

  use namelist_mod
  use restart_converter_mod
  implicit none

  type(namelist_type)  :: namelist
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Get namelist file name from command line
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call get_command_argument(1, namelist%namelist_name)
  if(namelist%namelist_name == "") then 
        print *,  "add namelist to the command line: "
        stop 10  
  endif
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Read namelist information and create date string
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call ReadNamelist(namelist)

  print*, "conversion direction: ",namelist%direction
  
  if(namelist%direction /= "tile2vector" .and. namelist%direction /= "vector2tile" ) then
    print*, "conversion direction: ",namelist%direction, " not recognized"
    stop 10 
  end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Decide the pathway
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  select case (namelist%direction)
  
    case ("tile2vector" : "vector2tile")

      write(*,*) "Option: "//trim(namelist%direction)
      call vector2tile_restart(namelist)
    
    case default
    
      write(*,*) "choose a valid conversion direction"
  
  end select 
end program vector2tile_converter
