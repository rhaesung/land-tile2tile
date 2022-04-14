module namelist_mod
  implicit none

  type namelist_type
    character*256      :: namelist_name = ""
    character*11       :: direction = ""
    character*256      :: tile_path = ""
    integer            :: tile_size
    character*19       :: restart_date = ""
    character*256      :: vector_restart_path = ""
    character*256      :: tile_restart_path = ""
    character*256      :: output_path = ""
    character*256      :: static_filename = ""
  end type namelist_type

contains

  subroutine ReadNamelist(namelist)

    type(namelist_type) :: namelist
    character*11        :: direction
    character*256       :: tile_path
    integer             :: tile_size
    character*19        :: restart_date
    character*256       :: vector_restart_path
    character*256       :: tile_restart_path
    character*256       :: output_path
    character*256       :: static_filename

    namelist / run_setup  / direction, tile_path, tile_size, restart_date, vector_restart_path, tile_restart_path, output_path, static_filename

    open(30, file=namelist%namelist_name, form="formatted")
     read(30, run_setup)
    close(30)

    namelist%direction           = direction
    namelist%tile_path           = tile_path
    namelist%tile_size           = tile_size
    namelist%restart_date        = restart_date
    namelist%vector_restart_path = vector_restart_path
    namelist%tile_restart_path   = tile_restart_path
    namelist%output_path         = output_path
    namelist%static_filename     = static_filename
  end subroutine ReadNamelist

end module namelist_mod
