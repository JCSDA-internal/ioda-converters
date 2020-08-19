# Extra macros to eliminate repetition

# Macro to copy list of files from source to destination
macro( copy_files filelist source destination )
  foreach(FILENAME ${filelist})
    execute_process( COMMAND ${CMAKE_COMMAND} -E copy
      ${source}/${FILENAME}
      ${destination}/${FILENAME}
    )
  endforeach()
endmacro()

# macro to set up target and dependencies from a list of files
macro( set_targets_deps filelist source destination deplist)
  foreach( FILENAME ${filelist} )
    set ( SOURCE_FILE ${source}/${FILENAME} )
    set ( DEST_FILE ${destination}/${FILENAME} )
    list( APPEND ${deplist} ${DEST_FILE} )
    configure_file( ${SOURCE_FILE} ${DEST_FILE} COPYONLY )
  endforeach()
endmacro()

# macro to configure target dependencies from a list of files
macro( conf_targets_deps filelist source destination deplist)
  foreach( FILENAME ${filelist} )
    set ( SOURCE_FILE ${source}/${FILENAME} )
    set ( DEST_FILE ${destination}/${FILENAME} )
    list( APPEND ${deplist} ${DEST_FILE} )
    if( EXISTS "${SOURCE_FILE}.in" )
        configure_file( ${SOURCE_FILE}.in ${DEST_FILE} @ONLY )
    else()
        configure_file( ${SOURCE_FILE}    ${DEST_FILE} @ONLY )
    endif()
  endforeach()
endmacro()

