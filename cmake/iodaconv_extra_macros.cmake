# Extra macros to eliminate repetition

# Macro to copy list of files from source to destination
macro( COPY_FILES filelist source destination )
  foreach(FILENAME ${filelist})
    execute_process( COMMAND ${CMAKE_COMMAND} -E copy
      ${source}/${FILENAME}
      ${destination}/${FILENAME}
    )
  endforeach(FILENAME)
endmacro()

# macro to set up target and dependencies from a list of files
macro( SET_TARGETS_DEPS filelist source destination deplist)
  foreach( FILENAME ${filelist} )
    set ( SOURCE_FILE ${source}/${FILENAME} )
    set ( DEST_FILE ${destination}/${FILENAME} )
    list( APPEND ${deplist} ${DEST_FILE} )
    add_custom_command(
      OUTPUT ${DEST_FILE}
      DEPENDS ${SOURCE_FILE}
      COMMAND ${CMAKE_COMMAND} -E make_directory ${destination}
      COMMAND ${CMAKE_COMMAND} -E copy ${SOURCE_FILE} ${DEST_FILE}
    )
  endforeach(FILENAME)
endmacro( SET_TARGETS_DEPS )

# macro to configure target dependencies from a list of files
macro( CONF_TARGETS_DEPS filelist source destination deplist)
  foreach( FILENAME ${filelist} )
    set ( SOURCE_FILE ${source}/${FILENAME} )
    set ( DEST_FILE ${destination}/${FILENAME} )
    list( APPEND ${deplist} ${DEST_FILE} )
    if( EXISTS "${SOURCE_FILE}.in" )
        configure_file( ${SOURCE_FILE}.in ${DEST_FILE} @ONLY )
    else()
        configure_file( ${SOURCE_FILE}    ${DEST_FILE} @ONLY )
    endif()
  endforeach(FILENAME)
endmacro( CONF_TARGETS_DEPS )
