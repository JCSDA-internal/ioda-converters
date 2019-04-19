# Extra macros to eliminate repetitioN

# Macro to copy list of files from source to destination
macro( COPY_FILES filelist source destination )
	foreach(FILENAME ${filelist})
	  execute_process( COMMAND ${CMAKE_COMMAND} -E copy
		 ${source}/${FILENAME}
		 ${destination}/${FILENAME} )
	endforeach(FILENAME)
endmacro()

# macro to set up targest and dependencies from a list of files
macro( SET_TARGETS_DEPS filelist source destination deplist)
    foreach( FILENAME ${filelist} )
        set ( SOURCE_FILE ${source}/${FILENAME} )
        set ( DEST_FILE ${destination}/bin/${FILENAME} )
        list( APPEND ${deplist} ${DEST_FILE} )
        add_custom_command(
            OUTPUT ${DEST_FILE}
            DEPENDS ${SOURCE_FILE}
            COMMAND cp ${SOURCE_FILE} ${DEST_FILE}
        )
    endforeach(FILENAME)
endmacro( SET_TARGETS_DEPS )
