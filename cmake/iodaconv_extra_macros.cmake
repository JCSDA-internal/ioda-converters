# Extra macros to eliminate repetitioN

# Macro to copy list of files from source to destination
macro( COPY_FILES filelist source destination )
	foreach(FILENAME ${filelist})
	  execute_process( COMMAND ${CMAKE_COMMAND} -E copy
		 ${source}/${FILENAME}
		 ${destination}/${FILENAME} )
	endforeach(FILENAME)
endmacro()
