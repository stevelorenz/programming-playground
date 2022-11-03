add_executable(mmcat mmcat.c)
target_link_libraries(mmcat tlpi)
add_test(NAME mmcat
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/mmcat /etc/hosts)
