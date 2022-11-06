add_executable(mmcat mmcat.c)
target_link_libraries(mmcat tlpi)
add_test(NAME mmcat
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/mmcat /etc/hosts)

add_executable(t_mmap t_mmap.c)
target_link_libraries(t_mmap tlpi)

add_executable(anon_mmap anon_mmap.c)
target_link_libraries(anon_mmap tlpi)
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
	target_compile_definitions(anon_mmap PUBLIC USE_MAP_ANON)
endif()
add_test(NAME anon_mmap
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/anon_mmap)