add_executable(copy copy.c)
target_link_libraries(copy tlpi)

add_executable(seek_io seek_io.c)
target_link_libraries(seek_io tlpi)

add_executable(bad_exclusive_open bad_exclusive_open.c)
target_link_libraries(bad_exclusive_open tlpi)