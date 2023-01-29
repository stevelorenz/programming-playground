add_executable(copy copy.c)
target_link_libraries(copy tlpi)

add_executable(t_readv t_readv.c)
target_link_libraries(t_readv tlpi)
