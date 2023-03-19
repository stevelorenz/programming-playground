add_executable(procfs_pidmax procfs_pidmax.c)
target_link_libraries(procfs_pidmax tlpi)

add_executable(t_uname t_uname.c)
target_link_libraries(t_uname tlpi)
