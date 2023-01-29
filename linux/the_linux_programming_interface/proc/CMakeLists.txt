add_executable(display_env display_env.c)
target_link_libraries(display_env tlpi)

add_executable(modify_env modify_env.c)
target_link_libraries(modify_env tlpi)
