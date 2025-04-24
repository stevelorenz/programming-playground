add_executable(test_script script.c pty_fork.c pty_master_open.c tty_functions.c)
target_link_libraries(test_script tlpi)
