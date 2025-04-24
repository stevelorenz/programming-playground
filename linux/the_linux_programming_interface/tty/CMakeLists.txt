add_executable(test_new_intr new_intr.c)
target_link_libraries(test_new_intr tlpi)

add_executable(test_no_echo no_echo.c)
target_link_libraries(test_no_echo tlpi)

add_executable(test_tty_functions test_tty_functions.c tty_functions.c)
target_link_libraries(test_tty_functions tlpi)

add_executable(test_demo_SIGWINCH demo_SIGWINCH.c)
target_link_libraries(test_demo_SIGWINCH tlpi)
