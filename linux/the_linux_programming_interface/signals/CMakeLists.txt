add_executable(ouch ouch.c)
target_link_libraries(ouch tlpi)

add_executable(intquit intquit.c)
target_link_libraries(intquit tlpi)

add_executable(t_kill t_kill.c)
target_link_libraries(t_kill tlpi)

add_executable(sig_sender sig_sender.c)
target_link_libraries(sig_sender tlpi)
