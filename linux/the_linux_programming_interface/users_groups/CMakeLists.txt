add_library(ugid ugid_functions.c)
target_include_directories(ugid PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(ugid tlpi)
