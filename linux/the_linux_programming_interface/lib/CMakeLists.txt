file(GLOB src "*.c")
add_library(tlpi ${src})
target_include_directories(tlpi PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
