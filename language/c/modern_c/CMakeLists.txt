cmake_minimum_required(VERSION 3.10)

project(ModernC VERSION 0.1)

add_definitions('-g')
add_definitions('-Wall')

add_executable(00 00.c)
target_link_libraries(00 -lm)
