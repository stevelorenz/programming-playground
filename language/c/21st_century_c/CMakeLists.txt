cmake_minimum_required(VERSION 3.10)

project(21stCenturyC VERSION 0.1)

set(TEST_VARIABLE "Test Variable")
message(STATUS "${TEST_VARIABLE}")

# The cached variable can be modified from CLI or CMake GUI tools
set(MY_CACHE_VAR "A cached variable" CACHE STRING "Description")
message(STATUS "${MY_CACHE_VAR}")

add_definitions('-Wall')
add_definitions('-g')
add_definitions('-std=gnu11')

add_executable(erf erf.c)
target_link_libraries(erf m)

add_executable(fibo fibo.c)

add_executable(getenv getenv.c)

add_executable(gsl_erf gsl_erf.c)
find_package(GSL REQUIRED)
target_link_libraries(gsl_erf GSL::gsl GSL::gslcblas)

###########
#  Tests  #
###########

