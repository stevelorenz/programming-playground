# This is also a playground for me to play with Cmake...

##############################
#  Configuration and Checks  #
##############################

cmake_minimum_required(VERSION 3.10)

if(NOT WIN32)
  string(ASCII 27 Esc)
  set(ColourReset "${Esc}[m")
  set(Red         "${Esc}[31m")
  set(Green       "${Esc}[32m")
  set(Yellow      "${Esc}[33m")
  set(Blue        "${Esc}[34m")
  set(Magenta     "${Esc}[35m")
  set(Cyan        "${Esc}[36m")
  set(White       "${Esc}[37m")
endif()

project(21stCenturyC 
	VERSION 0.1
	LANGUAGES C CXX)

message("${Green}CMake version: ${CMAKE_VERSION}")
message("${Green}Current shell: $ENV{SHELL}")

if(PROJECT_SOURCE_DIR STREQUAL PROJECT_BINARY_DIR)
	message(FATAL_ERROR "In-source builds are not allowed")
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
	message("${Green}The system is Linux: ${CMAKE_SYSTEM_VERSION}! Great!")
else()
	message(FATAL_ERROR "Code is only tested on Linux!")
endif()

cmake_host_system_information(RESULT PROCESSOR_DESCRIPTION QUERY PROCESSOR_DESCRIPTION)
message("${Green}Processor description: ${PROCESSOR_DESCRIPTION}")

cmake_host_system_information(RESULT RET QUERY IS_64BIT)
if(RET)
	message("${Green}The processor is 64 bit!")
else()
	message(FATAL_ERROR "Code is ONLY written for 64 bit processor")
endif()

include(TestBigEndian)
TEST_BIG_ENDIAN(IS_BIG_ENDIAN)
if(IS_BIG_ENDIAN)
	message(STATUS "${Green}System uses big endian")
else()
	message("${Green}System uses small endian")
endif()

include(CheckIPOSupported)
check_ipo_supported(RESULT ipo_supported)
if(ipo_supported)
	message("${Green}IPO is supported and enabled")
	set(CMAKE_INTERPROCEDURAL_OPTIMIZATION True)
endif()

list(APPEND CMAKE_MODULE_PATH
	"${CMAKE_SOURCE_DIR}/cmake/module")

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

SET(CMAKE_BUILD_TYPE "Debug")
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} \
		-O0 \
		-Wall -Wextra -Wpedantic -Werror \
		-fsanitize=address -fsanitize=leak -fsanitize=undefined")
endif()

try_run(run_result compile_result
	${CMAKE_CURRENT_BINARY_DIR}/quick_test
	${CMAKE_CURRENT_SOURCE_DIR}/quick_test.c
	RUN_OUTPUT_VARIABLE output)
# Assume every variable is a string, right? :)
if("${run_result}" STREQUAL "0" AND "${compile_result}" STREQUAL "TRUE")
else()
	message(FATAL_ERROR "Failed to run the quick test")
endif()

##################
#  Dependencies  #
##################
include(FetchContent)

# yaml-cpp ---------------------------------------------------------------------
find_package(yaml-cpp REQUIRED)
# ONLY use git submodule when yaml-cpp is NOT found in current system libraries
if(NOT yaml-cpp_FOUND)
	find_package(Git REQUIRED)
	if(NOT Git_FOUND)
		message(FATAL_ERROR "Git command not found! Can not use git submodule!")
	endif()

	message("${Red} yaml-cpp is not found! Init the git submodule")
	execute_process(
		COMMAND git submodule update --init -- extern/yaml-cpp
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)
	add_subdirectory(extern/yaml-cpp)
endif()

# GoogleTest -------------------------------------------------------------------
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.12.1
)
FetchContent_MakeAvailable(googletest)

#######################
#  Interface Targets  #
#######################

add_library(warning_props INTERFACE)
target_compile_options(warning_props INTERFACE
	-Wall -Wextra -Wpedantic
)

add_library(sanitizer_props INTERFACE
)
target_compile_options(sanitizer_props INTERFACE
	-fsanitize=address -fsanitize=leak -fsanitize=undefined
)

##############
#  Examples  #
##############

enable_testing()
configure_file(run_tests.sh run_tests.sh COPYONLY)

add_executable(test_cpp test_cpp.cpp)
add_test(NAME test_cpp_test
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/test_cpp)
target_link_libraries(test_cpp PRIVATE yaml-cpp GTest::gtest_main GTest::gmock)
include(GoogleTest)
gtest_discover_tests(test_cpp)
# Copy YAML file into the build directory
configure_file(test_config.yaml test_config.yaml COPYONLY)

add_executable(erf erf.c)
target_link_libraries(erf m warning_props sanitizer_props)
# Try to use precompile headers
target_precompile_headers(erf PRIVATE <math.h> <stdio.h>)
add_test(NAME erf_test
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/erf)

add_executable(fibo fibo.c)
add_test(NAME fibo
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/fibo)

add_executable(getenv getenv.c)
add_test(NAME getenv
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/getenv)

add_executable(gsl_erf gsl_erf.c)
find_package(GSL REQUIRED)
target_link_libraries(gsl_erf GSL::gsl GSL::gslcblas)
add_test(NAME gsl_erf
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/gsl_erf)


add_custom_target(clean_stale_coverage_files ALL
	COMMAND find ./ -name "*.gcda" -type f -delete)

add_custom_target(checksum ALL
	COMMAND sh -c "cksum gsl_erf>gsl_erf.ck"
	BYPRODUCTS gsl_erf.ck)
# Make sure that checksum is built AFTER gsl_erf is built
add_dependencies(checksum gsl_erf)

#######################
#  Test CMake Itself  #
#######################

message("")
message("${Yellow}[WARN]Following outputs are only used to learn and test CMake itself")
message("${Yellow}--------------------------------------------------------------------")

function(Inner)
	message(" > Inner: ${V}")
	set(V 3)
	message(" < Inner: ${V}")
endfunction()

function(Outer)
	message(" > Outer: ${V}")
	set(V 2)
	Inner()
	message(" < Outer: ${V}")
endfunction()

# set(V 1)
# message("> Global: ${V}")
# Outer()
# message("< Global: ${V}")

if(XXX_NOTFOUND)
	message("Print inside a \"if\"")
endif()

if(1.3.4 VERSION_GREATER_EQUAL 1.4)
	message("Version 1.3.4 is OK!")
endif()

# SET(MY_LIST 1;2;3)
# foreach(VAR IN LISTS MY_LIST ITEMS 4;5)
# 	message("${VAR}")
# endforeach()

macro(MyMacro myVar)
	set(myVar "New Value")
	message("Argument: ${myVar}")
endmacro()

# set(myVar "First Value")
# message("myVar is now: ${myVar}")
# MyMacro("called value")
# message("myVar is now: ${myVar}")

function(MyFunction FirstArg)
	message("Function: ${CMAKE_CURRENT_FUNCTION}")
	message("File: ${CMAKE_CURRENT_LIST_FILE}")
	message("ARGV0: ${ARGV0}, ARGV1: ${ARGV1}")
endfunction()

MyFunction("Value1" "Value2")

find_package(Protobuf QUIET)

find_package(GTest QUIET)

find_package(PkgConfig REQUIRED)
pkg_check_modules(PQXX QUIET IMPORTED_TARGET libpqxx)
find_package(PQXX REQUIRED)
message("${Green}Found the library PQXX: " ${PQXX_FOUND})

message("${Yellow}--------------------------------------------------------------------")
message("")
