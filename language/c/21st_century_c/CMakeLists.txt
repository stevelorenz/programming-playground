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

if(CMAKE_SYSTEM_NAME STREQUAL "Windows")
	message(FATAL_ERROR "Code has NOT been tested on Windows!")
else()
	message("${Green}Current OS: ${CMAKE_SYSTEM_NAME}")
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

# Set different compiler flags for debug and release modes
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
	message("${Yellow}Debug mode is used!!!")
	if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
		message("${Yellow}Enable sanitizers on Linux")
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} \
		-O0 \
		-Wall -Wextra -Werror \
		-Wno-unused \
		-fsanitize=address -fsanitize=leak -fsanitize=undefined \
		-Wno-dev \
		-pg")
	else()
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} \
		-O0 \
		-Wno-unused \
		-Wno-dev \
		-pg")
	endif()
else()
	message("${Yellow}Release mode is used!")
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} \
	-O3 \
	-Wno-dev")
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
find_package(yaml-cpp)
# ONLY use git submodule when yaml-cpp is NOT found in current system libraries
if(NOT yaml-cpp_FOUND)
	find_package(Git REQUIRED)
	if(NOT Git_FOUND)
		message(FATAL_ERROR "Git command not found! Can not use git submodule!")
	endif()

	execute_process(
		COMMAND git submodule update --init -- extern/yaml-cpp
		WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
	)
	add_subdirectory(extern/yaml-cpp)
endif()

# GoogleTest -------------------------------------------------------------------
find_package(GTest)
if(NOT GTest_FOUND)
	# MARK: FetchContent could take a long time if your Internet connection to GitHub is bad... (e.g. in China mainland)
	FetchContent_Declare(
		googletest
		GIT_REPOSITORY https://github.com/google/googletest.git
		GIT_TAG release-1.12.1
		)
	FetchContent_MakeAvailable(googletest)
endif()

# Glib -------------------------------------------------------------------------
find_package(PkgConfig)
pkg_search_module(GLIB REQUIRED glib-2.0)
include_directories(${GLIB_INCLUDE_DIRS})
link_directories(${GLIB_LIBRARY_DIRS})
# Not sure why, but include ${GLIB_INCLUDE_DIRS} does not include the glib installed
# by homebrew on macOS... So I added it manually.
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
	include_directories("/opt/homebrew/include")
endif()

#######################
#  Interface Targets  #
#######################

add_library(warning_props INTERFACE)
target_compile_options(warning_props INTERFACE
	-Wall -Wextra -Wpedantic
)

# Sanitizers are not included directly in CMAKE_C_FLAGS because even your own code works
# perfectly with sanitizers, but the used dependencies may not work with sanitizers...
add_library(sanitizer_props INTERFACE
)
target_compile_options(sanitizer_props INTERFACE
	-fsanitize=address -fsanitize=leak -fsanitize=undefined
)

##############
#  Examples  #
##############

enable_testing()
include_directories("${CMAKE_CURRENT_BINARY_DIR}")
configure_file(run_tests.sh run_tests.sh COPYONLY)
configure_file(run_cpack.sh run_cpack.sh COPYONLY)

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/config.h.in" "${CMAKE_CURRENT_BINARY_DIR}/config.h")

add_executable(test_cpp test_cpp.cpp)
add_test(NAME test_cpp_test
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/test_cpp)
# Use generator expression to make the target relocatable
target_include_directories(test_cpp INTERFACE 
	# BUILD_INTERFACE will only include for build stage
	"$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/>"
	# INSTALL_INTERFACE will only include for the install stage
	"$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
)
set_target_properties(test_cpp PROPERTIES
	PUBLIC_HEADER ./test_cpp.h
)
target_link_libraries(test_cpp PRIVATE yaml-cpp GTest::gtest_main GTest::gmock)
include(GoogleTest)
gtest_discover_tests(test_cpp)
# Copy YAML file into the build directory
configure_file(test_config.yaml test_config.yaml COPYONLY)

add_executable(erf erf.c)
target_link_libraries(erf m)
# Try to use precompile headers
target_precompile_headers(erf PRIVATE <math.h> <stdio.h>)
target_link_libraries(erf)
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

add_executable(stddev stddev.c)
target_link_libraries(stddev m)
add_test(NAME stddev
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/stddev)

add_executable(test_gdb test_gdb.c)
target_link_libraries(test_gdb ${GLIB_LIBRARIES})
add_test(NAME test_gdb
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/test_gdb)

add_executable(dict_test dict_test.c)
target_link_libraries(dict_test ${GLIB_LIBRARIES})
add_test(NAME dict_test
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/dict_test)

add_executable(structcopy structcopy.c)
target_link_libraries(structcopy)
add_test(NAME structcopy
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/structcopy)

add_executable(noreturn noreturn.c)
target_link_libraries(noreturn)

add_executable(dynamic dynamic.c)
target_link_libraries(dynamic m readline)

add_executable(uint uint.c)
add_test(NAME uint
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/uint)

add_executable(strtod strtod.c)
target_include_directories(strtod PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

add_executable(curly curly.c)
add_test(NAME curly
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/curly)

add_executable(preprocess preprocess.c)
target_link_libraries(preprocess m)
add_test(NAME preprocess
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/preprocess)

add_executable(sizesof sizesof.c)
add_test(NAME sizesof
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/sizesof)

add_executable(constchange constchange.c)
add_test(NAME constchange
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/constchange)

add_executable(conststruct conststruct.c)
add_test(NAME conststruct
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/conststruct)

add_executable(sadstrings sadstrings.c)

add_executable(getstrings getstrings.c)

add_executable(asprintf asprintf.c)
add_test(NAME asprintf
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/asprintf)

add_executable(sasprintf sasprintf.c)
add_test(NAME sasprintf
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/sasprintf)

add_executable(varad varad.c)
add_test(NAME varad
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/varad)

add_executable(vectorize vectorize.c)
add_test(NAME vectorize
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/vectorize)

add_executable(papersize papersize.c)
add_test(NAME papersize
	COMMAND ${CMAKE_CURRENT_BINARY_DIR}/papersize)


add_custom_target(clean_stale_coverage_files ALL
	COMMAND find ./ -name "*.gcda" -type f -delete)

add_custom_target(checksum ALL
	COMMAND sh -c "cksum gsl_erf>gsl_erf.ck"
	BYPRODUCTS gsl_erf.ck)
# Make sure that checksum is built AFTER gsl_erf is built
add_dependencies(checksum gsl_erf)

##############################
#  Exports and Installation  #
##############################

set(EXPORT_DIR "${CMAKE_CURRENT_BINARY_DIR}/cmake")
export(TARGETS test_cpp erf fibo
	FILE "${EXPORT_DIR}/ExportTargets.cmake"
)

include(CMakePackageConfigHelpers)
write_basic_package_version_file(
	"${CMAKE_CURRENT_BINARY_DIR}/TestCppConfigVersion.cmake"
	COMPATIBILITY SameMajorVersion
	)

include(GNUInstallDirs)
# I'm not sure if I use this install command correctly :see_no_evil:
install(TARGETS test_cpp
	EXPORT TestCppTargets
	ARCHIVE
	PUBLIC_HEADER
	DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/test_cpp
)
install(EXPORT TestCppTargets
	DESTINATION ${CMAKE_INSTALL_LIBDIR}/test_cpp/cmake)

install(FILES
	"${CMAKE_CURRENT_SOURCE_DIR}/test_config.yaml"
	# This should be prefix + share directory
	DESTINATION ${CMAKE_INSTALL_DATADIR}
)

install(FILES "TestCppConfig.cmake"
	"${CMAKE_CURRENT_BINARY_DIR}/TestCppConfigVersion.cmake"
	DESTINATION ${CMAKE_INSTALL_LIBDIR}/test_cpp/cmake
	)

###################
#  Documentation  #
###################

function(Doxygen input output)
	find_package(Doxygen)
	if (NOT DOXYGEN_FOUND)
		message(FATAL_ERROR "Doxygen not found!")
	endif()
	set(DOXYGEN_GENERATE_HTML YES)
	set(DOXYGEN_HTML_OUTPUT
		${PROJECT_BINARY_DIR}/${output})
	doxygen_add_docs(doxygen
		${PROJECT_SOURCE_DIR}/${input}
		COMMENT "Generate HTML documentation with Doxygen"
		)
endfunction()

Doxygen("" doc)

#####################
#  CPack Packaging  #
#####################

set(CPACK_PACKAGE_VENDOR "Zuo Xiang")
set(CPACK_PACKAGE_CONTACT "xianglinks@gmail.com")
include(CPack)

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

message("${Yellow}--------------------------------------------------------------------")
message("")
