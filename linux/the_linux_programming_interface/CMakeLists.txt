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

project(TheLinuxProgrammingInterface
	VERSION 0.1
	LANGUAGES C)

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
		-pg")
	else()
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} \
		-O0 \
		-Wno-unused \
		-pg")
	endif()
else()
	message("${Yellow}Release mode is used!")
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} \
	-O3")
endif()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

enable_testing()

##################
#  Dependencies  #
##################
include(FetchContent)

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

####################
#  Subdirectories  #
####################

add_subdirectory(lib)
add_subdirectory(mmap)
