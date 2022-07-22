# Works with 3.15 and tested through 3.21
cmake_minimum_required(VERSION 3.15...3.21)

project(
	ModernCMakeExample
	VERSION 0.1
	DESCRIPTION "A CMake example"
	LANGUAGES CXX)

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
	set(CMAKE_CXX_EXTENSIONS OFF)

	# Support folders in IDE
	set_property(GLOBAL PROPERTY USE_FOLDERS ON)

	include(CTest)

	find_package(Doxygen)
	if(Doxygen_FOUND)
		add_subdirectory(docs)
	else()
		message(STATUS "Doxygen not found, not building docs")
	endif()

endif()


include(FetchContent)

find_package(Boost REQUIRED)

FetchContent_Declare(
	fmtlib
	GIT_REPOSITORY https://github.com/fmtlib/fmt.git
	GIT_TAG 8.0.1)
FetchContent_MakeAvailable(fmtlib)

add_subdirectory(src)
add_subdirectory(apps)
add_subdirectory(tests)
