# HEADER_LIST is required by IDE for header information
set(HEADER_LIST "${ModernCMakeExample_SOURCE_DIR}/include/modern/lib.hpp")
add_library(modern_library lib.cpp ${HEADER_LIST})

# The header directory must be added publicly for users of this library
target_include_directories(modern_library PUBLIC ../include)

target_link_libraries(modern_library PRIVATE Boost::boost)
# The user's of the library MUST at least use C++11
target_compile_features(modern_library PUBLIC cxx_std_11)

source_group(
	TREE "${PROJECT_SOURCE_DIR}/include"
	PREFIX "Header Files"
	FILES ${HEADER_LIST})
