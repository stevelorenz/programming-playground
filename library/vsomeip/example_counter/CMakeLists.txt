cmake_minimum_required(VERSION 2.8.7)
project (ExampleCounter)

set(CMAKE_CXX_FLAGS "-g -std=c++0x")

find_package(vsomeip3 REQUIRED)
find_package(Boost 1.55 COMPONENTS system thread log REQUIRED)

include_directories (
    ${Boost_INCLUDE_DIR}
    ${VSOMEIP_INCLUDE_DIRS}
)

add_executable(service-example service-example.cpp)
target_link_libraries(service-example ${VSOMEIP_LIBRARIES} ${Boost_LIBRARIES})

add_executable(client-example client-example.cpp)
target_link_libraries(client-example ${VSOMEIP_LIBRARIES} ${Boost_LIBRARIES})
