cmake_minimum_required (VERSION 3.13)
project(ExampleCounter)

set(CMAKE_CXX_FLAGS "-g")

find_package(Boost 1.55 COMPONENTS system thread log REQUIRED)
find_package(vsomeip3 REQUIRED)

include_directories (
    ${Boost_INCLUDE_DIR}
    ${VSOMEIP_INCLUDE_DIRS}
)

add_executable(service-example common.cpp service-example.cpp)
target_link_libraries(service-example ${VSOMEIP_LIBRARIES} ${Boost_LIBRARIES})

add_executable(client-example common.cpp client-example.cpp)
target_link_libraries(client-example ${VSOMEIP_LIBRARIES} ${Boost_LIBRARIES})

add_executable(subscriber-example common.cpp subscriber-example.cpp)
target_link_libraries(subscriber-example ${VSOMEIP_LIBRARIES} ${Boost_LIBRARIES})
