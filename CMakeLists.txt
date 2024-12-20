cmake_minimum_required(VERSION 3.20)
project(minimx)

option(BUILD_MINIMX_EXAMPLES "Build minimx examples" OFF)

add_library(minimx INTERFACE)
add_library(minimx::minimx ALIAS minimx)
target_compile_features(minimx INTERFACE cxx_std_17)
target_include_directories(minimx INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(3rdparty/pugixml)
target_link_libraries(minimx INTERFACE pugixml)

if(BUILD_MINIMX_EXAMPLES)
    add_executable(parsemx example/parsemx.cpp)
    target_link_libraries(parsemx PRIVATE minimx)
endif()


