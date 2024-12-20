cmake_minimum_required(VERSION 3.20)
project(minimx)

option(BUILD_MINIMX_EXAMPLES "Build minimx examples" OFF)
option(BUILD_MINIMX_TESTS "Build minimx tests" OFF)

add_library(minimx INTERFACE)
add_library(minimx::minimx ALIAS minimx)
target_compile_features(minimx INTERFACE cxx_std_17)
target_include_directories(minimx INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

add_subdirectory(3rdparty/pugixml)
target_link_libraries(minimx INTERFACE pugixml)

if (BUILD_MINIMX_EXAMPLES)
    add_executable(parsemx example/parsemx.cpp)
    target_link_libraries(parsemx PRIVATE minimx)
endif ()

if (BUILD_MINIMX_TESTS)
    Include(FetchContent)
    FetchContent_Declare(
            Catch2
            GIT_REPOSITORY https://github.com/catchorg/Catch2.git
            GIT_TAG v3.4.0 # or a later release
    )
    FetchContent_MakeAvailable(Catch2)
    list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/extras)
    add_executable(minimx_parser_test test/parser_tests.cpp)
    target_link_libraries(minimx_parser_test PRIVATE Catch2::Catch2WithMain minimx)
    include(CTest)
    include(Catch)
    catch_discover_tests(minimx_parser_test)

    set(TEST_DATA_DIR ${CMAKE_CURRENT_SOURCE_DIR}/test/data)
    target_compile_definitions(minimx_parser_test PRIVATE TEST_DATA_DIR="${TEST_DATA_DIR}")
endif ()


